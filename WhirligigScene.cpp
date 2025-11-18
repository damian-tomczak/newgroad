#include "WhirligigScene.h"
#include <windowsx.h>

#include <d3dcompiler.h>
#include <algorithm>
#include <cmath>
#include <cstring>

using namespace DirectX;
using namespace std::chrono;

WhirligigScene::WhirligigScene()
    : trajCount(0)
    , trajStart(0)
    , trajVerts(nullptr)
    , gridVertexCount(0)
    , axesVertexCount(0)
    , width(1280)
    , height(720)
    , camYaw(XM_PIDIV4)
    , camPitch(XM_PIDIV4 * 0.5f)
    , camDist(4.0f)
    , camTarget{ 0.0f, 0.0f, 0.0f }
    , rotating(false)
    , panning(false)
    , lastMouse{ 0, 0 }
    , running(false)
    , showCube(true)
    , showDiagonal(true)
    , showTrajectory(true)
    , showPlane(true)
    , showAxes(true)
    , useGravity(true)
    , cubeSize(1.0f)
    , density(1.0f)
    , inflectionDeg(20.0f)
    , omegaMag(3.0f)
    , speed(10.0f)
    , dt(0.01f)
    , trajLength(1200)
    , mass(1.0f)
    , rCM{ 0.5f, 0.5f, 0.5f }
    , Q(XMQuaternionIdentity())
    , omega(XMVectorZero())
    , G(9.81f)
    , haveLast(false)
{
    I = XMFLOAT3X3{};
    Iinv = XMFLOAT3X3{};
}

XMFLOAT3X3 WhirligigScene::Inverse3x3(const XMFLOAT3X3& A) {
    float a11 = A._11, a12 = A._12, a13 = A._13;
    float a21 = A._21, a22 = A._22, a23 = A._23;
    float a31 = A._31, a32 = A._32, a33 = A._33;

    float det =
        a11 * (a22 * a33 - a23 * a32) -
        a12 * (a21 * a33 - a23 * a31) +
        a13 * (a21 * a32 - a22 * a31);

    XMFLOAT3X3 R{};
    if (std::fabs(det) < 1e-9f) {
        return R; // zero matrix if singular
    }

    float id = 1.0f / det;
    R._11 = (a22 * a33 - a23 * a32) * id;
    R._12 = -(a12 * a33 - a13 * a32) * id;
    R._13 = (a12 * a23 - a13 * a22) * id;
    R._21 = -(a21 * a33 - a23 * a31) * id;
    R._22 = (a11 * a33 - a13 * a31) * id;
    R._23 = -(a11 * a23 - a13 * a21) * id;
    R._31 = (a21 * a32 - a22 * a31) * id;
    R._32 = -(a11 * a32 - a12 * a31) * id;
    R._33 = (a11 * a22 - a12 * a21) * id;
    return R;
}

XMVECTOR WhirligigScene::MulMat3Vec(const XMFLOAT3X3& M, FXMVECTOR v) {
    XMFLOAT3 vf;
    XMStoreFloat3(&vf, v);

    float x = M._11 * vf.x + M._12 * vf.y + M._13 * vf.z;
    float y = M._21 * vf.x + M._22 * vf.y + M._23 * vf.z;
    float z = M._31 * vf.x + M._32 * vf.y + M._33 * vf.z;

    return XMVectorSet(x, y, z, 0.0f);
}

XMVECTOR WhirligigScene::TorqueBody(FXMVECTOR q) {
    if (!useGravity) return XMVectorZero();

    XMVECTOR gW = XMVectorSet(0.0f, -G, 0.0f, 0.0f);

    XMMATRIX R = XMMatrixRotationQuaternion(q);   // body -> world
    XMMATRIX RT = XMMatrixTranspose(R);           // world -> body

    XMVECTOR gB = XMVector3TransformNormal(gW, RT);
    XMVECTOR F = XMVectorScale(gB, mass);

    XMVECTOR r = XMLoadFloat3(&rCM);
    r = XMVectorSetW(r, 0.0f);

    XMVECTOR N = XMVector3Cross(r, F);
    N = XMVectorSetW(N, 0.0f);
    return N;
}

XMVECTOR WhirligigScene::fOmega(FXMVECTOR w, FXMVECTOR q) {
    XMVECTOR Iw = MulMat3Vec(I, w);
    XMVECTOR cor = XMVector3Cross(Iw, w); // Iw x w
    XMVECTOR N = TorqueBody(q);
    XMVECTOR rhs = XMVectorSubtract(N, cor);
    XMVECTOR dw = MulMat3Vec(Iinv, rhs);
    return XMVectorSetW(dw, 0.0f);
}

XMVECTOR WhirligigScene::fQ(FXMVECTOR w, FXMVECTOR q) {
    XMVECTOR omegaQuat = XMVectorSet(
        XMVectorGetX(w),
        XMVectorGetY(w),
        XMVectorGetZ(w),
        0.0f);

    // dq = q * O  (XMQuaternionMultiply(Q1,Q2) zwraca Q2*Q1)
    XMVECTOR dq = XMQuaternionMultiply(omegaQuat, q); // = q * omegaQuat
    dq = XMVectorScale(dq, 0.5f);
    return dq;
}

void WhirligigScene::RK4Step(float h) {
    XMVECTOR k1w = fOmega(omega, Q);
    XMVECTOR k1q = fQ(omega, Q);

    XMVECTOR w2 = XMVectorAdd(omega, XMVectorScale(k1w, h * 0.5f));
    XMVECTOR q2 = XMQuaternionNormalize(XMVectorAdd(Q, XMVectorScale(k1q, h * 0.5f)));
    XMVECTOR k2w = fOmega(w2, q2);
    XMVECTOR k2q = fQ(w2, q2);

    XMVECTOR w3 = XMVectorAdd(omega, XMVectorScale(k2w, h * 0.5f));
    XMVECTOR q3 = XMQuaternionNormalize(XMVectorAdd(Q, XMVectorScale(k2q, h * 0.5f)));
    XMVECTOR k3w = fOmega(w3, q3);
    XMVECTOR k3q = fQ(w3, q3);

    XMVECTOR w4 = XMVectorAdd(omega, XMVectorScale(k3w, h));
    XMVECTOR q4 = XMQuaternionNormalize(XMVectorAdd(Q, XMVectorScale(k3q, h)));
    XMVECTOR k4w = fOmega(w4, q4);
    XMVECTOR k4q = fQ(w4, q4);

    {
        XMVECTOR sumW = XMVectorAdd(
            k1w,
            XMVectorAdd(
                XMVectorScale(XMVectorAdd(k2w, k3w), 2.0f),
                k4w));
        omega = XMVectorAdd(omega, XMVectorScale(sumW, h / 6.0f));
        omega = XMVectorSetW(omega, 0.0f);
    }

    {
        XMVECTOR sumQ = XMVectorAdd(
            k1q,
            XMVectorAdd(
                XMVectorScale(XMVectorAdd(k2q, k3q), 2.0f),
                k4q));
        Q = XMQuaternionNormalize(XMVectorAdd(Q, XMVectorScale(sumQ, h / 6.0f)));
    }
}

void WhirligigScene::PushTrajectoryPoint() {
    if (!trajVerts) return;

    XMVECTOR diagBody = XMVectorSet(1.0f, 1.0f, 1.0f, 0.0f);
    XMVECTOR tipBody  = XMVectorScale(diagBody, cubeSize);

    XMMATRIX Rm = XMMatrixRotationQuaternion(Q);
    XMVECTOR tipWorld = XMVector3TransformCoord(tipBody, Rm);

    XMFLOAT3 tip3{};
    XMStoreFloat3(&tip3, tipWorld);

    UINT maxLen = (std::min)(trajLength, MaxTrajPoints);
    if (maxLen == 0) {
        trajCount = 0;
        trajStart = 0;
        return;
    }

    if (trajCount < maxLen) {
        UINT idx = (trajStart + trajCount) % MaxTrajPoints;
        trajVerts[idx] = { { tip3.x, tip3.y, tip3.z }, { 0.2f, 1.0f, 0.4f } };
        ++trajCount;
    } else {
        trajStart = (trajStart + 1) % MaxTrajPoints;
        UINT idx = (trajStart + trajCount - 1) % MaxTrajPoints;
        trajVerts[idx] = { { tip3.x, tip3.y, tip3.z }, { 0.2f, 1.0f, 0.4f } };
    }
}

void WhirligigScene::BuildRootSignature(ID3D12Device* device) {
    D3D12_ROOT_PARAMETER param{};
    param.ParameterType = D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS;
    param.Constants.Num32BitValues = 16;
    param.Constants.ShaderRegister = 0;
    param.ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;

    D3D12_ROOT_SIGNATURE_DESC rs{};
    rs.NumParameters = 1;
    rs.pParameters = &param;
    rs.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;

    ComPtr<ID3DBlob> sig, err;
    HR(D3D12SerializeRootSignature(&rs, D3D_ROOT_SIGNATURE_VERSION_1, &sig, &err),
        "D3D12SerializeRootSignature(Whirligig)",
        err ? std::string((char*)err->GetBufferPointer(), err->GetBufferSize()) : "");
    HR(device->CreateRootSignature(0, sig->GetBufferPointer(), sig->GetBufferSize(),
        IID_PPV_ARGS(&rootSig)), "CreateRootSignature(Whirligig)");
}

void WhirligigScene::BuildPSO(ID3D12Device* device) {
    const char* vsSrc = R"(
cbuffer Params : register(b0) { float4x4 MVP; }
struct VSIn { float3 pos:POSITION; float3 col:COLOR; };
struct PSIn { float4 pos:SV_POSITION; float3 col:COLOR; };
PSIn VSMain(VSIn i){
    PSIn o;
    o.pos = mul(float4(i.pos,1), MVP);
    o.col = i.col;
    return o;
}
)";
    const char* psSrc = R"(
struct PSIn { float4 pos:SV_POSITION; float3 col:COLOR; };
float4 PSMain(PSIn i):SV_TARGET { return float4(i.col,1); }
)";

    ComPtr<ID3DBlob> vs, ps, shErr;
    HR(D3DCompile(vsSrc, (UINT)std::strlen(vsSrc), nullptr, nullptr, nullptr,
        "VSMain", "vs_5_0", 0, 0, &vs, &shErr),
        "D3DCompile(VS Whirligig)",
        shErr ? std::string((char*)shErr->GetBufferPointer(), shErr->GetBufferSize()) : "");
    HR(D3DCompile(psSrc, (UINT)std::strlen(psSrc), nullptr, nullptr, nullptr,
        "PSMain", "ps_5_0", 0, 0, &ps, &shErr),
        "D3DCompile(PS Whirligig)",
        shErr ? std::string((char*)shErr->GetBufferPointer(), shErr->GetBufferSize()) : "");

    D3D12_INPUT_ELEMENT_DESC layout[] = {
        { "POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0,
            D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0 },
        { "COLOR",    0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 12,
            D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0 },
    };

    D3D12_GRAPHICS_PIPELINE_STATE_DESC psoDesc{};
    psoDesc.pRootSignature = rootSig.Get();
    psoDesc.VS = { vs->GetBufferPointer(), vs->GetBufferSize() };
    psoDesc.PS = { ps->GetBufferPointer(), ps->GetBufferSize() };
    psoDesc.InputLayout = { layout, _countof(layout) };
    psoDesc.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
    psoDesc.NumRenderTargets = 1;
    psoDesc.RTVFormats[0] = DXGI_FORMAT_R8G8B8A8_UNORM;
    psoDesc.DSVFormat = DXGI_FORMAT_D32_FLOAT;
    psoDesc.SampleDesc.Count = 1;
    psoDesc.SampleMask = UINT_MAX;

    // Blend
    psoDesc.BlendState.AlphaToCoverageEnable = FALSE;
    psoDesc.BlendState.IndependentBlendEnable = FALSE;
    const D3D12_RENDER_TARGET_BLEND_DESC rt = {
        FALSE,FALSE,
        D3D12_BLEND_ONE, D3D12_BLEND_ZERO, D3D12_BLEND_OP_ADD,
        D3D12_BLEND_ONE, D3D12_BLEND_ZERO, D3D12_BLEND_OP_ADD,
        D3D12_LOGIC_OP_NOOP, D3D12_COLOR_WRITE_ENABLE_ALL
    };
    for (int i = 0; i < 8; ++i) psoDesc.BlendState.RenderTarget[i] = rt;

    // Rasterizer
    psoDesc.RasterizerState.FillMode = D3D12_FILL_MODE_WIREFRAME;
    psoDesc.RasterizerState.CullMode = D3D12_CULL_MODE_NONE;
    psoDesc.RasterizerState.DepthClipEnable = TRUE;

    // Depth
    psoDesc.DepthStencilState.DepthEnable = TRUE;
    psoDesc.DepthStencilState.DepthWriteMask = D3D12_DEPTH_WRITE_MASK_ALL;
    psoDesc.DepthStencilState.DepthFunc = D3D12_COMPARISON_FUNC_LESS_EQUAL;
    psoDesc.DepthStencilState.StencilEnable = FALSE;

    HR(device->CreateGraphicsPipelineState(&psoDesc, IID_PPV_ARGS(&pso)),
        "CreateGraphicsPipelineState(Whirligig)");
}

void WhirligigScene::BuildGeometry(ID3D12Device* device) {
    struct V { float p[3]; float c[3]; };

    D3D12_HEAP_PROPERTIES heap{}; heap.Type = D3D12_HEAP_TYPE_UPLOAD;
    D3D12_RESOURCE_DESC buf{};
    buf.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER;
    buf.Height = 1; buf.DepthOrArraySize = 1; buf.MipLevels = 1;
    buf.SampleDesc.Count = 1; buf.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;

    // Cube
    {
        V vertsCube[] = {
            // front (z=1)
            {{0,0,1},{1,0,0}},
            {{1,0,1},{0,1,0}},
            {{1,1,1},{0,0,1}},
            {{0,1,1},{1.0f,0.5f,0.0f}},
            // back (z=0)
            {{0,0,0},{1,0,1}},
            {{1,0,0},{0,1,1}},
            {{1,1,0},{1,1,1}},
            {{0,1,0},{0.2f,0.6f,1.0f}},
        };
        uint16_t idxCube[] = {
            0,1,2, 0,2,3,
            1,5,6, 1,6,2,
            5,4,7, 5,7,6,
            4,0,3, 4,3,7,
            3,2,6, 3,6,7,
            4,5,1, 4,1,0
        };

        UINT vbSize = sizeof(vertsCube);
        buf.Width = vbSize;
        HR(device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &buf,
            D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&vbCube)),
            "CreateCommittedResource(WhirligigCubeVB)");
        void* p = nullptr; D3D12_RANGE r{ 0,0 };
        HR(vbCube->Map(0, &r, &p), "WhirligigCubeVB->Map");
        std::memcpy(p, vertsCube, vbSize);
        vbCube->Unmap(0, nullptr);
        vbvCube.BufferLocation = vbCube->GetGPUVirtualAddress();
        vbvCube.StrideInBytes = sizeof(V);
        vbvCube.SizeInBytes = vbSize;

        UINT ibSize = sizeof(idxCube);
        buf.Width = ibSize;
        HR(device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &buf,
            D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&ibCube)),
            "CreateCommittedResource(WhirligigCubeIB)");
        HR(ibCube->Map(0, &r, &p), "WhirligigCubeIB->Map");
        std::memcpy(p, idxCube, ibSize);
        ibCube->Unmap(0, nullptr);
        ibvCube.BufferLocation = ibCube->GetGPUVirtualAddress();
        ibvCube.Format = DXGI_FORMAT_R16_UINT;
        ibvCube.SizeInBytes = ibSize;
    }

    // Diagonal
    {
        V diag[2] = {
            {{0,0,0},{1,1,0}},
            {{1,1,1},{1,1,0}},
        };
        UINT diagSize = sizeof(diag);
        buf.Width = diagSize;
        HR(device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &buf,
            D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&vbDiag)),
            "CreateCommittedResource(WhirligigDiagVB)");
        void* p = nullptr; D3D12_RANGE r{ 0,0 };
        HR(vbDiag->Map(0, &r, &p), "WhirligigDiagVB->Map");
        std::memcpy(p, diag, diagSize);
        vbDiag->Unmap(0, nullptr);
        vbvDiag.BufferLocation = vbDiag->GetGPUVirtualAddress();
        vbvDiag.StrideInBytes = sizeof(V);
        vbvDiag.SizeInBytes = diagSize;
    }

    // Trajectory
    {
        UINT trajSize = MaxTrajPoints * sizeof(TrajVertex);
        buf.Width = trajSize;
        HR(device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &buf,
            D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&vbTraj)),
            "CreateCommittedResource(WhirligigTrajVB)");

        D3D12_RANGE r{ 0,0 };
        void* p = nullptr;
        HR(vbTraj->Map(0, &r, &p), "WhirligigTrajVB->Map(persistent)");
        trajVerts = reinterpret_cast<TrajVertex*>(p);

        for (UINT i = 0; i < MaxTrajPoints; ++i) {
            trajVerts[i] = { {0,0,0}, {0.2f, 1.0f, 0.4f} };
        }

        vbvTraj.BufferLocation = vbTraj->GetGPUVirtualAddress();
        vbvTraj.StrideInBytes = sizeof(TrajVertex);
        vbvTraj.SizeInBytes = trajSize;
    }

    // Grid in XZ plane (Y up, RH)
    {
        static const int GRID_HALF = 10;
        static const int GRID_LINES = 2 * GRID_HALF + 1;
        static const int GRID_VERTS = GRID_LINES * 4;
        V vertsGrid[GRID_VERTS];

        const float step = 1.0f;
        const float extent = GRID_HALF * step;
        int idx = 0;
        for (int i = -GRID_HALF; i <= GRID_HALF; ++i) {
            float x = (float)i * step;
            float z = (float)i * step;

            bool center = (i == 0);
            float c = center ? 0.4f : 0.25f;

            vertsGrid[idx++] = { { x, 0.0f, -extent }, { c, c, c } };
            vertsGrid[idx++] = { { x, 0.0f,  extent }, { c, c, c } };

            vertsGrid[idx++] = { { -extent, 0.0f, z }, { c, c, c } };
            vertsGrid[idx++] = { {  extent, 0.0f, z }, { c, c, c } };
        }
        gridVertexCount = GRID_VERTS;

        UINT gridSize = sizeof(vertsGrid);
        buf.Width = gridSize;
        HR(device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &buf,
            D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&vbGrid)),
            "CreateCommittedResource(WhirligigGridVB)");
        void* p = nullptr; D3D12_RANGE r{ 0,0 };
        HR(vbGrid->Map(0, &r, &p), "WhirligigGridVB->Map");
        std::memcpy(p, vertsGrid, gridSize);
        vbGrid->Unmap(0, nullptr);
        vbvGrid.BufferLocation = vbGrid->GetGPUVirtualAddress();
        vbvGrid.StrideInBytes = sizeof(V);
        vbvGrid.SizeInBytes = gridSize;
    }

    // Axes (positive directions only)
    {
        const float L = 3.0f;
        V axes[6] = {
            {{0.0f, 0.0f, 0.0f}, {1,0,0}},
            {{   L, 0.0f, 0.0f}, {1,0,0}},
            {{0.0f, 0.0f, 0.0f}, {0,1,0}},
            {{0.0f,    L, 0.0f}, {0,1,0}},
            {{0.0f, 0.0f, 0.0f}, {0,0,1}},
            {{0.0f, 0.0f,    L}, {0,0,1}},
        };
        axesVertexCount = 6;

        UINT axesSize = sizeof(axes);
        buf.Width = axesSize;
        HR(device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &buf,
            D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&vbAxes)),
            "CreateCommittedResource(WhirligigAxesVB)");
        void* p = nullptr; D3D12_RANGE r{ 0,0 };
        HR(vbAxes->Map(0, &r, &p), "WhirligigAxesVB->Map");
        std::memcpy(p, axes, axesSize);
        vbAxes->Unmap(0, nullptr);
        vbvAxes.BufferLocation = vbAxes->GetGPUVirtualAddress();
        vbvAxes.StrideInBytes = sizeof(V);
        vbvAxes.SizeInBytes = axesSize;
    }
}

void WhirligigScene::Init(ID3D12Device* device) {
    BuildRootSignature(device);
    BuildPSO(device);
    BuildGeometry(device);
    ResetState();
}

void WhirligigScene::ResetState() {
    float a = cubeSize;
    rCM = XMFLOAT3(a * 0.5f, a * 0.5f, a * 0.5f);
    mass = density * a * a * a;

    float s = mass * a * a;
    XMFLOAT3X3 Ibody{};
    Ibody._11 = 2.0f / 3.0f * s;
    Ibody._22 = 2.0f / 3.0f * s;
    Ibody._33 = 2.0f / 3.0f * s;
    Ibody._12 = Ibody._21 = -0.25f * s;
    Ibody._13 = Ibody._31 = -0.25f * s;
    Ibody._23 = Ibody._32 = -0.25f * s;
    I = Ibody;
    Iinv = Inverse3x3(I);

    float theta = XMConvertToRadians(inflectionDeg);
    float s2 = std::sqrt(2.0f);

    // body-space diagonal (pivot -> opposite corner)
    XMVECTOR uBody = XMVector3Normalize(
        XMVectorSet(1.0f, 1.0f, 1.0f, 0.0f));

    // desired direction in world-space
    XMVECTOR target = XMVectorSet(
        std::sin(theta) / s2,
        std::cos(theta),
        std::sin(theta) / s2,
        0.0f);

    // --- shortest arc from uBody to target (inline, no MakeShortestArcQuat) ---
    XMVECTOR axis = XMVector3Cross(uBody, target);
    float axisLenSq = XMVectorGetX(XMVector3LengthSq(axis));

    XMVECTOR q;
    if (axisLenSq < 1e-6f) {
        // vectors almost parallel or antiparallel:
        // in our parameter range (theta in [0,90]) this practically won't happen,
        // but for safety we just use identity (no extra tilt around uBody).
        q = XMQuaternionIdentity();
    }
    else {
        axis = XMVector3Normalize(axis);
        float dot = XMVectorGetX(XMVector3Dot(uBody, target));
        dot = std::clamp(dot, -1.0f, 1.0f);
        float angle = std::acos(dot);
        q = XMQuaternionRotationAxis(axis, angle);
    }

    Q = XMQuaternionNormalize(q);

    // spin around body diagonal with magnitude omegaMag
    omega = XMVectorScale(uBody, omegaMag);
    omega = XMVectorSetW(omega, 0.0f);

    trajCount = 0;
    trajStart = 0;
    haveLast = false;
}


XMVECTOR WhirligigScene::GetEye() const {
    float cy = std::cos(camYaw);
    float sy = std::sin(camYaw);
    float cp = std::cos(camPitch);
    float sp = std::sin(camPitch);

    XMFLOAT3 fwdF{ cp * cy, sp, cp * sy };
    XMVECTOR fwd = XMVector3Normalize(XMLoadFloat3(&fwdF));
    XMVECTOR tgt = XMLoadFloat3(&camTarget);

    XMVECTOR offset = XMVectorScale(fwd, camDist);
    return XMVectorSubtract(tgt, offset);
}

XMVECTOR WhirligigScene::GetAt() const {
    return XMLoadFloat3(&camTarget);
}

XMVECTOR WhirligigScene::GetUp() const {
    XMVECTOR eye = GetEye();
    XMVECTOR at = GetAt();
    XMVECTOR forward = XMVector3Normalize(XMVectorSubtract(at, eye));
    XMVECTOR worldUp = XMVectorSet(0, 1, 0, 0);

    XMVECTOR right = XMVector3Normalize(XMVector3Cross(forward, worldUp));
    XMVECTOR upv = XMVector3Normalize(XMVector3Cross(right, forward));
    return upv;
}

void WhirligigScene::OnMouseMessage(UINT msg, WPARAM wParam, LPARAM lParam) {
    int x = GET_X_LPARAM(lParam);
    int y = GET_Y_LPARAM(lParam);

    switch (msg) {
    case WM_LBUTTONDOWN:
        rotating = true;
        lastMouse.x = x;
        lastMouse.y = y;
        break;
    case WM_LBUTTONUP:
        rotating = false;
        break;
    case WM_RBUTTONDOWN:
        panning = true;
        lastMouse.x = x;
        lastMouse.y = y;
        break;
    case WM_RBUTTONUP:
        panning = false;
        break;
    case WM_MOUSEWHEEL: {
        short delta = GET_WHEEL_DELTA_WPARAM(wParam);
        float zoomFactor = std::exp(-(float)delta / 120.0f * 0.15f);
        camDist *= zoomFactor;
        camDist = std::clamp(camDist, 0.5f, 50.0f);
    } break;
    case WM_MOUSEMOVE: {
        int dx = x - lastMouse.x;
        int dy = y - lastMouse.y;
        lastMouse.x = x;
        lastMouse.y = y;

        if (rotating) {
            camYaw += dx * 0.01f;
            camPitch += dy * 0.01f;
            const float limit = XM_PIDIV2 - 0.1f;
            camPitch = std::clamp(camPitch, -limit, limit);
        }
        else if (panning) {
            XMVECTOR eye = GetEye();
            XMVECTOR at = GetAt();
            XMVECTOR viewDir = XMVector3Normalize(XMVectorSubtract(at, eye));
            XMVECTOR right = XMVector3Normalize(
                XMVector3Cross(viewDir, XMVectorSet(0, 1, 0, 0)));
            XMVECTOR upv = XMVector3Normalize(
                XMVector3Cross(right, viewDir));

            float panScale = 0.002f * camDist;
            XMVECTOR dRight = XMVectorScale(right, -dx * panScale);
            XMVECTOR dUp = XMVectorScale(upv, dy * panScale);
            XMVECTOR delta = XMVectorAdd(dRight, dUp);

            XMVECTOR newAt = XMVectorAdd(at, delta);
            XMStoreFloat3(&camTarget, newAt);
        }
    } break;
    default:
        break;
    }
}

void WhirligigScene::Render(ID3D12GraphicsCommandList* cl) {
    auto now = steady_clock::now();
    float realDt = 1.0f / 60.0f;
    if (haveLast) {
        realDt = duration_cast<duration<float>>(now - lastTime).count();
    }
    lastTime = now;
    haveLast = true;

    float simDt = realDt * speed;
    const int maxStepsPerFrame = 500;
    int steps = 0;
    while (simDt >= dt && steps < maxStepsPerFrame) {
        if (running) {
            RK4Step(dt);
            PushTrajectoryPoint();
        }
        simDt -= dt;
        ++steps;
    }

    float aspect = (height == 0) ? 1.0f : float(width) / float(height);
    XMVECTOR eye = GetEye();
    XMVECTOR at = GetAt();
    XMVECTOR up = GetUp();

    XMMATRIX view = XMMatrixLookAtRH(eye, at, up);
    XMMATRIX proj = XMMatrixPerspectiveFovRH(XM_PIDIV4, aspect, 0.1f, 100.0f);

    XMMATRIX Rm = XMMatrixRotationQuaternion(Q);
    XMMATRIX S = XMMatrixScaling(cubeSize, cubeSize, cubeSize);
    XMMATRIX worldCube = S * Rm;
    XMMATRIX worldI = XMMatrixIdentity();

    cl->SetPipelineState(pso.Get());
    cl->SetGraphicsRootSignature(rootSig.Get());

    // Grid + Axes
    {
        XMMATRIX wvpT = XMMatrixTranspose(worldI * view * proj);
        XMFLOAT4X4 m; XMStoreFloat4x4(&m, wvpT);
        cl->SetGraphicsRoot32BitConstants(0, 16, &m, 0);

        if (showPlane && vbGrid && gridVertexCount > 0) {
            cl->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_LINELIST);
            cl->IASetVertexBuffers(0, 1, &vbvGrid);
            cl->DrawInstanced(gridVertexCount, 1, 0, 0);
        }

        if (showAxes && vbAxes && axesVertexCount > 0) {
            cl->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_LINELIST);
            cl->IASetVertexBuffers(0, 1, &vbvAxes);
            cl->DrawInstanced(axesVertexCount, 1, 0, 0);
        }
    }

    // Cube + diagonal
    {
        XMMATRIX wvpT = XMMatrixTranspose(worldCube * view * proj);
        XMFLOAT4X4 m; XMStoreFloat4x4(&m, wvpT);
        cl->SetGraphicsRoot32BitConstants(0, 16, &m, 0);

        if (showCube) {
            cl->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
            cl->IASetVertexBuffers(0, 1, &vbvCube);
            cl->IASetIndexBuffer(&ibvCube);
            cl->DrawIndexedInstanced(36, 1, 0, 0, 0);
        }

        if (showDiagonal) {
            cl->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_LINELIST);
            cl->IASetVertexBuffers(0, 1, &vbvDiag);
            cl->DrawInstanced(2, 1, 0, 0);
        }
    }

    // Trajectory (ring buffer)
    if (showTrajectory && trajCount >= 2 && trajVerts) {
        cl->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_LINESTRIP);
        cl->IASetVertexBuffers(0, 1, &vbvTraj);

        XMMATRIX wvpT = XMMatrixTranspose(worldI * view * proj);
        XMFLOAT4X4 m; XMStoreFloat4x4(&m, wvpT);
        cl->SetGraphicsRoot32BitConstants(0, 16, &m, 0);

        UINT end = trajStart + trajCount;
        if (end <= MaxTrajPoints) {
            cl->DrawInstanced(trajCount, 1, trajStart, 0);
        } else {
            UINT firstCount = MaxTrajPoints - trajStart;
            UINT secondCount = trajCount - firstCount;

            cl->DrawInstanced(firstCount, 1, trajStart, 0);
            cl->DrawInstanced(secondCount, 1, 0, 0);
        }
    }
}

void WhirligigScene::OnResize(UINT w, UINT h) {
    width = (w == 0 ? 1 : w);
    height = (h == 0 ? 1 : h);
}

void WhirligigScene::Cleanup() {
    if (vbTraj && trajVerts) {
        vbTraj->Unmap(0, nullptr);
        trajVerts = nullptr;
    }
    vbCube.Reset(); ibCube.Reset();
    vbDiag.Reset();
    vbTraj.Reset();
    vbGrid.Reset();
    vbAxes.Reset();
    pso.Reset();
    rootSig.Reset();
}
