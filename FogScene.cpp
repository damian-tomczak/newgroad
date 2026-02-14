#include "FogScene.h"

#include <d3dcompiler.h>
#include <algorithm>
#include <cmath>
#include <cstring>

#pragma comment(lib, "d3dcompiler.lib")

namespace {
struct Vec3 {
    float x, y, z;
};

static Vec3 operator-(Vec3 a, Vec3 b) { return { a.x - b.x, a.y - b.y, a.z - b.z }; }
static Vec3 operator*(Vec3 a, float s) { return { a.x * s, a.y * s, a.z * s }; }

static float Dot(Vec3 a, Vec3 b) { return a.x * b.x + a.y * b.y + a.z * b.z; }
static float Len(Vec3 a) { return std::sqrt(std::max(0.0f, Dot(a, a))); }
static Vec3 Normalize(Vec3 a) { float l = Len(a); return (l > 1e-6f) ? (a * (1.0f / l)) : Vec3{ 0,0,0 }; }

struct Mat4 {
    float m[4][4];
};

static Mat4 Identity()
{
    Mat4 r{};
    for (int i = 0; i < 4; ++i) for (int j = 0; j < 4; ++j) r.m[i][j] = (i == j) ? 1.0f : 0.0f;
    return r;
}

static Mat4 Mul(const Mat4& A, const Mat4& B)
{
    Mat4 R{};
    for (int i = 0; i < 4; ++i) for (int j = 0; j < 4; ++j) {
        float s = 0.0f;
        for (int k = 0; k < 4; ++k) s += A.m[i][k] * B.m[k][j];
        R.m[i][j] = s;
    }
    return R;
}

static Mat4 Transpose(const Mat4& A)
{
    Mat4 R{};
    for (int i = 0; i < 4; ++i) for (int j = 0; j < 4; ++j) R.m[i][j] = A.m[j][i];
    return R;
}

static Mat4 Translation(float tx, float ty, float tz)
{
    Mat4 R = Identity();
    R.m[0][3] = tx; R.m[1][3] = ty; R.m[2][3] = tz;
    return R;
}

static Mat4 Scale(float sx, float sy, float sz)
{
    Mat4 R = Identity();
    R.m[0][0] = sx; R.m[1][1] = sy; R.m[2][2] = sz;
    return R;
}

static Mat4 RotX(float a)
{
    Mat4 R = Identity();
    float c = std::cos(a), s = std::sin(a);
    R.m[1][1] = c; R.m[1][2] = -s;
    R.m[2][1] = s; R.m[2][2] = c;
    return R;
}

static Mat4 RotY(float a)
{
    Mat4 R = Identity();
    float c = std::cos(a), s = std::sin(a);
    R.m[0][0] = c; R.m[0][2] = s;
    R.m[2][0] = -s; R.m[2][2] = c;
    return R;
}

static Mat4 InverseTRS(float tx, float ty, float tz, float rx, float ry, float sx, float sy, float sz)
{
    Mat4 tin = Translation(-tx, -ty, -tz);
    Mat4 ryin = Transpose(RotY(ry));
    Mat4 rxin = Transpose(RotX(rx));
    Mat4 sin = Scale((sx != 0) ? 1.0f / sx : 0.0f, (sy != 0) ? 1.0f / sy : 0.0f, (sz != 0) ? 1.0f / sz : 0.0f);
    return Mul(Mul(Mul(sin, rxin), ryin), tin);
}

static Mat4 BuildQuadricA(const FogScene& s)
{
    Mat4 D{};
    D.m[0][0] = 1.0f / (s.rx * s.rx);
    D.m[1][1] = 1.0f / (s.ry * s.ry);
    D.m[2][2] = 1.0f / (s.rz * s.rz);
    D.m[3][3] = -1.0f;

    Mat4 minv = InverseTRS(s.panX, s.panY, 0.0f, s.rotX, s.rotY, s.zoom, s.zoom, s.zoom);
    return Mul(Mul(Transpose(minv), D), minv);
}

static bool SolveForZ(const Mat4& A, float x, float y, float& zOut)
{
    float A00 = A.m[0][0], A11 = A.m[1][1], A22 = A.m[2][2], A33 = A.m[3][3];
    float A01 = A.m[0][1], A02 = A.m[0][2], A03 = A.m[0][3];
    float A12 = A.m[1][2], A13 = A.m[1][3], A23 = A.m[2][3];

    float a = A22;
    float b = 2.0f * (A02 * x + A12 * y + A23);
    float c = A00 * x * x + A11 * y * y + A33 + 2.0f * (A01 * x * y + A03 * x + A13 * y);

    if (std::fabs(a) < 1e-8f) {
        if (std::fabs(b) < 1e-8f) return false;
        zOut = -c / b;
        return true;
    }

    float disc = b * b - 4.0f * a * c;
    if (disc < 0.0f) return false;
    float s = std::sqrt(std::max(0.0f, disc));
    float z1 = (-b - s) / (2.0f * a);
    float z2 = (-b + s) / (2.0f * a);
    zOut = (z1 > z2) ? z1 : z2;
    return true;
}

static Vec3 SurfaceNormal(const Mat4& A, float x, float y, float z)
{
    float A00 = A.m[0][0], A11 = A.m[1][1], A22 = A.m[2][2];
    float A01 = A.m[0][1], A02 = A.m[0][2], A03 = A.m[0][3];
    float A12 = A.m[1][2], A13 = A.m[1][3], A23 = A.m[2][3];

    Vec3 g{
        A00 * x + A01 * y + A02 * z + A03,
        A01 * x + A11 * y + A12 * z + A13,
        A02 * x + A12 * y + A22 * z + A23
    };
    return Normalize(g);
}
} // namespace

void FogScene::Init(ID3D12Device* inDevice)
{
    device = inDevice;
    CreatePipeline();
    CreateOrResizeTexture(width, height);
}

void FogScene::CreatePipeline()
{
    D3D12_DESCRIPTOR_RANGE range{};
    range.RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_SRV;
    range.NumDescriptors = 1;
    range.BaseShaderRegister = 0;
    range.OffsetInDescriptorsFromTableStart = D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND;

    D3D12_ROOT_PARAMETER rp{};
    rp.ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
    rp.DescriptorTable.NumDescriptorRanges = 1;
    rp.DescriptorTable.pDescriptorRanges = &range;
    rp.ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    D3D12_STATIC_SAMPLER_DESC samp{};
    samp.Filter = D3D12_FILTER_MIN_MAG_MIP_LINEAR;
    samp.AddressU = D3D12_TEXTURE_ADDRESS_MODE_CLAMP;
    samp.AddressV = D3D12_TEXTURE_ADDRESS_MODE_CLAMP;
    samp.AddressW = D3D12_TEXTURE_ADDRESS_MODE_CLAMP;
    samp.ShaderRegister = 0;
    samp.ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    D3D12_ROOT_SIGNATURE_DESC rs{};
    rs.NumParameters = 1;
    rs.pParameters = &rp;
    rs.NumStaticSamplers = 1;
    rs.pStaticSamplers = &samp;
    rs.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;

    ComPtr<ID3DBlob> sig, err;
    HR(D3D12SerializeRootSignature(&rs, D3D_ROOT_SIGNATURE_VERSION_1, &sig, &err), "SerializeRootSignature(FogScene)");
    HR(device->CreateRootSignature(0, sig->GetBufferPointer(), sig->GetBufferSize(), IID_PPV_ARGS(&rootSig)),
        "CreateRootSignature(FogScene)");

    const char* shader = R"(
Texture2D tex0 : register(t0);
SamplerState samp0 : register(s0);

struct VSOut { float4 pos : SV_Position; float2 uv : TEXCOORD0; };
VSOut VSMain(uint vid : SV_VertexID)
{
    float2 p = (vid == 0) ? float2(-1, -1) : (vid == 1) ? float2(-1, 3) : float2(3, -1);
    VSOut o;
    o.pos = float4(p, 0, 1);
    o.uv = 0.5f * (p + 1.0f);
    return o;
}

float4 PSMain(float4 pos : SV_Position, float2 uv : TEXCOORD0) : SV_Target
{
    return tex0.Sample(samp0, uv);
}
)";

    ComPtr<ID3DBlob> vs, ps;
    HR(D3DCompile(shader, (UINT)std::strlen(shader), nullptr, nullptr, nullptr, "VSMain", "vs_5_0", 0, 0, &vs, &err),
        "D3DCompile(VS FogScene)", err ? std::string((char*)err->GetBufferPointer(), err->GetBufferSize()) : "");
    HR(D3DCompile(shader, (UINT)std::strlen(shader), nullptr, nullptr, nullptr, "PSMain", "ps_5_0", 0, 0, &ps, &err),
        "D3DCompile(PS FogScene)", err ? std::string((char*)err->GetBufferPointer(), err->GetBufferSize()) : "");

    D3D12_GRAPHICS_PIPELINE_STATE_DESC psoDesc{};
    psoDesc.pRootSignature = rootSig.Get();
    psoDesc.VS = { vs->GetBufferPointer(), vs->GetBufferSize() };
    psoDesc.PS = { ps->GetBufferPointer(), ps->GetBufferSize() };
    psoDesc.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
    psoDesc.NumRenderTargets = 1;
    psoDesc.RTVFormats[0] = DXGI_FORMAT_R8G8B8A8_UNORM;
    psoDesc.DSVFormat = DXGI_FORMAT_D32_FLOAT;
    psoDesc.SampleDesc.Count = 1;
    psoDesc.SampleMask = UINT_MAX;

    psoDesc.RasterizerState.FillMode = D3D12_FILL_MODE_SOLID;
    psoDesc.RasterizerState.CullMode = D3D12_CULL_MODE_NONE;
    psoDesc.RasterizerState.DepthClipEnable = TRUE;

    psoDesc.BlendState.AlphaToCoverageEnable = FALSE;
    psoDesc.BlendState.IndependentBlendEnable = FALSE;
    D3D12_RENDER_TARGET_BLEND_DESC rt{};
    rt.BlendEnable = FALSE;
    rt.LogicOpEnable = FALSE;
    rt.SrcBlend = D3D12_BLEND_ONE;
    rt.DestBlend = D3D12_BLEND_ZERO;
    rt.BlendOp = D3D12_BLEND_OP_ADD;
    rt.SrcBlendAlpha = D3D12_BLEND_ONE;
    rt.DestBlendAlpha = D3D12_BLEND_ZERO;
    rt.BlendOpAlpha = D3D12_BLEND_OP_ADD;
    rt.LogicOp = D3D12_LOGIC_OP_NOOP;
    rt.RenderTargetWriteMask = D3D12_COLOR_WRITE_ENABLE_ALL;
    for (int i = 0; i < 8; ++i) psoDesc.BlendState.RenderTarget[i] = rt;

    psoDesc.DepthStencilState.DepthEnable = FALSE;
    psoDesc.DepthStencilState.DepthWriteMask = D3D12_DEPTH_WRITE_MASK_ZERO;
    psoDesc.DepthStencilState.DepthFunc = D3D12_COMPARISON_FUNC_ALWAYS;
    psoDesc.DepthStencilState.StencilEnable = FALSE;

    HR(device->CreateGraphicsPipelineState(&psoDesc, IID_PPV_ARGS(&pso)), "CreateGraphicsPipelineState(FogScene)");

    D3D12_DESCRIPTOR_HEAP_DESC hd{};
    hd.Type = D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV;
    hd.NumDescriptors = 1;
    hd.Flags = D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE;
    HR(device->CreateDescriptorHeap(&hd, IID_PPV_ARGS(&srvHeap)), "CreateDescriptorHeap(FogScene)");
}

void FogScene::CreateOrResizeTexture(UINT w, UINT h)
{
    if (!device) return;

    width = std::max(1u, w);
    height = std::max(1u, h);
    UINT scale = (renderScale == 1 || renderScale == 2 || renderScale == 4) ? (UINT)renderScale : 1u;
    texW = std::max(1u, width / scale);
    texH = std::max(1u, height / scale);

    texture.Reset();
    textureUpload.Reset();
    uploadPtr = nullptr;

    D3D12_RESOURCE_DESC td{};
    td.Dimension = D3D12_RESOURCE_DIMENSION_TEXTURE2D;
    td.Width = texW;
    td.Height = texH;
    td.DepthOrArraySize = 1;
    td.MipLevels = 1;
    td.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
    td.SampleDesc.Count = 1;
    td.Layout = D3D12_TEXTURE_LAYOUT_UNKNOWN;

    D3D12_HEAP_PROPERTIES hpDef{};
    hpDef.Type = D3D12_HEAP_TYPE_DEFAULT;
    HR(device->CreateCommittedResource(&hpDef, D3D12_HEAP_FLAG_NONE, &td,
        D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE, nullptr, IID_PPV_ARGS(&texture)),
        "CreateCommittedResource(FogScene texture)");

    D3D12_PLACED_SUBRESOURCE_FOOTPRINT fp{};
    UINT numRows = 0;
    UINT64 rowBytes = 0;
    device->GetCopyableFootprints(&td, 0, 1, 0, &fp, &numRows, &rowBytes, &uploadSize);
    uploadRowPitch = fp.Footprint.RowPitch;

    D3D12_RESOURCE_DESC bd{};
    bd.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER;
    bd.Width = uploadSize;
    bd.Height = 1;
    bd.DepthOrArraySize = 1;
    bd.MipLevels = 1;
    bd.SampleDesc.Count = 1;
    bd.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;

    D3D12_HEAP_PROPERTIES hpUp{};
    hpUp.Type = D3D12_HEAP_TYPE_UPLOAD;
    HR(device->CreateCommittedResource(&hpUp, D3D12_HEAP_FLAG_NONE, &bd,
        D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&textureUpload)),
        "CreateCommittedResource(FogScene upload)");

    D3D12_RANGE r{ 0, 0 };
    HR(textureUpload->Map(0, &r, (void**)&uploadPtr), "Map(FogScene upload)");

    D3D12_SHADER_RESOURCE_VIEW_DESC sd{};
    sd.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
    sd.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
    sd.ViewDimension = D3D12_SRV_DIMENSION_TEXTURE2D;
    sd.Texture2D.MipLevels = 1;
    device->CreateShaderResourceView(texture.Get(), &sd, srvHeap->GetCPUDescriptorHandleForHeapStart());

    cpuPixels.resize((size_t)texW * (size_t)texH * 4u);
}

void FogScene::RenderCPU()
{
    Mat4 A = BuildQuadricA(*this);

    float aspect = (float)texW / (float)texH;
    float halfH = viewSize;
    float halfW = viewSize * aspect;

    Vec3 cam{ 0, 0, camZ };

    for (UINT j = 0; j < texH; ++j) {
        float v = 1.0f - 2.0f * ((j + 0.5f) / (float)texH);
        float y = v * halfH;
        for (UINT i = 0; i < texW; ++i) {
            float u = 2.0f * ((i + 0.5f) / (float)texW) - 1.0f;
            float x = u * halfW;
            float z = 0.0f;
            bool hit = SolveForZ(A, x, y, z);

            uint8_t r = 10, g = 12, b = 16, a = 255;
            if (hit && std::isfinite(z)) {
                Vec3 p{ x, y, z };
                Vec3 n = SurfaceNormal(A, x, y, z);
                Vec3 vdir = Normalize(cam - p);
                float ndv = std::max(0.0f, Dot(n, vdir));
                float I = std::pow(ndv, std::max(1.0f, shininess));
                r = (uint8_t)std::lround(std::clamp(1.0f * I, 0.0f, 1.0f) * 255.0f);
                g = (uint8_t)std::lround(std::clamp(1.0f * I, 0.0f, 1.0f) * 220.0f);
                b = (uint8_t)std::lround(std::clamp(0.2f * I, 0.0f, 1.0f) * 255.0f);
            }

            size_t idx = ((size_t)j * texW + i) * 4u;
            cpuPixels[idx + 0] = r;
            cpuPixels[idx + 1] = g;
            cpuPixels[idx + 2] = b;
            cpuPixels[idx + 3] = a;
        }
    }

    for (UINT y = 0; y < texH; ++y) {
        const uint8_t* src = cpuPixels.data() + (size_t)y * (size_t)texW * 4u;
        uint8_t* dst = uploadPtr + (size_t)y * (size_t)uploadRowPitch;
        std::memcpy(dst, src, (size_t)texW * 4u);
    }
}

void FogScene::Render(ID3D12GraphicsCommandList* cl)
{
    if (!texture || !textureUpload || !uploadPtr) return;

    RenderCPU();

    D3D12_RESOURCE_BARRIER b{};
    b.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
    b.Transition.pResource = texture.Get();
    b.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
    b.Transition.StateBefore = D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE;
    b.Transition.StateAfter = D3D12_RESOURCE_STATE_COPY_DEST;
    cl->ResourceBarrier(1, &b);

    D3D12_RESOURCE_DESC td = texture->GetDesc();
    D3D12_PLACED_SUBRESOURCE_FOOTPRINT fp{};
    UINT rows = 0;
    UINT64 rowBytes = 0;
    UINT64 total = 0;
    device->GetCopyableFootprints(&td, 0, 1, 0, &fp, &rows, &rowBytes, &total);

    D3D12_TEXTURE_COPY_LOCATION dst{};
    dst.pResource = texture.Get();
    dst.Type = D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX;
    dst.SubresourceIndex = 0;

    D3D12_TEXTURE_COPY_LOCATION src{};
    src.pResource = textureUpload.Get();
    src.Type = D3D12_TEXTURE_COPY_TYPE_PLACED_FOOTPRINT;
    src.PlacedFootprint = fp;

    cl->CopyTextureRegion(&dst, 0, 0, 0, &src, nullptr);

    b.Transition.StateBefore = D3D12_RESOURCE_STATE_COPY_DEST;
    b.Transition.StateAfter = D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE;
    cl->ResourceBarrier(1, &b);

    ID3D12DescriptorHeap* heaps[] = { srvHeap.Get() };
    cl->SetDescriptorHeaps(1, heaps);
    cl->SetPipelineState(pso.Get());
    cl->SetGraphicsRootSignature(rootSig.Get());
    cl->SetGraphicsRootDescriptorTable(0, srvHeap->GetGPUDescriptorHandleForHeapStart());
    cl->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
    cl->DrawInstanced(3, 1, 0, 0);
}

void FogScene::OnResize(UINT w, UINT h)
{
    CreateOrResizeTexture(w, h);
}

void FogScene::Cleanup()
{
    if (textureUpload && uploadPtr) {
        textureUpload->Unmap(0, nullptr);
        uploadPtr = nullptr;
    }

    textureUpload.Reset();
    texture.Reset();
    srvHeap.Reset();
    pso.Reset();
    rootSig.Reset();
    cpuPixels.clear();
}
