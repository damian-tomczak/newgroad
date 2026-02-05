#include "JellyScene.h"

#include <d3dcompiler.h>
#include <cstring>
#include <cmath>
#include <string>
#include <algorithm>
#include <random>

#pragma comment(lib, "d3dcompiler.lib")

using namespace DirectX;
using namespace std::chrono;

static inline float DegToRad(float angleDegrees)
{
    return angleDegrees * 3.1415926535f / 180.0f;
}

static inline std::string BlobToString(ID3DBlob* blob)
{
    if (!blob) return {};
    const char* data = (const char*)blob->GetBufferPointer();
    size_t size = (size_t)blob->GetBufferSize();
    return std::string(data, data + size);
}

JellyScene::JellyScene()
    : running(false)
    , showJellyPoints(true)
    , showBezierSurface(true)
    , showControlCube(true)
    , showBoundingBox(true)
    , showAxialSprings(true)
    , showDiagonalSprings(true)
    , showDeformedObject(true) // <-- WAŻNE
    , fixedTimeStep(0.01f)
    , simulationSpeed(1.0f)
    , initialRandomVelocityRange(0.0f)
    , gravityAcceleration{ 0.0f, -1.0f, 0.0f }
    , collisionRestitutionCoefficient(0.8f)
    , applyRestitutionToWholeVelocity(1)
    , particleMass(1.0f)
    , linearDampingCoefficient(4.0f)
    , axialSpringStiffness(36.19f)
    , shearSpringStiffness(33.34f)
    , enableControlCubeCoupling(true)
    , enableSurfaceControlCubeCoupling(false)
    , controlCubeCornerStiffness(200.0f)
    , controlCubeSurfaceStiffness(50.0f)
    , controlCubePosition{ 0.0f, 0.0f, 0.0f }
    , controlCubeEulerAnglesDeg{ 0.0f, 0.0f, 0.0f }
    , controlCubeEdgeLength(1.0f)
    , boundingBoxHalfExtent{ 2.0f, 2.0f, 2.0f }
    , pinchParticleIndex(5)
    , pinchDisplacement(0.0f)
    , punchParticleIndex(5)
    , punchVelocity(0.0f)
    , bezierTessellation(16)
    , hasPreviousFrameTime(false)
    , simulationTimeAccumulator(0.0f)
    , m_device(nullptr)
{
    ResetState();
}

void JellyScene::Init(ID3D12Device* device)
{
    m_device = device;

    BuildRootSignature(device);
    BuildPSO(device);
    BuildGeometry(device);

    m_cam.Reset(XM_PIDIV4, XM_PIDIV4 * 0.35f, 5.0f, { 0,0,0 });
    m_cam.SetViewport(width, height);

    hasPreviousFrameTime = false;
    simulationTimeAccumulator = 0.0f;
}

void JellyScene::OnResize(UINT newWidth, UINT newHeight)
{
    width = (newWidth == 0 ? 1u : newWidth);
    height = (newHeight == 0 ? 1u : newHeight);
    m_cam.SetViewport(width, height);
}

void JellyScene::Cleanup()
{
    if (m_vbJelly && m_mapJellyVB) { m_vbJelly->Unmap(0, nullptr);   m_mapJellyVB = nullptr; }
    if (m_vbCtrl && m_mapCtrlVB) { m_vbCtrl->Unmap(0, nullptr);    m_mapCtrlVB = nullptr; }
    if (m_vbBound && m_mapBoundVB) { m_vbBound->Unmap(0, nullptr);   m_mapBoundVB = nullptr; }
    if (m_vbBezier && m_mapBezierVB) { m_vbBezier->Unmap(0, nullptr);  m_mapBezierVB = nullptr; }
    if (m_vbObj && m_mapObjVB) { m_vbObj->Unmap(0, nullptr); m_mapObjVB = nullptr; }
    if (m_cb && m_mapCB) { m_cb->Unmap(0, nullptr);        m_mapCB = nullptr; }

    m_rootSig.Reset();
    m_psoLines.Reset();
    m_psoPoints.Reset();
    m_psoSolid.Reset();

    m_vbJelly.Reset();
    m_ibJellyAxial.Reset();
    m_ibJellyDiag.Reset();

    m_vbCtrl.Reset();
    m_ibCtrl.Reset();
    m_vbBound.Reset();
    m_ibBound.Reset();
    m_vbBezier.Reset();
    m_ibBezier.Reset();

    m_vbObj.Reset();
    m_ibObj.Reset();

    m_cb.Reset();

    m_device = nullptr;
}

void JellyScene::ResetState()
{
    int flatIndex = 0;
    for (int xIndex = 0; xIndex < N; ++xIndex)
    {
        float unitX = (float)xIndex / (N - 1) - 0.5f;
        for (int yIndex = 0; yIndex < N; ++yIndex)
        {
            float unitY = (float)yIndex / (N - 1) - 0.5f;
            for (int zIndex = 0; zIndex < N; ++zIndex)
            {
                float unitZ = (float)zIndex / (N - 1) - 0.5f;

                restUnitPositions[flatIndex] = F3(unitX, unitY, unitZ);
                particlePositions[flatIndex] = F3(
                    unitX * controlCubeEdgeLength,
                    unitY * controlCubeEdgeLength,
                    unitZ * controlCubeEdgeLength);
                particleVelocities[flatIndex] = F3(0, 0, 0);
                ++flatIndex;
            }
        }
    }

    controlCubePosition = { 0.0f, 0.0f, 0.0f };
    controlCubeEulerAnglesDeg = { 0.0f, 0.0f, 0.0f };

    simulationTimeAccumulator = 0.0f;
    hasPreviousFrameTime = false;

    ApplyInitialDisturbance();
}

void JellyScene::OnMouseMessage(UINT msg, WPARAM wParam, LPARAM lParam)
{
    auto mouseXFromLParam = [](LPARAM lp) -> int { return (int)(short)LOWORD(lp); };
    auto mouseYFromLParam = [](LPARAM lp) -> int { return (int)(short)HIWORD(lp); };
    auto wheelDeltaFromWParam = [](WPARAM wp) -> short { return (short)HIWORD(wp); };

    const bool shiftKeyDown = (wParam & MK_SHIFT) != 0;
    const bool ctrlKeyDown = (wParam & MK_CONTROL) != 0;

    int mouseX = mouseXFromLParam(lParam);
    int mouseY = mouseYFromLParam(lParam);

    if (msg == WM_MOUSEWHEEL && ctrlKeyDown)
    {
        short wheelDelta = wheelDeltaFromWParam(wParam);
        float wheelStep = (float)wheelDelta / 120.0f;
        controlCubeEdgeLength = std::clamp(controlCubeEdgeLength + wheelStep * 0.05f, 0.2f, 3.0f);
        return;
    }

    if (shiftKeyDown)
    {
        switch (msg)
        {
        case WM_LBUTTONDOWN:
            m_cam.rotating = false;
            m_cam.panning = false;
            m_cam.lastMouse = { mouseX, mouseY };
            return;

        case WM_MOUSEMOVE:
        {
            int deltaX = mouseX - m_cam.lastMouse.x;
            int deltaY = mouseY - m_cam.lastMouse.y;
            m_cam.lastMouse = { mouseX, mouseY };

            XMVECTOR cameraRight = m_cam.GetRight();
            XMVECTOR cameraUp = m_cam.GetUp();

            float panScale = 0.002f * m_cam.dist;
            XMVECTOR rightOffset = XMVectorScale(cameraRight, -deltaX * panScale);
            XMVECTOR upOffset = XMVectorScale(cameraUp, deltaY * panScale);
            XMVECTOR translation = XMVectorAdd(rightOffset, upOffset);

            XMFLOAT3 translationFloat3{};
            XMStoreFloat3(&translationFloat3, translation);
            controlCubePosition.x += translationFloat3.x;
            controlCubePosition.y += translationFloat3.y;
            controlCubePosition.z += translationFloat3.z;
        }
        return;

        default:
            break;
        }
    }

    m_cam.OnMouseMessage(msg, wParam, lParam);
}

void JellyScene::Render(ID3D12GraphicsCommandList* commandList)
{
    auto currentTime = steady_clock::now();
    if (!hasPreviousFrameTime)
    {
        hasPreviousFrameTime = true;
        lastTime = currentTime;
    }
    float frameDeltaTimeSeconds = duration<float>(currentTime - lastTime).count();
    lastTime = currentTime;

    if (running)
    {
        simulationTimeAccumulator += frameDeltaTimeSeconds * simulationSpeed;
        float fixedStep = std::max(0.0001f, fixedTimeStep);

        while (simulationTimeAccumulator >= fixedStep)
        {
            StepOnce(fixedStep);
            ApplyCollisions();
            simulationTimeAccumulator -= fixedStep;
        }
    }

    UpdateDynamicVBs();
    UpdateCB();

    commandList->SetGraphicsRootSignature(m_rootSig.Get());
    commandList->SetGraphicsRootConstantBufferView(0, m_cb->GetGPUVirtualAddress());

    if (showBezierSurface)
    {
        commandList->SetPipelineState(m_psoSolid.Get());
        commandList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
        commandList->IASetVertexBuffers(0, 1, &m_vbvBezier);
        commandList->IASetIndexBuffer(&m_ibvBezier);
        commandList->DrawIndexedInstanced((UINT)m_bezierIdx.size(), 1, 0, 0, 0);
    }

    if (showDeformedObject && !m_objIdx.empty())
    {
        commandList->SetPipelineState(m_psoSolid.Get());
        commandList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
        commandList->IASetVertexBuffers(0, 1, &m_vbvObj);
        commandList->IASetIndexBuffer(&m_ibvObj);
        commandList->DrawIndexedInstanced((UINT)m_objIdx.size(), 1, 0, 0, 0);
    }

    if (showBoundingBox)
    {
        commandList->SetPipelineState(m_psoLines.Get());
        commandList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_LINELIST);
        commandList->IASetVertexBuffers(0, 1, &m_vbvBound);
        commandList->IASetIndexBuffer(&m_ibvBound);
        commandList->DrawIndexedInstanced((UINT)m_boundIdx.size(), 1, 0, 0, 0);
    }

    if (showControlCube)
    {
        commandList->SetPipelineState(m_psoLines.Get());
        commandList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_LINELIST);
        commandList->IASetVertexBuffers(0, 1, &m_vbvCtrl);
        commandList->IASetIndexBuffer(&m_ibvCtrl);
        commandList->DrawIndexedInstanced((UINT)m_ctrlIdx.size(), 1, 0, 0, 0);
    }

    if (showAxialSprings && !m_jellyIdxAxial.empty())
    {
        commandList->SetPipelineState(m_psoLines.Get());
        commandList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_LINELIST);
        commandList->IASetVertexBuffers(0, 1, &m_vbvJelly);
        commandList->IASetIndexBuffer(&m_ibvJellyAxial);
        commandList->DrawIndexedInstanced((UINT)m_jellyIdxAxial.size(), 1, 0, 0, 0);
    }

    if (showDiagonalSprings && !m_jellyIdxDiag.empty())
    {
        commandList->SetPipelineState(m_psoLines.Get());
        commandList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_LINELIST);
        commandList->IASetVertexBuffers(0, 1, &m_vbvJelly);
        commandList->IASetIndexBuffer(&m_ibvJellyDiag);
        commandList->DrawIndexedInstanced((UINT)m_jellyIdxDiag.size(), 1, 0, 0, 0);
    }

    if (showJellyPoints)
    {
        commandList->SetPipelineState(m_psoPoints.Get());
        commandList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_POINTLIST);
        commandList->IASetVertexBuffers(0, 1, &m_vbvJelly);
        commandList->DrawInstanced((UINT)N3, 1, 0, 0);
    }
}

void JellyScene::BuildRootSignature(ID3D12Device* device)
{
    D3D12_ROOT_PARAMETER rootParameter{};
    rootParameter.ParameterType = D3D12_ROOT_PARAMETER_TYPE_CBV;
    rootParameter.Descriptor.ShaderRegister = 0;
    rootParameter.Descriptor.RegisterSpace = 0;
    rootParameter.ShaderVisibility = D3D12_SHADER_VISIBILITY_ALL;

    D3D12_ROOT_SIGNATURE_DESC rootSignatureDescription{};
    rootSignatureDescription.NumParameters = 1;
    rootSignatureDescription.pParameters = &rootParameter;
    rootSignatureDescription.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;

    ComPtr<ID3DBlob> serialized, error;
    HRESULT hr = D3D12SerializeRootSignature(
        &rootSignatureDescription,
        D3D_ROOT_SIGNATURE_VERSION_1,
        &serialized,
        &error);

    if (FAILED(hr))
    {
        HR(hr, "D3D12SerializeRootSignature(Jelly)", BlobToString(error.Get()));
    }

    HR(device->CreateRootSignature(
        0,
        serialized->GetBufferPointer(),
        serialized->GetBufferSize(),
        IID_PPV_ARGS(&m_rootSig)),
        "CreateRootSignature(Jelly)");
}

void JellyScene::BuildPSO(ID3D12Device* device)
{
    const char* vertexShaderPositionColor = R"(
cbuffer PerObject : register(b0)
{
    float4x4 gMVP;
    float3   gLightDir; float _pad0;
    float3   gBaseColor; float gAmbient;
};
struct VSIn { float3 pos:POSITION; float3 col:COLOR; };
struct VSOut{ float4 pos:SV_POSITION; float3 col:COLOR; };
VSOut main(VSIn input){
    VSOut output;
    output.pos = mul(float4(input.pos,1), gMVP);
    output.col = input.col;
    return output;
}
)";

    const char* pixelShaderPositionColor = R"(
struct PSIn{ float4 pos:SV_POSITION; float3 col:COLOR; };
float4 main(PSIn input):SV_TARGET
{
    return float4(input.col,1);
}
)";

    const char* vertexShaderSolid = R"(
cbuffer PerObject : register(b0)
{
    float4x4 gMVP;
    float3   gLightDir; float _pad0;
    float3   gBaseColor; float gAmbient;
};
struct VSIn  { float3 pos:POSITION; float3 nrm:NORMAL; };
struct VSOut { float4 pos:SV_POSITION; float3 nrm:NORMAL; };
VSOut main(VSIn input){
    VSOut output;
    output.pos = mul(float4(input.pos,1), gMVP);
    output.nrm = input.nrm;
    return output;
}
)";

    const char* pixelShaderSolid = R"(
cbuffer PerObject : register(b0)
{
    float4x4 gMVP;
    float3   gLightDir; float _pad0;
    float3   gBaseColor; float gAmbient;
};
struct PSIn { float4 pos:SV_POSITION; float3 nrm:NORMAL; };
float4 main(PSIn input):SV_TARGET
{
    float3 normal = normalize(input.nrm);
    float3 lightVector = normalize(-gLightDir);
    float ndl = saturate(dot(normal, lightVector));
    float3 color = gBaseColor * (gAmbient + (1.0 - gAmbient) * ndl);
    return float4(color, 1);
}
)";

    auto CompileShader = [&](const char* sourceCode,
        const char* entryPoint,
        const char* profile,
        ComPtr<ID3DBlob>& outputBlob)
        {
            ComPtr<ID3DBlob> error;
            HRESULT hr = D3DCompile(
                sourceCode,
                (UINT)std::strlen(sourceCode),
                nullptr,
                nullptr,
                nullptr,
                entryPoint,
                profile,
                0,
                0,
                &outputBlob,
                &error);
            if (FAILED(hr))
            {
                HR(hr, "D3DCompile(Jelly)", BlobToString(error.Get()));
            }
        };

    ComPtr<ID3DBlob> vsPCBlob, psPCBlob, vsSolidBlob, psSolidBlob;
    CompileShader(vertexShaderPositionColor, "main", "vs_5_0", vsPCBlob);
    CompileShader(pixelShaderPositionColor, "main", "ps_5_0", psPCBlob);
    CompileShader(vertexShaderSolid, "main", "vs_5_0", vsSolidBlob);
    CompileShader(pixelShaderSolid, "main", "ps_5_0", psSolidBlob);

    D3D12_RASTERIZER_DESC rasterizer{};
    rasterizer.FillMode = D3D12_FILL_MODE_SOLID;
    rasterizer.CullMode = D3D12_CULL_MODE_NONE;
    rasterizer.FrontCounterClockwise = FALSE;
    rasterizer.DepthBias = D3D12_DEFAULT_DEPTH_BIAS;
    rasterizer.DepthBiasClamp = D3D12_DEFAULT_DEPTH_BIAS_CLAMP;
    rasterizer.SlopeScaledDepthBias = D3D12_DEFAULT_SLOPE_SCALED_DEPTH_BIAS;
    rasterizer.DepthClipEnable = TRUE;
    rasterizer.MultisampleEnable = FALSE;
    rasterizer.AntialiasedLineEnable = FALSE;
    rasterizer.ForcedSampleCount = 0;
    rasterizer.ConservativeRaster = D3D12_CONSERVATIVE_RASTERIZATION_MODE_OFF;

    D3D12_BLEND_DESC blend{};
    blend.RenderTarget[0].RenderTargetWriteMask = D3D12_COLOR_WRITE_ENABLE_ALL;

    D3D12_DEPTH_STENCIL_DESC depthStencil{};
    depthStencil.DepthEnable = TRUE;
    depthStencil.DepthWriteMask = D3D12_DEPTH_WRITE_MASK_ALL;
    depthStencil.DepthFunc = D3D12_COMPARISON_FUNC_LESS_EQUAL;
    depthStencil.StencilEnable = FALSE;

    auto MakePSO = [&](D3D12_INPUT_ELEMENT_DESC* inputLayout,
        UINT inputLayoutCount,
        ID3DBlob* vertexShader,
        ID3DBlob* pixelShader,
        D3D12_PRIMITIVE_TOPOLOGY_TYPE topologyType,
        ComPtr<ID3D12PipelineState>& outputPSO)
        {
            D3D12_GRAPHICS_PIPELINE_STATE_DESC pso{};
            pso.InputLayout = { inputLayout, inputLayoutCount };
            pso.pRootSignature = m_rootSig.Get();
            pso.VS = { vertexShader->GetBufferPointer(), vertexShader->GetBufferSize() };
            pso.PS = { pixelShader->GetBufferPointer(), pixelShader->GetBufferSize() };
            pso.RasterizerState = rasterizer;
            pso.BlendState = blend;
            pso.DepthStencilState = depthStencil;
            pso.SampleMask = UINT_MAX;
            pso.PrimitiveTopologyType = topologyType;
            pso.NumRenderTargets = 1;
            pso.RTVFormats[0] = DXGI_FORMAT_R8G8B8A8_UNORM;
            pso.DSVFormat = DXGI_FORMAT_D32_FLOAT;
            pso.SampleDesc.Count = 1;
            HR(device->CreateGraphicsPipelineState(
                &pso,
                IID_PPV_ARGS(&outputPSO)),
                "CreateGraphicsPipelineState(Jelly)");
        };

    D3D12_INPUT_ELEMENT_DESC inputLayoutPositionColor[] =
    {
        { "POSITION",0,DXGI_FORMAT_R32G32B32_FLOAT,0,0,
          D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA,0 },
        { "COLOR",   0,DXGI_FORMAT_R32G32B32_FLOAT,0,12,
          D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA,0 },
    };

    D3D12_INPUT_ELEMENT_DESC inputLayoutPositionNormal[] =
    {
        { "POSITION",0,DXGI_FORMAT_R32G32B32_FLOAT,0,0,
          D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA,0 },
        { "NORMAL",  0,DXGI_FORMAT_R32G32B32_FLOAT,0,12,
          D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA,0 },
    };

    MakePSO(inputLayoutPositionColor, _countof(inputLayoutPositionColor),
        vsPCBlob.Get(), psPCBlob.Get(),
        D3D12_PRIMITIVE_TOPOLOGY_TYPE_LINE, m_psoLines);

    MakePSO(inputLayoutPositionColor, _countof(inputLayoutPositionColor),
        vsPCBlob.Get(), psPCBlob.Get(),
        D3D12_PRIMITIVE_TOPOLOGY_TYPE_POINT, m_psoPoints);

    MakePSO(inputLayoutPositionNormal, _countof(inputLayoutPositionNormal),
        vsSolidBlob.Get(), psSolidBlob.Get(),
        D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE, m_psoSolid);

    UINT cbSize = (UINT)((sizeof(CB) + 255) & ~255u);
    CreateUploadBuffer(cbSize, (void**)&m_mapCB, m_cb, nullptr);
}

void JellyScene::BuildGeometry(ID3D12Device* device)
{
    for (int index = 0; index < N3; ++index)
    {
        m_jellyVerts[index].p[0] = particlePositions[index].x;
        m_jellyVerts[index].p[1] = particlePositions[index].y;
        m_jellyVerts[index].p[2] = particlePositions[index].z;

        m_jellyVerts[index].c[0] = 0.0f;
        m_jellyVerts[index].c[1] = 0.6f;
        m_jellyVerts[index].c[2] = 1.0f;
    }

    CreateUploadBuffer(
        (UINT)sizeof(m_jellyVerts),
        (void**)&m_mapJellyVB,
        m_vbJelly,
        m_jellyVerts.data());

    m_vbvJelly.BufferLocation = m_vbJelly->GetGPUVirtualAddress();
    m_vbvJelly.SizeInBytes = (UINT)sizeof(m_jellyVerts);
    m_vbvJelly.StrideInBytes = (UINT)sizeof(VertexPC);

    m_jellyIdxAxial.clear();
    m_jellyIdxDiag.clear();
    m_jellyIdxAxial.reserve(N3 * 3 * 2);
    m_jellyIdxDiag.reserve(N3 * 6 * 2);

    for (int xIndex = 0; xIndex < N; ++xIndex)
    {
        for (int yIndex = 0; yIndex < N; ++yIndex)
        {
            for (int zIndex = 0; zIndex < N; ++zIndex)
            {
                int currentIndex = Index(xIndex, yIndex, zIndex);

                if (xIndex < N - 1)
                {
                    int neighborIndex = Index(xIndex + 1, yIndex, zIndex);
                    m_jellyIdxAxial.push_back(currentIndex);
                    m_jellyIdxAxial.push_back(neighborIndex);
                }
                if (yIndex < N - 1)
                {
                    int neighborIndex = Index(xIndex, yIndex + 1, zIndex);
                    m_jellyIdxAxial.push_back(currentIndex);
                    m_jellyIdxAxial.push_back(neighborIndex);
                }
                if (zIndex < N - 1)
                {
                    int neighborIndex = Index(xIndex, yIndex, zIndex + 1);
                    m_jellyIdxAxial.push_back(currentIndex);
                    m_jellyIdxAxial.push_back(neighborIndex);
                }

                if (xIndex < N - 1 && yIndex < N - 1)
                {
                    int neighborIndex = Index(xIndex + 1, yIndex + 1, zIndex);
                    m_jellyIdxDiag.push_back(currentIndex);
                    m_jellyIdxDiag.push_back(neighborIndex);
                }
                if (xIndex < N - 1 && yIndex > 0)
                {
                    int neighborIndex = Index(xIndex + 1, yIndex - 1, zIndex);
                    m_jellyIdxDiag.push_back(currentIndex);
                    m_jellyIdxDiag.push_back(neighborIndex);
                }

                if (xIndex < N - 1 && zIndex < N - 1)
                {
                    int neighborIndex = Index(xIndex + 1, yIndex, zIndex + 1);
                    m_jellyIdxDiag.push_back(currentIndex);
                    m_jellyIdxDiag.push_back(neighborIndex);
                }
                if (xIndex < N - 1 && zIndex > 0)
                {
                    int neighborIndex = Index(xIndex + 1, yIndex, zIndex - 1);
                    m_jellyIdxDiag.push_back(currentIndex);
                    m_jellyIdxDiag.push_back(neighborIndex);
                }

                if (yIndex < N - 1 && zIndex < N - 1)
                {
                    int neighborIndex = Index(xIndex, yIndex + 1, zIndex + 1);
                    m_jellyIdxDiag.push_back(currentIndex);
                    m_jellyIdxDiag.push_back(neighborIndex);
                }
                if (yIndex < N - 1 && zIndex > 0)
                {
                    int neighborIndex = Index(xIndex, yIndex + 1, zIndex - 1);
                    m_jellyIdxDiag.push_back(currentIndex);
                    m_jellyIdxDiag.push_back(neighborIndex);
                }
            }
        }
    }

    CreateUploadBuffer(
        (UINT)(m_jellyIdxAxial.size() * sizeof(uint32_t)),
        nullptr,
        m_ibJellyAxial,
        m_jellyIdxAxial.data());

    m_ibvJellyAxial.BufferLocation = m_ibJellyAxial->GetGPUVirtualAddress();
    m_ibvJellyAxial.SizeInBytes = (UINT)(m_jellyIdxAxial.size() * sizeof(uint32_t));
    m_ibvJellyAxial.Format = DXGI_FORMAT_R32_UINT;

    CreateUploadBuffer(
        (UINT)(m_jellyIdxDiag.size() * sizeof(uint32_t)),
        nullptr,
        m_ibJellyDiag,
        m_jellyIdxDiag.data());

    m_ibvJellyDiag.BufferLocation = m_ibJellyDiag->GetGPUVirtualAddress();
    m_ibvJellyDiag.SizeInBytes = (UINT)(m_jellyIdxDiag.size() * sizeof(uint32_t));
    m_ibvJellyDiag.Format = DXGI_FORMAT_R32_UINT;

    m_ctrlIdx = {
        0,1, 1,3, 3,2, 2,0,
        4,5, 5,7, 7,6, 6,4,
        0,4, 1,5, 2,6, 3,7
    };
    m_boundIdx = m_ctrlIdx;

    Float3 controlCornersWorld[8];
    ControlCorners(controlCornersWorld);
    for (int i = 0; i < 8; ++i)
    {
        m_ctrlVerts[i].p[0] = controlCornersWorld[i].x;
        m_ctrlVerts[i].p[1] = controlCornersWorld[i].y;
        m_ctrlVerts[i].p[2] = controlCornersWorld[i].z;

        m_ctrlVerts[i].c[0] = 1.0f;
        m_ctrlVerts[i].c[1] = 0.6f;
        m_ctrlVerts[i].c[2] = 0.1f;
    }

    CreateUploadBuffer(
        (UINT)sizeof(m_ctrlVerts),
        (void**)&m_mapCtrlVB,
        m_vbCtrl,
        m_ctrlVerts.data());

    m_vbvCtrl.BufferLocation = m_vbCtrl->GetGPUVirtualAddress();
    m_vbvCtrl.SizeInBytes = (UINT)sizeof(m_ctrlVerts);
    m_vbvCtrl.StrideInBytes = (UINT)sizeof(VertexPC);

    CreateUploadBuffer(
        (UINT)(m_ctrlIdx.size() * sizeof(uint32_t)),
        nullptr,
        m_ibCtrl,
        m_ctrlIdx.data());

    m_ibvCtrl.BufferLocation = m_ibCtrl->GetGPUVirtualAddress();
    m_ibvCtrl.SizeInBytes = (UINT)(m_ctrlIdx.size() * sizeof(uint32_t));
    m_ibvCtrl.Format = DXGI_FORMAT_R32_UINT;

    CreateUploadBuffer(
        (UINT)sizeof(m_boundVerts),
        (void**)&m_mapBoundVB,
        m_vbBound,
        m_boundVerts.data());

    m_vbvBound.BufferLocation = m_vbBound->GetGPUVirtualAddress();
    m_vbvBound.SizeInBytes = (UINT)sizeof(m_boundVerts);
    m_vbvBound.StrideInBytes = (UINT)sizeof(VertexPC);

    CreateUploadBuffer(
        (UINT)(m_boundIdx.size() * sizeof(uint32_t)),
        nullptr,
        m_ibBound,
        m_boundIdx.data());

    m_ibvBound.BufferLocation = m_ibBound->GetGPUVirtualAddress();
    m_ibvBound.SizeInBytes = (UINT)(m_boundIdx.size() * sizeof(uint32_t));
    m_ibvBound.Format = DXGI_FORMAT_R32_UINT;

    BuildBezierIndexBuffer();
    m_bezierVtxCount = 6u * (UINT)(bezierTessellation + 1) * (UINT)(bezierTessellation + 1);
    UINT vertexBufferBytes = m_bezierVtxCount * (UINT)sizeof(VertexPN);

    CreateUploadBuffer(vertexBufferBytes, (void**)&m_mapBezierVB, m_vbBezier, nullptr);
    m_vbvBezier.BufferLocation = m_vbBezier->GetGPUVirtualAddress();
    m_vbvBezier.SizeInBytes = vertexBufferBytes;
    m_vbvBezier.StrideInBytes = (UINT)sizeof(VertexPN);

    CreateUploadBuffer(
        (UINT)(m_bezierIdx.size() * sizeof(uint32_t)),
        nullptr,
        m_ibBezier,
        m_bezierIdx.data());

    m_ibvBezier.BufferLocation = m_ibBezier->GetGPUVirtualAddress();
    m_ibvBezier.SizeInBytes = (UINT)(m_bezierIdx.size() * sizeof(uint32_t));
    m_ibvBezier.Format = DXGI_FORMAT_R32_UINT;

    BuildDeformedObjectMesh();
}

void JellyScene::CreateUploadBuffer(
    UINT byteSize,
    void** mappedPointer,
    ComPtr<ID3D12Resource>& resource,
    const void* initialData)
{
    D3D12_HEAP_PROPERTIES heap{};
    heap.Type = D3D12_HEAP_TYPE_UPLOAD;

    D3D12_RESOURCE_DESC desc{};
    desc.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER;
    desc.Width = byteSize;
    desc.Height = 1;
    desc.DepthOrArraySize = 1;
    desc.MipLevels = 1;
    desc.SampleDesc.Count = 1;
    desc.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;

    HR(m_device->CreateCommittedResource(
        &heap,
        D3D12_HEAP_FLAG_NONE,
        &desc,
        D3D12_RESOURCE_STATE_GENERIC_READ,
        nullptr,
        IID_PPV_ARGS(&resource)),
        "CreateCommittedResource(Upload)");

    if (mappedPointer || initialData)
    {
        void* mappedMemory = nullptr;
        D3D12_RANGE range{ 0,0 };
        HR(resource->Map(0, &range, &mappedMemory), "Map(Upload)");

        if (initialData)
        {
            std::memcpy(mappedMemory, initialData, byteSize);
        }

        if (mappedPointer)
        {
            *mappedPointer = mappedMemory;
        }
        else
        {
            resource->Unmap(0, nullptr);
        }
    }
}

void JellyScene::UpdateDynamicVBs()
{
    for (int index = 0; index < N3; ++index)
    {
        m_jellyVerts[index].p[0] = particlePositions[index].x;
        m_jellyVerts[index].p[1] = particlePositions[index].y;
        m_jellyVerts[index].p[2] = particlePositions[index].z;
    }
    std::memcpy(m_mapJellyVB, m_jellyVerts.data(), sizeof(m_jellyVerts));

    Float3 controlCornersWorld[8];
    ControlCorners(controlCornersWorld);
    for (int i = 0; i < 8; ++i)
    {
        m_ctrlVerts[i].p[0] = controlCornersWorld[i].x;
        m_ctrlVerts[i].p[1] = controlCornersWorld[i].y;
        m_ctrlVerts[i].p[2] = controlCornersWorld[i].z;
    }
    std::memcpy(m_mapCtrlVB, m_ctrlVerts.data(), sizeof(m_ctrlVerts));

    float halfExtentX = boundingBoxHalfExtent.x;
    float halfExtentY = boundingBoxHalfExtent.y;
    float halfExtentZ = boundingBoxHalfExtent.z;

    Float3 boundingCorners[8] =
    {
        { -halfExtentX, -halfExtentY, -halfExtentZ },
        { +halfExtentX, -halfExtentY, -halfExtentZ },
        { -halfExtentX, +halfExtentY, -halfExtentZ },
        { +halfExtentX, +halfExtentY, -halfExtentZ },
        { -halfExtentX, -halfExtentY, +halfExtentZ },
        { +halfExtentX, -halfExtentY, +halfExtentZ },
        { -halfExtentX, +halfExtentY, +halfExtentZ },
        { +halfExtentX, +halfExtentY, +halfExtentZ }
    };

    for (int i = 0; i < 8; ++i)
    {
        m_boundVerts[i].p[0] = boundingCorners[i].x;
        m_boundVerts[i].p[1] = boundingCorners[i].y;
        m_boundVerts[i].p[2] = boundingCorners[i].z;
        m_boundVerts[i].c[0] = 0.7f;
        m_boundVerts[i].c[1] = 0.7f;
        m_boundVerts[i].c[2] = 0.7f;
    }
    std::memcpy(m_mapBoundVB, m_boundVerts.data(), sizeof(m_boundVerts));

    UpdateBezierSurfaceVB();
    UpdateDeformedObjectVB();
}

void JellyScene::UpdateCB()
{
    XMMATRIX world = XMMatrixIdentity();
    XMMATRIX view = m_cam.ViewRH();
    XMMATRIX proj = m_cam.ProjRH();

    CB cb{};
    XMStoreFloat4x4(&cb.mvp, XMMatrixTranspose(world * view * proj));

    XMVECTOR eye = m_cam.GetEye();
    XMVECTOR at = m_cam.GetAt();
    XMVECTOR forward = XMVector3Normalize(XMVectorSubtract(at, eye));

    XMFLOAT3 forwardFloat{};
    XMStoreFloat3(&forwardFloat, forward);

    cb.lightDir = { forwardFloat.x, forwardFloat.y, forwardFloat.z };

    cb.baseColor = { 0.20f, 0.70f, 1.00f };
    cb.ambient = 0.25f;

    std::memcpy(m_mapCB, &cb, sizeof(cb));
}

int JellyScene::Index(int xIndex, int yIndex, int zIndex) const
{
    return (xIndex * N + yIndex) * N + zIndex;
}

bool JellyScene::IsCorner(int xIndex, int yIndex, int zIndex) const
{
    return ((xIndex == 0 || xIndex == N - 1) &&
        (yIndex == 0 || yIndex == N - 1) &&
        (zIndex == 0 || zIndex == N - 1));
}

bool JellyScene::IsSurface(int xIndex, int yIndex, int zIndex) const
{
    return (xIndex == 0 || xIndex == N - 1 ||
        yIndex == 0 || yIndex == N - 1 ||
        zIndex == 0 || zIndex == N - 1);
}

XMMATRIX JellyScene::ControlRotation() const
{
    float rotationX = DegToRad(controlCubeEulerAnglesDeg.x);
    float rotationY = DegToRad(controlCubeEulerAnglesDeg.y);
    float rotationZ = DegToRad(controlCubeEulerAnglesDeg.z);
    return XMMatrixRotationX(rotationX) * XMMatrixRotationY(rotationY) * XMMatrixRotationZ(rotationZ);
}

JellyScene::Float3 JellyScene::MapRestToWorld(const Float3& restUnitPosition) const
{
    XMMATRIX rotation = ControlRotation();
    XMVECTOR translation = XMVectorSet(
        controlCubePosition.x,
        controlCubePosition.y,
        controlCubePosition.z, 0);

    XMVECTOR point = XMVectorSet(
        restUnitPosition.x * controlCubeEdgeLength,
        restUnitPosition.y * controlCubeEdgeLength,
        restUnitPosition.z * controlCubeEdgeLength,
        0);
    point = XMVector3Transform(point, rotation) + translation;

    XMFLOAT3 pointFloat3;
    XMStoreFloat3(&pointFloat3, point);
    return { pointFloat3.x, pointFloat3.y, pointFloat3.z };
}

void JellyScene::ControlCorners(Float3 outCorners[8]) const
{
    float halfSize = 0.5f * controlCubeEdgeLength;
    Float3 localCorners[8] =
    {
        { -halfSize, -halfSize, -halfSize },
        { +halfSize, -halfSize, -halfSize },
        { -halfSize, +halfSize, -halfSize },
        { +halfSize, +halfSize, -halfSize },
        { -halfSize, -halfSize, +halfSize },
        { +halfSize, -halfSize, +halfSize },
        { -halfSize, +halfSize, +halfSize },
        { +halfSize, +halfSize, +halfSize }
    };

    XMMATRIX rotation = ControlRotation();
    XMVECTOR translation = XMVectorSet(
        controlCubePosition.x,
        controlCubePosition.y,
        controlCubePosition.z,
        0);

    for (int i = 0; i < 8; ++i)
    {
        XMVECTOR point = XMVectorSet(localCorners[i].x, localCorners[i].y, localCorners[i].z, 0);
        point = XMVector3Transform(point, rotation) + translation;
        XMFLOAT3 pointFloat3;
        XMStoreFloat3(&pointFloat3, point);
        outCorners[i] = { pointFloat3.x, pointFloat3.y, pointFloat3.z };
    }
}

void JellyScene::ApplyInitialDisturbance()
{
    if (initialRandomVelocityRange == 0.0f)
        return;

    std::random_device randomDevice;
    std::mt19937 randomGenerator(randomDevice());
    std::uniform_real_distribution<float> randomDistribution(-initialRandomVelocityRange,
        initialRandomVelocityRange);

    for (int index = 0; index < N3; ++index)
    {
        particleVelocities[index] = F3(
            randomDistribution(randomGenerator),
            randomDistribution(randomGenerator),
            randomDistribution(randomGenerator));
    }
}

void JellyScene::StepOnce(float timeStep)
{
    const float spacing = controlCubeEdgeLength / (N - 1);
    const float restLengthAxial = spacing;
    const float restLengthShear = spacing * 1.41421356f;

    Float3 gravityVector = {
        gravityAcceleration.x,
        gravityAcceleration.y,
        gravityAcceleration.z
    };

    std::array<Float3, N3> newParticlePositions = particlePositions;
    std::array<Float3, N3> newParticleVelocities = particleVelocities;

    auto addSpringForce = [&](Float3& totalForce,
        const Float3& currentPosition,
        const Float3& neighborPosition,
        float stiffness,
        float restLength)
        {
            Float3 displacement = sub(neighborPosition, currentPosition);
            float distance = len(displacement);
            if (distance < 1e-6f) return;
            float stretch = distance - restLength;
            Float3 direction = mul(displacement, 1.0f / distance);
            totalForce = add(totalForce, mul(direction, stiffness * stretch));
        };

    for (int flatIndex = 0; flatIndex < N3; ++flatIndex)
    {
        int planeSize = N * N;
        int xIndex = flatIndex / planeSize;
        int remainder = flatIndex % planeSize;
        int yIndex = remainder / N;
        int zIndex = remainder % N;

        Float3 positionCurrent = particlePositions[flatIndex];
        Float3 velocityCurrent = particleVelocities[flatIndex];

        Float3 totalForce = F3(0, 0, 0);
        totalForce = add(totalForce, mul(gravityVector, particleMass));
        totalForce = add(totalForce, mul(velocityCurrent, -linearDampingCoefficient));

        if (xIndex > 0)
            addSpringForce(totalForce, positionCurrent, particlePositions[Index(xIndex - 1, yIndex, zIndex)],
                axialSpringStiffness, restLengthAxial);
        if (xIndex < N - 1)
            addSpringForce(totalForce, positionCurrent, particlePositions[Index(xIndex + 1, yIndex, zIndex)],
                axialSpringStiffness, restLengthAxial);
        if (yIndex > 0)
            addSpringForce(totalForce, positionCurrent, particlePositions[Index(xIndex, yIndex - 1, zIndex)],
                axialSpringStiffness, restLengthAxial);
        if (yIndex < N - 1)
            addSpringForce(totalForce, positionCurrent, particlePositions[Index(xIndex, yIndex + 1, zIndex)],
                axialSpringStiffness, restLengthAxial);
        if (zIndex > 0)
            addSpringForce(totalForce, positionCurrent, particlePositions[Index(xIndex, yIndex, zIndex - 1)],
                axialSpringStiffness, restLengthAxial);
        if (zIndex < N - 1)
            addSpringForce(totalForce, positionCurrent, particlePositions[Index(xIndex, yIndex, zIndex + 1)],
                axialSpringStiffness, restLengthAxial);

        if (xIndex > 0 && yIndex > 0)
            addSpringForce(totalForce, positionCurrent, particlePositions[Index(xIndex - 1, yIndex - 1, zIndex)],
                shearSpringStiffness, restLengthShear);
        if (xIndex > 0 && yIndex < N - 1)
            addSpringForce(totalForce, positionCurrent, particlePositions[Index(xIndex - 1, yIndex + 1, zIndex)],
                shearSpringStiffness, restLengthShear);
        if (xIndex < N - 1 && yIndex > 0)
            addSpringForce(totalForce, positionCurrent, particlePositions[Index(xIndex + 1, yIndex - 1, zIndex)],
                shearSpringStiffness, restLengthShear);
        if (xIndex < N - 1 && yIndex < N - 1)
            addSpringForce(totalForce, positionCurrent, particlePositions[Index(xIndex + 1, yIndex + 1, zIndex)],
                shearSpringStiffness, restLengthShear);

        if (xIndex > 0 && zIndex > 0)
            addSpringForce(totalForce, positionCurrent, particlePositions[Index(xIndex - 1, yIndex, zIndex - 1)],
                shearSpringStiffness, restLengthShear);
        if (xIndex > 0 && zIndex < N - 1)
            addSpringForce(totalForce, positionCurrent, particlePositions[Index(xIndex - 1, yIndex, zIndex + 1)],
                shearSpringStiffness, restLengthShear);
        if (xIndex < N - 1 && zIndex > 0)
            addSpringForce(totalForce, positionCurrent, particlePositions[Index(xIndex + 1, yIndex, zIndex - 1)],
                shearSpringStiffness, restLengthShear);
        if (xIndex < N - 1 && zIndex < N - 1)
            addSpringForce(totalForce, positionCurrent, particlePositions[Index(xIndex + 1, yIndex, zIndex + 1)],
                shearSpringStiffness, restLengthShear);

        if (yIndex > 0 && zIndex > 0)
            addSpringForce(totalForce, positionCurrent, particlePositions[Index(xIndex, yIndex - 1, zIndex - 1)],
                shearSpringStiffness, restLengthShear);
        if (yIndex > 0 && zIndex < N - 1)
            addSpringForce(totalForce, positionCurrent, particlePositions[Index(xIndex, yIndex - 1, zIndex + 1)],
                shearSpringStiffness, restLengthShear);
        if (yIndex < N - 1 && zIndex > 0)
            addSpringForce(totalForce, positionCurrent, particlePositions[Index(xIndex, yIndex + 1, zIndex - 1)],
                shearSpringStiffness, restLengthShear);
        if (yIndex < N - 1 && zIndex < N - 1)
            addSpringForce(totalForce, positionCurrent, particlePositions[Index(xIndex, yIndex + 1, zIndex + 1)],
                shearSpringStiffness, restLengthShear);

        if (enableControlCubeCoupling)
        {
            if (IsCorner(xIndex, yIndex, zIndex))
            {
                Float3 targetPosition = MapRestToWorld(restUnitPositions[flatIndex]);
                totalForce = add(totalForce, mul(sub(targetPosition, positionCurrent),
                    controlCubeCornerStiffness));
            }
            else if (enableSurfaceControlCubeCoupling &&
                IsSurface(xIndex, yIndex, zIndex))
            {
                Float3 targetPosition = MapRestToWorld(restUnitPositions[flatIndex]);
                totalForce = add(totalForce, mul(sub(targetPosition, positionCurrent),
                    controlCubeSurfaceStiffness));
            }
        }

        Float3 acceleration = mul(totalForce, 1.0f / particleMass);
        Float3 newVelocity = add(velocityCurrent, mul(acceleration, timeStep));
        Float3 newPosition = add(positionCurrent, mul(newVelocity, timeStep));

        newParticleVelocities[flatIndex] = newVelocity;
        newParticlePositions[flatIndex] = newPosition;
    }

    particlePositions = newParticlePositions;
    particleVelocities = newParticleVelocities;
}

void JellyScene::ApplyCollisions()
{
    float boundX = boundingBoxHalfExtent.x;
    float boundY = boundingBoxHalfExtent.y;
    float boundZ = boundingBoxHalfExtent.z;

    for (int flatIndex = 0; flatIndex < N3; ++flatIndex)
    {
        auto& position = particlePositions[flatIndex];
        auto& velocity = particleVelocities[flatIndex];

        bool hasCollision;
        do
        {
            hasCollision = false;

            auto handleCollisionAxis = [&](float& coordinate,
                float& velocityComponent,
                float bound)
                {
                    if (coordinate > bound)
                    {
                        float penetration = coordinate - bound;
                        coordinate -= 2.0f * penetration;
                        velocityComponent = -velocityComponent;
                        if (applyRestitutionToWholeVelocity)
                            velocity = mul(velocity, collisionRestitutionCoefficient);
                        else
                            velocityComponent *= collisionRestitutionCoefficient;
                        hasCollision = true;
                    }
                    else if (coordinate < -bound)
                    {
                        float penetration = -bound - coordinate;
                        coordinate += 2.0f * penetration;
                        velocityComponent = -velocityComponent;
                        if (applyRestitutionToWholeVelocity)
                            velocity = mul(velocity, collisionRestitutionCoefficient);
                        else
                            velocityComponent *= collisionRestitutionCoefficient;
                        hasCollision = true;
                    }
                };

            handleCollisionAxis(position.x, velocity.x, boundX);
            handleCollisionAxis(position.y, velocity.y, boundY);
            handleCollisionAxis(position.z, velocity.z, boundZ);

        } while (hasCollision);
    }
}

void JellyScene::Bernstein3(float parameter, float bernstein[4])
{
    float complement = 1.0f - parameter;
    bernstein[0] = complement * complement * complement;
    bernstein[1] = 3.0f * parameter * complement * complement;
    bernstein[2] = 3.0f * parameter * parameter * complement;
    bernstein[3] = parameter * parameter * parameter;
}

void JellyScene::Bernstein2(float parameter, float bernstein[3])
{
    float complement = 1.0f - parameter;
    bernstein[0] = complement * complement;
    bernstein[1] = 2.0f * parameter * complement;
    bernstein[2] = parameter * parameter;
}

JellyScene::Float3 JellyScene::EvalPatch(
    int faceIndex,
    float sParameter,
    float tParameter,
    Float3& derivativeS,
    Float3& derivativeT) const
{
    Float3 controlPoints[4][4]{};

    auto ControlPoint = [&](int xIndex, int yIndex, int zIndex) -> Float3
        {
            return particlePositions[Index(xIndex, yIndex, zIndex)];
        };

    switch (faceIndex)
    {
    case 0:
        for (int xIndex = 0; xIndex < 4; ++xIndex)
            for (int yIndex = 0; yIndex < 4; ++yIndex)
                controlPoints[xIndex][yIndex] = ControlPoint(xIndex, yIndex, 3);
        break;
    case 1:
        for (int xIndex = 0; xIndex < 4; ++xIndex)
            for (int yIndex = 0; yIndex < 4; ++yIndex)
                controlPoints[xIndex][yIndex] = ControlPoint(xIndex, yIndex, 0);
        break;
    case 2:
        for (int yIndex = 0; yIndex < 4; ++yIndex)
            for (int zIndex = 0; zIndex < 4; ++zIndex)
                controlPoints[yIndex][zIndex] = ControlPoint(3, yIndex, zIndex);
        break;
    case 3:
        for (int yIndex = 0; yIndex < 4; ++yIndex)
            for (int zIndex = 0; zIndex < 4; ++zIndex)
                controlPoints[yIndex][zIndex] = ControlPoint(0, yIndex, zIndex);
        break;
    case 4:
        for (int xIndex = 0; xIndex < 4; ++xIndex)
            for (int zIndex = 0; zIndex < 4; ++zIndex)
                controlPoints[xIndex][zIndex] = ControlPoint(xIndex, 3, zIndex);
        break;
    case 5:
        for (int xIndex = 0; xIndex < 4; ++xIndex)
            for (int zIndex = 0; zIndex < 4; ++zIndex)
                controlPoints[xIndex][zIndex] = ControlPoint(xIndex, 0, zIndex);
        break;
    }

    float bernsteinS[4], bernsteinT[4], bernsteinS2[3], bernsteinT2[3];
    Bernstein3(sParameter, bernsteinS);
    Bernstein3(tParameter, bernsteinT);
    Bernstein2(sParameter, bernsteinS2);
    Bernstein2(tParameter, bernsteinT2);

    Float3 position = F3(0, 0, 0);
    for (int xIndex = 0; xIndex < 4; ++xIndex)
        for (int yIndex = 0; yIndex < 4; ++yIndex)
            position = add(position, mul(controlPoints[xIndex][yIndex],
                bernsteinS[xIndex] * bernsteinT[yIndex]));

    derivativeS = F3(0, 0, 0);
    for (int xIndex = 0; xIndex < 3; ++xIndex)
        for (int yIndex = 0; yIndex < 4; ++yIndex)
            derivativeS = add(
                derivativeS,
                mul(
                    sub(controlPoints[xIndex + 1][yIndex], controlPoints[xIndex][yIndex]),
                    3.0f * bernsteinS2[xIndex] * bernsteinT[yIndex]));

    derivativeT = F3(0, 0, 0);
    for (int xIndex = 0; xIndex < 4; ++xIndex)
        for (int yIndex = 0; yIndex < 3; ++yIndex)
            derivativeT = add(
                derivativeT,
                mul(
                    sub(controlPoints[xIndex][yIndex + 1], controlPoints[xIndex][yIndex]),
                    3.0f * bernsteinS[xIndex] * bernsteinT2[yIndex]));

    return position;
}

void JellyScene::BuildBezierIndexBuffer()
{
    m_bezierIdx.clear();

    const int tessellation = std::max(1, bezierTessellation);
    uint32_t stride = (uint32_t)(tessellation + 1);
    uint32_t verticesPerFace = stride * stride;

    for (uint32_t face = 0; face < 6; ++face)
    {
        uint32_t base = face * verticesPerFace;
        for (uint32_t y = 0; y < (uint32_t)tessellation; ++y)
        {
            for (uint32_t x = 0; x < (uint32_t)tessellation; ++x)
            {
                uint32_t v0 = base + y * stride + x;
                uint32_t v1 = v0 + 1;
                uint32_t v2 = v0 + stride;
                uint32_t v3 = v2 + 1;

                m_bezierIdx.push_back(v0);
                m_bezierIdx.push_back(v2);
                m_bezierIdx.push_back(v1);

                m_bezierIdx.push_back(v1);
                m_bezierIdx.push_back(v2);
                m_bezierIdx.push_back(v3);
            }
        }
    }
}

void JellyScene::UpdateBezierSurfaceVB()
{
    if (!m_mapBezierVB) return;

    const int tessellation = std::max(1, bezierTessellation);
    const float invT = 1.0f / (float)tessellation;

    const uint32_t stride = (uint32_t)(tessellation + 1);
    const uint32_t verticesPerFace = stride * stride;

    const float faceNormalSign[6] = { +1.f, -1.f, +1.f, -1.f, -1.f, +1.f };

    for (int faceIndex = 0; faceIndex < 6; ++faceIndex)
    {
        uint32_t base = (uint32_t)faceIndex * verticesPerFace;

        for (int yIndex = 0; yIndex <= tessellation; ++yIndex)
        {
            for (int xIndex = 0; xIndex <= tessellation; ++xIndex)
            {
                float sParameter = xIndex * invT;
                float tParameter = yIndex * invT;

                Float3 derivativeS, derivativeT;
                Float3 position = EvalPatch(faceIndex, sParameter, tParameter, derivativeS, derivativeT);

                Float3 normal = cross3(derivativeS, derivativeT);
                normal = mul(normal, faceNormalSign[faceIndex]);
                normal = normalize3(normal);

                VertexPN vertex{};
                vertex.p[0] = position.x;
                vertex.p[1] = position.y;
                vertex.p[2] = position.z;
                vertex.n[0] = normal.x;
                vertex.n[1] = normal.y;
                vertex.n[2] = normal.z;

                m_mapBezierVB[base + (uint32_t)yIndex * stride + (uint32_t)xIndex] = vertex;
            }
        }
    }
}

// ======================
// Bezier volume + obiekt
// ======================

JellyScene::Float3 JellyScene::EvalVolume(float u, float v, float w) const
{
    float Bu[4], Bv[4], Bw[4];
    Bernstein3(u, Bu);
    Bernstein3(v, Bv);
    Bernstein3(w, Bw);

    Float3 out = F3(0, 0, 0);

    for (int i = 0; i < 4; ++i)
        for (int j = 0; j < 4; ++j)
            for (int k = 0; k < 4; ++k)
            {
                float weight = Bu[i] * Bv[j] * Bw[k];
                out = add(out, mul(particlePositions[Index(i, j, k)], weight));
            }

    return out;
}

void JellyScene::BuildDeformedObjectMesh()
{
    const int slices = OBJ_SLICES;
    const int stacks = OBJ_STACKS;

    const float radius = 0.35f;
    const Float3 center = F3(0.5f, 0.5f, 0.5f);

    m_objParam.clear();
    m_objIdx.clear();

    for (int i = 0; i <= stacks; ++i)
    {
        float vv = (float)i / (float)stacks;
        float theta = vv * XM_PI;

        float st = std::sin(theta);
        float ct = std::cos(theta);

        for (int j = 0; j <= slices; ++j)
        {
            float uu = (float)j / (float)slices;
            float phi = uu * XM_2PI;

            float sp = std::sin(phi);
            float cp = std::cos(phi);

            Float3 Q = F3(
                center.x + radius * (st * cp),
                center.y + radius * (ct),
                center.z + radius * (st * sp));

            Q.x = std::clamp(Q.x, 0.0f, 1.0f);
            Q.y = std::clamp(Q.y, 0.0f, 1.0f);
            Q.z = std::clamp(Q.z, 0.0f, 1.0f);

            m_objParam.push_back(Q);
        }
    }

    // --- POPRAWIONE INDEKSY (winding) -> poprawne normalne "na zewnątrz" ---
    const uint32_t stride = (uint32_t)(slices + 1);
    for (int i = 0; i < stacks; ++i)
    {
        for (int j = 0; j < slices; ++j)
        {
            uint32_t a = (uint32_t)i * stride + (uint32_t)j;
            uint32_t b = a + 1;
            uint32_t c = a + stride;
            uint32_t d = c + 1;

            // zamiast (a,c,b) i (b,c,d)
            m_objIdx.push_back(a);
            m_objIdx.push_back(b);
            m_objIdx.push_back(c);

            m_objIdx.push_back(b);
            m_objIdx.push_back(d);
            m_objIdx.push_back(c);
        }
    }

    m_objVtxCount = (uint32_t)m_objParam.size();
    UINT vbBytes = m_objVtxCount * (UINT)sizeof(VertexPN);

    CreateUploadBuffer(vbBytes, (void**)&m_mapObjVB, m_vbObj, nullptr);

    m_vbvObj.BufferLocation = m_vbObj->GetGPUVirtualAddress();
    m_vbvObj.SizeInBytes = vbBytes;
    m_vbvObj.StrideInBytes = (UINT)sizeof(VertexPN);

    CreateUploadBuffer((UINT)(m_objIdx.size() * sizeof(uint32_t)), nullptr, m_ibObj, m_objIdx.data());
    m_ibvObj.BufferLocation = m_ibObj->GetGPUVirtualAddress();
    m_ibvObj.SizeInBytes = (UINT)(m_objIdx.size() * sizeof(uint32_t));
    m_ibvObj.Format = DXGI_FORMAT_R32_UINT;

    UpdateDeformedObjectVB();
}

void JellyScene::UpdateDeformedObjectVB()
{
    if (!m_mapObjVB || m_objParam.empty() || m_objIdx.empty())
        return;

    const int slices = OBJ_SLICES;
    const int stacks = OBJ_STACKS;
    const uint32_t stride = (uint32_t)(slices + 1);

    const size_t vCount = m_objParam.size();

    std::vector<Float3> pos(vCount);
    std::vector<Float3> nrm(vCount, F3(0, 0, 0));

    // 1) deformacja: P = F(Q)
    for (size_t i = 0; i < vCount; ++i)
    {
        const Float3& Q = m_objParam[i];
        pos[i] = EvalVolume(Q.x, Q.y, Q.z);
    }

    // 2) normalne: suma normalnych trójkątów
    for (size_t t = 0; t + 2 < m_objIdx.size(); t += 3)
    {
        uint32_t i0 = m_objIdx[t + 0];
        uint32_t i1 = m_objIdx[t + 1];
        uint32_t i2 = m_objIdx[t + 2];

        const Float3& p0 = pos[i0];
        const Float3& p1 = pos[i1];
        const Float3& p2 = pos[i2];

        Float3 e1 = sub(p1, p0);
        Float3 e2 = sub(p2, p0);

        Float3 faceN = cross3(e1, e2); // ważone polem

        nrm[i0] = add(nrm[i0], faceN);
        nrm[i1] = add(nrm[i1], faceN);
        nrm[i2] = add(nrm[i2], faceN);
    }

    // --- POPRAWKA OŚWIETLENIA: weld normalnych na szwie i biegunach ---
    // Szew: j=0 i j=slices to ten sam punkt
    for (int i = 0; i <= stacks; ++i)
    {
        uint32_t left = (uint32_t)i * stride + 0;
        uint32_t right = (uint32_t)i * stride + (uint32_t)slices;
        Float3 avg = add(nrm[left], nrm[right]);
        nrm[left] = avg;
        nrm[right] = avg;
    }

    // Biegun górny: i=0
    {
        Float3 sum = F3(0, 0, 0);
        for (int j = 0; j <= slices; ++j) sum = add(sum, nrm[(uint32_t)j]);
        for (int j = 0; j <= slices; ++j) nrm[(uint32_t)j] = sum;
    }

    // Biegun dolny: i=stacks
    {
        uint32_t base = (uint32_t)stacks * stride;
        Float3 sum = F3(0, 0, 0);
        for (int j = 0; j <= slices; ++j) sum = add(sum, nrm[base + (uint32_t)j]);
        for (int j = 0; j <= slices; ++j) nrm[base + (uint32_t)j] = sum;
    }

    // 3) zapis do VB
    for (size_t i = 0; i < vCount; ++i)
    {
        Float3 normal = normalize3(nrm[i]);

        VertexPN v{};
        v.p[0] = pos[i].x; v.p[1] = pos[i].y; v.p[2] = pos[i].z;
        v.n[0] = normal.x; v.n[1] = normal.y; v.n[2] = normal.z;

        m_mapObjVB[i] = v;
    }
}

// ======================
// Matematyka pomocnicza
// ======================

JellyScene::Float3 JellyScene::F3(float x, float y, float z)
{
    return { x, y, z };
}

JellyScene::Float3 JellyScene::add(const Float3& a, const Float3& b)
{
    return { a.x + b.x, a.y + b.y, a.z + b.z };
}

JellyScene::Float3 JellyScene::sub(const Float3& a, const Float3& b)
{
    return { a.x - b.x, a.y - b.y, a.z - b.z };
}

JellyScene::Float3 JellyScene::mul(const Float3& a, float scalar)
{
    return { a.x * scalar, a.y * scalar, a.z * scalar };
}

float JellyScene::dot(const Float3& a, const Float3& b)
{
    return a.x * b.x + a.y * b.y + a.z * b.z;
}

float JellyScene::len(const Float3& a)
{
    return std::sqrt(dot(a, a));
}

JellyScene::Float3 JellyScene::cross3(const Float3& a, const Float3& b)
{
    return
    {
        a.y * b.z - a.z * b.y,
        a.z * b.x - a.x * b.z,
        a.x * b.y - a.y * b.x
    };
}

JellyScene::Float3 JellyScene::normalize3(const Float3& a)
{
    float length = len(a);
    if (length < 1e-6f) return { 0, 1, 0 };
    return mul(a, 1.0f / length);
}
