#include "CubeScene.h"

#include <d3dcompiler.h>
#include <cstring>

using namespace DirectX;

namespace {
UINT Align256(UINT x) { return (x + 255u) & ~255u; }

const char* kCubeShader = R"(
cbuffer CB : register(b0) { float4x4 gMVP; };

struct VSIn  { float3 pos : POSITION; float3 col : COLOR; };
struct VSOut { float4 pos : SV_POSITION; float3 col : COLOR; };

VSOut VSMain(VSIn input)
{
    VSOut output;
    output.pos = mul(float4(input.pos, 1.0f), gMVP);
    output.col = input.col;
    return output;
}

float4 PSMain(VSOut input) : SV_TARGET
{
    return float4(input.col, 1.0f);
}
)";
}

void CubeScene::Init(ID3D12Device* device)
{
    D3D12_ROOT_PARAMETER param{};
    param.ParameterType = D3D12_ROOT_PARAMETER_TYPE_CBV;
    param.Descriptor.ShaderRegister = 0;
    param.Descriptor.RegisterSpace = 0;
    param.ShaderVisibility = D3D12_SHADER_VISIBILITY_VERTEX;

    D3D12_ROOT_SIGNATURE_DESC rsDesc{};
    rsDesc.NumParameters = 1;
    rsDesc.pParameters = &param;
    rsDesc.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;

    ComPtr<ID3DBlob> sig;
    ComPtr<ID3DBlob> err;
    HR(D3D12SerializeRootSignature(&rsDesc, D3D_ROOT_SIGNATURE_VERSION_1, &sig, &err),
        "D3D12SerializeRootSignature(Cube)",
        err ? std::string((char*)err->GetBufferPointer(), err->GetBufferSize()) : "");
    HR(device->CreateRootSignature(0, sig->GetBufferPointer(), sig->GetBufferSize(), IID_PPV_ARGS(&rootSig)),
        "CreateRootSignature(Cube)");

    ComPtr<ID3DBlob> vs;
    ComPtr<ID3DBlob> ps;
    HR(D3DCompile(kCubeShader, (UINT)std::strlen(kCubeShader), nullptr, nullptr, nullptr,
        "VSMain", "vs_5_0", 0, 0, &vs, &err),
        "D3DCompile(VS Cube)",
        err ? std::string((char*)err->GetBufferPointer(), err->GetBufferSize()) : "");
    HR(D3DCompile(kCubeShader, (UINT)std::strlen(kCubeShader), nullptr, nullptr, nullptr,
        "PSMain", "ps_5_0", 0, 0, &ps, &err),
        "D3DCompile(PS Cube)",
        err ? std::string((char*)err->GetBufferPointer(), err->GetBufferSize()) : "");

    D3D12_INPUT_ELEMENT_DESC layout[] = {
        { "POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0 },
        { "COLOR", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 12, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0 },
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

    psoDesc.RasterizerState.FillMode = D3D12_FILL_MODE_SOLID;
    psoDesc.RasterizerState.CullMode = D3D12_CULL_MODE_BACK;
    psoDesc.RasterizerState.DepthClipEnable = TRUE;

    psoDesc.BlendState.AlphaToCoverageEnable = FALSE;
    psoDesc.BlendState.IndependentBlendEnable = FALSE;
    D3D12_RENDER_TARGET_BLEND_DESC rtDesc{};
    rtDesc.BlendEnable = FALSE;
    rtDesc.LogicOpEnable = FALSE;
    rtDesc.SrcBlend = D3D12_BLEND_ONE;
    rtDesc.DestBlend = D3D12_BLEND_ZERO;
    rtDesc.BlendOp = D3D12_BLEND_OP_ADD;
    rtDesc.SrcBlendAlpha = D3D12_BLEND_ONE;
    rtDesc.DestBlendAlpha = D3D12_BLEND_ZERO;
    rtDesc.BlendOpAlpha = D3D12_BLEND_OP_ADD;
    rtDesc.LogicOp = D3D12_LOGIC_OP_NOOP;
    rtDesc.RenderTargetWriteMask = D3D12_COLOR_WRITE_ENABLE_ALL;
    for (int i = 0; i < 8; ++i) psoDesc.BlendState.RenderTarget[i] = rtDesc;

    psoDesc.DepthStencilState.DepthEnable = TRUE;
    psoDesc.DepthStencilState.DepthWriteMask = D3D12_DEPTH_WRITE_MASK_ALL;
    psoDesc.DepthStencilState.DepthFunc = D3D12_COMPARISON_FUNC_LESS;
    psoDesc.DepthStencilState.StencilEnable = FALSE;

    HR(device->CreateGraphicsPipelineState(&psoDesc, IID_PPV_ARGS(&pso)),
        "CreateGraphicsPipelineState(Cube)");

    const Vertex verts[] = {
        {{-1,-1,-1},{1,0,0}}, {{-1, 1,-1},{0,1,0}}, {{ 1, 1,-1},{0,0,1}}, {{ 1,-1,-1},{1,1,0}},
        {{-1,-1, 1},{1,0,1}}, {{-1, 1, 1},{0,1,1}}, {{ 1, 1, 1},{1,1,1}}, {{ 1,-1, 1},{0.2f,0.8f,0.2f}},
    };
    const uint16_t indices[] = {
        0,1,2, 0,2,3,
        4,6,5, 4,7,6,
        4,5,1, 4,1,0,
        3,2,6, 3,6,7,
        1,5,6, 1,6,2,
        4,0,3, 4,3,7,
    };

    D3D12_HEAP_PROPERTIES uploadHeap{};
    uploadHeap.Type = D3D12_HEAP_TYPE_UPLOAD;

    D3D12_RESOURCE_DESC bufferDesc{};
    bufferDesc.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER;
    bufferDesc.Height = 1;
    bufferDesc.DepthOrArraySize = 1;
    bufferDesc.MipLevels = 1;
    bufferDesc.SampleDesc.Count = 1;
    bufferDesc.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;

    const UINT vbSize = (UINT)sizeof(verts);
    bufferDesc.Width = vbSize;
    HR(device->CreateCommittedResource(&uploadHeap, D3D12_HEAP_FLAG_NONE, &bufferDesc,
        D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&vb)),
        "CreateCommittedResource(CubeVB)");
    void* vbData = nullptr;
    D3D12_RANGE readRange{ 0, 0 };
    HR(vb->Map(0, &readRange, &vbData), "CubeVB->Map");
    std::memcpy(vbData, verts, vbSize);
    vb->Unmap(0, nullptr);

    vbv.BufferLocation = vb->GetGPUVirtualAddress();
    vbv.StrideInBytes = sizeof(Vertex);
    vbv.SizeInBytes = vbSize;

    const UINT ibSize = (UINT)sizeof(indices);
    bufferDesc.Width = ibSize;
    HR(device->CreateCommittedResource(&uploadHeap, D3D12_HEAP_FLAG_NONE, &bufferDesc,
        D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&ib)),
        "CreateCommittedResource(CubeIB)");
    void* ibData = nullptr;
    HR(ib->Map(0, &readRange, &ibData), "CubeIB->Map");
    std::memcpy(ibData, indices, ibSize);
    ib->Unmap(0, nullptr);

    ibv.BufferLocation = ib->GetGPUVirtualAddress();
    ibv.SizeInBytes = ibSize;
    ibv.Format = DXGI_FORMAT_R16_UINT;

    const UINT cbSize = Align256((UINT)sizeof(CBData));
    bufferDesc.Width = cbSize;
    HR(device->CreateCommittedResource(&uploadHeap, D3D12_HEAP_FLAG_NONE, &bufferDesc,
        D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&cb)),
        "CreateCommittedResource(CubeCB)");
    HR(cb->Map(0, &readRange, (void**)&cbMapped), "CubeCB->Map");

    startTime = std::chrono::steady_clock::now();
}

void CubeScene::Render(ID3D12GraphicsCommandList* cl)
{
    float t = std::chrono::duration<float>(std::chrono::steady_clock::now() - startTime).count() * rotationSpeed;

    XMMATRIX world = XMMatrixRotationY(t) * XMMatrixRotationX(t * 0.6f);
    XMVECTOR eye = XMVectorSet(0.0f, 0.0f, -5.0f, 1.0f);
    XMVECTOR at = XMVectorSet(0.0f, 0.0f, 0.0f, 1.0f);
    XMVECTOR up = XMVectorSet(0.0f, 1.0f, 0.0f, 0.0f);

    float aspect = (height == 0) ? 1.0f : (float)width / (float)height;
    XMMATRIX view = XMMatrixLookAtLH(eye, at, up);
    XMMATRIX proj = XMMatrixPerspectiveFovLH(XM_PIDIV4, aspect, 0.1f, 100.0f);

    CBData cbData{};
    XMStoreFloat4x4(&cbData.mvp, XMMatrixTranspose(world * view * proj));
    std::memcpy(cbMapped, &cbData, sizeof(cbData));

    cl->SetPipelineState(pso.Get());
    cl->SetGraphicsRootSignature(rootSig.Get());
    cl->SetGraphicsRootConstantBufferView(0, cb->GetGPUVirtualAddress());
    cl->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
    cl->IASetVertexBuffers(0, 1, &vbv);
    cl->IASetIndexBuffer(&ibv);
    cl->DrawIndexedInstanced(36, 1, 0, 0, 0);
}

void CubeScene::OnResize(UINT w, UINT h)
{
    width = (w == 0) ? 1u : w;
    height = (h == 0) ? 1u : h;
}

void CubeScene::Cleanup()
{
    if (cb && cbMapped) {
        cb->Unmap(0, nullptr);
        cbMapped = nullptr;
    }

    cb.Reset();
    ib.Reset();
    vb.Reset();
    pso.Reset();
    rootSig.Reset();
}
