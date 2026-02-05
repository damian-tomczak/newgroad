#include "TriangleScene.h"

void TriangleScene::Init(ID3D12Device* device) {
    D3D12_ROOT_SIGNATURE_DESC rs{};
    rs.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;
    ComPtr<ID3DBlob> sig, err;
    HR(D3D12SerializeRootSignature(&rs, D3D_ROOT_SIGNATURE_VERSION_1, &sig, &err),
        "D3D12SerializeRootSignature(Triangle)",
        err ? std::string((char*)err->GetBufferPointer(), err->GetBufferSize()) : "");
    HR(device->CreateRootSignature(0, sig->GetBufferPointer(), sig->GetBufferSize(),
        IID_PPV_ARGS(&rootSig)), "CreateRootSignature(Triangle)");
    const char* src = R"(
struct VSIn { float3 pos:POSITION; float3 col:COLOR; };
struct PSIn { float4 pos:SV_POSITION; float3 col:COLOR; };
PSIn VSMain(VSIn i){ PSIn o; o.pos=float4(i.pos,1); o.col=i.col; return o; }
float4 PSMain(PSIn i):SV_TARGET{ return float4(i.col,1); }
)";
    ComPtr<ID3DBlob> vs, ps, shErr;
    HR(D3DCompile(src, (UINT)std::strlen(src), nullptr, nullptr, nullptr,
        "VSMain", "vs_5_0", 0, 0, &vs, &shErr),
        "D3DCompile(VS Triangle)", shErr ? std::string((char*)shErr->GetBufferPointer(), shErr->GetBufferSize()) : "");
    HR(D3DCompile(src, (UINT)std::strlen(src), nullptr, nullptr, nullptr,
        "PSMain", "ps_5_0", 0, 0, &ps, &shErr),
        "D3DCompile(PS Triangle)", shErr ? std::string((char*)shErr->GetBufferPointer(), shErr->GetBufferSize()) : "");
    D3D12_INPUT_ELEMENT_DESC layout[] = {
        { "POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0,   D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0 },
        { "COLOR",    0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 12,  D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0 },
    };
    D3D12_GRAPHICS_PIPELINE_STATE_DESC psoDesc{};
    psoDesc.pRootSignature = rootSig.Get();
    psoDesc.VS = { vs->GetBufferPointer(), vs->GetBufferSize() };
    psoDesc.PS = { ps->GetBufferPointer(), ps->GetBufferSize() };
    psoDesc.InputLayout = { layout, _countof(layout) };
    psoDesc.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
    psoDesc.NumRenderTargets = 1;
    psoDesc.RTVFormats[0] = DXGI_FORMAT_R8G8B8A8_UNORM;
    psoDesc.SampleDesc.Count = 1;
    psoDesc.SampleMask = UINT_MAX;
    psoDesc.BlendState.AlphaToCoverageEnable = FALSE;
    psoDesc.BlendState.IndependentBlendEnable = FALSE;
    const D3D12_RENDER_TARGET_BLEND_DESC rt = {
        FALSE,FALSE,
        D3D12_BLEND_ONE, D3D12_BLEND_ZERO, D3D12_BLEND_OP_ADD,
        D3D12_BLEND_ONE, D3D12_BLEND_ZERO, D3D12_BLEND_OP_ADD,
        D3D12_LOGIC_OP_NOOP, D3D12_COLOR_WRITE_ENABLE_ALL
    };
    for (int i = 0; i < 8; ++i) psoDesc.BlendState.RenderTarget[i] = rt;
    psoDesc.RasterizerState.FillMode = D3D12_FILL_MODE_SOLID;
    psoDesc.RasterizerState.CullMode = D3D12_CULL_MODE_NONE;
    psoDesc.RasterizerState.DepthClipEnable = TRUE;
    psoDesc.DepthStencilState.DepthEnable = FALSE;
    psoDesc.DepthStencilState.StencilEnable = FALSE;

    HR(device->CreateGraphicsPipelineState(&psoDesc, IID_PPV_ARGS(&pso)),
        "CreateGraphicsPipelineState(Triangle)");
    struct V { float p[3]; float c[3]; };
    V verts[] = {
        {{  0.0f,  0.5f, 0.0f}, {1,0,0}},
        {{  0.5f, -0.5f, 0.0f}, {0,1,0}},
        {{ -0.5f, -0.5f, 0.0f}, {0,0,1}},
    };
    UINT size = sizeof(verts);
    D3D12_HEAP_PROPERTIES heap{}; heap.Type = D3D12_HEAP_TYPE_UPLOAD;
    D3D12_RESOURCE_DESC buf{}; buf.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER; buf.Width = size;
    buf.Height = 1; buf.DepthOrArraySize = 1; buf.MipLevels = 1; buf.SampleDesc.Count = 1; buf.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;
    HR(device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &buf,
        D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&vb)),
        "CreateCommittedResource(TriangleVB)");
    void* p = nullptr; D3D12_RANGE r{ 0,0 };
    HR(vb->Map(0, &r, &p), "TriangleVB->Map"); std::memcpy(p, verts, size); vb->Unmap(0, nullptr);
    vbv.BufferLocation = vb->GetGPUVirtualAddress(); vbv.StrideInBytes = sizeof(V); vbv.SizeInBytes = size;
}

void TriangleScene::Render(ID3D12GraphicsCommandList* cl) {
    cl->SetPipelineState(pso.Get());
    cl->SetGraphicsRootSignature(rootSig.Get());
    cl->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
    cl->IASetVertexBuffers(0, 1, &vbv);
    cl->DrawInstanced(3, 1, 0, 0);
}

void TriangleScene::Cleanup() {
    vb.Reset(); pso.Reset(); rootSig.Reset();
}
