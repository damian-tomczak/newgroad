#include "FogScene.h"

#include <algorithm>
#include <cstring>

#include <d3dcompiler.h>
#pragma comment(lib, "d3dcompiler.lib")

#include "imgui.h"

namespace {
constexpr UINT kFrameCount = 2;

UINT Align256(UINT x) { return (x + 255u) & ~255u; }

D3D12_CPU_DESCRIPTOR_HANDLE CpuAt(ID3D12DescriptorHeap* heap, UINT idx, UINT inc)
{
    auto h = heap->GetCPUDescriptorHandleForHeapStart();
    h.ptr += static_cast<UINT64>(idx) * inc;
    return h;
}

D3D12_GPU_DESCRIPTOR_HANDLE GpuAt(ID3D12DescriptorHeap* heap, UINT idx, UINT inc)
{
    auto h = heap->GetGPUDescriptorHandleForHeapStart();
    h.ptr += static_cast<UINT64>(idx) * inc;
    return h;
}

D3D12_RASTERIZER_DESC DefaultRaster()
{
    D3D12_RASTERIZER_DESC r{};
    r.FillMode = D3D12_FILL_MODE_SOLID;
    r.CullMode = D3D12_CULL_MODE_BACK;
    r.FrontCounterClockwise = FALSE;
    r.DepthBias = D3D12_DEFAULT_DEPTH_BIAS;
    r.DepthBiasClamp = D3D12_DEFAULT_DEPTH_BIAS_CLAMP;
    r.SlopeScaledDepthBias = D3D12_DEFAULT_SLOPE_SCALED_DEPTH_BIAS;
    r.DepthClipEnable = TRUE;
    return r;
}

D3D12_BLEND_DESC DefaultBlend()
{
    D3D12_BLEND_DESC b{};
    auto& rt = b.RenderTarget[0];
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
    return b;
}

D3D12_DEPTH_STENCIL_DESC NoDepth()
{
    D3D12_DEPTH_STENCIL_DESC d{};
    d.DepthEnable = FALSE;
    d.DepthWriteMask = D3D12_DEPTH_WRITE_MASK_ZERO;
    d.DepthFunc = D3D12_COMPARISON_FUNC_ALWAYS;
    return d;
}

const char* kFogPostHLSL = R"(
Texture2D gScene : register(t0);
Texture2D<float> gDepth : register(t1);
SamplerState gSamp : register(s0);

cbuffer FogCB : register(b0)
{
    float2 gInvSize;
    float2 _pad0;

    float3 gFogColor;
    float  gFogDensity;

    float  gFogStart;
    float  gFogEnd;
    float  gNearZ;
    float  gFarZ;
};

struct VSOut { float4 pos : SV_POSITION; };

VSOut VSMain(uint vid : SV_VertexID)
{
    float2 p = (vid == 0) ? float2(-1, -1) :
               (vid == 1) ? float2(-1,  3) :
                            float2( 3, -1);
    VSOut o;
    o.pos = float4(p, 0, 1);
    return o;
}

float LinearizeDepth(float d, float n, float f)
{
    return (n * f) / (f - d * (f - n));
}

float4 PSMain(float4 svpos : SV_POSITION) : SV_Target
{
    float2 uv = svpos.xy * gInvSize;
    float3 col = gScene.SampleLevel(gSamp, uv, 0).rgb;

    float d = gDepth.SampleLevel(gSamp, uv, 0);
    float z = LinearizeDepth(d, gNearZ, gFarZ);

    float fogT = saturate((z - gFogStart) / max(1e-4, (gFogEnd - gFogStart)));
    float fog  = 1.0 - exp(-gFogDensity * fogT);
    fog = saturate(fog);

    col = lerp(col, gFogColor, fog);
    return float4(col, 1);
}
)";
} // namespace

void FogScene::Init(ID3D12Device* inDevice,
    ID3D12DescriptorHeap* inSrvHeap,
    UINT inSrvDescSize,
    UINT inWidth,
    UINT inHeight,
    ID3D12Resource* sceneColor,
    ID3D12Resource* depth)
{
    device = inDevice;
    srvHeap = inSrvHeap;
    srvDescSize = inSrvDescSize;
    width = inWidth;
    height = inHeight;

    D3D12_DESCRIPTOR_RANGE range{};
    range.RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_SRV;
    range.NumDescriptors = 2;
    range.BaseShaderRegister = 0;

    D3D12_ROOT_PARAMETER rp[2]{};
    rp[0].ParameterType = D3D12_ROOT_PARAMETER_TYPE_CBV;
    rp[0].Descriptor.ShaderRegister = 0;
    rp[0].ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    rp[1].ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
    rp[1].DescriptorTable.NumDescriptorRanges = 1;
    rp[1].DescriptorTable.pDescriptorRanges = &range;
    rp[1].ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    D3D12_STATIC_SAMPLER_DESC samp{};
    samp.Filter = D3D12_FILTER_MIN_MAG_MIP_LINEAR;
    samp.AddressU = samp.AddressV = samp.AddressW = D3D12_TEXTURE_ADDRESS_MODE_CLAMP;
    samp.ShaderRegister = 0;
    samp.ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    D3D12_ROOT_SIGNATURE_DESC rs{};
    rs.NumParameters = 2;
    rs.pParameters = rp;
    rs.NumStaticSamplers = 1;
    rs.pStaticSamplers = &samp;
    rs.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;

    ComPtr<ID3DBlob> sig;
    ComPtr<ID3DBlob> err;
    HR(D3D12SerializeRootSignature(&rs, D3D_ROOT_SIGNATURE_VERSION_1, &sig, &err), "SerializeRootSignature");
    HR(device->CreateRootSignature(0, sig->GetBufferPointer(), sig->GetBufferSize(), IID_PPV_ARGS(&fogRootSig)),
        "CreateRootSignature");

    UINT flags = 0;
#if defined(_DEBUG)
    flags = D3DCOMPILE_DEBUG | D3DCOMPILE_SKIP_OPTIMIZATION;
#endif

    ComPtr<ID3DBlob> vs;
    ComPtr<ID3DBlob> ps;
    HRESULT hrv = D3DCompile(kFogPostHLSL, static_cast<UINT>(std::strlen(kFogPostHLSL)),
        "FogPostEmbedded", nullptr, nullptr, "VSMain", "vs_5_0", flags, 0, &vs, &err);
    if (FAILED(hrv)) {
        if (err) OutputDebugStringA(static_cast<const char*>(err->GetBufferPointer()));
        HR(hrv, "Compile VS");
    }

    HRESULT hrp = D3DCompile(kFogPostHLSL, static_cast<UINT>(std::strlen(kFogPostHLSL)),
        "FogPostEmbedded", nullptr, nullptr, "PSMain", "ps_5_0", flags, 0, &ps, &err);
    if (FAILED(hrp)) {
        if (err) OutputDebugStringA(static_cast<const char*>(err->GetBufferPointer()));
        HR(hrp, "Compile PS");
    }

    D3D12_GRAPHICS_PIPELINE_STATE_DESC pso{};
    pso.pRootSignature = fogRootSig.Get();
    pso.VS = { vs->GetBufferPointer(), vs->GetBufferSize() };
    pso.PS = { ps->GetBufferPointer(), ps->GetBufferSize() };
    pso.BlendState = DefaultBlend();
    pso.RasterizerState = DefaultRaster();
    pso.DepthStencilState = NoDepth();
    pso.SampleMask = UINT_MAX;
    pso.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
    pso.NumRenderTargets = 1;
    pso.RTVFormats[0] = DXGI_FORMAT_R8G8B8A8_UNORM;
    pso.SampleDesc.Count = 1;
    HR(device->CreateGraphicsPipelineState(&pso, IID_PPV_ARGS(&fogPSO)), "Create fog PSO");

    fogCBStride = Align256(static_cast<UINT>(sizeof(FogCBData)));
    const UINT totalSize = fogCBStride * kFrameCount;

    D3D12_HEAP_PROPERTIES up{};
    up.Type = D3D12_HEAP_TYPE_UPLOAD;

    D3D12_RESOURCE_DESC buf{};
    buf.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER;
    buf.Width = totalSize;
    buf.Height = 1;
    buf.DepthOrArraySize = 1;
    buf.MipLevels = 1;
    buf.SampleDesc.Count = 1;
    buf.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;

    HR(device->CreateCommittedResource(&up, D3D12_HEAP_FLAG_NONE, &buf,
        D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&fogCB)),
        "Create fog CB");

    HR(fogCB->Map(0, nullptr, reinterpret_cast<void**>(&fogCBMapped)), "Map fog CB");
    CreateDescriptors(sceneColor, depth);
}

void FogScene::CreateDescriptors(ID3D12Resource* sceneColor, ID3D12Resource* depth)
{
    if (!device || !srvHeap || !sceneColor || !depth) {
        return;
    }

    D3D12_SHADER_RESOURCE_VIEW_DESC s0{};
    s0.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
    s0.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
    s0.ViewDimension = D3D12_SRV_DIMENSION_TEXTURE2D;
    s0.Texture2D.MipLevels = 1;
    device->CreateShaderResourceView(sceneColor, &s0, CpuAt(srvHeap, 1, srvDescSize));

    D3D12_SHADER_RESOURCE_VIEW_DESC s1{};
    s1.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
    s1.Format = DXGI_FORMAT_R32_FLOAT;
    s1.ViewDimension = D3D12_SRV_DIMENSION_TEXTURE2D;
    s1.Texture2D.MipLevels = 1;
    device->CreateShaderResourceView(depth, &s1, CpuAt(srvHeap, 2, srvDescSize));
}

void FogScene::OnResize(UINT inWidth, UINT inHeight)
{
    width = std::max(1u, inWidth);
    height = std::max(1u, inHeight);
}

void FogScene::Draw(ID3D12GraphicsCommandList* commandList, UINT frameIndex)
{
    FogCBData cb{};
    cb.invSize[0] = 1.0f / static_cast<float>(width);
    cb.invSize[1] = 1.0f / static_cast<float>(height);
    cb.fogColor[0] = fogColor[0];
    cb.fogColor[1] = fogColor[1];
    cb.fogColor[2] = fogColor[2];
    cb.fogDensity = fogEnabled ? fogDensity : 0.0f;
    cb.fogStart = fogStart;
    cb.fogEnd = fogEnd;
    cb.nearZ = nearZ;
    cb.farZ = farZ;

    uint8_t* dst = fogCBMapped + static_cast<size_t>(frameIndex) * fogCBStride;
    std::memcpy(dst, &cb, sizeof(cb));

    D3D12_GPU_VIRTUAL_ADDRESS cbAddr =
        fogCB->GetGPUVirtualAddress() + static_cast<UINT64>(frameIndex) * static_cast<UINT64>(fogCBStride);

    commandList->SetPipelineState(fogPSO.Get());
    commandList->SetGraphicsRootSignature(fogRootSig.Get());
    commandList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
    commandList->SetGraphicsRootConstantBufferView(0, cbAddr);
    commandList->SetGraphicsRootDescriptorTable(1, GpuAt(srvHeap, 1, srvDescSize));
    commandList->DrawInstanced(3, 1, 0, 0);
}

void FogScene::DrawUI()
{
    ImGui::Separator();
    ImGui::Text("Postprocess fog");
    ImGui::Checkbox("Enable fog", &fogEnabled);
    ImGui::ColorEdit3("Fog color", fogColor);
    ImGui::SliderFloat("Fog density", &fogDensity, 0.0f, 20.0f);
    ImGui::SliderFloat("Fog start", &fogStart, 0.0f, 200.0f);
    ImGui::SliderFloat("Fog end", &fogEnd, 0.01f, 500.0f);
    if (fogEnd < fogStart) {
        fogEnd = fogStart + 0.01f;
    }

    ImGui::SliderFloat("Near (proj)", &nearZ, 0.01f, 10.0f);
    ImGui::SliderFloat("Far (proj)", &farZ, 10.0f, 2000.0f);
    if (farZ <= nearZ + 0.01f) {
        farZ = nearZ + 0.01f;
    }
}

void FogScene::Cleanup()
{
    if (fogCB && fogCBMapped) {
        fogCB->Unmap(0, nullptr);
        fogCBMapped = nullptr;
    }
}
