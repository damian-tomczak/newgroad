// main.cpp — DX12 + ImGui + CAD Camera + Grid/Axes + Scenes (Triangle, Cube, Whirligig, RH)
// LEFT SIDEBAR UI (scrollable). Whirligig has a visible Presets list at the top,
// plus a Quick Presets strip at the top of the sidebar. Includes the requested preset
// where the cube spins about itself (arrow up +Y) and the whole cube orbits around +Y.
//
// Build (MSVC example):
//   cl /EHsc /std:c++17 main.cpp /DUNICODE /DWIN32 /DWIN32_LEAN_AND_MEAN d3d12.lib dxgi.lib d3dcompiler.lib
//
// Dear ImGui files required in your project:
//   imgui.cpp, imgui_draw.cpp, imgui_tables.cpp, imgui_widgets.cpp, imgui_demo.cpp
//   backends/imgui_impl_win32.cpp, backends/imgui_impl_dx12.cpp
// Include paths:
//   <your>/third_party/imgui
//   <your>/third_party/imgui/backends

#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#include <wrl.h>
#include <d3d12.h>
#include <dxgi1_6.h>
#include <d3dcompiler.h>
#include <comdef.h>
#include <DirectXMath.h>
#include <string>
#include <stdexcept>
#include <cstring>
#include <vector>
#include <chrono>
#include <algorithm>
#include <cmath>

#include "imgui.h"
#include "backends/imgui_impl_win32.h"
#include "backends/imgui_impl_dx12.h"

#pragma comment(lib, "d3d12.lib")
#pragma comment(lib, "dxgi.lib")
#pragma comment(lib, "d3dcompiler.lib")

using Microsoft::WRL::ComPtr;
using namespace DirectX;

static const UINT FrameCount = 2;

static std::string Narrow(LPCWSTR ws) {
    if (!ws) return {};
    int len = WideCharToMultiByte(CP_UTF8, 0, ws, -1, nullptr, 0, nullptr, nullptr);
    if (len <= 0) return {};
    std::string out;
    out.resize(len - 1);
    WideCharToMultiByte(CP_UTF8, 0, ws, -1, &out[0], len, nullptr, nullptr);
    return out;
}

struct DxError : std::runtime_error {
    HRESULT hr;
    DxError(const char* where, HRESULT h, const std::string& extra = {})
        : std::runtime_error(std::string(where) + " failed: " +
            Narrow(_com_error(h).ErrorMessage()) +
            (extra.empty() ? "" : std::string("\nDetails:\n") + extra)),
        hr(h) {
    }
};
static inline void HR(HRESULT hr, const char* where, const std::string& extra = {}) {
    if (FAILED(hr)) throw DxError(where, hr, extra);
}

// ImGui Win32 handler forward
extern IMGUI_IMPL_API LRESULT ImGui_ImplWin32_WndProcHandler(HWND, UINT, WPARAM, LPARAM);

// -------------------- CAD Camera --------------------
struct CadCamera {
    XMFLOAT3 target{ 0,0,0 };
    float yaw = XM_PIDIV4;
    float pitch = XMConvertToRadians(25.0f);
    float distance = 4.0f;
    float fovY = XM_PIDIV4;
    float znearP = 0.1f, zfarP = 100.0f;

    float rotateSpeed = 0.010f;
    float panSpeed = 0.0025f;
    float zoomSpeed = 0.12f;

    UINT vpW = 1280, vpH = 720;

    void SetViewport(UINT w, UINT h) { vpW = (w ? w : 1); vpH = (h ? h : 1); }
    void ResetNiceView(float setDistance = 4.0f) {
        yaw = XM_PIDIV4; pitch = XMConvertToRadians(25.0f);
        distance = setDistance; target = { 0,0,0 };
    }

    void UpdateFromImGui(const ImGuiIO& io, float) {
        if (io.WantCaptureMouse) return;
        if (io.MouseDown[0]) {
            yaw -= io.MouseDelta.x * rotateSpeed;
            pitch -= io.MouseDelta.y * rotateSpeed;
            float lim = XMConvertToRadians(89.0f);
            pitch = std::clamp(pitch, -lim, lim);
        }
        if (io.MouseDown[1] || io.MouseDown[2]) {
            XMFLOAT3 eyeF = GetEye();
            XMVECTOR eye = XMLoadFloat3(&eyeF);
            XMVECTOR at = XMLoadFloat3(&target);
            XMVECTOR fwd = XMVector3Normalize(XMVectorSubtract(at, eye));
            XMVECTOR worldUp = XMVectorSet(0, 1, 0, 0);
            XMVECTOR right = XMVector3Normalize(XMVector3Cross(fwd, worldUp));
            XMVECTOR up = XMVector3Normalize(XMVector3Cross(right, fwd));
            float scale = panSpeed * distance;
            XMVECTOR pan = XMVectorAdd(
                XMVectorScale(right, -io.MouseDelta.x * scale),
                XMVectorScale(up, io.MouseDelta.y * scale)
            );
            XMStoreFloat3(&target, XMVectorAdd(XMLoadFloat3(&target), pan));
        }
        if (io.MouseWheel != 0.0f) {
            float factor = std::exp(-io.MouseWheel * zoomSpeed);
            distance = std::clamp(distance * factor, 0.05f, 1e4f);
        }
    }

    XMFLOAT3 GetEye() const {
        float cp = std::cos(pitch), sp = std::sin(pitch);
        float cy = std::cos(yaw), sy = std::sin(yaw);
        XMFLOAT3 dir{ cp * sy, sp, cp * cy };
        return XMFLOAT3{
            target.x + dir.x * distance,
            target.y + dir.y * distance,
            target.z + dir.z * distance
        };
    }

    XMMATRIX GetViewRH() const {
        XMFLOAT3 eyeF = GetEye();
        return XMMatrixLookAtRH(XMLoadFloat3(&eyeF), XMLoadFloat3(&target), XMVectorSet(0, 1, 0, 0));
    }
    XMMATRIX GetProjRH() const {
        float aspect = (float)vpW / (float)vpH;
        return XMMatrixPerspectiveFovRH(fovY, aspect, znearP, zfarP);
    }

    void DrawUiInline() {
        if (ImGui::CollapsingHeader("Camera", ImGuiTreeNodeFlags_DefaultOpen)) {
            if (ImGui::Button("Reset")) ResetNiceView(distance);
            ImGui::SameLine();
            XMFLOAT3 e = GetEye();
            ImGui::Text("Eye: (%.2f, %.2f, %.2f)", e.x, e.y, e.z);
            ImGui::SeparatorText("View");
            ImGui::SliderFloat("Distance", &distance, 0.05f, 100.0f, "%.2f", ImGuiSliderFlags_Logarithmic);
            ImGui::SliderAngle("Yaw", &yaw, -180.0f, 180.0f);
            ImGui::SliderAngle("Pitch", &pitch, -89.0f, 89.0f);
            ImGui::SliderAngle("FOV Y", &fovY, 15.0f, 100.0f);
            ImGui::SeparatorText("Speeds");
            ImGui::SliderFloat("Rotate speed", &rotateSpeed, 0.001f, 0.05f, "%.3f");
            ImGui::SliderFloat("Pan speed", &panSpeed, 0.0005f, 0.01f, "%.4f");
            ImGui::SliderFloat("Zoom speed", &zoomSpeed, 0.02f, 0.5f, "%.3f");
            ImGui::TextDisabled("Mouse: LMB orbit, RMB/MMB pan, wheel zoom");
        }
    }
};

// -------------------- Scene Interface --------------------
struct IScene {
    virtual ~IScene() = default;
    virtual void Init(ID3D12Device* device) = 0;
    virtual void Update(float /*dt*/) {}
    virtual void Render(ID3D12GraphicsCommandList* cl, const XMMATRIX& view, const XMMATRIX& proj) = 0;
    virtual void OnResize(UINT /*w*/, UINT /*h*/) {}
    virtual void Cleanup() = 0;
    virtual void DrawUiInline() {}
};

struct VtxPC { float p[3]; float c[3]; };

// -------------------- Grid + Axes (lines) --------------------
struct GridAxes {
    ComPtr<ID3D12RootSignature> rootSig;
    ComPtr<ID3D12PipelineState> psoLine;
    ComPtr<ID3D12Resource>      vb;
    D3D12_VERTEX_BUFFER_VIEW     vbv{};
    std::vector<VtxPC>           verts;

    bool  visible = true;
    bool  showGrid = true;
    bool  showAxes = true;
    float extent = 10.0f;
    float step = 1.0f;
    int   majorEvery = 5;
    float axisLength = 11.0f;

    void Init(ID3D12Device* device) {
        D3D12_ROOT_PARAMETER param{};
        param.ParameterType = D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS;
        param.Constants.Num32BitValues = 16;
        param.Constants.ShaderRegister = 0;
        D3D12_ROOT_SIGNATURE_DESC rs{};
        rs.NumParameters = 1;
        rs.pParameters = &param;
        rs.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;

        ComPtr<ID3DBlob> sig, err;
        HR(D3D12SerializeRootSignature(&rs, D3D_ROOT_SIGNATURE_VERSION_1, &sig, &err),
            "D3D12SerializeRootSignature(GridAxes)",
            err ? std::string((char*)err->GetBufferPointer(), err->GetBufferSize()) : "");
        HR(device->CreateRootSignature(0, sig->GetBufferPointer(), sig->GetBufferSize(),
            IID_PPV_ARGS(&rootSig)), "CreateRootSignature(GridAxes)");

        const char* vsSrc = R"(
cbuffer Params : register(b0) { float4x4 MVP; }
struct VSIn { float3 pos:POSITION; float3 col:COLOR; };
struct PSIn { float4 pos:SV_POSITION; float3 col:COLOR; };
PSIn VSMain(VSIn i){ PSIn o; o.pos = mul(float4(i.pos,1), MVP); o.col = i.col; return o; }
)";
        const char* psSrc = R"(
struct PSIn { float4 pos:SV_POSITION; float3 col:COLOR; };
float4 PSMain(PSIn i):SV_TARGET { return float4(i.col,1); }
)";
        ComPtr<ID3DBlob> vs, ps, shErr;
        HR(D3DCompile(vsSrc, (UINT)std::strlen(vsSrc), nullptr, nullptr, nullptr,
            "VSMain", "vs_5_0", 0, 0, &vs, &shErr),
            "D3DCompile(VS grid)", shErr ? std::string((char*)shErr->GetBufferPointer(), shErr->GetBufferSize()) : "");
        HR(D3DCompile(psSrc, (UINT)std::strlen(psSrc), nullptr, nullptr, nullptr,
            "PSMain", "ps_5_0", 0, 0, &ps, &shErr),
            "D3DCompile(PS grid)", shErr ? std::string((char*)shErr->GetBufferPointer(), shErr->GetBufferSize()) : "");

        D3D12_INPUT_ELEMENT_DESC layout[] = {
            { "POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0,  D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0 },
            { "COLOR",    0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 12, D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0 },
        };

        D3D12_GRAPHICS_PIPELINE_STATE_DESC pso{};
        pso.pRootSignature = rootSig.Get();
        pso.VS = { vs->GetBufferPointer(), vs->GetBufferSize() };
        pso.PS = { ps->GetBufferPointer(), ps->GetBufferSize() };
        pso.InputLayout = { layout, _countof(layout) };
        pso.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_LINE;
        pso.NumRenderTargets = 1;
        pso.RTVFormats[0] = DXGI_FORMAT_R8G8B8A8_UNORM;
        pso.DSVFormat = DXGI_FORMAT_D32_FLOAT;
        pso.SampleDesc.Count = 1;
        pso.SampleMask = UINT_MAX;

        // Blend (default opaque)
        pso.BlendState.AlphaToCoverageEnable = FALSE;
        pso.BlendState.IndependentBlendEnable = FALSE;
        D3D12_RENDER_TARGET_BLEND_DESC rt{};
        rt.BlendEnable = FALSE;
        rt.LogicOpEnable = FALSE;
        rt.SrcBlend = D3D12_BLEND_ONE; rt.DestBlend = D3D12_BLEND_ZERO; rt.BlendOp = D3D12_BLEND_OP_ADD;
        rt.SrcBlendAlpha = D3D12_BLEND_ONE; rt.DestBlendAlpha = D3D12_BLEND_ZERO; rt.BlendOpAlpha = D3D12_BLEND_OP_ADD;
        rt.LogicOp = D3D12_LOGIC_OP_NOOP; rt.RenderTargetWriteMask = D3D12_COLOR_WRITE_ENABLE_ALL;
        for (int i = 0; i < 8; ++i) pso.BlendState.RenderTarget[i] = rt;

        // Rasterizer
        pso.RasterizerState.FillMode = D3D12_FILL_MODE_SOLID;
        pso.RasterizerState.CullMode = D3D12_CULL_MODE_NONE;
        pso.RasterizerState.FrontCounterClockwise = FALSE;
        pso.RasterizerState.DepthBias = D3D12_DEFAULT_DEPTH_BIAS;
        pso.RasterizerState.DepthBiasClamp = D3D12_DEFAULT_DEPTH_BIAS_CLAMP;
        pso.RasterizerState.SlopeScaledDepthBias = D3D12_DEFAULT_SLOPE_SCALED_DEPTH_BIAS;
        pso.RasterizerState.DepthClipEnable = TRUE;
        pso.RasterizerState.MultisampleEnable = FALSE;
        pso.RasterizerState.AntialiasedLineEnable = FALSE;
        pso.RasterizerState.ForcedSampleCount = 0;
        pso.RasterizerState.ConservativeRaster = D3D12_CONSERVATIVE_RASTERIZATION_MODE_OFF;

        // Depth
        pso.DepthStencilState.DepthEnable = TRUE;
        pso.DepthStencilState.DepthWriteMask = D3D12_DEPTH_WRITE_MASK_ALL;
        pso.DepthStencilState.DepthFunc = D3D12_COMPARISON_FUNC_LESS_EQUAL;
        pso.DepthStencilState.StencilEnable = FALSE;

        HR(device->CreateGraphicsPipelineState(&pso, IID_PPV_ARGS(&psoLine)),
            "CreateGraphicsPipelineState(GridAxes)");

        RebuildVB(device);
    }

    void RebuildVB(ID3D12Device* device) {
        verts.clear();
        auto pushLine = [&](XMFLOAT3 a, XMFLOAT3 b, XMFLOAT3 col) {
            verts.push_back({ {a.x,a.y,a.z}, {col.x,col.y,col.z} });
            verts.push_back({ {b.x,b.y,b.z}, {col.x,col.y,col.z} });
            };

        const XMFLOAT3 colMinor{ 0.25f,0.25f,0.28f };
        const XMFLOAT3 colMajor{ 0.35f,0.35f,0.40f };

        if (showGrid && step > 0.0f) {
            int n = (int)std::floor(extent / step);
            for (int i = -n; i <= n; ++i) {
                float v = i * step;
                bool major = (majorEvery > 0) && (i % majorEvery == 0);
                XMFLOAT3 c = major ? colMajor : colMinor;
                if (i != 0) pushLine({ -extent, 0.0f, v }, { +extent, 0.0f, v }, c);
                if (i != 0) pushLine({ v, 0.0f, -extent }, { v, 0.0f, +extent }, c);
            }
        }
        if (showAxes) {
            const float L = axisLength;
            pushLine({ 0,0,0 }, { L,0,0 }, { 1,0,0 }); // +X red
            pushLine({ 0,0,0 }, { 0,L,0 }, { 0,1,0 }); // +Y green
            pushLine({ 0,0,0 }, { 0,0,L }, { 0,0,1 }); // +Z blue
        }

        UINT size = (UINT)(verts.size() * sizeof(VtxPC));
        if (size == 0) size = sizeof(VtxPC);

        D3D12_HEAP_PROPERTIES heap{}; heap.Type = D3D12_HEAP_TYPE_UPLOAD;
        D3D12_RESOURCE_DESC desc{};
        desc.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER;
        desc.Width = size;
        desc.Height = 1; desc.DepthOrArraySize = 1; desc.MipLevels = 1;
        desc.SampleDesc.Count = 1;
        desc.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;

        vb.Reset();
        HR(device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &desc,
            D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&vb)),
            "CreateCommittedResource(GridVB)");

        if (!verts.empty()) {
            void* p = nullptr; D3D12_RANGE r{ 0,0 };
            HR(vb->Map(0, &r, &p), "GridVB->Map");
            std::memcpy(p, verts.data(), verts.size() * sizeof(VtxPC));
            vb->Unmap(0, nullptr);
        }

        vbv.BufferLocation = vb->GetGPUVirtualAddress();
        vbv.StrideInBytes = sizeof(VtxPC);
        vbv.SizeInBytes = size;
    }

    void Render(ID3D12GraphicsCommandList* cl, const XMMATRIX& view, const XMMATRIX& proj) {
        if (!visible || verts.empty()) return;
        XMMATRIX mvpT = XMMatrixTranspose(view * proj);
        XMFLOAT4X4 m; XMStoreFloat4x4(&m, mvpT);
        cl->SetPipelineState(psoLine.Get());
        cl->SetGraphicsRootSignature(rootSig.Get());
        cl->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_LINELIST);
        cl->IASetVertexBuffers(0, 1, &vbv);
        cl->SetGraphicsRoot32BitConstants(0, 16, &m, 0);
        cl->DrawInstanced((UINT)verts.size(), 1, 0, 0);
    }

    void DrawUiInline(ID3D12Device* device) {
        if (ImGui::CollapsingHeader("Grid / Axes", ImGuiTreeNodeFlags_DefaultOpen)) {
            ImGui::Checkbox("Visible", &visible);
            bool rebuild = false;
            rebuild |= ImGui::Checkbox("Grid", &showGrid);
            ImGui::SameLine();
            rebuild |= ImGui::Checkbox("Axes", &showAxes);
            rebuild |= ImGui::SliderFloat("Extent", &extent, 1.0f, 100.0f, "%.1f", ImGuiSliderFlags_Logarithmic);
            rebuild |= ImGui::SliderFloat("Step", &step, 0.1f, 10.0f, "%.1f");
            rebuild |= ImGui::SliderInt("Major every", &majorEvery, 0, 20);
            rebuild |= ImGui::SliderFloat("Axis length", &axisLength, 0.5f, 200.0f, "%.1f", ImGuiSliderFlags_Logarithmic);
            if (rebuild) RebuildVB(device);
        }
    }

    void Cleanup() {
        vb.Reset(); psoLine.Reset(); rootSig.Reset(); verts.clear();
    }
};

// =============== Triangle Scene ===============
struct TriangleScene : IScene {
    ComPtr<ID3D12PipelineState>    pso;
    ComPtr<ID3D12RootSignature>    rootSig;
    ComPtr<ID3D12Resource>         vb;
    D3D12_VERTEX_BUFFER_VIEW       vbv{};

    void Init(ID3D12Device* device) override {
        D3D12_ROOT_SIGNATURE_DESC rs{}; rs.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;
        ComPtr<ID3DBlob> sig, err;
        HR(D3D12SerializeRootSignature(&rs, D3D_ROOT_SIGNATURE_VERSION_1, &sig, &err),
            "D3D12SerializeRootSignature",
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
            "D3DCompile(VS triangle)", shErr ? std::string((char*)shErr->GetBufferPointer(), shErr->GetBufferSize()) : "");
        HR(D3DCompile(src, (UINT)std::strlen(src), nullptr, nullptr, nullptr,
            "PSMain", "ps_5_0", 0, 0, &ps, &shErr),
            "D3DCompile(PS triangle)", shErr ? std::string((char*)shErr->GetBufferPointer(), shErr->GetBufferSize()) : "");

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

        // Blend (opaque)
        psoDesc.BlendState.AlphaToCoverageEnable = FALSE;
        psoDesc.BlendState.IndependentBlendEnable = FALSE;
        D3D12_RENDER_TARGET_BLEND_DESC rt{};
        rt.BlendEnable = FALSE; rt.LogicOpEnable = FALSE;
        rt.SrcBlend = D3D12_BLEND_ONE; rt.DestBlend = D3D12_BLEND_ZERO; rt.BlendOp = D3D12_BLEND_OP_ADD;
        rt.SrcBlendAlpha = D3D12_BLEND_ONE; rt.DestBlendAlpha = D3D12_BLEND_ZERO; rt.BlendOpAlpha = D3D12_BLEND_OP_ADD;
        rt.LogicOp = D3D12_LOGIC_OP_NOOP; rt.RenderTargetWriteMask = D3D12_COLOR_WRITE_ENABLE_ALL;
        for (int i = 0; i < 8; ++i) psoDesc.BlendState.RenderTarget[i] = rt;

        // Rasterizer
        psoDesc.RasterizerState.FillMode = D3D12_FILL_MODE_SOLID;
        psoDesc.RasterizerState.CullMode = D3D12_CULL_MODE_NONE;
        psoDesc.RasterizerState.FrontCounterClockwise = FALSE;
        psoDesc.RasterizerState.DepthClipEnable = TRUE;

        // No depth
        psoDesc.DSVFormat = DXGI_FORMAT_UNKNOWN;
        psoDesc.DepthStencilState.DepthEnable = FALSE;
        psoDesc.DepthStencilState.StencilEnable = FALSE;

        HR(device->CreateGraphicsPipelineState(&psoDesc, IID_PPV_ARGS(&pso)),
            "CreateGraphicsPipelineState(Triangle)");

        VtxPC verts[] = {
            {{  0.0f,  0.5f, 0.0f}, {1,0,0}},
            {{  0.5f, -0.5f, 0.0f}, {0,1,0}},
            {{ -0.5f, -0.5f, 0.0f}, {0,0,1}},
        };
        UINT size = sizeof(verts);
        D3D12_HEAP_PROPERTIES heap{}; heap.Type = D3D12_HEAP_TYPE_UPLOAD;
        D3D12_RESOURCE_DESC buf{}; buf.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER; buf.Width = size; buf.Height = 1; buf.DepthOrArraySize = 1; buf.MipLevels = 1; buf.SampleDesc.Count = 1; buf.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;
        HR(device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &buf, D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&vb)),
            "CreateCommittedResource(TriangleVB)");
        void* p = nullptr; D3D12_RANGE r{ 0,0 };
        HR(vb->Map(0, &r, &p), "TriangleVB->Map"); std::memcpy(p, verts, size); vb->Unmap(0, nullptr);
        vbv.BufferLocation = vb->GetGPUVirtualAddress(); vbv.StrideInBytes = sizeof(VtxPC); vbv.SizeInBytes = size;
    }

    void Render(ID3D12GraphicsCommandList* cl, const XMMATRIX&, const XMMATRIX&) override {
        cl->SetPipelineState(pso.Get());
        cl->SetGraphicsRootSignature(rootSig.Get());
        cl->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
        cl->IASetVertexBuffers(0, 1, &vbv);
        cl->DrawInstanced(3, 1, 0, 0);
    }
    void Cleanup() override { vb.Reset(); pso.Reset(); rootSig.Reset(); }
};

// =============== Cube Scene (Right-Handed camera/projection) ===============
struct CubeScene : IScene {
    ComPtr<ID3D12PipelineState>    pso;
    ComPtr<ID3D12RootSignature>    rootSig;
    ComPtr<ID3D12Resource>         vb, ib;
    D3D12_VERTEX_BUFFER_VIEW       vbv{};
    D3D12_INDEX_BUFFER_VIEW        ibv{};
    float angle = 0.0f;

    void Init(ID3D12Device* device) override {
        D3D12_ROOT_PARAMETER param{};
        param.ParameterType = D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS;
        param.Constants.Num32BitValues = 16;
        param.Constants.ShaderRegister = 0;
        D3D12_ROOT_SIGNATURE_DESC rs{};
        rs.NumParameters = 1; rs.pParameters = &param;
        rs.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;
        ComPtr<ID3DBlob> sig, err;
        HR(D3D12SerializeRootSignature(&rs, D3D_ROOT_SIGNATURE_VERSION_1, &sig, &err),
            "D3D12SerializeRootSignature(Cube)",
            err ? std::string((char*)err->GetBufferPointer(), err->GetBufferSize()) : "");
        HR(device->CreateRootSignature(0, sig->GetBufferPointer(), sig->GetBufferSize(),
            IID_PPV_ARGS(&rootSig)), "CreateRootSignature(Cube)");

        const char* vsSrc = R"(
cbuffer Params : register(b0) { float4x4 MVP; }
struct VSIn { float3 pos:POSITION; float3 col:COLOR; };
struct PSIn { float4 pos:SV_POSITION; float3 col:COLOR; };
PSIn VSMain(VSIn i){ PSIn o; o.pos = mul(float4(i.pos,1), MVP); o.col = i.col; return o; }
)";
        const char* psSrc = R"(
struct PSIn { float4 pos:SV_POSITION; float3 col:COLOR; };
float4 PSMain(PSIn i):SV_TARGET { return float4(i.col,1); }
)";
        ComPtr<ID3DBlob> vs, ps, shErr;
        HR(D3DCompile(vsSrc, (UINT)std::strlen(vsSrc), nullptr, nullptr, nullptr,
            "VSMain", "vs_5_0", 0, 0, &vs, &shErr),
            "D3DCompile(VS cube)", shErr ? std::string((char*)shErr->GetBufferPointer(), shErr->GetBufferSize()) : "");
        HR(D3DCompile(psSrc, (UINT)std::strlen(psSrc), nullptr, nullptr, nullptr,
            "PSMain", "ps_5_0", 0, 0, &ps, &shErr),
            "D3DCompile(PS cube)", shErr ? std::string((char*)shErr->GetBufferPointer(), shErr->GetBufferSize()) : "");

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
        psoDesc.DSVFormat = DXGI_FORMAT_D32_FLOAT;
        psoDesc.SampleDesc.Count = 1;
        psoDesc.SampleMask = UINT_MAX;

        // Blend
        psoDesc.BlendState.AlphaToCoverageEnable = FALSE;
        psoDesc.BlendState.IndependentBlendEnable = FALSE;
        D3D12_RENDER_TARGET_BLEND_DESC rt{};
        rt.BlendEnable = FALSE; rt.LogicOpEnable = FALSE;
        rt.SrcBlend = D3D12_BLEND_ONE; rt.DestBlend = D3D12_BLEND_ZERO; rt.BlendOp = D3D12_BLEND_OP_ADD;
        rt.SrcBlendAlpha = D3D12_BLEND_ONE; rt.DestBlendAlpha = D3D12_BLEND_ZERO; rt.BlendOpAlpha = D3D12_BLEND_OP_ADD;
        rt.LogicOp = D3D12_LOGIC_OP_NOOP; rt.RenderTargetWriteMask = D3D12_COLOR_WRITE_ENABLE_ALL;
        for (int i = 0; i < 8; ++i) psoDesc.BlendState.RenderTarget[i] = rt;

        // Rasterizer
        psoDesc.RasterizerState.FillMode = D3D12_FILL_MODE_SOLID;
        psoDesc.RasterizerState.CullMode = D3D12_CULL_MODE_NONE;
        psoDesc.RasterizerState.FrontCounterClockwise = FALSE;
        psoDesc.RasterizerState.DepthClipEnable = TRUE;

        // Depth
        psoDesc.DepthStencilState.DepthEnable = TRUE;
        psoDesc.DepthStencilState.DepthWriteMask = D3D12_DEPTH_WRITE_MASK_ALL;
        psoDesc.DepthStencilState.DepthFunc = D3D12_COMPARISON_FUNC_LESS_EQUAL;
        psoDesc.DepthStencilState.StencilEnable = FALSE;

        HR(device->CreateGraphicsPipelineState(&psoDesc, IID_PPV_ARGS(&pso)),
            "CreateGraphicsPipelineState(Cube)");

        const float s = 0.5f;
        VtxPC verts[] = {
            {{-s,-s, s},{1,0,0}}, {{ s,-s, s},{0,1,0}}, {{ s, s, s},{0,0,1}}, {{-s, s, s},{1,1,0}},
            {{-s,-s,-s},{1,0,1}}, {{ s,-s,-s},{0,1,1}}, {{ s, s,-s},{1,1,1}}, {{-s, s,-s},{0.2f,0.6f,1}},
        };
        uint16_t idx[] = {
            0,1,2, 0,2,3,  1,5,6, 1,6,2,  5,4,7, 5,7,6,
            4,0,3, 4,3,7,  3,2,6, 3,6,7,  4,5,1, 4,1,0
        };
        UINT vbSize = sizeof(verts);
        D3D12_HEAP_PROPERTIES heap{}; heap.Type = D3D12_HEAP_TYPE_UPLOAD;
        D3D12_RESOURCE_DESC vbDesc{}; vbDesc.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER; vbDesc.Width = vbSize; vbDesc.Height = 1; vbDesc.DepthOrArraySize = 1; vbDesc.MipLevels = 1; vbDesc.SampleDesc.Count = 1; vbDesc.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;
        HR(device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &vbDesc, D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&vb)),
            "CreateCommittedResource(CubeVB)");
        void* p = nullptr; D3D12_RANGE r{ 0,0 };
        HR(vb->Map(0, &r, &p), "CubeVB->Map"); std::memcpy(p, verts, vbSize); vb->Unmap(0, nullptr);
        vbv.BufferLocation = vb->GetGPUVirtualAddress(); vbv.StrideInBytes = sizeof(VtxPC); vbv.SizeInBytes = vbSize;

        UINT ibSize = sizeof(idx);
        D3D12_RESOURCE_DESC ibDesc{}; ibDesc.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER; ibDesc.Width = ibSize; ibDesc.Height = 1; ibDesc.DepthOrArraySize = 1; ibDesc.MipLevels = 1; ibDesc.SampleDesc.Count = 1; ibDesc.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;
        HR(device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &ibDesc, D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&ib)),
            "CreateCommittedResource(CubeIB)");
        HR(ib->Map(0, &r, &p), "CubeIB->Map"); std::memcpy(p, idx, ibSize); ib->Unmap(0, nullptr);
        ibv.BufferLocation = ib->GetGPUVirtualAddress(); ibv.Format = DXGI_FORMAT_R16_UINT; ibv.SizeInBytes = ibSize;
    }

    void Update(float dt) override { angle += dt * 0.8f; }

    void Render(ID3D12GraphicsCommandList* cl, const XMMATRIX& view, const XMMATRIX& proj) override {
        XMMATRIX world = XMMatrixRotationY(angle);
        XMMATRIX wvpT = XMMatrixTranspose(world * view * proj);
        XMFLOAT4X4 m; XMStoreFloat4x4(&m, wvpT);
        cl->SetPipelineState(pso.Get());
        cl->SetGraphicsRootSignature(rootSig.Get());
        cl->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
        cl->IASetVertexBuffers(0, 1, &vbv);
        cl->IASetIndexBuffer(&ibv);
        cl->SetGraphicsRoot32BitConstants(0, 16, &m, 0);
        cl->DrawIndexedInstanced(36, 1, 0, 0, 0);
    }
    void Cleanup() override { vb.Reset(); ib.Reset(); pso.Reset(); rootSig.Reset(); }
};

// =============== Whirling Cube Scene (with Presets) ===============
struct WhirligigScene : IScene {
    // PSOs & root
    ComPtr<ID3D12PipelineState>    psoSolid, psoLine;
    ComPtr<ID3D12RootSignature>    rootSig;

    // Mesh
    ComPtr<ID3D12Resource>         vb, ib;
    D3D12_VERTEX_BUFFER_VIEW       vbv{};
    D3D12_INDEX_BUFFER_VIEW        ibv{};

    // Lines
    ComPtr<ID3D12Resource>         diagVB, trajVB;
    D3D12_VERTEX_BUFFER_VIEW       diagVBV{}, trajVBV{};
    std::vector<XMFLOAT3>          trajCPU;
    UINT                           trajMax = 8192;

    // UI/state
    bool showCube = true, showDiagonal = true, showTrajectory = true;
    bool start = false;
    bool use_grav = true;

    // Parameters
    float sizeL = 1.2f;
    float density = 1.0f;
    float spinDeg = 25.0f;     // rotation about +Y at t=0 (visual spin)
    float angVel = 4.0f;      // body-frame angular velocity magnitude along body diagonal
    int   trajLen = 1200;
    int   speed = 10;
    float step = 0.01f;

    // Extra: world yaw (orbit) around +Y (requested preset behavior)
    bool  applyWorldYaw = false;
    float worldYawSpeed = 1.2f;    // rad/s
    float worldYawAngle = 0.0f;

    // Rigid body state
    XMFLOAT4 q = { 0,0,0,1 };
    XMFLOAT3 w_body = { 0,0,0 };
    float a = 0.0f, b = 0.0f, ia = 0.0f, ibeta = 0.0f;
    XMFLOAT3 r_com_body = { 0.5f,0.5f,0.5f };
    XMFLOAT3 diag_body = { 1,1,1 };
    double acc = 0.0;

    // Presets
    int presetIndex = 0;
    const char* presetNames[5]{
        "Default",
        "Top Spin (+Y)",
        "Top Spin (+Y) + World Y Orbit", // requested
        "Zero-Gravity Top",
        "Heavy & Slow"
    };

    // Helpers
    static inline XMFLOAT3 ABJ_apply(float a, float b, const XMFLOAT3& v) {
        float s = v.x + v.y + v.z;
        return { a * v.x + b * s, a * v.y + b * s, a * v.z + b * s };
    }
    static inline XMFLOAT3 Cross(const XMFLOAT3& x, const XMFLOAT3& y) {
        return { x.y * y.z - x.z * y.y, x.z * y.x - x.x * y.z, x.x * y.y - x.y * y.x };
    }
    static inline XMFLOAT3 Add(const XMFLOAT3& x, const XMFLOAT3& y) {
        return { x.x + y.x, x.y + y.y, x.z + y.z };
    }
    static inline XMFLOAT3 Sub(const XMFLOAT3& x, const XMFLOAT3& y) {
        return { x.x - y.x, x.y - y.y, x.z - y.z };
    }
    static inline XMFLOAT3 Mul(const XMFLOAT3& x, float s) {
        return { x.x * s, x.y * s, x.z * s };
    }

    static XMVECTOR QuaternionFromTo(XMVECTOR from, XMVECTOR to) {
        XMVECTOR f = XMVector3Normalize(from);
        XMVECTOR t = XMVector3Normalize(to);
        float d = XMVectorGetX(XMVector3Dot(f, t));
        if (d > 0.9999f) return XMQuaternionIdentity();
        if (d < -0.9999f) {
            XMVECTOR axis = XMVector3Cross(f, XMVectorSet(1, 0, 0, 0));
            float len2 = XMVectorGetX(XMVector3LengthSq(axis));
            if (len2 < 1e-6f) axis = XMVector3Cross(f, XMVectorSet(0, 0, 1, 0));
            axis = XMVector3Normalize(axis);
            return XMQuaternionRotationAxis(axis, XM_PI);
        }
        XMVECTOR axis = XMVector3Normalize(XMVector3Cross(f, t));
        float ang = std::acosf(std::clamp(d, -1.0f, 1.0f));
        return XMQuaternionRotationAxis(axis, ang);
    }

    void BuildInertia() {
        float m = density * sizeL * sizeL * sizeL;
        float k = m * sizeL * sizeL;
        a = (11.0f / 12.0f) * k;
        b = (-1.0f / 4.0f) * k;
        ia = 1.0f / a;
        ibeta = -b / (a * (a + 3.0f * b));
        r_com_body = { 0.5f * sizeL, 0.5f * sizeL, 0.5f * sizeL };
        diag_body = { sizeL, sizeL, sizeL };
    }

    void ResetState() {
        BuildInertia();
        XMVECTOR dB = XMLoadFloat3(&diag_body);
        XMVECTOR upv = XMVectorSet(0, 1, 0, 0);
        XMVECTOR qAlign = QuaternionFromTo(dB, upv);
        XMVECTOR qSpin = XMQuaternionRotationAxis(upv, XMConvertToRadians(spinDeg));
        XMVECTOR qv = XMQuaternionMultiply(qSpin, qAlign);
        qv = XMQuaternionNormalize(qv);
        XMStoreFloat4(&q, qv);

        // Angular velocity in body frame along body diagonal
        XMVECTOR ud = XMVector3Normalize(dB);
        XMFLOAT3 udb; XMStoreFloat3(&udb, ud);
        w_body = { udb.x * angVel, udb.y * angVel, udb.z * angVel };

        trajCPU.clear();
        acc = 0.0;
        worldYawAngle = 0.0f;
    }

    void ApplyPreset(int idx) {
        switch (idx) {
        default:
        case 0: // Default
            sizeL = 1.2f; density = 1.0f;
            spinDeg = 25.0f; angVel = 4.0f;
            use_grav = true; speed = 10; step = 0.01f;
            applyWorldYaw = false; worldYawSpeed = 1.2f;
            break;
        case 1: // Top Spin (+Y)
            sizeL = 1.0f; density = 1.0f;
            spinDeg = 30.0f; angVel = 10.0f;
            use_grav = false; speed = 10; step = 0.01f;
            applyWorldYaw = false; worldYawSpeed = 1.2f;
            break;
        case 2: // Top Spin (+Y) + World Y Orbit (requested)
            sizeL = 1.0f; density = 1.0f;
            spinDeg = 25.0f; angVel = 12.0f;
            use_grav = false; speed = 10; step = 0.01f;
            applyWorldYaw = true; worldYawSpeed = 1.0f;
            break;
        case 3: // Zero-Gravity Top
            sizeL = 1.2f; density = 1.0f;
            spinDeg = 0.0f; angVel = 6.0f;
            use_grav = false; speed = 10; step = 0.01f;
            applyWorldYaw = false; worldYawSpeed = 0.7f;
            break;
        case 4: // Heavy & Slow (with gravity)
            sizeL = 1.5f; density = 5.0f;
            spinDeg = 10.0f; angVel = 2.0f;
            use_grav = true; speed = 8; step = 0.008f;
            applyWorldYaw = false; worldYawSpeed = 0.6f;
            break;
        }
        ResetState();
    }

    // IScene
    void Init(ID3D12Device* device) override {
        // Root signature: MVP constants
        D3D12_ROOT_PARAMETER param{};
        param.ParameterType = D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS;
        param.Constants.Num32BitValues = 16;
        param.Constants.ShaderRegister = 0;
        D3D12_ROOT_SIGNATURE_DESC rs{}; rs.NumParameters = 1; rs.pParameters = &param;
        rs.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;
        ComPtr<ID3DBlob> sig, err;
        HR(D3D12SerializeRootSignature(&rs, D3D_ROOT_SIGNATURE_VERSION_1, &sig, &err),
            "D3D12SerializeRootSignature(Whirligig)",
            err ? std::string((char*)err->GetBufferPointer(), err->GetBufferSize()) : "");
        HR(device->CreateRootSignature(0, sig->GetBufferPointer(), sig->GetBufferSize(),
            IID_PPV_ARGS(&rootSig)), "CreateRootSignature(Whirligig)");

        // VS/PS
        const char* vsSrc = R"(
cbuffer Params : register(b0) { float4x4 MVP; }
struct VSIn { float3 pos:POSITION; float3 col:COLOR; };
struct PSIn { float4 pos:SV_POSITION; float3 col:COLOR; };
PSIn VSMain(VSIn i){ PSIn o; o.pos = mul(float4(i.pos,1), MVP); o.col = i.col; return o; }
)";
        const char* psSrc = R"(
struct PSIn { float4 pos:SV_POSITION; float3 col:COLOR; };
float4 PSMain(PSIn i):SV_TARGET { return float4(i.col,1); }
)";
        ComPtr<ID3DBlob> vs, ps, shErr;
        HR(D3DCompile(vsSrc, (UINT)std::strlen(vsSrc), nullptr, nullptr, nullptr,
            "VSMain", "vs_5_0", 0, 0, &vs, &shErr),
            "D3DCompile(VS whirligig)", shErr ? std::string((char*)shErr->GetBufferPointer(), shErr->GetBufferSize()) : "");
        HR(D3DCompile(psSrc, (UINT)std::strlen(psSrc), nullptr, nullptr, nullptr,
            "PSMain", "ps_5_0", 0, 0, &ps, &shErr),
            "D3DCompile(PS whirligig)", shErr ? std::string((char*)shErr->GetBufferPointer(), shErr->GetBufferSize()) : "");

        D3D12_INPUT_ELEMENT_DESC layout[] = {
            { "POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0,   D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0 },
            { "COLOR",    0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 12,  D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0 },
        };

        // Solid PSO
        D3D12_GRAPHICS_PIPELINE_STATE_DESC psoS{};
        psoS.pRootSignature = rootSig.Get();
        psoS.VS = { vs->GetBufferPointer(), vs->GetBufferSize() };
        psoS.PS = { ps->GetBufferPointer(), ps->GetBufferSize() };
        psoS.InputLayout = { layout, _countof(layout) };
        psoS.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
        psoS.NumRenderTargets = 1;
        psoS.RTVFormats[0] = DXGI_FORMAT_R8G8B8A8_UNORM;
        psoS.DSVFormat = DXGI_FORMAT_D32_FLOAT;
        psoS.SampleDesc.Count = 1;
        psoS.SampleMask = UINT_MAX;

        // Blend opaque
        psoS.BlendState.AlphaToCoverageEnable = FALSE;
        psoS.BlendState.IndependentBlendEnable = FALSE;
        D3D12_RENDER_TARGET_BLEND_DESC rt{};
        rt.BlendEnable = FALSE; rt.LogicOpEnable = FALSE;
        rt.SrcBlend = D3D12_BLEND_ONE; rt.DestBlend = D3D12_BLEND_ZERO; rt.BlendOp = D3D12_BLEND_OP_ADD;
        rt.SrcBlendAlpha = D3D12_BLEND_ONE; rt.DestBlendAlpha = D3D12_BLEND_ZERO; rt.BlendOpAlpha = D3D12_BLEND_OP_ADD;
        rt.LogicOp = D3D12_LOGIC_OP_NOOP; rt.RenderTargetWriteMask = D3D12_COLOR_WRITE_ENABLE_ALL;
        for (int i = 0; i < 8; ++i) psoS.BlendState.RenderTarget[i] = rt;

        // Rasterizer
        psoS.RasterizerState.FillMode = D3D12_FILL_MODE_SOLID;
        psoS.RasterizerState.CullMode = D3D12_CULL_MODE_NONE;
        psoS.RasterizerState.DepthClipEnable = TRUE;

        // Depth
        psoS.DepthStencilState.DepthEnable = TRUE;
        psoS.DepthStencilState.DepthWriteMask = D3D12_DEPTH_WRITE_MASK_ALL;
        psoS.DepthStencilState.DepthFunc = D3D12_COMPARISON_FUNC_LESS_EQUAL;
        psoS.DepthStencilState.StencilEnable = FALSE;

        HR(device->CreateGraphicsPipelineState(&psoS, IID_PPV_ARGS(&psoSolid)), "CreatePSO(Whirligig solid)");

        // Line PSO
        D3D12_GRAPHICS_PIPELINE_STATE_DESC psoL = psoS;
        psoL.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_LINE;
        HR(device->CreateGraphicsPipelineState(&psoL, IID_PPV_ARGS(&psoLine)), "CreatePSO(Whirligig line)");

        // Unit cube
        const float s = 0.5f;
        VtxPC verts[] = {
            {{-s,-s, s},{1,0,0}}, {{ s,-s, s},{0,1,0}}, {{ s, s, s},{0,0,1}}, {{-s, s, s},{1,1,0}},
            {{-s,-s,-s},{1,0,1}}, {{ s,-s,-s},{0,1,1}}, {{ s, s,-s},{1,1,1}}, {{-s, s,-s},{0.2f,0.6f,1}},
        };
        uint16_t idx[] = {
            0,1,2, 0,2,3,  1,5,6, 1,6,2,  5,4,7, 5,7,6,
            4,0,3, 4,3,7,  3,2,6, 3,6,7,  4,5,1, 4,1,0
        };
        UINT vbSize = sizeof(verts);
        D3D12_HEAP_PROPERTIES heap{}; heap.Type = D3D12_HEAP_TYPE_UPLOAD;
        D3D12_RESOURCE_DESC vbDesc{}; vbDesc.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER; vbDesc.Width = vbSize; vbDesc.Height = 1; vbDesc.DepthOrArraySize = 1; vbDesc.MipLevels = 1; vbDesc.SampleDesc.Count = 1; vbDesc.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;
        HR(device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &vbDesc, D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&vb)),
            "CreateCommittedResource(WhirligigVB)");
        void* p = nullptr; D3D12_RANGE r{ 0,0 };
        HR(vb->Map(0, &r, &p), "WhirligigVB->Map"); std::memcpy(p, verts, vbSize); vb->Unmap(0, nullptr);
        vbv.BufferLocation = vb->GetGPUVirtualAddress(); vbv.StrideInBytes = sizeof(VtxPC); vbv.SizeInBytes = vbSize;

        UINT ibSize = sizeof(idx);
        D3D12_RESOURCE_DESC ibDesc{}; ibDesc.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER; ibDesc.Width = ibSize; ibDesc.Height = 1; ibDesc.DepthOrArraySize = 1; ibDesc.MipLevels = 1; ibDesc.SampleDesc.Count = 1; ibDesc.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;
        HR(device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &ibDesc, D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&ib)),
            "CreateCommittedResource(WhirligigIB)");
        HR(ib->Map(0, &r, &p), "WhirligigIB->Map"); std::memcpy(p, idx, ibSize); ib->Unmap(0, nullptr);
        ibv.BufferLocation = ib->GetGPUVirtualAddress(); ibv.Format = DXGI_FORMAT_R16_UINT; ibv.SizeInBytes = ibSize;

        // Dynamic line buffers
        D3D12_RESOURCE_DESC ddesc{}; ddesc.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER; ddesc.Width = sizeof(VtxPC) * 2; ddesc.Height = 1; ddesc.DepthOrArraySize = 1; ddesc.MipLevels = 1; ddesc.SampleDesc.Count = 1; ddesc.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;
        HR(device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &ddesc, D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&diagVB)),
            "CreateCommittedResource(DiagVB)");
        diagVBV.BufferLocation = diagVB->GetGPUVirtualAddress(); diagVBV.StrideInBytes = sizeof(VtxPC); diagVBV.SizeInBytes = sizeof(VtxPC) * 2;

        D3D12_RESOURCE_DESC tdesc{}; tdesc.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER; tdesc.Width = sizeof(VtxPC) * trajMax; tdesc.Height = 1; tdesc.DepthOrArraySize = 1; tdesc.MipLevels = 1; tdesc.SampleDesc.Count = 1; tdesc.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;
        HR(device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &tdesc, D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&trajVB)),
            "CreateCommittedResource(TrajVB)");
        trajVBV.BufferLocation = trajVB->GetGPUVirtualAddress(); trajVBV.StrideInBytes = sizeof(VtxPC); trajVBV.SizeInBytes = sizeof(VtxPC) * trajMax;

        // Start on Default preset
        ApplyPreset(0);
    }

    void Update(float dt) override {
        if (start && applyWorldYaw) worldYawAngle += dt * worldYawSpeed;

        if (!start) return;
        acc += dt * (double)speed;
        while (acc > step) {
            Integrate(step);
            XMFLOAT3 p1 = DiagonalTipWorld();
            trajCPU.push_back(p1);
            if ((int)trajCPU.size() > trajLen) {
                int drop = (int)trajCPU.size() - trajLen;
                trajCPU.erase(trajCPU.begin(), trajCPU.begin() + drop);
            }
            acc -= step;
        }
    }

    void Integrate(float h) {
        XMVECTOR qv = XMLoadFloat4(&q);
        XMMATRIX R = XMMatrixRotationQuaternion(qv);
        XMVECTOR gW = XMVectorSet(0.0f, use_grav ? -9.81f : 0.0f, 0.0f, 0.0f);
        XMMATRIX Rt = XMMatrixTranspose(R);
        XMFLOAT3 gB; XMStoreFloat3(&gB, XMVector3TransformNormal(gW, Rt));

        float m = density * sizeL * sizeL * sizeL;
        XMFLOAT3 mg = Mul(gB, m);
        XMFLOAT3 N = Cross(r_com_body, mg);

        XMFLOAT3 Iw = ABJ_apply(a, b, w_body);
        XMFLOAT3 wX_Iw = Cross(w_body, Iw);
        XMFLOAT3 rhs = Sub(N, wX_Iw);
        XMFLOAT3 domega = ABJ_apply(ia, ibeta, rhs);
        w_body = Add(w_body, Mul(domega, h));

        XMVECTOR wBv = XMLoadFloat3(&w_body);
        XMVECTOR wWv = XMVector3TransformNormal(wBv, R);
        XMFLOAT3 wW; XMStoreFloat3(&wW, wWv);
        XMVECTOR wq = XMVectorSet(wW.x, wW.y, wW.z, 0.0f);
        XMVECTOR qdot = XMQuaternionMultiply(qv, wq);
        qdot = XMVectorScale(qdot, 0.5f);
        qv = XMVectorAdd(qv, XMVectorScale(qdot, h));
        qv = XMQuaternionNormalize(qv);
        XMStoreFloat4(&q, qv);
    }

    XMFLOAT3 DiagonalTipWorld() const {
        XMVECTOR qv = XMLoadFloat4(&q);
        XMMATRIX R = XMMatrixRotationQuaternion(qv);
        XMFLOAT3 out; XMStoreFloat3(&out, XMVector3TransformNormal(XMLoadFloat3(&diag_body), R));
        return out;
    }

    void Render(ID3D12GraphicsCommandList* cl, const XMMATRIX& view, const XMMATRIX& proj) override {
        XMVECTOR qv = XMLoadFloat4(&q);
        XMMATRIX R = XMMatrixRotationQuaternion(qv);
        XMMATRIX S = XMMatrixScaling(sizeL, sizeL, sizeL);
        XMMATRIX T = XMMatrixTranslation(+0.5f * sizeL, +0.5f * sizeL, +0.5f * sizeL);
        XMMATRIX world = S * T * R;
        if (applyWorldYaw) world = world * XMMatrixRotationY(worldYawAngle);

        if (showCube) {
            XMFLOAT4X4 m; XMStoreFloat4x4(&m, XMMatrixTranspose(world * view * proj));
            cl->SetPipelineState(psoSolid.Get());
            cl->SetGraphicsRootSignature(rootSig.Get());
            cl->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
            cl->IASetVertexBuffers(0, 1, &vbv);
            cl->IASetIndexBuffer(&ibv);
            cl->SetGraphicsRoot32BitConstants(0, 16, &m, 0);
            cl->DrawIndexedInstanced(36, 1, 0, 0, 0);
        }

        XMFLOAT4X4 vp; XMStoreFloat4x4(&vp, XMMatrixTranspose(view * proj));
        cl->SetPipelineState(psoLine.Get());
        cl->SetGraphicsRootSignature(rootSig.Get());
        cl->SetGraphicsRoot32BitConstants(0, 16, &vp, 0);

        if (showDiagonal) {
            VtxPC diag[2]{};
            diag[0] = { {0,0,0}, {1,1,0} };
            XMFLOAT3 tip = DiagonalTipWorld();
            diag[1] = { {tip.x, tip.y, tip.z}, {1,1,0} };
            void* p = nullptr; D3D12_RANGE r{ 0,0 };
            HR(diagVB->Map(0, &r, &p), "DiagVB->Map");
            std::memcpy(p, diag, sizeof(diag));
            diagVB->Unmap(0, nullptr);
            cl->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_LINESTRIP);
            cl->IASetVertexBuffers(0, 1, &diagVBV);
            cl->DrawInstanced(2, 1, 0, 0);
        }

        if (showTrajectory && !trajCPU.empty()) {
            void* p = nullptr; D3D12_RANGE r{ 0,0 };
            HR(trajVB->Map(0, &r, &p), "TrajVB->Map");
            auto* dst = (VtxPC*)p;
            int n = (int)std::min<size_t>(trajCPU.size(), (size_t)trajMax);
            int start = (int)trajCPU.size() - n;
            int denom = (n > 1) ? (n - 1) : 1;
            for (int i = 0; i < n; ++i) {
                float t = float(i) / float(denom);
                dst[i] = { { trajCPU[start + i].x, trajCPU[start + i].y, trajCPU[start + i].z },
                           { 1.0f - t, 0.2f, t } };
            }
            trajVB->Unmap(0, nullptr);
            cl->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_LINESTRIP);
            trajVBV.SizeInBytes = n * sizeof(VtxPC);
            cl->IASetVertexBuffers(0, 1, &trajVBV);
            cl->DrawInstanced(n, 1, 0, 0);
        }
    }

    void Cleanup() override {
        vb.Reset(); ib.Reset(); diagVB.Reset(); trajVB.Reset();
        psoSolid.Reset(); psoLine.Reset(); rootSig.Reset();
    }

    void DrawUiInline() override {
        if (ImGui::CollapsingHeader("Whirligig Scene", ImGuiTreeNodeFlags_DefaultOpen)) {
            // --- Presets (always first, full width) ---
            ImGui::SeparatorText("Presets");
            ImGui::SetNextItemWidth(-1.0f); // fill width
            int idx = presetIndex;
            if (ImGui::Combo("##WhirlPresetList", &idx, presetNames, IM_ARRAYSIZE(presetNames))) {
                presetIndex = idx;
                ApplyPreset(presetIndex);   // immediate apply on change
            }
            ImGui::SameLine();
            if (ImGui::Button("Reapply")) ApplyPreset(presetIndex);

            ImGui::Separator();

            ImGui::Text("Controls");
            if (ImGui::Button("Start")) { start = true; }
            ImGui::SameLine();
            if (ImGui::Button("Stop")) { start = false; }
            ImGui::SameLine();
            if (ImGui::Button("Reset")) { start = false; ResetState(); }

            ImGui::SeparatorText("Show");
            ImGui::Checkbox("Cube", &showCube);
            ImGui::Checkbox("Cube Diagonal (yellow)", &showDiagonal);
            ImGui::Checkbox("Cube Trajectory", &showTrajectory);

            ImGui::SeparatorText("Parameters");
            bool changed = false;
            changed |= ImGui::SliderFloat("Cube size (L)", &sizeL, 0.5f, 2.0f);
            changed |= ImGui::SliderFloat("Density", &density, 0.1f, 10.0f);
            changed |= ImGui::SliderFloat("Spin about +Y (deg)", &spinDeg, -180.0f, 180.0f, "%.1f");
            changed |= ImGui::SliderFloat("Angular speed (rad/s)", &angVel, 0.0f, 20.0f);
            changed |= ImGui::SliderInt("Trajectory length", &trajLen, 100, (int)trajMax);
            ImGui::Checkbox("Use gravitation", &use_grav);
            ImGui::SliderInt("Speed (sim x)", &speed, 1, 200);
            ImGui::SliderFloat("Step dt", &step, 0.001f, 0.05f, "%.3f", ImGuiSliderFlags_Logarithmic);

            ImGui::SeparatorText("World Y Orbit");
            ImGui::Checkbox("Orbit whole cube around +Y", &applyWorldYaw);
            ImGui::SliderFloat("Orbit speed (rad/s)", &worldYawSpeed, 0.0f, 6.28f, "%.2f");

            if (changed) { ResetState(); }
        }
    }
};

// -------------------- App --------------------
class D3D12ScenesApp {
public:
    D3D12ScenesApp(UINT w, UINT h) : m_width(w), m_height(h) {}
    void Run(HINSTANCE hInstance, int nCmdShow) {
        RegisterWindowClass(hInstance);
        CreateAppWindow(hInstance, nCmdShow);
        InitD3D();
        InitImGui();
        m_cam.SetViewport(m_width, m_height);
        m_cam.ResetNiceView(4.0f);
        InitScenes();
        m_grid.Init(m_device.Get());
        MainLoop();
        Cleanup();
    }

private:
    HWND m_hwnd = nullptr;
    UINT m_width = 1280, m_height = 720;

    ComPtr<IDXGIFactory6>          m_factory;
    ComPtr<ID3D12Device>           m_device;
    ComPtr<ID3D12CommandQueue>     m_commandQueue;
    ComPtr<IDXGISwapChain3>        m_swapChain;
    ComPtr<ID3D12DescriptorHeap>   m_rtvHeap;
    ComPtr<ID3D12Resource>         m_renderTargets[FrameCount];
    UINT                           m_rtvDescriptorSize = 0;
    UINT                           m_frameIndex = 0;

    ComPtr<ID3D12DescriptorHeap>   m_dsvHeap;
    ComPtr<ID3D12Resource>         m_depth;
    D3D12_CPU_DESCRIPTOR_HANDLE    m_dsv{};

    ComPtr<ID3D12CommandAllocator> m_commandAllocators[FrameCount];
    ComPtr<ID3D12GraphicsCommandList> m_commandList;

    ComPtr<ID3D12Fence>            m_fence;
    UINT64                         m_fenceValues[FrameCount] = {};
    HANDLE                         m_fenceEvent = nullptr;

    D3D12_VIEWPORT                 m_viewport{};
    D3D12_RECT                     m_scissor{};

    ComPtr<ID3D12DescriptorHeap>   m_imguiSrvHeap;

    enum class SceneKind { Triangle = 0, Cube = 1, Whirligig = 2 };
    SceneKind                      m_sceneKind = SceneKind::Whirligig;
    TriangleScene                  m_triangle;
    CubeScene                      m_cube;
    WhirligigScene                 m_whirl;

    CadCamera                      m_cam;
    GridAxes                       m_grid;

    float                          m_sidebarWidth = 340.0f;
    std::chrono::steady_clock::time_point m_prev = std::chrono::steady_clock::now();

    // Win32
    static LRESULT CALLBACK WndProcSetup(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {
        if (msg == WM_NCCREATE) {
            auto cs = reinterpret_cast<CREATESTRUCT*>(lParam);
            auto app = reinterpret_cast<D3D12ScenesApp*>(cs->lpCreateParams);
            SetWindowLongPtr(hWnd, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(app));
            SetWindowLongPtr(hWnd, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(WndProcThunk));
            return app->WndProc(hWnd, msg, wParam, lParam);
        }
        return DefWindowProc(hWnd, msg, wParam, lParam);
    }
    static LRESULT CALLBACK WndProcThunk(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {
        auto app = reinterpret_cast<D3D12ScenesApp*>(GetWindowLongPtr(hWnd, GWLP_USERDATA));
        return app->WndProc(hWnd, msg, wParam, lParam);
    }
    LRESULT WndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {
        if (ImGui_ImplWin32_WndProcHandler(hWnd, msg, wParam, lParam)) return 1;
        switch (msg) {
        case WM_SIZE:
            if (m_swapChain && wParam != SIZE_MINIMIZED) {
                UINT w = LOWORD(lParam), h = HIWORD(lParam);
                OnResize(w ? w : 1, h ? h : 1);
            }
            return 0;
        case WM_DESTROY: PostQuitMessage(0); return 0;
        default: return DefWindowProc(hWnd, msg, wParam, lParam);
        }
    }
    void RegisterWindowClass(HINSTANCE hInstance) {
        WNDCLASS wc{};
        wc.lpfnWndProc = WndProcSetup;
        wc.hInstance = hInstance;
        wc.lpszClassName = L"DX12ScenesWinClass";
        wc.hCursor = LoadCursor(NULL, IDC_ARROW);
        wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
        RegisterClass(&wc);
    }
    void CreateAppWindow(HINSTANCE hInstance, int nCmdShow) {
        RECT rc{ 0,0,(LONG)m_width,(LONG)m_height };
        AdjustWindowRect(&rc, WS_OVERLAPPEDWINDOW, FALSE);
        m_hwnd = CreateWindowEx(0, L"DX12ScenesWinClass", L"DX12 Scenes + CAD Camera + Grid — Presets",
            WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT,
            rc.right - rc.left, rc.bottom - rc.top,
            nullptr, nullptr, hInstance, this);
        ShowWindow(m_hwnd, nCmdShow);
        UpdateWindow(m_hwnd);
    }

    // D3D
    void InitD3D() {
        UINT factoryFlags = 0;
#if defined(_DEBUG)
        if (ComPtr<ID3D12Debug> dbg; SUCCEEDED(D3D12GetDebugInterface(IID_PPV_ARGS(&dbg)))) {
            dbg->EnableDebugLayer();
            factoryFlags |= DXGI_CREATE_FACTORY_DEBUG;
        }
#endif
        HR(CreateDXGIFactory2(factoryFlags, IID_PPV_ARGS(&m_factory)), "CreateDXGIFactory2");

        HRESULT hr = D3D12CreateDevice(nullptr, D3D_FEATURE_LEVEL_11_0, IID_PPV_ARGS(&m_device));
        if (FAILED(hr)) {
            ComPtr<IDXGIAdapter> warp;
            HR(m_factory->EnumWarpAdapter(IID_PPV_ARGS(&warp)), "EnumWarpAdapter");
            HR(D3D12CreateDevice(warp.Get(), D3D_FEATURE_LEVEL_11_0, IID_PPV_ARGS(&m_device)),
                "D3D12CreateDevice(WARP)");
        }

        D3D12_COMMAND_QUEUE_DESC q{}; q.Type = D3D12_COMMAND_LIST_TYPE_DIRECT;
        HR(m_device->CreateCommandQueue(&q, IID_PPV_ARGS(&m_commandQueue)), "CreateCommandQueue");

        DXGI_SWAP_CHAIN_DESC1 sc{};
        sc.BufferCount = FrameCount; sc.Width = m_width; sc.Height = m_height;
        sc.Format = DXGI_FORMAT_R8G8B8A8_UNORM; sc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
        sc.SwapEffect = DXGI_SWAP_EFFECT_FLIP_DISCARD; sc.SampleDesc.Count = 1;
        ComPtr<IDXGISwapChain1> sc1;
        HR(m_factory->CreateSwapChainForHwnd(m_commandQueue.Get(), m_hwnd, &sc, nullptr, nullptr, &sc1),
            "CreateSwapChainForHwnd");
        HR(sc1.As(&m_swapChain), "SwapChain1->As<IDXGISwapChain3>");
        m_frameIndex = m_swapChain->GetCurrentBackBufferIndex();

        D3D12_DESCRIPTOR_HEAP_DESC rtvDesc{}; rtvDesc.NumDescriptors = FrameCount; rtvDesc.Type = D3D12_DESCRIPTOR_HEAP_TYPE_RTV;
        HR(m_device->CreateDescriptorHeap(&rtvDesc, IID_PPV_ARGS(&m_rtvHeap)), "CreateDescriptorHeap(RTV)");
        m_rtvDescriptorSize = m_device->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_RTV);
        CreateRTVs();

        D3D12_DESCRIPTOR_HEAP_DESC dsvDesc{}; dsvDesc.NumDescriptors = 1; dsvDesc.Type = D3D12_DESCRIPTOR_HEAP_TYPE_DSV;
        HR(m_device->CreateDescriptorHeap(&dsvDesc, IID_PPV_ARGS(&m_dsvHeap)), "CreateDescriptorHeap(DSV)");
        m_dsv = m_dsvHeap->GetCPUDescriptorHandleForHeapStart();
        CreateDepth(m_width, m_height);

        for (UINT i = 0; i < FrameCount; ++i)
            HR(m_device->CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT, IID_PPV_ARGS(&m_commandAllocators[i])),
                "CreateCommandAllocator");
        HR(m_device->CreateCommandList(0, D3D12_COMMAND_LIST_TYPE_DIRECT,
            m_commandAllocators[m_frameIndex].Get(), nullptr, IID_PPV_ARGS(&m_commandList)),
            "CreateCommandList");
        HR(m_commandList->Close(), "Close(CommandList)");

        HR(m_device->CreateFence(0, D3D12_FENCE_FLAG_NONE, IID_PPV_ARGS(&m_fence)), "CreateFence");
        m_fenceValues[m_frameIndex] = 1;
        m_fenceEvent = CreateEvent(nullptr, FALSE, FALSE, nullptr);
        if (!m_fenceEvent) HR(HRESULT_FROM_WIN32(GetLastError()), "CreateEvent");

        m_viewport = { 0.0f, 0.0f, float(m_width), float(m_height), 0.0f, 1.0f };
        m_scissor = { 0, 0, (LONG)m_width, (LONG)m_height };
    }

    void CreateRTVs() {
        D3D12_CPU_DESCRIPTOR_HANDLE h = m_rtvHeap->GetCPUDescriptorHandleForHeapStart();
        for (UINT i = 0; i < FrameCount; ++i) {
            HR(m_swapChain->GetBuffer(i, IID_PPV_ARGS(&m_renderTargets[i])), "SwapChain->GetBuffer");
            m_device->CreateRenderTargetView(m_renderTargets[i].Get(), nullptr, h);
            h.ptr += m_rtvDescriptorSize;
        }
    }

    void CreateDepth(UINT w, UINT h) {
        m_depth.Reset();
        D3D12_RESOURCE_DESC tex{};
        tex.Dimension = D3D12_RESOURCE_DIMENSION_TEXTURE2D;
        tex.Width = w; tex.Height = h; tex.DepthOrArraySize = 1; tex.MipLevels = 1;
        tex.SampleDesc.Count = 1; tex.Format = DXGI_FORMAT_D32_FLOAT;
        tex.Flags = D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL;
        D3D12_CLEAR_VALUE clear{}; clear.Format = DXGI_FORMAT_D32_FLOAT; clear.DepthStencil.Depth = 1.0f;

        D3D12_HEAP_PROPERTIES heap{}; heap.Type = D3D12_HEAP_TYPE_DEFAULT;
        HR(m_device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &tex,
            D3D12_RESOURCE_STATE_DEPTH_WRITE, &clear, IID_PPV_ARGS(&m_depth)),
            "CreateCommittedResource(Depth)");

        D3D12_DEPTH_STENCIL_VIEW_DESC dsv{};
        dsv.Format = DXGI_FORMAT_D32_FLOAT; dsv.ViewDimension = D3D12_DSV_DIMENSION_TEXTURE2D;
        m_device->CreateDepthStencilView(m_depth.Get(), &dsv, m_dsv);
    }

    // ImGui
    void InitImGui() {
        IMGUI_CHECKVERSION();
        ImGui::CreateContext();
        ImGui::StyleColorsDark();
        D3D12_DESCRIPTOR_HEAP_DESC heap{};
        heap.Type = D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV;
        heap.NumDescriptors = 1;
        heap.Flags = D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE;
        HR(m_device->CreateDescriptorHeap(&heap, IID_PPV_ARGS(&m_imguiSrvHeap)), "CreateDescriptorHeap(ImGui)");

        ImGui_ImplWin32_Init(m_hwnd);
        ImGui_ImplDX12_Init(
            m_device.Get(), FrameCount,
            DXGI_FORMAT_R8G8B8A8_UNORM,
            m_imguiSrvHeap.Get(),
            m_imguiSrvHeap->GetCPUDescriptorHandleForHeapStart(),
            m_imguiSrvHeap->GetGPUDescriptorHandleForHeapStart());
    }

    void InitScenes() {
        m_triangle.Init(m_device.Get());
        m_cube.Init(m_device.Get());
        m_whirl.Init(m_device.Get());
    }

    void MainLoop() {
        MSG msg{};
        while (msg.message != WM_QUIT) {
            if (PeekMessage(&msg, nullptr, 0, 0, PM_REMOVE)) {
                TranslateMessage(&msg); DispatchMessage(&msg);
            }
            else {
                auto now = std::chrono::steady_clock::now();
                float dt = std::chrono::duration<float>(now - m_prev).count();
                m_prev = now;
                Update(dt);
                RenderFrame();
            }
        }
        WaitForGPU();
    }

    void Update(float dt) {
        ImGui_ImplDX12_NewFrame();
        ImGui_ImplWin32_NewFrame();
        ImGui::NewFrame();

        m_cam.UpdateFromImGui(ImGui::GetIO(), dt);

        switch (m_sceneKind) {
        case SceneKind::Triangle:  m_triangle.Update(dt); break;
        case SceneKind::Cube:      m_cube.Update(dt);     break;
        case SceneKind::Whirligig: m_whirl.Update(dt);    break;
        }
        DrawSidebar();
    }

    void RenderFrame() {
        XMMATRIX view = m_cam.GetViewRH();
        XMMATRIX proj = m_cam.GetProjRH();

        HR(m_commandAllocators[m_frameIndex]->Reset(), "CmdAlloc->Reset");
        HR(m_commandList->Reset(m_commandAllocators[m_frameIndex].Get(), nullptr), "CmdList->Reset");

        D3D12_RESOURCE_BARRIER toRT{};
        toRT.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
        toRT.Transition.pResource = m_renderTargets[m_frameIndex].Get();
        toRT.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
        toRT.Transition.StateBefore = D3D12_RESOURCE_STATE_PRESENT;
        toRT.Transition.StateAfter = D3D12_RESOURCE_STATE_RENDER_TARGET;
        m_commandList->ResourceBarrier(1, &toRT);

        m_commandList->RSSetViewports(1, &m_viewport);
        m_commandList->RSSetScissorRects(1, &m_scissor);

        D3D12_CPU_DESCRIPTOR_HANDLE rtv = m_rtvHeap->GetCPUDescriptorHandleForHeapStart();
        rtv.ptr += m_frameIndex * m_rtvDescriptorSize;
        m_commandList->OMSetRenderTargets(1, &rtv, FALSE, &m_dsv);

        const float clear[4] = { 0.06f, 0.07f, 0.10f, 1.0f };
        m_commandList->ClearRenderTargetView(rtv, clear, 0, nullptr);
        m_commandList->ClearDepthStencilView(m_dsv, D3D12_CLEAR_FLAG_DEPTH, 1.0f, 0, 0, nullptr);

        m_grid.Render(m_commandList.Get(), view, proj);

        switch (m_sceneKind) {
        case SceneKind::Triangle:  m_triangle.Render(m_commandList.Get(), view, proj); break;
        case SceneKind::Cube:      m_cube.Render(m_commandList.Get(), view, proj);     break;
        case SceneKind::Whirligig: m_whirl.Render(m_commandList.Get(), view, proj);    break;
        }

        ImGui::Render();
        ID3D12DescriptorHeap* heaps[] = { m_imguiSrvHeap.Get() };
        m_commandList->SetDescriptorHeaps(1, heaps);
        ImGui_ImplDX12_RenderDrawData(ImGui::GetDrawData(), m_commandList.Get());

        D3D12_RESOURCE_BARRIER toPresent{};
        toPresent.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
        toPresent.Transition.pResource = m_renderTargets[m_frameIndex].Get();
        toPresent.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
        toPresent.Transition.StateBefore = D3D12_RESOURCE_STATE_RENDER_TARGET;
        toPresent.Transition.StateAfter = D3D12_RESOURCE_STATE_PRESENT;
        m_commandList->ResourceBarrier(1, &toPresent);

        HR(m_commandList->Close(), "CmdList->Close");
        ID3D12CommandList* lists[] = { m_commandList.Get() };
        m_commandQueue->ExecuteCommandLists(1, lists);
        HR(m_swapChain->Present(1, 0), "SwapChain->Present");

        MoveToNextFrame();
    }

    void DrawSidebar() {
        ImVec2 display = ImGui::GetIO().DisplaySize;
        float sidebarWidth = std::min(m_sidebarWidth, display.x * 0.8f);
        ImGui::SetNextWindowPos(ImVec2(0, 0), ImGuiCond_Always);
        ImGui::SetNextWindowSize(ImVec2(sidebarWidth, display.y), ImGuiCond_Always);

        ImGuiWindowFlags flags = ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize |
            ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoBringToFrontOnFocus |
            ImGuiWindowFlags_NoSavedSettings;
        if (ImGui::Begin("Controls", nullptr, flags)) {
            ImGui::SliderFloat("Sidebar width", &m_sidebarWidth, 200.0f, 600.0f, "%.0f px");
            ImGui::BeginChild("ControlsScroll", ImVec2(0, 0), false, ImGuiWindowFlags_AlwaysVerticalScrollbar);

            if (ImGui::CollapsingHeader("Scene", ImGuiTreeNodeFlags_DefaultOpen)) {
                const char* items[] = { "Triangle", "Cube", "Whirligig" };
                int idx = (m_sceneKind == SceneKind::Triangle) ? 0 : (m_sceneKind == SceneKind::Cube ? 1 : 2);
                if (ImGui::Combo("Type", &idx, items, IM_ARRAYSIZE(items))) {
                    m_sceneKind = (idx == 0) ? SceneKind::Triangle : (idx == 1 ? SceneKind::Cube : SceneKind::Whirligig);
                }
                ImGui::Text("Resolution: %u x %u", m_width, m_height);
            }

            // Quick access presets when Whirligig is selected (always visible near top)
            if (m_sceneKind == SceneKind::Whirligig) {
                ImGui::SeparatorText("Whirligig Presets (Quick)");
                int idx = m_whirl.presetIndex;
                ImGui::SetNextItemWidth(-1.0f);
                if (ImGui::Combo("##WhirlQuickPreset", &idx, m_whirl.presetNames, IM_ARRAYSIZE(m_whirl.presetNames))) {
                    m_whirl.presetIndex = idx;
                    m_whirl.ApplyPreset(m_whirl.presetIndex);
                }
            }

            m_cam.DrawUiInline();
            m_grid.DrawUiInline(m_device.Get());

            switch (m_sceneKind) {
            case SceneKind::Triangle:  m_triangle.DrawUiInline(); break;
            case SceneKind::Cube:      m_cube.DrawUiInline();     break;
            case SceneKind::Whirligig: m_whirl.DrawUiInline();    break;
            }

            ImGui::EndChild();
        }
        ImGui::End();
    }

    void MoveToNextFrame() {
        const UINT64 fence = m_fenceValues[m_frameIndex];
        HR(m_commandQueue->Signal(m_fence.Get(), fence), "Queue->Signal");
        m_frameIndex = m_swapChain->GetCurrentBackBufferIndex();
        if (m_fence->GetCompletedValue() < m_fenceValues[m_frameIndex]) {
            HR(m_fence->SetEventOnCompletion(m_fenceValues[m_frameIndex], m_fenceEvent), "Fence->SetEventOnCompletion");
            WaitForSingleObject(m_fenceEvent, INFINITE);
        }
        m_fenceValues[m_frameIndex] = fence + 1;
    }

    void WaitForGPU() {
        HR(m_commandQueue->Signal(m_fence.Get(), m_fenceValues[m_frameIndex]), "Queue->Signal(wait)");
        HR(m_fence->SetEventOnCompletion(m_fenceValues[m_frameIndex], m_fenceEvent), "Fence->SetEventOnCompletion(wait)");
        WaitForSingleObject(m_fenceEvent, INFINITE);
        m_fenceValues[m_frameIndex]++;
    }

    void OnResize(UINT newW, UINT newH) {
        if (newW == m_width && newH == m_height) return;
        WaitForGPU();
        for (UINT i = 0; i < FrameCount; ++i) {
            m_renderTargets[i].Reset();
            m_fenceValues[i] = m_fenceValues[m_frameIndex];
        }
        DXGI_SWAP_CHAIN_DESC desc{};
        HR(m_swapChain->GetDesc(&desc), "SwapChain->GetDesc");
        HR(m_swapChain->ResizeBuffers(FrameCount, newW, newH, desc.BufferDesc.Format, desc.Flags),
            "SwapChain->ResizeBuffers");
        m_frameIndex = m_swapChain->GetCurrentBackBufferIndex();
        CreateRTVs();
        CreateDepth(newW, newH);
        m_width = newW; m_height = newH;
        m_viewport.Width = float(m_width); m_viewport.Height = float(m_height);
        m_scissor.right = (LONG)m_width;   m_scissor.bottom = (LONG)m_height;
        m_cam.SetViewport(m_width, m_height);
    }

    void Cleanup() {
        if (m_commandQueue && m_fence) WaitForGPU();
        ImGui_ImplDX12_Shutdown();
        ImGui_ImplWin32_Shutdown();
        ImGui::DestroyContext();

        m_triangle.Cleanup();
        m_cube.Cleanup();
        m_whirl.Cleanup();
        m_grid.Cleanup();

        if (m_fenceEvent) CloseHandle(m_fenceEvent);
    }
};

// -------------------- Entry Points --------------------
int WINAPI wWinMain(HINSTANCE hInstance, HINSTANCE, PWSTR, int nCmdShow) {
    try {
        D3D12ScenesApp app(1280, 720);
        app.Run(hInstance, nCmdShow);
    }
    catch (const DxError& e) {
        MessageBoxA(nullptr, e.what(), "DirectX 12 Error", MB_ICONERROR | MB_OK);
        return -1;
    }
    catch (const std::exception& e) {
        MessageBoxA(nullptr, e.what(), "Error", MB_ICONERROR | MB_OK);
        return -1;
    }
    return 0;
}

// If your project is Console (/SUBSYSTEM:CONSOLE), keep this adapter.
int main() {
    return wWinMain(GetModuleHandle(nullptr), nullptr, GetCommandLineW(), SW_SHOWDEFAULT);
}
