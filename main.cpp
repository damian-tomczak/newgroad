// main.cpp — DX12 + ImGui with a tiny Scene System
// Scene 1: Triangle (no depth) ; Scene 2: Rotating Cube (with depth)
// Single file, no d3dx12.h.
// Build (MSVC):
//   cl /EHsc /std:c++17 main.cpp /DUNICODE /DWIN32 /DWIN32_LEAN_AND_MEAN d3d12.lib dxgi.lib d3dcompiler.lib
//
// You also need Dear ImGui sources in your project:
//   imgui.cpp, imgui_draw.cpp, imgui_tables.cpp, imgui_widgets.cpp, imgui_demo.cpp
//   backends/imgui_impl_win32.cpp, backends/imgui_impl_dx12.cpp
// Include paths: <your>/third_party/imgui and <your>/third_party/imgui/backends

#include <windows.h>
#include <wrl.h>
#include <d3d12.h>
#include <dxgi1_6.h>
#include <d3dcompiler.h>
#include <comdef.h>
#include <string>
#include <stdexcept>
#include <cstring>
#include <chrono>

#include "imgui.h"
#include "backends/imgui_impl_win32.h"
#include "backends/imgui_impl_dx12.h"

#pragma comment(lib, "d3d12.lib")
#pragma comment(lib, "dxgi.lib")
#pragma comment(lib, "d3dcompiler.lib")

using Microsoft::WRL::ComPtr;
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

// -------------------- Scene Interfaces --------------------
struct IScene {
    virtual ~IScene() = default;
    virtual void Init(ID3D12Device* device) = 0;
    virtual void Render(ID3D12GraphicsCommandList* cl) = 0;
    virtual void OnResize(UINT /*w*/, UINT /*h*/) {}
    virtual void Cleanup() = 0;
};

// Triangle Scene (no depth)
struct TriangleScene : IScene {
    ComPtr<ID3D12PipelineState>    pso;
    ComPtr<ID3D12RootSignature>    rootSig;
    ComPtr<ID3D12Resource>         vb;
    D3D12_VERTEX_BUFFER_VIEW       vbv{};

    void Init(ID3D12Device* device) override {
        // Root signature (empty)
        D3D12_ROOT_SIGNATURE_DESC rs{};
        rs.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;
        ComPtr<ID3DBlob> sig, err;
        HR(D3D12SerializeRootSignature(&rs, D3D_ROOT_SIGNATURE_VERSION_1, &sig, &err),
            "D3D12SerializeRootSignature",
            err ? std::string((char*)err->GetBufferPointer(), err->GetBufferSize()) : "");
        HR(device->CreateRootSignature(0, sig->GetBufferPointer(), sig->GetBufferSize(),
            IID_PPV_ARGS(&rootSig)), "CreateRootSignature(Triangle)");

        // Shaders
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

        // Input layout
        D3D12_INPUT_ELEMENT_DESC layout[] = {
            { "POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0,   D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0 },
            { "COLOR",    0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 12,  D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0 },
        };

        // PSO (depth disabled)
        D3D12_GRAPHICS_PIPELINE_STATE_DESC psoDesc{};
        psoDesc.pRootSignature = rootSig.Get();
        psoDesc.VS = { vs->GetBufferPointer(), vs->GetBufferSize() };
        psoDesc.PS = { ps->GetBufferPointer(), ps->GetBufferSize() };
        psoDesc.InputLayout = { layout, _countof(layout) };
        psoDesc.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
        psoDesc.NumRenderTargets = 1;
        psoDesc.RTVFormats[0] = DXGI_FORMAT_R8G8B8A8_UNORM;
        psoDesc.SampleDesc.Count = 1;
        psoDesc.SampleMask = UINT_MAX;                    // <-- IMPORTANT
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
        psoDesc.RasterizerState.FillMode = D3D12_FILL_MODE_SOLID;
        psoDesc.RasterizerState.CullMode = D3D12_CULL_MODE_NONE; // <-- safer for demo
        psoDesc.RasterizerState.DepthClipEnable = TRUE;
        // Depth off
        psoDesc.DepthStencilState.DepthEnable = FALSE;
        psoDesc.DepthStencilState.StencilEnable = FALSE;

        HR(device->CreateGraphicsPipelineState(&psoDesc, IID_PPV_ARGS(&pso)),
            "CreateGraphicsPipelineState(Triangle)");


        // Geometry (upload heap for simplicity)
        struct V { float p[3]; float c[3]; };
        V verts[] = {
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
        HR(vb->Map(0, &r, &p), "VB->Map"); std::memcpy(p, verts, size); vb->Unmap(0, nullptr);
        vbv.BufferLocation = vb->GetGPUVirtualAddress(); vbv.StrideInBytes = sizeof(V); vbv.SizeInBytes = size;
    }

    void Render(ID3D12GraphicsCommandList* cl) override {
        cl->SetPipelineState(pso.Get());
        cl->SetGraphicsRootSignature(rootSig.Get());
        cl->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
        cl->IASetVertexBuffers(0, 1, &vbv);
        cl->DrawInstanced(3, 1, 0, 0);
    }

    void Cleanup() override {
        vb.Reset(); pso.Reset(); rootSig.Reset();
    }
};

// Cube Scene (depth on, root constants: angle)
struct CubeScene : IScene {
    ComPtr<ID3D12PipelineState>    pso;
    ComPtr<ID3D12RootSignature>    rootSig;
    ComPtr<ID3D12Resource>         vb, ib;
    D3D12_VERTEX_BUFFER_VIEW       vbv{};
    D3D12_INDEX_BUFFER_VIEW        ibv{};
    float angle = 0.0f;

    void Init(ID3D12Device* device) override {
        // Root signature with 1x 32-bit constant (float Angle) at b0
        D3D12_ROOT_PARAMETER param{};
        param.ParameterType = D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS;
        param.Constants.Num32BitValues = 1;
        param.Constants.ShaderRegister = 0;
        param.Constants.RegisterSpace = 0;
        D3D12_ROOT_SIGNATURE_DESC rs{};
        rs.NumParameters = 1;
        rs.pParameters = &param;
        rs.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;
        ComPtr<ID3DBlob> sig, err;
        HR(D3D12SerializeRootSignature(&rs, D3D_ROOT_SIGNATURE_VERSION_1, &sig, &err),
            "D3D12SerializeRootSignature(Cube)",
            err ? std::string((char*)err->GetBufferPointer(), err->GetBufferSize()) : "");
        HR(device->CreateRootSignature(0, sig->GetBufferPointer(), sig->GetBufferSize(),
            IID_PPV_ARGS(&rootSig)), "CreateRootSignature(Cube)");

        // Shaders: do rotation in VS using Angle; simple 0..1 z-map, no perspective
        const char* vsSrc = R"(
cbuffer Params : register(b0) { float Angle; }
struct VSIn { float3 pos:POSITION; float3 col:COLOR; };
struct PSIn { float4 pos:SV_POSITION; float3 col:COLOR; };
PSIn VSMain(VSIn i){
    float s = sin(Angle), c = cos(Angle);
    float3 p = i.pos;
    // Rotate around Y
    float3 r = float3(c*p.x + s*p.z, p.y, -s*p.x + c*p.z);
    // Map z from [-1,1] to [0,1] for D3D depth
    float z = r.z * 0.5 + 0.5;
    PSIn o;
    o.pos = float4(r.x, r.y, z, 1.0);
    o.col = i.col;
    return o;
}
)";
        const char* psSrc = R"(
struct PSIn { float4 pos:SV_POSITION; float3 col:COLOR; };
float4 PSMain(PSIn i):SV_TARGET{ return float4(i.col,1); }
)";
        ComPtr<ID3DBlob> vs, ps, shErr;
        HR(D3DCompile(vsSrc, (UINT)std::strlen(vsSrc), nullptr, nullptr, nullptr,
            "VSMain", "vs_5_0", 0, 0, &vs, &shErr),
            "D3DCompile(VS cube)", shErr ? std::string((char*)shErr->GetBufferPointer(), shErr->GetBufferSize()) : "");
        HR(D3DCompile(psSrc, (UINT)std::strlen(psSrc), nullptr, nullptr, nullptr,
            "PSMain", "ps_5_0", 0, 0, &ps, &shErr),
            "D3DCompile(PS cube)", shErr ? std::string((char*)shErr->GetBufferPointer(), shErr->GetBufferSize()) : "");

        // Input layout
        D3D12_INPUT_ELEMENT_DESC layout[] = {
            { "POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0,   D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0 },
            { "COLOR",    0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 12,  D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0 },
        };

        // PSO (depth enabled)
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
        psoDesc.SampleMask = UINT_MAX;                    // <-- IMPORTANT
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
        psoDesc.RasterizerState.FillMode = D3D12_FILL_MODE_SOLID;
        psoDesc.RasterizerState.CullMode = D3D12_CULL_MODE_NONE; // <-- safer for demo
        psoDesc.RasterizerState.DepthClipEnable = TRUE;
        // Depth
        psoDesc.DepthStencilState.DepthEnable = TRUE;
        psoDesc.DepthStencilState.DepthWriteMask = D3D12_DEPTH_WRITE_MASK_ALL;
        psoDesc.DepthStencilState.DepthFunc = D3D12_COMPARISON_FUNC_LESS_EQUAL;
        psoDesc.DepthStencilState.StencilEnable = FALSE;

        HR(device->CreateGraphicsPipelineState(&psoDesc, IID_PPV_ARGS(&pso)),
            "CreateGraphicsPipelineState(Cube)");


        // Geometry: a unit cube centered at origin
        struct V { float p[3]; float c[3]; };
        const float s = 0.5f;
        V verts[] = {
            // Front (z=+s)
            {{-s,-s, s},{1,0,0}}, {{ s,-s, s},{0,1,0}}, {{ s, s, s},{0,0,1}}, {{-s, s, s},{1,1,0}},
            // Back (z=-s)
            {{-s,-s,-s},{1,0,1}}, {{ s,-s,-s},{0,1,1}}, {{ s, s,-s},{1,1,1}}, {{-s, s,-s},{0.2f,0.6f,1}},
        };
        uint16_t idx[] = {
            // Front
            0,1,2, 0,2,3,
            // Right
            1,5,6, 1,6,2,
            // Back
            5,4,7, 5,7,6,
            // Left
            4,0,3, 4,3,7,
            // Top
            3,2,6, 3,6,7,
            // Bottom
            4,5,1, 4,1,0
        };

        // VB
        UINT vbSize = sizeof(verts);
        D3D12_HEAP_PROPERTIES heap{}; heap.Type = D3D12_HEAP_TYPE_UPLOAD;
        D3D12_RESOURCE_DESC vbDesc{}; vbDesc.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER; vbDesc.Width = vbSize; vbDesc.Height = 1; vbDesc.DepthOrArraySize = 1; vbDesc.MipLevels = 1; vbDesc.SampleDesc.Count = 1; vbDesc.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;
        HR(device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &vbDesc, D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&vb)),
            "CreateCommittedResource(CubeVB)");
        void* p = nullptr; D3D12_RANGE r{ 0,0 };
        HR(vb->Map(0, &r, &p), "CubeVB->Map"); std::memcpy(p, verts, vbSize); vb->Unmap(0, nullptr);
        vbv.BufferLocation = vb->GetGPUVirtualAddress(); vbv.StrideInBytes = sizeof(V); vbv.SizeInBytes = vbSize;

        // IB
        UINT ibSize = sizeof(idx);
        D3D12_RESOURCE_DESC ibDesc{}; ibDesc.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER; ibDesc.Width = ibSize; ibDesc.Height = 1; ibDesc.DepthOrArraySize = 1; ibDesc.MipLevels = 1; ibDesc.SampleDesc.Count = 1; ibDesc.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;
        HR(device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &ibDesc, D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&ib)),
            "CreateCommittedResource(CubeIB)");
        HR(ib->Map(0, &r, &p), "CubeIB->Map"); std::memcpy(p, idx, ibSize); ib->Unmap(0, nullptr);
        ibv.BufferLocation = ib->GetGPUVirtualAddress(); ibv.Format = DXGI_FORMAT_R16_UINT; ibv.SizeInBytes = ibSize;
    }

    void Render(ID3D12GraphicsCommandList* cl) override {
        // Update angle over time (small step per frame; the app can also set this)
        angle += 0.01f;
        cl->SetPipelineState(pso.Get());
        cl->SetGraphicsRootSignature(rootSig.Get());
        cl->SetGraphicsRoot32BitConstants(0, 1, &angle, 0);
        cl->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
        cl->IASetVertexBuffers(0, 1, &vbv);
        cl->IASetIndexBuffer(&ibv);
        cl->DrawIndexedInstanced(36, 1, 0, 0, 0);
    }

    void Cleanup() override {
        vb.Reset(); ib.Reset(); pso.Reset(); rootSig.Reset();
    }
};

// -------------------- Application --------------------
class D3D12ScenesApp {
public:
    D3D12ScenesApp(UINT w, UINT h) : m_width(w), m_height(h) {}
    void Run(HINSTANCE hInstance, int nCmdShow) {
        RegisterWindowClass(hInstance);
        CreateAppWindow(hInstance, nCmdShow);
        InitD3D();
        InitImGui();
        InitScenes();
        MainLoop();
        Cleanup();
    }

private:
    // Win32
    HWND m_hwnd = nullptr;
    UINT m_width = 1280, m_height = 720;

    // D3D12 core
    ComPtr<IDXGIFactory6>          m_factory;
    ComPtr<ID3D12Device>           m_device;
    ComPtr<ID3D12CommandQueue>     m_commandQueue;
    ComPtr<IDXGISwapChain3>        m_swapChain;
    ComPtr<ID3D12DescriptorHeap>   m_rtvHeap;
    ComPtr<ID3D12Resource>         m_renderTargets[FrameCount];
    UINT                           m_rtvDescriptorSize = 0;
    UINT                           m_frameIndex = 0;

    // Depth
    ComPtr<ID3D12DescriptorHeap>   m_dsvHeap;
    ComPtr<ID3D12Resource>         m_depth;
    D3D12_CPU_DESCRIPTOR_HANDLE    m_dsv{};

    // Cmd
    ComPtr<ID3D12CommandAllocator> m_commandAllocators[FrameCount];
    ComPtr<ID3D12GraphicsCommandList> m_commandList;

    // Sync
    ComPtr<ID3D12Fence>            m_fence;
    UINT64                         m_fenceValues[FrameCount] = {};
    HANDLE                         m_fenceEvent = nullptr;

    // View
    D3D12_VIEWPORT                 m_viewport{};
    D3D12_RECT                     m_scissor{};

    // ImGui
    ComPtr<ID3D12DescriptorHeap>   m_imguiSrvHeap;

    // Scenes
    enum class SceneKind { Triangle = 0, Cube = 1 };
    SceneKind                      m_sceneKind = SceneKind::Triangle;
    TriangleScene                  m_triangle;
    CubeScene                      m_cube;

    // Timing for UI info
    std::chrono::steady_clock::time_point m_prev = std::chrono::steady_clock::now();

    // --- Win32 ---
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
        if (ImGui_ImplWin32_WndProcHandler(hWnd, msg, wParam, lParam))
            return 1;
        switch (msg) {
        case WM_SIZE:
            if (m_swapChain && wParam != SIZE_MINIMIZED) {
                UINT w = LOWORD(lParam), h = HIWORD(lParam);
                if (!w) w = 1; if (!h) h = 1;
                OnResize(w, h);
            }
            return 0;
        case WM_DESTROY:
            PostQuitMessage(0);
            return 0;
        default:
            return DefWindowProc(hWnd, msg, wParam, lParam);
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
        m_hwnd = CreateWindowEx(0, L"DX12ScenesWinClass", L"DX12 Scenes: Triangle & Cube (ImGui)",
            WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT,
            rc.right - rc.left, rc.bottom - rc.top,
            nullptr, nullptr, hInstance, this);
        ShowWindow(m_hwnd, nCmdShow);
        UpdateWindow(m_hwnd);
    }

    // --- D3D ---
    void InitD3D() {
        UINT factoryFlags = 0;
#if defined(_DEBUG)
        if (ComPtr<ID3D12Debug> dbg; SUCCEEDED(D3D12GetDebugInterface(IID_PPV_ARGS(&dbg)))) {
            dbg->EnableDebugLayer();
            factoryFlags |= DXGI_CREATE_FACTORY_DEBUG;
        }
#endif
        HR(CreateDXGIFactory2(factoryFlags, IID_PPV_ARGS(&m_factory)), "CreateDXGIFactory2");

        // Device (HW then WARP)
        HRESULT hr = D3D12CreateDevice(nullptr, D3D_FEATURE_LEVEL_11_0, IID_PPV_ARGS(&m_device));
        if (FAILED(hr)) {
            ComPtr<IDXGIAdapter> warp;
            HR(m_factory->EnumWarpAdapter(IID_PPV_ARGS(&warp)), "EnumWarpAdapter");
            HR(D3D12CreateDevice(warp.Get(), D3D_FEATURE_LEVEL_11_0, IID_PPV_ARGS(&m_device)),
                "D3D12CreateDevice(WARP)");
        }

        // Queue
        D3D12_COMMAND_QUEUE_DESC q{}; q.Type = D3D12_COMMAND_LIST_TYPE_DIRECT;
        HR(m_device->CreateCommandQueue(&q, IID_PPV_ARGS(&m_commandQueue)), "CreateCommandQueue");

        // Swapchain
        DXGI_SWAP_CHAIN_DESC1 sc{};
        sc.BufferCount = FrameCount;
        sc.Width = m_width; sc.Height = m_height;
        sc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
        sc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
        sc.SwapEffect = DXGI_SWAP_EFFECT_FLIP_DISCARD;
        sc.SampleDesc.Count = 1;
        ComPtr<IDXGISwapChain1> sc1;
        HR(m_factory->CreateSwapChainForHwnd(m_commandQueue.Get(), m_hwnd, &sc, nullptr, nullptr, &sc1),
            "CreateSwapChainForHwnd");
        HR(sc1.As(&m_swapChain), "SwapChain1->As<IDXGISwapChain3>");
        m_frameIndex = m_swapChain->GetCurrentBackBufferIndex();

        // RTV heap + RTVs
        D3D12_DESCRIPTOR_HEAP_DESC rtvDesc{}; rtvDesc.NumDescriptors = FrameCount; rtvDesc.Type = D3D12_DESCRIPTOR_HEAP_TYPE_RTV;
        HR(m_device->CreateDescriptorHeap(&rtvDesc, IID_PPV_ARGS(&m_rtvHeap)), "CreateDescriptorHeap(RTV)");
        m_rtvDescriptorSize = m_device->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_RTV);
        CreateRTVs();

        // DSV heap + depth
        D3D12_DESCRIPTOR_HEAP_DESC dsvDesc{}; dsvDesc.NumDescriptors = 1; dsvDesc.Type = D3D12_DESCRIPTOR_HEAP_TYPE_DSV;
        HR(m_device->CreateDescriptorHeap(&dsvDesc, IID_PPV_ARGS(&m_dsvHeap)), "CreateDescriptorHeap(DSV)");
        m_dsv = m_dsvHeap->GetCPUDescriptorHandleForHeapStart();
        CreateDepth(m_width, m_height);

        // Allocators & command list
        for (UINT i = 0; i < FrameCount; ++i)
            HR(m_device->CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT, IID_PPV_ARGS(&m_commandAllocators[i])),
                "CreateCommandAllocator");
        HR(m_device->CreateCommandList(0, D3D12_COMMAND_LIST_TYPE_DIRECT,
            m_commandAllocators[m_frameIndex].Get(), nullptr, IID_PPV_ARGS(&m_commandList)),
            "CreateCommandList");
        HR(m_commandList->Close(), "Close(CommandList)");

        // Fence
        HR(m_device->CreateFence(0, D3D12_FENCE_FLAG_NONE, IID_PPV_ARGS(&m_fence)), "CreateFence");
        m_fenceValues[m_frameIndex] = 1;
        m_fenceEvent = CreateEvent(nullptr, FALSE, FALSE, nullptr);
        if (!m_fenceEvent) HR(HRESULT_FROM_WIN32(GetLastError()), "CreateEvent");

        // Viewport & scissor
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
        // Release old
        m_depth.Reset();

        D3D12_RESOURCE_DESC tex{};
        tex.Dimension = D3D12_RESOURCE_DIMENSION_TEXTURE2D;
        tex.Width = w; tex.Height = h;
        tex.DepthOrArraySize = 1; tex.MipLevels = 1;
        tex.SampleDesc.Count = 1;
        tex.Format = DXGI_FORMAT_D32_FLOAT;
        tex.Flags = D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL;

        D3D12_CLEAR_VALUE clear{}; clear.Format = DXGI_FORMAT_D32_FLOAT; clear.DepthStencil.Depth = 1.0f; clear.DepthStencil.Stencil = 0;

        D3D12_HEAP_PROPERTIES heap{}; heap.Type = D3D12_HEAP_TYPE_DEFAULT;
        HR(m_device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &tex,
            D3D12_RESOURCE_STATE_DEPTH_WRITE, &clear, IID_PPV_ARGS(&m_depth)),
            "CreateCommittedResource(Depth)");

        D3D12_DEPTH_STENCIL_VIEW_DESC dsv{};
        dsv.Format = DXGI_FORMAT_D32_FLOAT;
        dsv.ViewDimension = D3D12_DSV_DIMENSION_TEXTURE2D;
        m_device->CreateDepthStencilView(m_depth.Get(), &dsv, m_dsv);
    }

    // --- ImGui ---
    void InitImGui() {
        IMGUI_CHECKVERSION();
        ImGui::CreateContext();
        ImGui::StyleColorsDark();
        // SRV heap for ImGui font texture
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
    }

    // --- Loop ---
    void MainLoop() {
        MSG msg{};
        while (msg.message != WM_QUIT) {
            if (PeekMessage(&msg, nullptr, 0, 0, PM_REMOVE)) {
                TranslateMessage(&msg);
                DispatchMessage(&msg);
            }
            else {
                RenderFrame();
            }
        }
        WaitForGPU();
    }

    void RenderFrame() {
        // Reset command list
        HR(m_commandAllocators[m_frameIndex]->Reset(), "CmdAlloc->Reset");
        HR(m_commandList->Reset(m_commandAllocators[m_frameIndex].Get(), nullptr), "CmdList->Reset");

        // Transition: Present -> RT
        D3D12_RESOURCE_BARRIER toRT{};
        toRT.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
        toRT.Transition.pResource = m_renderTargets[m_frameIndex].Get();
        toRT.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
        toRT.Transition.StateBefore = D3D12_RESOURCE_STATE_PRESENT;
        toRT.Transition.StateAfter = D3D12_RESOURCE_STATE_RENDER_TARGET;
        m_commandList->ResourceBarrier(1, &toRT);

        // Bind viewport, scissor, RT+DS
        m_commandList->RSSetViewports(1, &m_viewport);
        m_commandList->RSSetScissorRects(1, &m_scissor);

        D3D12_CPU_DESCRIPTOR_HANDLE rtv = m_rtvHeap->GetCPUDescriptorHandleForHeapStart();
        rtv.ptr += m_frameIndex * m_rtvDescriptorSize;
        m_commandList->OMSetRenderTargets(1, &rtv, FALSE, &m_dsv);

        // Clear
        const float clear[4] = { 0.06f, 0.07f, 0.10f, 1.0f };
        m_commandList->ClearRenderTargetView(rtv, clear, 0, nullptr);
        m_commandList->ClearDepthStencilView(m_dsv, D3D12_CLEAR_FLAG_DEPTH, 1.0f, 0, 0, nullptr);

        // Draw current scene
        if (m_sceneKind == SceneKind::Triangle) {
            m_triangle.Render(m_commandList.Get());
        }
        else {
            m_cube.Render(m_commandList.Get());
        }

        // --- ImGui ---
        ImGui_ImplDX12_NewFrame();
        ImGui_ImplWin32_NewFrame();
        ImGui::NewFrame();
        DrawUI();
        ImGui::Render();
        ID3D12DescriptorHeap* heaps[] = { m_imguiSrvHeap.Get() };
        m_commandList->SetDescriptorHeaps(1, heaps);
        ImGui_ImplDX12_RenderDrawData(ImGui::GetDrawData(), m_commandList.Get());

        // Transition: RT -> Present
        D3D12_RESOURCE_BARRIER toPresent{};
        toPresent.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
        toPresent.Transition.pResource = m_renderTargets[m_frameIndex].Get();
        toPresent.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
        toPresent.Transition.StateBefore = D3D12_RESOURCE_STATE_RENDER_TARGET;
        toPresent.Transition.StateAfter = D3D12_RESOURCE_STATE_PRESENT;
        m_commandList->ResourceBarrier(1, &toPresent);

        // Execute + present
        HR(m_commandList->Close(), "CmdList->Close");
        ID3D12CommandList* lists[] = { m_commandList.Get() };
        m_commandQueue->ExecuteCommandLists(1, lists);
        HR(m_swapChain->Present(1, 0), "SwapChain->Present");

        MoveToNextFrame();
    }

    void DrawUI() {
        using namespace std::chrono;
        auto now = steady_clock::now();
        float dt = duration_cast<duration<float>>(now - m_prev).count();
        m_prev = now;

        ImGui::Begin("Scenes");
        const char* items[] = { "Triangle", "Cube" };
        int idx = (m_sceneKind == SceneKind::Triangle) ? 0 : 1;
        if (ImGui::Combo("Scene", &idx, items, IM_ARRAYSIZE(items))) {
            m_sceneKind = (idx == 0) ? SceneKind::Triangle : SceneKind::Cube;
        }
        ImGui::Text("Frame %.3f ms (%.1f FPS)", dt * 1000.0f, dt > 0 ? (1.0f / dt) : 0.0f);
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

        // depth
        CreateDepth(newW, newH);

        m_width = newW; m_height = newH;
        m_viewport.Width = float(m_width);
        m_viewport.Height = float(m_height);
        m_scissor.right = (LONG)m_width;
        m_scissor.bottom = (LONG)m_height;

        // Scenes could react if needed
        m_triangle.OnResize(newW, newH);
        m_cube.OnResize(newW, newH);
    }

    void Cleanup() {
        if (m_commandQueue && m_fence) WaitForGPU();
        ImGui_ImplDX12_Shutdown();
        ImGui_ImplWin32_Shutdown();
        ImGui::DestroyContext();

        m_triangle.Cleanup();
        m_cube.Cleanup();

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
