// main.cpp — Minimal DirectX 12 triangle (single file, no d3dx12.h)
// Build (MSVC):
//   cl /EHsc /std:c++17 main.cpp /DUNICODE /DWIN32 /DWIN32_LEAN_AND_MEAN d3d12.lib dxgi.lib d3dcompiler.lib

#include <windows.h>
#include <wrl.h>
#include <d3d12.h>
#include <dxgi1_6.h>
#include <d3dcompiler.h>
#include <comdef.h>   // _com_error for readable HRESULT messages
#include <string>
#include <stdexcept>
#include <cstring>

#pragma comment(lib, "d3d12.lib")
#pragma comment(lib, "dxgi.lib")
#pragma comment(lib, "d3dcompiler.lib")

using Microsoft::WRL::ComPtr;

static const UINT FrameCount = 2;

// --- Helpers -----------------------------------------------------------------
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

// --- App ---------------------------------------------------------------------
class D3D12TriangleApp {
public:
    D3D12TriangleApp(UINT w, UINT h) : m_width(w), m_height(h) {}

    void Run(HINSTANCE hInstance, int nCmdShow) {
        RegisterWindowClass(hInstance);
        CreateAppWindow(hInstance, nCmdShow);
        InitD3D();
        MainLoop();
        Cleanup();
    }

private:
    // Win32
    HWND m_hwnd = nullptr;
    UINT m_width = 800, m_height = 600;

    // D3D12 core
    ComPtr<IDXGIFactory6>          m_factory;
    ComPtr<ID3D12Device>           m_device;
    ComPtr<ID3D12CommandQueue>     m_commandQueue;
    ComPtr<IDXGISwapChain3>        m_swapChain;
    ComPtr<ID3D12DescriptorHeap>   m_rtvHeap;
    ComPtr<ID3D12Resource>         m_renderTargets[FrameCount];
    ComPtr<ID3D12CommandAllocator> m_commandAllocators[FrameCount];
    ComPtr<ID3D12GraphicsCommandList> m_commandList;
    ComPtr<ID3D12PipelineState>    m_pipelineState;
    ComPtr<ID3D12RootSignature>    m_rootSignature;

    // Sync
    ComPtr<ID3D12Fence>            m_fence;
    UINT64                         m_fenceValues[FrameCount] = {};
    HANDLE                         m_fenceEvent = nullptr;
    UINT                           m_rtvDescriptorSize = 0;
    UINT                           m_frameIndex = 0;

    // Geometry
    ComPtr<ID3D12Resource>         m_vertexBuffer;
    D3D12_VERTEX_BUFFER_VIEW       m_vbView{};

    // View
    D3D12_VIEWPORT                 m_viewport{};
    D3D12_RECT                     m_scissor{};

    // --- Win32 boilerplate ---
    static LRESULT CALLBACK WndProcSetup(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {
        if (msg == WM_NCCREATE) {
            auto cs = reinterpret_cast<CREATESTRUCT*>(lParam);
            auto app = reinterpret_cast<D3D12TriangleApp*>(cs->lpCreateParams);
            SetWindowLongPtr(hWnd, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(app));
            SetWindowLongPtr(hWnd, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(WndProcThunk));
            return app->WndProc(hWnd, msg, wParam, lParam);
        }
        return DefWindowProc(hWnd, msg, wParam, lParam);
    }
    static LRESULT CALLBACK WndProcThunk(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {
        auto app = reinterpret_cast<D3D12TriangleApp*>(GetWindowLongPtr(hWnd, GWLP_USERDATA));
        return app->WndProc(hWnd, msg, wParam, lParam);
    }
    LRESULT WndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {
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
        wc.lpszClassName = L"D3D12TriangleWindowClass";
        wc.hCursor = LoadCursor(NULL, IDC_ARROW);
        wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
        RegisterClass(&wc);
    }
    void CreateAppWindow(HINSTANCE hInstance, int nCmdShow) {
        RECT rc{ 0,0,(LONG)m_width,(LONG)m_height };
        AdjustWindowRect(&rc, WS_OVERLAPPEDWINDOW, FALSE);
        m_hwnd = CreateWindowEx(
            0, L"D3D12TriangleWindowClass", L"DirectX 12 Triangle",
            WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT,
            rc.right - rc.left, rc.bottom - rc.top,
            nullptr, nullptr, hInstance, this);
        ShowWindow(m_hwnd, nCmdShow);
        UpdateWindow(m_hwnd);
    }

    // --- D3D init ---
    void InitD3D() {
        UINT factoryFlags = 0;
#if defined(_DEBUG)
        if (ComPtr<ID3D12Debug> dbg; SUCCEEDED(D3D12GetDebugInterface(IID_PPV_ARGS(&dbg)))) {
            dbg->EnableDebugLayer();
            factoryFlags |= DXGI_CREATE_FACTORY_DEBUG;
        }
#endif
        HR(CreateDXGIFactory2(factoryFlags, IID_PPV_ARGS(&m_factory)), "CreateDXGIFactory2");

        // Try hardware device first, fallback to WARP if necessary.
        HRESULT hr = D3D12CreateDevice(nullptr, D3D_FEATURE_LEVEL_11_0, IID_PPV_ARGS(&m_device));
        if (FAILED(hr)) {
            ComPtr<IDXGIAdapter> warp;
            HR(m_factory->EnumWarpAdapter(IID_PPV_ARGS(&warp)), "EnumWarpAdapter");
            HR(D3D12CreateDevice(warp.Get(), D3D_FEATURE_LEVEL_11_0, IID_PPV_ARGS(&m_device)),
                "D3D12CreateDevice(WARP)");
        }

        // Command queue
        {
            D3D12_COMMAND_QUEUE_DESC desc{};
            desc.Type = D3D12_COMMAND_LIST_TYPE_DIRECT;
            HR(m_device->CreateCommandQueue(&desc, IID_PPV_ARGS(&m_commandQueue)), "CreateCommandQueue");
        }

        // Swap chain
        {
            DXGI_SWAP_CHAIN_DESC1 sc{};
            sc.BufferCount = FrameCount;
            sc.Width = m_width;
            sc.Height = m_height;
            sc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
            sc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
            sc.SwapEffect = DXGI_SWAP_EFFECT_FLIP_DISCARD;
            sc.SampleDesc.Count = 1;

            ComPtr<IDXGISwapChain1> sc1;
            HR(m_factory->CreateSwapChainForHwnd(
                m_commandQueue.Get(), m_hwnd, &sc, nullptr, nullptr, &sc1), "CreateSwapChainForHwnd");
            HR(sc1.As(&m_swapChain), "SwapChain1->As<IDXGISwapChain3>");
            m_frameIndex = m_swapChain->GetCurrentBackBufferIndex();
        }

        // RTV heap
        {
            D3D12_DESCRIPTOR_HEAP_DESC heap{};
            heap.NumDescriptors = FrameCount;
            heap.Type = D3D12_DESCRIPTOR_HEAP_TYPE_RTV;
            HR(m_device->CreateDescriptorHeap(&heap, IID_PPV_ARGS(&m_rtvHeap)), "CreateDescriptorHeap(RTV)");
            m_rtvDescriptorSize = m_device->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_RTV);
        }

        CreateRTVs();

        // Command allocators
        for (UINT i = 0; i < FrameCount; ++i)
            HR(m_device->CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT, IID_PPV_ARGS(&m_commandAllocators[i])),
                "CreateCommandAllocator");

        CreatePipeline(); // root sig + PSO
        CreateTriangleVB();

        // Command list (start closed)
        HR(m_device->CreateCommandList(0, D3D12_COMMAND_LIST_TYPE_DIRECT,
            m_commandAllocators[m_frameIndex].Get(),
            m_pipelineState.Get(), IID_PPV_ARGS(&m_commandList)),
            "CreateCommandList");
        HR(m_commandList->Close(), "Close(CommandList)");

        // Fence
        HR(m_device->CreateFence(0, D3D12_FENCE_FLAG_NONE, IID_PPV_ARGS(&m_fence)), "CreateFence");
        m_fenceValues[m_frameIndex] = 1;
        m_fenceEvent = CreateEvent(nullptr, FALSE, FALSE, nullptr);
        if (!m_fenceEvent) HR(HRESULT_FROM_WIN32(GetLastError()), "CreateEvent");

        // Viewport/scissor
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

    void CreatePipeline() {
        // Root signature (empty)
        D3D12_ROOT_SIGNATURE_DESC rs{};
        rs.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;
        ComPtr<ID3DBlob> sig, rsErr;
        HR(D3D12SerializeRootSignature(&rs, D3D_ROOT_SIGNATURE_VERSION_1, &sig, &rsErr),
            "D3D12SerializeRootSignature",
            rsErr ? std::string((char*)rsErr->GetBufferPointer(), rsErr->GetBufferSize()) : "");
        HR(m_device->CreateRootSignature(0, sig->GetBufferPointer(), sig->GetBufferSize(),
            IID_PPV_ARGS(&m_rootSignature)), "CreateRootSignature");

        // Shaders
        const char* src = R"(
struct VSInput { float3 pos:POSITION; float3 col:COLOR; };
struct PSInput { float4 pos:SV_POSITION; float3 col:COLOR; };
PSInput VSMain(VSInput i){ PSInput o; o.pos=float4(i.pos,1); o.col=i.col; return o; }
float4 PSMain(PSInput i):SV_TARGET{ return float4(i.col,1); }
)";
        ComPtr<ID3DBlob> vs, ps, err;

        HRESULT hr = D3DCompile(src, (UINT)std::strlen(src), nullptr, nullptr, nullptr,
            "VSMain", "vs_5_0", 0, 0, &vs, &err);
        if (FAILED(hr))
            throw DxError("D3DCompile(VS)", hr, err ? std::string((char*)err->GetBufferPointer(), err->GetBufferSize()) : "");
        hr = D3DCompile(src, (UINT)std::strlen(src), nullptr, nullptr, nullptr,
            "PSMain", "ps_5_0", 0, 0, &ps, &err);
        if (FAILED(hr))
            throw DxError("D3DCompile(PS)", hr, err ? std::string((char*)err->GetBufferPointer(), err->GetBufferSize()) : "");

        // Input layout
        D3D12_INPUT_ELEMENT_DESC layout[] = {
            { "POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0,   D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0 },
            { "COLOR",    0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 12,  D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0 },
        };

        // PSO
        D3D12_GRAPHICS_PIPELINE_STATE_DESC pso{};
        pso.pRootSignature = m_rootSignature.Get();
        pso.VS = { vs->GetBufferPointer(), vs->GetBufferSize() };
        pso.PS = { ps->GetBufferPointer(), ps->GetBufferSize() };
        // Blend (default)
        pso.BlendState.AlphaToCoverageEnable = FALSE;
        pso.BlendState.IndependentBlendEnable = FALSE;
        const D3D12_RENDER_TARGET_BLEND_DESC rt = {
            FALSE,FALSE,
            D3D12_BLEND_ONE, D3D12_BLEND_ZERO, D3D12_BLEND_OP_ADD,
            D3D12_BLEND_ONE, D3D12_BLEND_ZERO, D3D12_BLEND_OP_ADD,
            D3D12_LOGIC_OP_NOOP,
            D3D12_COLOR_WRITE_ENABLE_ALL
        };
        for (int i = 0; i < 8; ++i) pso.BlendState.RenderTarget[i] = rt;
        pso.SampleMask = UINT_MAX;
        // Rasterizer
        pso.RasterizerState.FillMode = D3D12_FILL_MODE_SOLID;
        pso.RasterizerState.CullMode = D3D12_CULL_MODE_BACK;
        pso.RasterizerState.FrontCounterClockwise = FALSE;
        pso.RasterizerState.DepthBias = D3D12_DEFAULT_DEPTH_BIAS;
        pso.RasterizerState.DepthBiasClamp = D3D12_DEFAULT_DEPTH_BIAS_CLAMP;
        pso.RasterizerState.SlopeScaledDepthBias = D3D12_DEFAULT_SLOPE_SCALED_DEPTH_BIAS;
        pso.RasterizerState.DepthClipEnable = TRUE;
        pso.RasterizerState.MultisampleEnable = FALSE;
        pso.RasterizerState.AntialiasedLineEnable = FALSE;
        // Depth-stencil off
        pso.DepthStencilState.DepthEnable = FALSE;
        pso.DepthStencilState.StencilEnable = FALSE;
        // IA/RT
        pso.InputLayout = { layout, _countof(layout) };
        pso.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
        pso.NumRenderTargets = 1;
        pso.RTVFormats[0] = DXGI_FORMAT_R8G8B8A8_UNORM;
        pso.SampleDesc.Count = 1;

        HR(m_device->CreateGraphicsPipelineState(&pso, IID_PPV_ARGS(&m_pipelineState)), "CreateGraphicsPipelineState");
    }

    void CreateTriangleVB() {
        struct Vertex { float pos[3]; float col[3]; };
        Vertex vertices[] = {
            { {  0.0f,  0.5f, 0.0f }, { 1.0f, 0.0f, 0.0f } },
            { {  0.5f, -0.5f, 0.0f }, { 0.0f, 1.0f, 0.0f } },
            { { -0.5f, -0.5f, 0.0f }, { 0.0f, 0.0f, 1.0f } },
        };
        const UINT vbSize = sizeof(vertices);

        D3D12_HEAP_PROPERTIES heap{};
        heap.Type = D3D12_HEAP_TYPE_UPLOAD;

        D3D12_RESOURCE_DESC buf{};
        buf.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER;
        buf.Width = vbSize;
        buf.Height = 1;
        buf.DepthOrArraySize = 1;
        buf.MipLevels = 1;
        buf.SampleDesc.Count = 1;
        buf.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;

        HR(m_device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &buf,
            D3D12_RESOURCE_STATE_GENERIC_READ, nullptr,
            IID_PPV_ARGS(&m_vertexBuffer)), "CreateCommittedResource(VB)");

        void* p = nullptr;
        D3D12_RANGE readRange{ 0, 0 };
        HR(m_vertexBuffer->Map(0, &readRange, &p), "VB->Map");
        std::memcpy(p, vertices, vbSize);
        m_vertexBuffer->Unmap(0, nullptr);

        m_vbView.BufferLocation = m_vertexBuffer->GetGPUVirtualAddress();
        m_vbView.StrideInBytes = sizeof(Vertex);
        m_vbView.SizeInBytes = vbSize;
    }

    // --- Loop / render / sync / resize ---
    void MainLoop() {
        MSG msg{};
        while (msg.message != WM_QUIT) {
            if (PeekMessage(&msg, nullptr, 0, 0, PM_REMOVE)) {
                TranslateMessage(&msg);
                DispatchMessage(&msg);
            }
            else {
                Render();
            }
        }
        WaitForGPU();
    }

    void Render() {
        HR(m_commandAllocators[m_frameIndex]->Reset(), "CmdAlloc->Reset");
        HR(m_commandList->Reset(m_commandAllocators[m_frameIndex].Get(), m_pipelineState.Get()),
            "CmdList->Reset");

        m_commandList->SetGraphicsRootSignature(m_rootSignature.Get());
        m_commandList->RSSetViewports(1, &m_viewport);
        m_commandList->RSSetScissorRects(1, &m_scissor);

        D3D12_RESOURCE_BARRIER toRT{};
        toRT.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
        toRT.Transition.pResource = m_renderTargets[m_frameIndex].Get();
        toRT.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
        toRT.Transition.StateBefore = D3D12_RESOURCE_STATE_PRESENT;
        toRT.Transition.StateAfter = D3D12_RESOURCE_STATE_RENDER_TARGET;
        m_commandList->ResourceBarrier(1, &toRT);

        D3D12_CPU_DESCRIPTOR_HANDLE rtv = m_rtvHeap->GetCPUDescriptorHandleForHeapStart();
        rtv.ptr += m_frameIndex * m_rtvDescriptorSize;
        m_commandList->OMSetRenderTargets(1, &rtv, FALSE, nullptr);

        const float clear[4] = { 0.1f, 0.1f, 0.15f, 1.0f };
        m_commandList->ClearRenderTargetView(rtv, clear, 0, nullptr);

        m_commandList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
        m_commandList->IASetVertexBuffers(0, 1, &m_vbView);
        m_commandList->DrawInstanced(3, 1, 0, 0);

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

        m_width = newW; m_height = newH;
        m_viewport.Width = float(m_width);
        m_viewport.Height = float(m_height);
        m_scissor.right = (LONG)m_width;
        m_scissor.bottom = (LONG)m_height;
    }

    void Cleanup() {
        if (m_commandQueue && m_fence) WaitForGPU();
        if (m_fenceEvent) CloseHandle(m_fenceEvent);
    }
};

// --- Entry points ------------------------------------------------------------
int WINAPI wWinMain(HINSTANCE hInstance, HINSTANCE, PWSTR, int nCmdShow) {
    try {
        D3D12TriangleApp app(800, 600);
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

// If your project is Console (/SUBSYSTEM:CONSOLE), keep this adapter to satisfy the CRT.
int main() {
    return wWinMain(GetModuleHandle(nullptr), nullptr, GetCommandLineW(), SW_SHOWDEFAULT);
}
