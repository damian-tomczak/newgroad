#include "App.h"
#include <windowsx.h>
#include <algorithm>

#include "imgui.h"
#include "backends/imgui_impl_win32.h"
#include "backends/imgui_impl_dx12.h"

using namespace std::chrono;

extern IMGUI_IMPL_API LRESULT ImGui_ImplWin32_WndProcHandler(HWND, UINT, WPARAM, LPARAM);

D3D12ScenesApp::D3D12ScenesApp(UINT w, UINT h)
    : m_hwnd(nullptr)
    , m_width(w)
    , m_height(h)
    , m_rtvDescriptorSize(0)
    , m_frameIndex(0)
    , m_fenceEvent(nullptr)
    , m_sceneKind(SceneKind::Triangle)
    , m_prev(steady_clock::now())
    , m_menuWidth(320.0f)
{
    m_dsv = {};
    m_viewport = {};
    m_scissor = {};
    for (UINT i = 0; i < FrameCount; ++i) {
        m_renderTargets[i].Reset();
        m_commandAllocators[i].Reset();
        m_fenceValues[i] = 0;
    }
}

D3D12ScenesApp::~D3D12ScenesApp() {
}

void D3D12ScenesApp::Run(HINSTANCE hInstance, int nCmdShow) {
    RegisterWindowClass(hInstance);
    CreateAppWindow(hInstance, nCmdShow);
    InitD3D();
    InitImGui();
    InitScenes();
    MainLoop();
    Cleanup();
}

LRESULT CALLBACK D3D12ScenesApp::WndProcSetup(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    if (msg == WM_NCCREATE) {
        auto cs = reinterpret_cast<CREATESTRUCT*>(lParam);
        auto app = reinterpret_cast<D3D12ScenesApp*>(cs->lpCreateParams);
        SetWindowLongPtr(hWnd, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(app));
        SetWindowLongPtr(hWnd, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(WndProcThunk));
        return app->WndProc(hWnd, msg, wParam, lParam);
    }
    return DefWindowProc(hWnd, msg, wParam, lParam);
}

LRESULT CALLBACK D3D12ScenesApp::WndProcThunk(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    auto app = reinterpret_cast<D3D12ScenesApp*>(GetWindowLongPtr(hWnd, GWLP_USERDATA));
    return app->WndProc(hWnd, msg, wParam, lParam);
}

LRESULT D3D12ScenesApp::WndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    // Najpierw ImGui
    if (ImGui_ImplWin32_WndProcHandler(hWnd, msg, wParam, lParam))
        return 1;

    switch (msg) {
    case WM_LBUTTONDOWN:
    case WM_LBUTTONUP:
    case WM_RBUTTONDOWN:
    case WM_RBUTTONUP:
    case WM_MOUSEMOVE:
    {
        if (m_sceneKind == SceneKind::Whirligig && ImGui::GetCurrentContext()) {
            ImGuiIO& io = ImGui::GetIO();
            bool imguiWantsMouse = io.WantCaptureMouse;
            if (!imguiWantsMouse) {
                int x = GET_X_LPARAM(lParam); // client coords
                if (x >= (int)m_menuWidth) {
                    m_whirligig.OnMouseMessage(msg, wParam, lParam);
                }
            }
        }
        return 0;
    }
    case WM_MOUSEWHEEL:
    {
        if (m_sceneKind == SceneKind::Whirligig && ImGui::GetCurrentContext()) {
            ImGuiIO& io = ImGui::GetIO();
            bool imguiWantsMouse = io.WantCaptureMouse;
            if (!imguiWantsMouse) {
                m_whirligig.OnMouseMessage(msg, wParam, lParam);
            }
        }
        return 0;
    }

    case WM_SIZE:
        if (m_swapChain && wParam != SIZE_MINIMIZED) {
            UINT w = LOWORD(lParam);
            UINT h = HIWORD(lParam);
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

void D3D12ScenesApp::RegisterWindowClass(HINSTANCE hInstance) {
    WNDCLASS wc{};
    wc.lpfnWndProc = WndProcSetup;
    wc.hInstance = hInstance;
    wc.lpszClassName = L"DX12ScenesWinClass";
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
    RegisterClass(&wc);
}

void D3D12ScenesApp::CreateAppWindow(HINSTANCE hInstance, int nCmdShow) {
    RECT rc{ 0,0,(LONG)m_width,(LONG)m_height };
    AdjustWindowRect(&rc, WS_OVERLAPPEDWINDOW, FALSE);
    m_hwnd = CreateWindowEx(0, L"DX12ScenesWinClass", L"groad",
        WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT,
        rc.right - rc.left, rc.bottom - rc.top,
        nullptr, nullptr, hInstance, this);
    ShowWindow(m_hwnd, nCmdShow);
    UpdateWindow(m_hwnd);
}

void D3D12ScenesApp::InitD3D() {
    UINT factoryFlags = 0;
#if defined(_DEBUG)
    ComPtr<ID3D12Debug> dbg;
    if (SUCCEEDED(D3D12GetDebugInterface(IID_PPV_ARGS(&dbg)))) {
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

void D3D12ScenesApp::CreateRTVs() {
    D3D12_CPU_DESCRIPTOR_HANDLE h = m_rtvHeap->GetCPUDescriptorHandleForHeapStart();
    for (UINT i = 0; i < FrameCount; ++i) {
        HR(m_swapChain->GetBuffer(i, IID_PPV_ARGS(&m_renderTargets[i])), "SwapChain->GetBuffer");
        m_device->CreateRenderTargetView(m_renderTargets[i].Get(), nullptr, h);
        h.ptr += m_rtvDescriptorSize;
    }
}

void D3D12ScenesApp::CreateDepth(UINT w, UINT h) {
    m_depth.Reset();

    D3D12_RESOURCE_DESC tex{};
    tex.Dimension = D3D12_RESOURCE_DIMENSION_TEXTURE2D;
    tex.Width = w; tex.Height = h; tex.DepthOrArraySize = 1; tex.MipLevels = 1;
    tex.SampleDesc.Count = 1;
    tex.Format = DXGI_FORMAT_D32_FLOAT;
    tex.Flags = D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL;

    D3D12_CLEAR_VALUE clear{}; clear.Format = DXGI_FORMAT_D32_FLOAT; clear.DepthStencil.Depth = 1.0f;

    D3D12_HEAP_PROPERTIES heap{}; heap.Type = D3D12_HEAP_TYPE_DEFAULT;
    HR(m_device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &tex,
        D3D12_RESOURCE_STATE_DEPTH_WRITE, &clear, IID_PPV_ARGS(&m_depth)),
        "CreateCommittedResource(Depth)");

    D3D12_DEPTH_STENCIL_VIEW_DESC dsv{};
    dsv.Format = DXGI_FORMAT_D32_FLOAT;
    dsv.ViewDimension = D3D12_DSV_DIMENSION_TEXTURE2D;
    m_device->CreateDepthStencilView(m_depth.Get(), &dsv, m_dsv);
}

void D3D12ScenesApp::InitImGui() {
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

void D3D12ScenesApp::InitScenes() {
    m_triangle.Init(m_device.Get());
    m_whirligig.Init(m_device.Get());
}

void D3D12ScenesApp::MainLoop() {
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

void D3D12ScenesApp::RenderFrame() {
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
    m_commandList->ClearDepthStencilView(m_dsv, D3D12_CLEAR_FLAG_DEPTH, 1.0f, 0, 0, 0);

    if (m_sceneKind == SceneKind::Triangle) {
        m_triangle.Render(m_commandList.Get());
    }
    else {
        m_whirligig.Render(m_commandList.Get());
    }

    ImGui_ImplDX12_NewFrame();
    ImGui_ImplWin32_NewFrame();
    ImGui::NewFrame();
    DrawUI();
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


static bool SliderFloatWithInput(const char* label,
    float* v,
    float v_min,
    float v_max,
    const char* format = "%.3f")
{
    bool changed = false;

    ImGui::PushID(label);

    // Label
    ImGui::AlignTextToFramePadding();
    ImGui::TextUnformatted(label);
    ImGui::SameLine();

    // Slider (clamped)
    ImGui::SetNextItemWidth(150.0f);
    changed |= ImGui::SliderFloat("##slider", v, v_min, v_max, format);
    ImGui::SameLine();

    // Text input (free typing, then we clamp)
    ImGui::SetNextItemWidth(80.0f);
    changed |= ImGui::InputFloat("##input", v, 0.0f, 0.0f, format);

    // Clamp to slider range for safety; remove if you truly want out-of-range values.
    if (*v < v_min) { *v = v_min; }
    if (*v > v_max) { *v = v_max; }

    ImGui::PopID();
    return changed;
}

static bool SliderIntWithInput(const char* label,
    int* v,
    int v_min,
    int v_max)
{
    bool changed = false;

    ImGui::PushID(label);

    ImGui::AlignTextToFramePadding();
    ImGui::TextUnformatted(label);
    ImGui::SameLine();

    // Slider (clamped)
    ImGui::SetNextItemWidth(150.0f);
    changed |= ImGui::SliderInt("##slider", v, v_min, v_max);
    ImGui::SameLine();

    // Text input (free typing, then we clamp)
    ImGui::SetNextItemWidth(80.0f);
    changed |= ImGui::InputInt("##input", v);

    if (*v < v_min) { *v = v_min; }
    if (*v > v_max) { *v = v_max; }

    ImGui::PopID();
    return changed;
}

//---------------------------------------------------------------------
// UI
//---------------------------------------------------------------------

void D3D12ScenesApp::DrawUI()
{
    using namespace std::chrono;

    auto now = steady_clock::now();
    float dt = duration_cast<duration<float>>(now - m_prev).count();
    m_prev = now;

    float minWidth = 200.0f;
    float maxWidth = (float)std::max(250u, m_width - 100u);
    m_menuWidth = std::clamp(m_menuWidth, minWidth, maxWidth);

    ImGui::SetNextWindowPos(ImVec2(0.0f, 0.0f));
    ImGui::SetNextWindowSize(ImVec2(m_menuWidth, (float)m_height));

    ImGuiWindowFlags flags = ImGuiWindowFlags_NoMove |
        ImGuiWindowFlags_NoResize |
        ImGuiWindowFlags_NoCollapse;

    ImGui::Begin("Scenes", nullptr, flags);

    // Menu width: slider + input
    ImGui::SliderFloat("Menu width", &m_menuWidth, minWidth, maxWidth);

    const char* items[] = { "Triangle", "Whirligig" };
    int idx = (m_sceneKind == SceneKind::Triangle) ? 0 : 1;
    if (ImGui::Combo("Scene", &idx, items, IM_ARRAYSIZE(items))) {
        m_sceneKind = (idx == 0) ? SceneKind::Triangle : SceneKind::Whirligig;
    }

    if (m_sceneKind == SceneKind::Whirligig) {
        ImGui::Separator();
        if (ImGui::Button(m_whirligig.running ? "Stop" : "Start")) {
            m_whirligig.running = !m_whirligig.running;
        }
        ImGui::SameLine();
        if (ImGui::Button("Reset")) {
            m_whirligig.ResetState();
        }

        ImGui::Checkbox("Cube", &m_whirligig.showCube);
        ImGui::Checkbox("Diagonal", &m_whirligig.showDiagonal);
        ImGui::Checkbox("Trajectory", &m_whirligig.showTrajectory);
        ImGui::Checkbox("Grid", &m_whirligig.showPlane);
        ImGui::Checkbox("Axes", &m_whirligig.showAxes);

        bool changed = false;

        changed |= SliderFloatWithInput("Cube size",
            &m_whirligig.cubeSize,
            0.2f, 2.0f);

        changed |= SliderFloatWithInput("Density",
            &m_whirligig.density,
            0.1f, 10.0f);

        changed |= SliderFloatWithInput("Inflection [deg]",
            &m_whirligig.inflectionDeg,
            0.0f, 90.0f);

        changed |= SliderFloatWithInput("|omega|",
            &m_whirligig.omegaMag,
            0.0f, 10.0f);

        int len = (int)m_whirligig.trajLength;
        changed |= SliderIntWithInput("Trajectory length",
            &len,
            100,
            (int)WhirligigScene::MaxTrajPoints);
        m_whirligig.trajLength = (UINT)len;

        ImGui::Checkbox("Use gravity", &m_whirligig.useGravity);

        changed |= SliderFloatWithInput("Speed",
            &m_whirligig.speed,
            1.0f, 100.0f);

        changed |= SliderFloatWithInput("dt",
            &m_whirligig.dt,
            0.001f, 0.1f,
            "%.4f");

        if (changed) {
            m_whirligig.ResetState();
        }

        ImGui::Separator();
        //ImGui::Text("CAD camera (RH):");
        //ImGui::BulletText("LMB drag: orbit");
        //ImGui::BulletText("RMB drag: pan");
        //ImGui::BulletText("Wheel: zoom");
    }

    ImGui::Separator();
    ImGui::Text("Frame %.3f ms (%.1f FPS)",
        dt * 1000.0f,
        dt > 0 ? (1.0f / dt) : 0.0f);

    ImGui::End();
}

void D3D12ScenesApp::MoveToNextFrame() {
    const UINT64 fence = m_fenceValues[m_frameIndex];
    HR(m_commandQueue->Signal(m_fence.Get(), fence), "Queue->Signal");
    m_frameIndex = m_swapChain->GetCurrentBackBufferIndex();
    if (m_fence->GetCompletedValue() < m_fenceValues[m_frameIndex]) {
        HR(m_fence->SetEventOnCompletion(m_fenceValues[m_frameIndex], m_fenceEvent), "Fence->SetEventOnCompletion");
        WaitForSingleObject(m_fenceEvent, INFINITE);
    }
    m_fenceValues[m_frameIndex] = fence + 1;
}

void D3D12ScenesApp::WaitForGPU() {
    HR(m_commandQueue->Signal(m_fence.Get(), m_fenceValues[m_frameIndex]), "Queue->Signal(wait)");
    HR(m_fence->SetEventOnCompletion(m_fenceValues[m_frameIndex], m_fenceEvent), "Fence->SetEventOnCompletion(wait)");
    WaitForSingleObject(m_fenceEvent, INFINITE);
    m_fenceValues[m_frameIndex]++;
}

void D3D12ScenesApp::OnResize(UINT newW, UINT newH) {
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
    m_viewport.Width = float(m_width);
    m_viewport.Height = float(m_height);
    m_scissor.right = (LONG)m_width;
    m_scissor.bottom = (LONG)m_height;

    m_triangle.OnResize(newW, newH);
    m_whirligig.OnResize(newW, newH);
}

void D3D12ScenesApp::Cleanup() {
    if (m_commandQueue && m_fence) WaitForGPU();

    ImGui_ImplDX12_Shutdown();
    ImGui_ImplWin32_Shutdown();
    ImGui::DestroyContext();

    m_triangle.Cleanup();
    m_whirligig.Cleanup();

    if (m_fenceEvent) CloseHandle(m_fenceEvent);
}
