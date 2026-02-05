// App.cpp
#include "App.h"
#include <windowsx.h>
#include <algorithm>
#include <fstream>

#include "imgui.h"
#include "backends/imgui_impl_win32.h"
#include "backends/imgui_impl_dx12.h"

using namespace std::chrono;

extern IMGUI_IMPL_API LRESULT ImGui_ImplWin32_WndProcHandler(HWND, UINT, WPARAM, LPARAM);

// -------------------- Helpers --------------------
static void Transition(ID3D12GraphicsCommandList* cl,
    ID3D12Resource* res,
    D3D12_RESOURCE_STATES before,
    D3D12_RESOURCE_STATES after)
{
    if (!res || before == after) return;
    D3D12_RESOURCE_BARRIER b{};
    b.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
    b.Transition.pResource = res;
    b.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;
    b.Transition.StateBefore = before;
    b.Transition.StateAfter = after;
    cl->ResourceBarrier(1, &b);
}

// -------------------- GRoad --------------------
GRoad::GRoad(UINT w, UINT h)
    : hwnd(nullptr)
    , width(w)
    , height(h)
    , rtvDescriptorSize(0)
    , frameIndex(0)
    , fenceEvent(nullptr)
    , sceneKind(SceneKind::Jelly)
    , prev(steady_clock::now())
    , menuWidth(320.0f)
{
    dsv = {};
    viewport = {};
    scissor = {};
    sceneRtv = {};
    for (UINT i = 0; i < FrameCount; ++i) {
        renderTargets[i].Reset();
        commandAllocators[i].Reset();
        fenceValues[i] = 0;
    }
}

GRoad::~GRoad() {}

void GRoad::Run(HINSTANCE hInstance, int nCmdShow) {
    RegisterWindowClass(hInstance);
    CreateAppWindow(hInstance, nCmdShow);
    InitD3D();
    InitImGui();
    InitScenes();
    LoadSettings();
    MainLoop();
    Cleanup();
}

void GRoad::LoadSettings()
{
    std::ifstream in("groad_settings.ini");
    if (!in) return;

    int savedScene = static_cast<int>(SceneKind::Jelly);
    in >> savedScene;
    if (!in) return;

    savedScene = std::clamp(savedScene, 0, 3);
    sceneKind = static_cast<SceneKind>(savedScene);
}

void GRoad::SaveSettings() const
{
    std::ofstream out("groad_settings.ini", std::ios::trunc);
    if (!out) return;

    out << static_cast<int>(sceneKind) << "\n";
}

LRESULT CALLBACK GRoad::WndProcSetup(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    if (msg == WM_NCCREATE) {
        auto cs = reinterpret_cast<CREATESTRUCT*>(lParam);
        auto app = reinterpret_cast<GRoad*>(cs->lpCreateParams);
        SetWindowLongPtr(hWnd, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(app));
        SetWindowLongPtr(hWnd, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(WndProcThunk));
        return app->WndProc(hWnd, msg, wParam, lParam);
    }
    return DefWindowProc(hWnd, msg, wParam, lParam);
}

LRESULT CALLBACK GRoad::WndProcThunk(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    auto app = reinterpret_cast<GRoad*>(GetWindowLongPtr(hWnd, GWLP_USERDATA));
    return app->WndProc(hWnd, msg, wParam, lParam);
}

LRESULT GRoad::WndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    if (ImGui_ImplWin32_WndProcHandler(hWnd, msg, wParam, lParam))
        return 1;

    switch (msg) {
    case WM_LBUTTONDOWN:
    case WM_LBUTTONUP:
    case WM_RBUTTONDOWN:
    case WM_RBUTTONUP:
    case WM_MOUSEMOVE:
    {
        if ((sceneKind == SceneKind::Whirligig || sceneKind == SceneKind::Jelly || sceneKind == SceneKind::Fog) && ImGui::GetCurrentContext()) {
            ImGuiIO& io = ImGui::GetIO();
            bool imguiWantsMouse = io.WantCaptureMouse;
            if (!imguiWantsMouse) {
                int x = GET_X_LPARAM(lParam);
                if (x >= (int)menuWidth) {
                    if (sceneKind == SceneKind::Whirligig || sceneKind == SceneKind::Fog) whirligig.OnMouseMessage(msg, wParam, lParam);
                    else jelly.OnMouseMessage(msg, wParam, lParam);
                }
            }
        }
        if (sceneKind == SceneKind::Jelly && ImGui::GetCurrentContext()) {
            ImGuiIO& io = ImGui::GetIO();
            if (!io.WantCaptureMouse) {
                int x = (int)(short)LOWORD(lParam);
                if (x >= (int)menuWidth) {
                    jelly.OnMouseMessage(msg, wParam, lParam);
                }
            }
        }
        return 0;
    }
    case WM_MOUSEWHEEL:
    {
        if ((sceneKind == SceneKind::Whirligig || sceneKind == SceneKind::Jelly || sceneKind == SceneKind::Fog) && ImGui::GetCurrentContext()) {
            ImGuiIO& io = ImGui::GetIO();
            bool imguiWantsMouse = io.WantCaptureMouse;
            if (!imguiWantsMouse) {
                if (sceneKind == SceneKind::Whirligig || sceneKind == SceneKind::Fog) {
                    whirligig.OnMouseMessage(msg, wParam, lParam);
                }
                if (sceneKind == SceneKind::Jelly) {
                    jelly.OnMouseMessage(msg, wParam, lParam);
                }
            }
        }
        return 0;
    }

    case WM_SIZE:
        if (swapChain && wParam != SIZE_MINIMIZED) {
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

void GRoad::RegisterWindowClass(HINSTANCE hInstance) {
    WNDCLASS wc{};
    wc.lpfnWndProc = WndProcSetup;
    wc.hInstance = hInstance;
    wc.lpszClassName = L"DX12ScenesWinClass";
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
    RegisterClass(&wc);
}

void GRoad::CreateAppWindow(HINSTANCE hInstance, int nCmdShow) {
    RECT rc{ 0,0,(LONG)width,(LONG)height };
    AdjustWindowRect(&rc, WS_OVERLAPPEDWINDOW, FALSE);
    hwnd = CreateWindowEx(0, L"DX12ScenesWinClass", L"GRoad",
        WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT,
        rc.right - rc.left, rc.bottom - rc.top,
        nullptr, nullptr, hInstance, this);
    ShowWindow(hwnd, nCmdShow);
    UpdateWindow(hwnd);
}

void GRoad::InitD3D() {
    UINT factoryFlags = 0;
#if defined(_DEBUG)
    ComPtr<ID3D12Debug> dbg;
    if (SUCCEEDED(D3D12GetDebugInterface(IID_PPV_ARGS(&dbg)))) {
        dbg->EnableDebugLayer();
        factoryFlags |= DXGI_CREATE_FACTORY_DEBUG;
    }
#endif
    HR(CreateDXGIFactory2(factoryFlags, IID_PPV_ARGS(&factory)), "CreateDXGIFactory2");

    HRESULT hr = D3D12CreateDevice(nullptr, D3D_FEATURE_LEVEL_11_0, IID_PPV_ARGS(&device));
    if (FAILED(hr)) {
        ComPtr<IDXGIAdapter> warp;
        HR(factory->EnumWarpAdapter(IID_PPV_ARGS(&warp)), "EnumWarpAdapter");
        HR(D3D12CreateDevice(warp.Get(), D3D_FEATURE_LEVEL_11_0, IID_PPV_ARGS(&device)),
            "D3D12CreateDevice(WARP)");
    }

    D3D12_COMMAND_QUEUE_DESC q{}; q.Type = D3D12_COMMAND_LIST_TYPE_DIRECT;
    HR(device->CreateCommandQueue(&q, IID_PPV_ARGS(&commandQueue)), "CreateCommandQueue");

    DXGI_SWAP_CHAIN_DESC1 sc{};
    sc.BufferCount = FrameCount;
    sc.Width = width; sc.Height = height;
    sc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
    sc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
    sc.SwapEffect = DXGI_SWAP_EFFECT_FLIP_DISCARD;
    sc.SampleDesc.Count = 1;
    ComPtr<IDXGISwapChain1> sc1;
    HR(factory->CreateSwapChainForHwnd(commandQueue.Get(), hwnd, &sc, nullptr, nullptr, &sc1),
        "CreateSwapChainForHwnd");
    HR(sc1.As(&swapChain), "SwapChain1->As<IDXGISwapChain3>");
    frameIndex = swapChain->GetCurrentBackBufferIndex();

    D3D12_DESCRIPTOR_HEAP_DESC rtvDesc{}; rtvDesc.NumDescriptors = FrameCount; rtvDesc.Type = D3D12_DESCRIPTOR_HEAP_TYPE_RTV;
    HR(device->CreateDescriptorHeap(&rtvDesc, IID_PPV_ARGS(&rtvHeap)), "CreateDescriptorHeap(RTV)");
    rtvDescriptorSize = device->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_RTV);
    CreateRTVs();

    D3D12_DESCRIPTOR_HEAP_DESC dsvDesc{}; dsvDesc.NumDescriptors = 1; dsvDesc.Type = D3D12_DESCRIPTOR_HEAP_TYPE_DSV;
    HR(device->CreateDescriptorHeap(&dsvDesc, IID_PPV_ARGS(&dsvHeap)), "CreateDescriptorHeap(DSV)");
    dsv = dsvHeap->GetCPUDescriptorHandleForHeapStart();
    CreateDepth(width, height);

    // Offscreen scene color target
    CreateSceneColor(width, height);

    for (UINT i = 0; i < FrameCount; ++i)
        HR(device->CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT, IID_PPV_ARGS(&commandAllocators[i])),
            "CreateCommandAllocator");
    HR(device->CreateCommandList(0, D3D12_COMMAND_LIST_TYPE_DIRECT,
        commandAllocators[frameIndex].Get(), nullptr, IID_PPV_ARGS(&commandList)),
        "CreateCommandList");
    HR(commandList->Close(), "Close(CommandList)");

    HR(device->CreateFence(0, D3D12_FENCE_FLAG_NONE, IID_PPV_ARGS(&fence)), "CreateFence");
    fenceValues[frameIndex] = 1;
    fenceEvent = CreateEvent(nullptr, FALSE, FALSE, nullptr);
    if (!fenceEvent) HR(HRESULT_FROM_WIN32(GetLastError()), "CreateEvent");

    viewport = { 0.0f, 0.0f, float(width), float(height), 0.0f, 1.0f };
    scissor = { 0, 0, (LONG)width, (LONG)height };
}

void GRoad::CreateRTVs() {
    D3D12_CPU_DESCRIPTOR_HANDLE h = rtvHeap->GetCPUDescriptorHandleForHeapStart();
    for (UINT i = 0; i < FrameCount; ++i) {
        HR(swapChain->GetBuffer(i, IID_PPV_ARGS(&renderTargets[i])), "SwapChain->GetBuffer");
        device->CreateRenderTargetView(renderTargets[i].Get(), nullptr, h);
        h.ptr += rtvDescriptorSize;
    }
}

void GRoad::CreateDepth(UINT w, UINT h) {
    depth.Reset();

    D3D12_RESOURCE_DESC tex{};
    tex.Dimension = D3D12_RESOURCE_DIMENSION_TEXTURE2D;
    tex.Width = w; tex.Height = h; tex.DepthOrArraySize = 1; tex.MipLevels = 1;
    tex.SampleDesc.Count = 1;

    // typeless so we can have DSV + SRV
    tex.Format = DXGI_FORMAT_R32_TYPELESS;
    tex.Flags = D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL;

    D3D12_CLEAR_VALUE clear{};
    clear.Format = DXGI_FORMAT_D32_FLOAT;
    clear.DepthStencil.Depth = 1.0f;

    D3D12_HEAP_PROPERTIES heap{}; heap.Type = D3D12_HEAP_TYPE_DEFAULT;
    HR(device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &tex,
        D3D12_RESOURCE_STATE_DEPTH_WRITE, &clear, IID_PPV_ARGS(&depth)),
        "CreateCommittedResource(Depth)");

    D3D12_DEPTH_STENCIL_VIEW_DESC dsvDesc{};
    dsvDesc.Format = DXGI_FORMAT_D32_FLOAT;
    dsvDesc.ViewDimension = D3D12_DSV_DIMENSION_TEXTURE2D;
    device->CreateDepthStencilView(depth.Get(), &dsvDesc, dsv);
}

void GRoad::CreateSceneColor(UINT w, UINT h)
{
    sceneColor.Reset();

    if (!sceneRtvHeap) {
        D3D12_DESCRIPTOR_HEAP_DESC rtv{};
        rtv.Type = D3D12_DESCRIPTOR_HEAP_TYPE_RTV;
        rtv.NumDescriptors = 1;
        HR(device->CreateDescriptorHeap(&rtv, IID_PPV_ARGS(&sceneRtvHeap)), "CreateScene RTV heap");
    }
    sceneRtv = sceneRtvHeap->GetCPUDescriptorHandleForHeapStart();

    D3D12_RESOURCE_DESC tex{};
    tex.Dimension = D3D12_RESOURCE_DIMENSION_TEXTURE2D;
    tex.Width = w; tex.Height = h;
    tex.DepthOrArraySize = 1;
    tex.MipLevels = 1;
    tex.SampleDesc.Count = 1;
    tex.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
    tex.Flags = D3D12_RESOURCE_FLAG_ALLOW_RENDER_TARGET;

    D3D12_CLEAR_VALUE clear{};
    clear.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
    clear.Color[0] = 0.06f; clear.Color[1] = 0.07f; clear.Color[2] = 0.10f; clear.Color[3] = 1.0f;

    D3D12_HEAP_PROPERTIES heap{}; heap.Type = D3D12_HEAP_TYPE_DEFAULT;
    HR(device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &tex,
        D3D12_RESOURCE_STATE_RENDER_TARGET, &clear, IID_PPV_ARGS(&sceneColor)),
        "CreateCommittedResource(SceneColor)");

    device->CreateRenderTargetView(sceneColor.Get(), nullptr, sceneRtv);
}

void GRoad::InitImGui() {
    IMGUI_CHECKVERSION();
    ImGui::CreateContext();
    ImGui::StyleColorsDark();

    srvDescSize = device->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);

    D3D12_DESCRIPTOR_HEAP_DESC heap{};
    heap.Type = D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV;
    heap.NumDescriptors = 16; // [0]=ImGui font, [1]=scene SRV, [2]=depth SRV
    heap.Flags = D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE;
    HR(device->CreateDescriptorHeap(&heap, IID_PPV_ARGS(&srvHeap)), "CreateDescriptorHeap(SRV)");

    auto cpu0 = srvHeap->GetCPUDescriptorHandleForHeapStart();
    auto gpu0 = srvHeap->GetGPUDescriptorHandleForHeapStart();

    ImGui_ImplWin32_Init(hwnd);
    ImGui_ImplDX12_Init(
        device.Get(), FrameCount,
        DXGI_FORMAT_R8G8B8A8_UNORM,
        srvHeap.Get(),
        cpu0, gpu0);

    fogScene.Init(device.Get(), srvHeap.Get(), srvDescSize, width, height, sceneColor.Get(), depth.Get());
}

void GRoad::InitScenes() {
    triangle.Init(device.Get());
    whirligig.Init(device.Get());
    jelly.Init(device.Get());
}

void GRoad::MainLoop() {
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

void GRoad::RenderFrame() {
    HR(commandAllocators[frameIndex]->Reset(), "CmdAlloc->Reset");
    HR(commandList->Reset(commandAllocators[frameIndex].Get(), nullptr), "CmdList->Reset");

    // Scene pass: render into offscreen scene color + depth
    commandList->RSSetViewports(1, &viewport);
    commandList->RSSetScissorRects(1, &scissor);

    commandList->OMSetRenderTargets(1, &sceneRtv, FALSE, &dsv);

    const float clear[4] = { 0.06f, 0.07f, 0.10f, 1.0f };
    commandList->ClearRenderTargetView(sceneRtv, clear, 0, nullptr);
    commandList->ClearDepthStencilView(dsv, D3D12_CLEAR_FLAG_DEPTH, 1.0f, 0, 0, 0);

    if (sceneKind == SceneKind::Triangle) {
        triangle.Render(commandList.Get());
    }
    else if (sceneKind == SceneKind::Whirligig) {
        whirligig.Render(commandList.Get());
    }
    else if (sceneKind == SceneKind::Jelly) {
        jelly.Render(commandList.Get());
    }
    else {
        whirligig.Render(commandList.Get());
    }

    // Prepare SRVs for postprocess
    Transition(commandList.Get(), sceneColor.Get(),
        D3D12_RESOURCE_STATE_RENDER_TARGET, D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE);

    Transition(commandList.Get(), depth.Get(),
        D3D12_RESOURCE_STATE_DEPTH_WRITE, D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE);

    // Backbuffer to RT for post + UI
    Transition(commandList.Get(), renderTargets[frameIndex].Get(),
        D3D12_RESOURCE_STATE_PRESENT, D3D12_RESOURCE_STATE_RENDER_TARGET);

    D3D12_CPU_DESCRIPTOR_HANDLE backRtv = rtvHeap->GetCPUDescriptorHandleForHeapStart();
    backRtv.ptr += frameIndex * rtvDescriptorSize;
    commandList->OMSetRenderTargets(1, &backRtv, FALSE, nullptr);

    // Shared heap for postprocess + ImGui
    ID3D12DescriptorHeap* heaps[] = { srvHeap.Get() };
    commandList->SetDescriptorHeaps(1, heaps);

    // Fog postprocess into backbuffer
    fogScene.Draw(commandList.Get(), frameIndex);

    // ImGui on top
    ImGui_ImplDX12_NewFrame();
    ImGui_ImplWin32_NewFrame();
    ImGui::NewFrame();
    DrawUI();
    ImGui::Render();
    ImGui_ImplDX12_RenderDrawData(ImGui::GetDrawData(), commandList.Get());

    // Restore resources for next frame
    Transition(commandList.Get(), sceneColor.Get(),
        D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE, D3D12_RESOURCE_STATE_RENDER_TARGET);

    Transition(commandList.Get(), depth.Get(),
        D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE, D3D12_RESOURCE_STATE_DEPTH_WRITE);

    // Backbuffer to present
    Transition(commandList.Get(), renderTargets[frameIndex].Get(),
        D3D12_RESOURCE_STATE_RENDER_TARGET, D3D12_RESOURCE_STATE_PRESENT);

    HR(commandList->Close(), "CmdList->Close");
    ID3D12CommandList* lists[] = { commandList.Get() };
    commandQueue->ExecuteCommandLists(1, lists);
    HR(swapChain->Present(1, 0), "SwapChain->Present");

    MoveToNextFrame();
}

// -------------------- UI helpers (unchanged) --------------------
static bool SliderFloatWithInput(const char* label,
    float* v,
    float v_min,
    float v_max,
    const char* format = "%.3f")
{
    bool changed = false;

    ImGui::PushID(label);

    ImGui::AlignTextToFramePadding();
    ImGui::TextUnformatted(label);
    ImGui::SameLine();

    ImGui::SetNextItemWidth(150.0f);
    changed |= ImGui::SliderFloat("##slider", v, v_min, v_max, format);
    ImGui::SameLine();

    ImGui::SetNextItemWidth(80.0f);
    changed |= ImGui::InputFloat("##input", v, 0.0f, 0.0f, format);

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

    ImGui::SetNextItemWidth(150.0f);
    changed |= ImGui::SliderInt("##slider", v, v_min, v_max);
    ImGui::SameLine();

    ImGui::SetNextItemWidth(80.0f);
    changed |= ImGui::InputInt("##input", v);

    if (*v < v_min) { *v = v_min; }
    if (*v > v_max) { *v = v_max; }

    ImGui::PopID();
    return changed;
}

void GRoad::DrawUI()
{
    auto now = steady_clock::now();
    float dt = duration_cast<duration<float>>(now - prev).count();
    prev = now;

    float minWidth = 200.0f;
    float maxWidth = (float)std::max(250u, width - 100u);
    menuWidth = std::clamp(menuWidth, minWidth, maxWidth);

    ImGui::SetNextWindowPos(ImVec2(0.0f, 0.0f));
    ImGui::SetNextWindowSize(ImVec2(menuWidth, (float)height));

    ImGuiWindowFlags flags = ImGuiWindowFlags_NoMove |
        ImGuiWindowFlags_NoResize |
        ImGuiWindowFlags_NoCollapse;

    ImGui::Begin("Scenes", nullptr, flags);
    ImGui::SliderFloat("Menu width", &menuWidth, minWidth, maxWidth);

    const char* items[] = { "Triangle", "Whirligig", "Jelly", "Fog" };
    int idx = static_cast<int>(sceneKind);

    if (ImGui::Combo("Scene", &idx, items, IM_ARRAYSIZE(items))) {
        idx = std::clamp(idx, 0, 3);
        sceneKind = static_cast<SceneKind>(idx);
        SaveSettings();
    }

    if (sceneKind == SceneKind::Triangle)
    {
        ImGui::Separator();
        ImGui::Text("Triangle scene");
    }
    else if (sceneKind == SceneKind::Whirligig)
    {
        ImGui::Separator();
        if (ImGui::Button(whirligig.running ? "Stop" : "Start")) {
            whirligig.running = !whirligig.running;
        }
        ImGui::SameLine();
        if (ImGui::Button("Reset")) {
            whirligig.ResetState();
        }

        ImGui::Checkbox("Cube", &whirligig.showCube);
        ImGui::Checkbox("Diagonal", &whirligig.showDiagonal);
        ImGui::Checkbox("Trajectory", &whirligig.showTrajectory);
        ImGui::Checkbox("Grid", &whirligig.showPlane);
        ImGui::Checkbox("Axes", &whirligig.showAxes);

        bool changed = false;

        changed |= SliderFloatWithInput("Cube size [m]",
            &whirligig.cubeSize,
            0.2f, 2.0f);

        changed |= SliderFloatWithInput("Density [kg/m^3]",
            &whirligig.density,
            0.1f, 10.0f);

        changed |= SliderFloatWithInput("Inflection [deg]",
            &whirligig.inflectionDeg,
            0.0f, 90.0f);

        changed |= SliderFloatWithInput("|omega| [rad/s]",
            &whirligig.omegaMag,
            0.0f, 50.0f);

        int len = (int)whirligig.trajLength;
        changed |= SliderIntWithInput("Trajectory length [points]",
            &len,
            100,
            (int)WhirligigScene::MaxTrajPoints);
        whirligig.trajLength = (UINT)len;

        ImGui::Checkbox("Use gravity (g = 9.81 m/s^2)", &whirligig.useGravity);

        changed |= SliderFloatWithInput("Speed [× real time]",
            &whirligig.speed,
            1.0f, 100.0f);

        changed |= SliderFloatWithInput("dt [s]",
            &whirligig.dt,
            0.001f, 0.1f,
            "%.4f");

        if (changed) {
            whirligig.ResetState();
        }

        ImGui::Separator();
    }
    else if (sceneKind == SceneKind::Jelly)
    {
        ImGui::Separator();
        ImGui::Text("Jelly simulation");

        if (ImGui::Button("Start")) {
            jelly.running = true;
        }
        ImGui::SameLine();
        if (ImGui::Button("Pause")) {
            jelly.running = false;
        }
        ImGui::SameLine();
        if (ImGui::Button("Reset")) {
            jelly.running = false;
            jelly.ResetState();
        }

        ImGui::Separator();

        SliderFloatWithInput("delta",
            &jelly.fixedTimeStep,
            0.001f, 0.05f, "%.4f");

        SliderFloatWithInput("simulation_speed",
            &jelly.simulationSpeed,
            0.1f, 100.0f);

        {
            float oldRange = jelly.initialRandomVelocityRange;
            SliderFloatWithInput("distribution",
                &jelly.initialRandomVelocityRange,
                -30.0f, 30.0f);
            if (jelly.initialRandomVelocityRange != oldRange) {
                jelly.ResetState();
            }
        }

        ImGui::Separator();

        ImGui::Checkbox("Draw jelly points", &jelly.showJellyPoints);
        ImGui::Checkbox("Draw axial springs", &jelly.showAxialSprings);
        ImGui::Checkbox("Draw diagonal springs", &jelly.showDiagonalSprings);
        ImGui::Checkbox("Draw bezier", &jelly.showBezierSurface);
        ImGui::Checkbox("process_cube_constraints",
            &jelly.enableControlCubeCoupling);

        ImGui::Separator();

        ImGui::InputFloat3("Gravitation",
            (float*)&jelly.gravityAcceleration);

        SliderFloatWithInput("mu",
            &jelly.collisionRestitutionCoefficient,
            0.0f, 1.0f);

        ImGui::Text("Velocity after collision type");
        ImGui::RadioButton("one component",
            &jelly.applyRestitutionToWholeVelocity, 0);
        ImGui::RadioButton("whole vector",
            &jelly.applyRestitutionToWholeVelocity, 1);

        ImGui::Separator();

        SliderFloatWithInput("mass",
            &jelly.particleMass,
            0.01f, 20.0f);

        SliderFloatWithInput("damping coeff. (k)",
            &jelly.linearDampingCoefficient,
            0.0f, 50.0f);

        SliderFloatWithInput("c1",
            &jelly.axialSpringStiffness,
            0.0f, 300.0f);

        SliderFloatWithInput("c2",
            &jelly.shearSpringStiffness,
            0.0f, 300.0f);

        ImGui::Separator();

        ImGui::InputFloat3("control cube position",
            (float*)&jelly.controlCubePosition);

        ImGui::InputFloat3("control cube rotation (deg)",
            (float*)&jelly.controlCubeEulerAnglesDeg);

        SliderFloatWithInput("control cube depth on",
            &jelly.controlCubeEdgeLength,
            0.2f, 3.0f);

        ImGui::Separator();
    }


    else if (sceneKind == SceneKind::Fog)
    {
        ImGui::Separator();
        ImGui::Text("Fog scene");
        ImGui::TextWrapped("Renders the Whirligig geometry with postprocess fog controls below.");

        if (ImGui::Button(whirligig.running ? "Stop" : "Start")) {
            whirligig.running = !whirligig.running;
        }
        ImGui::SameLine();
        if (ImGui::Button("Reset")) {
            whirligig.ResetState();
        }

        ImGui::Checkbox("Cube", &whirligig.showCube);
        ImGui::Checkbox("Trajectory", &whirligig.showTrajectory);
        ImGui::Checkbox("Grid", &whirligig.showPlane);
        ImGui::Checkbox("Axes", &whirligig.showAxes);

        ImGui::Separator();
    }
    if (sceneKind == SceneKind::Fog) {
        fogScene.DrawUI();
    }

    ImGui::Separator();
    ImGui::Text("Frame %.3f ms (%.1f FPS)",
        dt * 1000.0f,
        dt > 0 ? (1.0f / dt) : 0.0f);

    ImGui::End();
}

void GRoad::MoveToNextFrame() {
    const UINT64 currentFenceValue = fenceValues[frameIndex];
    HR(commandQueue->Signal(fence.Get(), currentFenceValue), "Queue->Signal");
    frameIndex = swapChain->GetCurrentBackBufferIndex();
    if (fence->GetCompletedValue() < fenceValues[frameIndex]) {
        HR(fence->SetEventOnCompletion(fenceValues[frameIndex], fenceEvent), "Fence->SetEventOnCompletion");
        WaitForSingleObject(fenceEvent, INFINITE);
    }
    fenceValues[frameIndex] = currentFenceValue + 1;
}

void GRoad::WaitForGPU() {
    HR(commandQueue->Signal(fence.Get(), fenceValues[frameIndex]), "Queue->Signal(wait)");
    HR(fence->SetEventOnCompletion(fenceValues[frameIndex], fenceEvent), "Fence->SetEventOnCompletion(wait)");
    WaitForSingleObject(fenceEvent, INFINITE);
    fenceValues[frameIndex]++;
}

void GRoad::OnResize(UINT newW, UINT newH) {
    if (newW == width && newH == height) return;
    WaitForGPU();

    for (UINT i = 0; i < FrameCount; ++i) {
        renderTargets[i].Reset();
        fenceValues[i] = fenceValues[frameIndex];
    }

    DXGI_SWAP_CHAIN_DESC desc{};
    HR(swapChain->GetDesc(&desc), "SwapChain->GetDesc");
    HR(swapChain->ResizeBuffers(FrameCount, newW, newH, desc.BufferDesc.Format, desc.Flags),
        "SwapChain->ResizeBuffers");
    frameIndex = swapChain->GetCurrentBackBufferIndex();
    CreateRTVs();

    CreateDepth(newW, newH);
    CreateSceneColor(newW, newH);
    fogScene.OnResize(newW, newH);
    fogScene.CreateDescriptors(sceneColor.Get(), depth.Get());

    width = newW; height = newH;
    viewport.Width = float(width);
    viewport.Height = float(height);
    scissor.right = (LONG)width;
    scissor.bottom = (LONG)height;

    triangle.OnResize(newW, newH);
    whirligig.OnResize(newW, newH);
    jelly.OnResize(newW, newH);
}

void GRoad::Cleanup() {
    SaveSettings();

    if (commandQueue && fence) WaitForGPU();

    ImGui_ImplDX12_Shutdown();
    ImGui_ImplWin32_Shutdown();
    ImGui::DestroyContext();

    triangle.Cleanup();
    whirligig.Cleanup();
    jelly.Cleanup();

    fogScene.Cleanup();

    if (fenceEvent) CloseHandle(fenceEvent);
}
