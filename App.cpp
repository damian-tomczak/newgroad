// App.cpp
#include "App.h"
#include <windowsx.h>
#include <algorithm>
#include <fstream>
#include <cstring>

#include <d3dcompiler.h>
#pragma comment(lib, "d3dcompiler.lib")

#include "imgui.h"
#include "backends/imgui_impl_win32.h"
#include "backends/imgui_impl_dx12.h"

using namespace std::chrono;

extern IMGUI_IMPL_API LRESULT ImGui_ImplWin32_WndProcHandler(HWND, UINT, WPARAM, LPARAM);

// -------------------- Helpers --------------------
static UINT Align256(UINT x) { return (x + 255u) & ~255u; }

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

static D3D12_CPU_DESCRIPTOR_HANDLE CpuAt(ID3D12DescriptorHeap* heap, UINT idx, UINT inc)
{
    auto h = heap->GetCPUDescriptorHandleForHeapStart();
    h.ptr += (UINT64)idx * inc;
    return h;
}

static D3D12_GPU_DESCRIPTOR_HANDLE GpuAt(ID3D12DescriptorHeap* heap, UINT idx, UINT inc)
{
    auto h = heap->GetGPUDescriptorHandleForHeapStart();
    h.ptr += (UINT64)idx * inc;
    return h;
}

static D3D12_RASTERIZER_DESC DefaultRaster()
{
    D3D12_RASTERIZER_DESC r{};
    r.FillMode = D3D12_FILL_MODE_SOLID;
    r.CullMode = D3D12_CULL_MODE_BACK;
    r.FrontCounterClockwise = FALSE;
    r.DepthBias = D3D12_DEFAULT_DEPTH_BIAS;
    r.DepthBiasClamp = D3D12_DEFAULT_DEPTH_BIAS_CLAMP;
    r.SlopeScaledDepthBias = D3D12_DEFAULT_SLOPE_SCALED_DEPTH_BIAS;
    r.DepthClipEnable = TRUE;
    r.MultisampleEnable = FALSE;
    r.AntialiasedLineEnable = FALSE;
    r.ForcedSampleCount = 0;
    r.ConservativeRaster = D3D12_CONSERVATIVE_RASTERIZATION_MODE_OFF;
    return r;
}

static D3D12_BLEND_DESC DefaultBlend()
{
    D3D12_BLEND_DESC b{};
    b.AlphaToCoverageEnable = FALSE;
    b.IndependentBlendEnable = FALSE;

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

static D3D12_DEPTH_STENCIL_DESC NoDepth()
{
    D3D12_DEPTH_STENCIL_DESC d{};
    d.DepthEnable = FALSE;
    d.DepthWriteMask = D3D12_DEPTH_WRITE_MASK_ZERO;
    d.DepthFunc = D3D12_COMPARISON_FUNC_ALWAYS;
    d.StencilEnable = FALSE;
    d.StencilReadMask = D3D12_DEFAULT_STENCIL_READ_MASK;
    d.StencilWriteMask = D3D12_DEFAULT_STENCIL_WRITE_MASK;
    return d;
}

// -------------------- Embedded Fog Post HLSL --------------------
static const char* kFogPostHLSL = R"(
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
    // Fullscreen triangle
    float2 p = (vid == 0) ? float2(-1, -1) :
               (vid == 1) ? float2(-1,  3) :
                            float2( 3, -1);
    VSOut o;
    o.pos = float4(p, 0, 1);
    return o;
}

// Typical D3D 0..1 depth for a standard perspective matrix
float LinearizeDepth(float d, float n, float f)
{
    // z = n*f / ( f - d*(f-n) )
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

// -------------------- Fog CB --------------------
struct FogCBData {
    float invSize[2];
    float pad0[2];
    float fogColor[3];
    float fogDensity;
    float fogStart;
    float fogEnd;
    float nearZ;
    float farZ;
};

// -------------------- GRoad --------------------
GRoad::GRoad(UINT w, UINT h)
    : m_hwnd(nullptr)
    , m_width(w)
    , m_height(h)
    , m_rtvDescriptorSize(0)
    , m_frameIndex(0)
    , m_fenceEvent(nullptr)
    , m_sceneKind(SceneKind::Jelly)
    , m_prev(steady_clock::now())
    , m_menuWidth(320.0f)
{
    m_dsv = {};
    m_viewport = {};
    m_scissor = {};
    m_sceneRtv = {};
    for (UINT i = 0; i < FrameCount; ++i) {
        m_renderTargets[i].Reset();
        m_commandAllocators[i].Reset();
        m_fenceValues[i] = 0;
    }
}

GRoad::~GRoad() {}

void GRoad::Run(HINSTANCE hInstance, int nCmdShow) {
    RegisterWindowClass(hInstance);
    CreateAppWindow(hInstance, nCmdShow);
    InitD3D();
    InitImGui();
    InitScenes();
    MainLoop();
    Cleanup();
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
        if ((m_sceneKind == SceneKind::Whirligig || m_sceneKind == SceneKind::Jelly) && ImGui::GetCurrentContext()) {
            ImGuiIO& io = ImGui::GetIO();
            bool imguiWantsMouse = io.WantCaptureMouse;
            if (!imguiWantsMouse) {
                int x = GET_X_LPARAM(lParam);
                if (x >= (int)m_menuWidth) {
                    if (m_sceneKind == SceneKind::Whirligig) m_whirligig.OnMouseMessage(msg, wParam, lParam);
                    else m_jelly.OnMouseMessage(msg, wParam, lParam);
                }
            }
        }
        if (m_sceneKind == SceneKind::Jelly && ImGui::GetCurrentContext()) {
            ImGuiIO& io = ImGui::GetIO();
            if (!io.WantCaptureMouse) {
                int x = (int)(short)LOWORD(lParam);
                if (x >= (int)m_menuWidth) {
                    m_jelly.OnMouseMessage(msg, wParam, lParam);
                }
            }
        }
        return 0;
    }
    case WM_MOUSEWHEEL:
    {
        if ((m_sceneKind == SceneKind::Whirligig || m_sceneKind == SceneKind::Jelly) && ImGui::GetCurrentContext()) {
            ImGuiIO& io = ImGui::GetIO();
            bool imguiWantsMouse = io.WantCaptureMouse;
            if (!imguiWantsMouse) {
                m_whirligig.OnMouseMessage(msg, wParam, lParam);
                m_jelly.OnMouseMessage(msg, wParam, lParam);
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
    RECT rc{ 0,0,(LONG)m_width,(LONG)m_height };
    AdjustWindowRect(&rc, WS_OVERLAPPEDWINDOW, FALSE);
    m_hwnd = CreateWindowEx(0, L"DX12ScenesWinClass", L"GRoad",
        WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT,
        rc.right - rc.left, rc.bottom - rc.top,
        nullptr, nullptr, hInstance, this);
    ShowWindow(m_hwnd, nCmdShow);
    UpdateWindow(m_hwnd);
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

    // Offscreen scene color target
    CreateSceneColor(m_width, m_height);

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

void GRoad::CreateRTVs() {
    D3D12_CPU_DESCRIPTOR_HANDLE h = m_rtvHeap->GetCPUDescriptorHandleForHeapStart();
    for (UINT i = 0; i < FrameCount; ++i) {
        HR(m_swapChain->GetBuffer(i, IID_PPV_ARGS(&m_renderTargets[i])), "SwapChain->GetBuffer");
        m_device->CreateRenderTargetView(m_renderTargets[i].Get(), nullptr, h);
        h.ptr += m_rtvDescriptorSize;
    }
}

void GRoad::CreateDepth(UINT w, UINT h) {
    m_depth.Reset();

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
    HR(m_device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &tex,
        D3D12_RESOURCE_STATE_DEPTH_WRITE, &clear, IID_PPV_ARGS(&m_depth)),
        "CreateCommittedResource(Depth)");

    D3D12_DEPTH_STENCIL_VIEW_DESC dsv{};
    dsv.Format = DXGI_FORMAT_D32_FLOAT;
    dsv.ViewDimension = D3D12_DSV_DIMENSION_TEXTURE2D;
    m_device->CreateDepthStencilView(m_depth.Get(), &dsv, m_dsv);
}

void GRoad::CreateSceneColor(UINT w, UINT h)
{
    m_sceneColor.Reset();

    if (!m_sceneRtvHeap) {
        D3D12_DESCRIPTOR_HEAP_DESC rtv{};
        rtv.Type = D3D12_DESCRIPTOR_HEAP_TYPE_RTV;
        rtv.NumDescriptors = 1;
        HR(m_device->CreateDescriptorHeap(&rtv, IID_PPV_ARGS(&m_sceneRtvHeap)), "CreateScene RTV heap");
    }
    m_sceneRtv = m_sceneRtvHeap->GetCPUDescriptorHandleForHeapStart();

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
    HR(m_device->CreateCommittedResource(&heap, D3D12_HEAP_FLAG_NONE, &tex,
        D3D12_RESOURCE_STATE_RENDER_TARGET, &clear, IID_PPV_ARGS(&m_sceneColor)),
        "CreateCommittedResource(SceneColor)");

    m_device->CreateRenderTargetView(m_sceneColor.Get(), nullptr, m_sceneRtv);
}

void GRoad::InitImGui() {
    IMGUI_CHECKVERSION();
    ImGui::CreateContext();
    ImGui::StyleColorsDark();

    m_srvDescSize = m_device->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);

    D3D12_DESCRIPTOR_HEAP_DESC heap{};
    heap.Type = D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV;
    heap.NumDescriptors = 16; // [0]=ImGui font, [1]=scene SRV, [2]=depth SRV
    heap.Flags = D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE;
    HR(m_device->CreateDescriptorHeap(&heap, IID_PPV_ARGS(&m_srvHeap)), "CreateDescriptorHeap(SRV)");

    auto cpu0 = m_srvHeap->GetCPUDescriptorHandleForHeapStart();
    auto gpu0 = m_srvHeap->GetGPUDescriptorHandleForHeapStart();

    ImGui_ImplWin32_Init(m_hwnd);
    ImGui_ImplDX12_Init(
        m_device.Get(), FrameCount,
        DXGI_FORMAT_R8G8B8A8_UNORM,
        m_srvHeap.Get(),
        cpu0, gpu0);

    CreatePostprocess();
    CreatePostprocessDescriptors();
}

void GRoad::InitScenes() {
    m_triangle.Init(m_device.Get());
    m_whirligig.Init(m_device.Get());
    m_jelly.Init(m_device.Get());
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

void GRoad::CreatePostprocess()
{
    // Root signature: CBV(b0) + SRV table(t0..t1) + static sampler(s0)
    D3D12_DESCRIPTOR_RANGE range{};
    range.RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_SRV;
    range.NumDescriptors = 2;
    range.BaseShaderRegister = 0;
    range.RegisterSpace = 0;
    range.OffsetInDescriptorsFromTableStart = 0;

    D3D12_ROOT_PARAMETER rp[2]{};

    rp[0].ParameterType = D3D12_ROOT_PARAMETER_TYPE_CBV;
    rp[0].Descriptor.ShaderRegister = 0;
    rp[0].Descriptor.RegisterSpace = 0;
    rp[0].ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    rp[1].ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
    rp[1].DescriptorTable.NumDescriptorRanges = 1;
    rp[1].DescriptorTable.pDescriptorRanges = &range;
    rp[1].ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    D3D12_STATIC_SAMPLER_DESC samp{};
    samp.Filter = D3D12_FILTER_MIN_MAG_MIP_LINEAR;
    samp.AddressU = samp.AddressV = samp.AddressW = D3D12_TEXTURE_ADDRESS_MODE_CLAMP;
    samp.ShaderRegister = 0;
    samp.RegisterSpace = 0;
    samp.ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL;

    D3D12_ROOT_SIGNATURE_DESC rs{};
    rs.NumParameters = 2;
    rs.pParameters = rp;
    rs.NumStaticSamplers = 1;
    rs.pStaticSamplers = &samp;
    rs.Flags = D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT;

    ComPtr<ID3DBlob> sig, err;
    HR(D3D12SerializeRootSignature(&rs, D3D_ROOT_SIGNATURE_VERSION_1, &sig, &err), "SerializeRootSignature");
    HR(m_device->CreateRootSignature(0, sig->GetBufferPointer(), sig->GetBufferSize(),
        IID_PPV_ARGS(&m_fogRootSig)), "CreateRootSignature");

    UINT flags = 0;
#if defined(_DEBUG)
    flags = D3DCOMPILE_DEBUG | D3DCOMPILE_SKIP_OPTIMIZATION;
#endif

    ComPtr<ID3DBlob> vs, ps;
    HRESULT hrv = D3DCompile(kFogPostHLSL, (UINT)std::strlen(kFogPostHLSL),
        "FogPostEmbedded", nullptr, nullptr, "VSMain", "vs_5_0", flags, 0, &vs, &err);
    if (FAILED(hrv)) {
        if (err) OutputDebugStringA((const char*)err->GetBufferPointer());
        HR(hrv, "Compile VS");
    }

    HRESULT hrp = D3DCompile(kFogPostHLSL, (UINT)std::strlen(kFogPostHLSL),
        "FogPostEmbedded", nullptr, nullptr, "PSMain", "ps_5_0", flags, 0, &ps, &err);
    if (FAILED(hrp)) {
        if (err) OutputDebugStringA((const char*)err->GetBufferPointer());
        HR(hrp, "Compile PS");
    }

    D3D12_GRAPHICS_PIPELINE_STATE_DESC pso{};
    pso.pRootSignature = m_fogRootSig.Get();
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

    HR(m_device->CreateGraphicsPipelineState(&pso, IID_PPV_ARGS(&m_fogPSO)), "Create fog PSO");

    // Constant buffer (double buffered)
    m_fogCBStride = Align256((UINT)sizeof(FogCBData));
    UINT totalSize = m_fogCBStride * FrameCount;

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

    HR(m_device->CreateCommittedResource(&up, D3D12_HEAP_FLAG_NONE, &buf,
        D3D12_RESOURCE_STATE_GENERIC_READ, nullptr, IID_PPV_ARGS(&m_fogCB)),
        "Create fog CB");

    HR(m_fogCB->Map(0, nullptr, (void**)&m_fogCBMapped), "Map fog CB");
}

void GRoad::CreatePostprocessDescriptors()
{
    if (!m_srvHeap || !m_sceneColor || !m_depth) return;

    // t0 = scene color at heap index 1
    D3D12_SHADER_RESOURCE_VIEW_DESC s0{};
    s0.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
    s0.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
    s0.ViewDimension = D3D12_SRV_DIMENSION_TEXTURE2D;
    s0.Texture2D.MipLevels = 1;
    m_device->CreateShaderResourceView(m_sceneColor.Get(), &s0, CpuAt(m_srvHeap.Get(), 1, m_srvDescSize));

    // t1 = depth (R32_FLOAT view of R32_TYPELESS resource) at heap index 2
    D3D12_SHADER_RESOURCE_VIEW_DESC s1{};
    s1.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
    s1.Format = DXGI_FORMAT_R32_FLOAT;
    s1.ViewDimension = D3D12_SRV_DIMENSION_TEXTURE2D;
    s1.Texture2D.MipLevels = 1;
    m_device->CreateShaderResourceView(m_depth.Get(), &s1, CpuAt(m_srvHeap.Get(), 2, m_srvDescSize));
}

void GRoad::DrawFogPostprocess()
{
    FogCBData cb{};
    cb.invSize[0] = 1.0f / (float)m_width;
    cb.invSize[1] = 1.0f / (float)m_height;

    cb.fogColor[0] = m_fogColor[0];
    cb.fogColor[1] = m_fogColor[1];
    cb.fogColor[2] = m_fogColor[2];

    cb.fogDensity = m_fogEnabled ? m_fogDensity : 0.0f;
    cb.fogStart = m_fogStart;
    cb.fogEnd = m_fogEnd;
    cb.nearZ = m_nearZ;
    cb.farZ = m_farZ;

    uint8_t* dst = m_fogCBMapped + (size_t)m_frameIndex * m_fogCBStride;
    std::memcpy(dst, &cb, sizeof(cb));

    D3D12_GPU_VIRTUAL_ADDRESS cbAddr =
        m_fogCB->GetGPUVirtualAddress() + (UINT64)m_frameIndex * (UINT64)m_fogCBStride;

    // SRV table starts at heap index 1: t0=scene, t1=depth
    auto srvGpu = GpuAt(m_srvHeap.Get(), 1, m_srvDescSize);

    m_commandList->SetPipelineState(m_fogPSO.Get());
    m_commandList->SetGraphicsRootSignature(m_fogRootSig.Get());
    m_commandList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
    m_commandList->SetGraphicsRootConstantBufferView(0, cbAddr);
    m_commandList->SetGraphicsRootDescriptorTable(1, srvGpu);
    m_commandList->DrawInstanced(3, 1, 0, 0);
}

void GRoad::RenderFrame() {
    HR(m_commandAllocators[m_frameIndex]->Reset(), "CmdAlloc->Reset");
    HR(m_commandList->Reset(m_commandAllocators[m_frameIndex].Get(), nullptr), "CmdList->Reset");

    // Scene pass: render into offscreen scene color + depth
    m_commandList->RSSetViewports(1, &m_viewport);
    m_commandList->RSSetScissorRects(1, &m_scissor);

    m_commandList->OMSetRenderTargets(1, &m_sceneRtv, FALSE, &m_dsv);

    const float clear[4] = { 0.06f, 0.07f, 0.10f, 1.0f };
    m_commandList->ClearRenderTargetView(m_sceneRtv, clear, 0, nullptr);
    m_commandList->ClearDepthStencilView(m_dsv, D3D12_CLEAR_FLAG_DEPTH, 1.0f, 0, 0, 0);

    if (m_sceneKind == SceneKind::Triangle) {
        m_triangle.Render(m_commandList.Get());
    }
    else if (m_sceneKind == SceneKind::Whirligig) {
        m_whirligig.Render(m_commandList.Get());
    }
    else {
        m_jelly.Render(m_commandList.Get());
    }

    // Prepare SRVs for postprocess
    Transition(m_commandList.Get(), m_sceneColor.Get(),
        D3D12_RESOURCE_STATE_RENDER_TARGET, D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE);

    Transition(m_commandList.Get(), m_depth.Get(),
        D3D12_RESOURCE_STATE_DEPTH_WRITE, D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE);

    // Backbuffer to RT for post + UI
    Transition(m_commandList.Get(), m_renderTargets[m_frameIndex].Get(),
        D3D12_RESOURCE_STATE_PRESENT, D3D12_RESOURCE_STATE_RENDER_TARGET);

    D3D12_CPU_DESCRIPTOR_HANDLE backRtv = m_rtvHeap->GetCPUDescriptorHandleForHeapStart();
    backRtv.ptr += m_frameIndex * m_rtvDescriptorSize;
    m_commandList->OMSetRenderTargets(1, &backRtv, FALSE, nullptr);

    // Shared heap for postprocess + ImGui
    ID3D12DescriptorHeap* heaps[] = { m_srvHeap.Get() };
    m_commandList->SetDescriptorHeaps(1, heaps);

    // Fog postprocess into backbuffer
    DrawFogPostprocess();

    // ImGui on top
    ImGui_ImplDX12_NewFrame();
    ImGui_ImplWin32_NewFrame();
    ImGui::NewFrame();
    DrawUI();
    ImGui::Render();
    ImGui_ImplDX12_RenderDrawData(ImGui::GetDrawData(), m_commandList.Get());

    // Restore resources for next frame
    Transition(m_commandList.Get(), m_sceneColor.Get(),
        D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE, D3D12_RESOURCE_STATE_RENDER_TARGET);

    Transition(m_commandList.Get(), m_depth.Get(),
        D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE, D3D12_RESOURCE_STATE_DEPTH_WRITE);

    // Backbuffer to present
    Transition(m_commandList.Get(), m_renderTargets[m_frameIndex].Get(),
        D3D12_RESOURCE_STATE_RENDER_TARGET, D3D12_RESOURCE_STATE_PRESENT);

    HR(m_commandList->Close(), "CmdList->Close");
    ID3D12CommandList* lists[] = { m_commandList.Get() };
    m_commandQueue->ExecuteCommandLists(1, lists);
    HR(m_swapChain->Present(1, 0), "SwapChain->Present");

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
    ImGui::SliderFloat("Menu width", &m_menuWidth, minWidth, maxWidth);

    const char* items[] = { "Triangle", "Whirligig", "Jelly" };
    int idx = static_cast<int>(m_sceneKind);

    if (ImGui::Combo("Scene", &idx, items, IM_ARRAYSIZE(items))) {
        idx = std::clamp(idx, 0, 2);
        m_sceneKind = static_cast<SceneKind>(idx);
    }

    if (m_sceneKind == SceneKind::Triangle)
    {
        ImGui::Separator();
        ImGui::Text("Triangle scene");
    }
    else if (m_sceneKind == SceneKind::Whirligig)
    {
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

        changed |= SliderFloatWithInput("Cube size [m]",
            &m_whirligig.cubeSize,
            0.2f, 2.0f);

        changed |= SliderFloatWithInput("Density [kg/m^3]",
            &m_whirligig.density,
            0.1f, 10.0f);

        changed |= SliderFloatWithInput("Inflection [deg]",
            &m_whirligig.inflectionDeg,
            0.0f, 90.0f);

        changed |= SliderFloatWithInput("|omega| [rad/s]",
            &m_whirligig.omegaMag,
            0.0f, 50.0f);

        int len = (int)m_whirligig.trajLength;
        changed |= SliderIntWithInput("Trajectory length [points]",
            &len,
            100,
            (int)WhirligigScene::MaxTrajPoints);
        m_whirligig.trajLength = (UINT)len;

        ImGui::Checkbox("Use gravity (g = 9.81 m/s^2)", &m_whirligig.useGravity);

        changed |= SliderFloatWithInput("Speed [× real time]",
            &m_whirligig.speed,
            1.0f, 100.0f);

        changed |= SliderFloatWithInput("dt [s]",
            &m_whirligig.dt,
            0.001f, 0.1f,
            "%.4f");

        if (changed) {
            m_whirligig.ResetState();
        }

        ImGui::Separator();
    }
    else if (m_sceneKind == SceneKind::Jelly)
    {
        ImGui::Separator();
        ImGui::Text("Jelly simulation");

        if (ImGui::Button("Start")) {
            m_jelly.running = true;
        }
        ImGui::SameLine();
        if (ImGui::Button("Pause")) {
            m_jelly.running = false;
        }
        ImGui::SameLine();
        if (ImGui::Button("Reset")) {
            m_jelly.running = false;
            m_jelly.ResetState();
        }

        ImGui::Separator();

        SliderFloatWithInput("delta",
            &m_jelly.fixedTimeStep,
            0.001f, 0.05f, "%.4f");

        SliderFloatWithInput("simulation_speed",
            &m_jelly.simulationSpeed,
            0.1f, 100.0f);

        {
            float oldRange = m_jelly.initialRandomVelocityRange;
            SliderFloatWithInput("distribution",
                &m_jelly.initialRandomVelocityRange,
                -30.0f, 30.0f);
            if (m_jelly.initialRandomVelocityRange != oldRange) {
                m_jelly.ResetState();
            }
        }

        ImGui::Separator();

        ImGui::Checkbox("Draw jelly points", &m_jelly.showJellyPoints);
        ImGui::Checkbox("Draw axial springs", &m_jelly.showAxialSprings);
        ImGui::Checkbox("Draw diagonal springs", &m_jelly.showDiagonalSprings);
        ImGui::Checkbox("Draw bezier", &m_jelly.showBezierSurface);
        ImGui::Checkbox("process_cube_constraints",
            &m_jelly.enableControlCubeCoupling);

        ImGui::Separator();

        ImGui::InputFloat3("Gravitation",
            (float*)&m_jelly.gravityAcceleration);

        SliderFloatWithInput("mu",
            &m_jelly.collisionRestitutionCoefficient,
            0.0f, 1.0f);

        ImGui::Text("Velocity after collision type");
        ImGui::RadioButton("one component",
            &m_jelly.applyRestitutionToWholeVelocity, 0);
        ImGui::RadioButton("whole vector",
            &m_jelly.applyRestitutionToWholeVelocity, 1);

        ImGui::Separator();

        SliderFloatWithInput("mass",
            &m_jelly.particleMass,
            0.01f, 20.0f);

        SliderFloatWithInput("damping coeff. (k)",
            &m_jelly.linearDampingCoefficient,
            0.0f, 50.0f);

        SliderFloatWithInput("c1",
            &m_jelly.axialSpringStiffness,
            0.0f, 300.0f);

        SliderFloatWithInput("c2",
            &m_jelly.shearSpringStiffness,
            0.0f, 300.0f);

        ImGui::Separator();

        ImGui::InputFloat3("control cube position",
            (float*)&m_jelly.controlCubePosition);

        ImGui::InputFloat3("control cube rotation (deg)",
            (float*)&m_jelly.controlCubeEulerAnglesDeg);

        SliderFloatWithInput("control cube depth on",
            &m_jelly.controlCubeEdgeLength,
            0.2f, 3.0f);

        ImGui::Separator();
    }

    // ---- Postprocess fog UI ----
    ImGui::Separator();
    ImGui::Text("Postprocess fog");
    ImGui::Checkbox("Enable fog", &m_fogEnabled);
    ImGui::ColorEdit3("Fog color", m_fogColor);
    ImGui::SliderFloat("Fog density", &m_fogDensity, 0.0f, 20.0f);
    ImGui::SliderFloat("Fog start", &m_fogStart, 0.0f, 200.0f);
    ImGui::SliderFloat("Fog end", &m_fogEnd, 0.01f, 500.0f);
    if (m_fogEnd < m_fogStart) m_fogEnd = m_fogStart + 0.01f;

    ImGui::SliderFloat("Near (proj)", &m_nearZ, 0.01f, 10.0f);
    ImGui::SliderFloat("Far (proj)", &m_farZ, 10.0f, 2000.0f);
    if (m_farZ <= m_nearZ + 0.01f) m_farZ = m_nearZ + 0.01f;

    ImGui::Separator();
    ImGui::Text("Frame %.3f ms (%.1f FPS)",
        dt * 1000.0f,
        dt > 0 ? (1.0f / dt) : 0.0f);

    ImGui::End();
}

void GRoad::MoveToNextFrame() {
    const UINT64 fence = m_fenceValues[m_frameIndex];
    HR(m_commandQueue->Signal(m_fence.Get(), fence), "Queue->Signal");
    m_frameIndex = m_swapChain->GetCurrentBackBufferIndex();
    if (m_fence->GetCompletedValue() < m_fenceValues[m_frameIndex]) {
        HR(m_fence->SetEventOnCompletion(m_fenceValues[m_frameIndex], m_fenceEvent), "Fence->SetEventOnCompletion");
        WaitForSingleObject(m_fenceEvent, INFINITE);
    }
    m_fenceValues[m_frameIndex] = fence + 1;
}

void GRoad::WaitForGPU() {
    HR(m_commandQueue->Signal(m_fence.Get(), m_fenceValues[m_frameIndex]), "Queue->Signal(wait)");
    HR(m_fence->SetEventOnCompletion(m_fenceValues[m_frameIndex], m_fenceEvent), "Fence->SetEventOnCompletion(wait)");
    WaitForSingleObject(m_fenceEvent, INFINITE);
    m_fenceValues[m_frameIndex]++;
}

void GRoad::OnResize(UINT newW, UINT newH) {
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
    CreateSceneColor(newW, newH);
    CreatePostprocessDescriptors();

    m_width = newW; m_height = newH;
    m_viewport.Width = float(m_width);
    m_viewport.Height = float(m_height);
    m_scissor.right = (LONG)m_width;
    m_scissor.bottom = (LONG)m_height;

    m_triangle.OnResize(newW, newH);
    m_whirligig.OnResize(newW, newH);
    m_jelly.OnResize(newW, newH);
}

void GRoad::Cleanup() {
    if (m_commandQueue && m_fence) WaitForGPU();

    ImGui_ImplDX12_Shutdown();
    ImGui_ImplWin32_Shutdown();
    ImGui::DestroyContext();

    m_triangle.Cleanup();
    m_whirligig.Cleanup();
    m_jelly.Cleanup();

    if (m_fogCB && m_fogCBMapped) {
        m_fogCB->Unmap(0, nullptr);
        m_fogCBMapped = nullptr;
    }

    if (m_fenceEvent) CloseHandle(m_fenceEvent);
}
