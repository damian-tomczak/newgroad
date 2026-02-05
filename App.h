// App.h
#pragma once

#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>
#include <d3d12.h>
#include <dxgi1_6.h>
#include <chrono>
#include <cstdint>

#include "Scene.h"
#include "DxUtils.h"

#include "TriangleScene.h"
#include "WhirligigScene.h"
#include "JellyScene.h"

static const UINT FrameCount = 2;

class GRoad {
public:
    GRoad(UINT w, UINT h);
    ~GRoad();

    void Run(HINSTANCE hInstance, int nCmdShow);

private:
    void RegisterWindowClass(HINSTANCE hInstance);
    void CreateAppWindow(HINSTANCE hInstance, int nCmdShow);

    void InitD3D();
    void CreateRTVs();
    void CreateDepth(UINT w, UINT h);

    void InitImGui();
    void InitScenes();

    void MainLoop();
    void RenderFrame();
    void DrawUI();

    void MoveToNextFrame();
    void WaitForGPU();
    void OnResize(UINT newW, UINT newH);
    void Cleanup();

    static LRESULT CALLBACK WndProcSetup(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
    static LRESULT CALLBACK WndProcThunk(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
    LRESULT WndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);

    // ---- Postprocess fog
    void CreateSceneColor(UINT w, UINT h);
    void CreatePostprocess();
    void CreatePostprocessDescriptors();
    void DrawFogPostprocess();

private:
    HWND m_hwnd;
    UINT m_width;
    UINT m_height;

    ComPtr<IDXGIFactory6>          m_factory;
    ComPtr<ID3D12Device>           m_device;
    ComPtr<ID3D12CommandQueue>     m_commandQueue;
    ComPtr<IDXGISwapChain3>        m_swapChain;
    ComPtr<ID3D12DescriptorHeap>   m_rtvHeap;
    ComPtr<ID3D12Resource>         m_renderTargets[FrameCount];
    UINT                           m_rtvDescriptorSize;
    UINT                           m_frameIndex;

    ComPtr<ID3D12DescriptorHeap>   m_dsvHeap;
    ComPtr<ID3D12Resource>         m_depth;   // typeless resource (R32_TYPELESS)
    D3D12_CPU_DESCRIPTOR_HANDLE    m_dsv;     // DSV view (D32_FLOAT)

    ComPtr<ID3D12CommandAllocator> m_commandAllocators[FrameCount];
    ComPtr<ID3D12GraphicsCommandList> m_commandList;

    ComPtr<ID3D12Fence>            m_fence;
    UINT64                         m_fenceValues[FrameCount];
    HANDLE                         m_fenceEvent;

    D3D12_VIEWPORT                 m_viewport;
    D3D12_RECT                     m_scissor;

    // ---- Shared shader-visible heap: ImGui + postprocess SRVs
    ComPtr<ID3D12DescriptorHeap>   m_srvHeap;
    UINT                           m_srvDescSize = 0;

    // ---- Offscreen scene render target (color)
    ComPtr<ID3D12DescriptorHeap>   m_sceneRtvHeap;
    ComPtr<ID3D12Resource>         m_sceneColor;
    D3D12_CPU_DESCRIPTOR_HANDLE    m_sceneRtv{};

    // ---- Postprocess fog pipeline
    ComPtr<ID3D12RootSignature>    m_fogRootSig;
    ComPtr<ID3D12PipelineState>    m_fogPSO;

    ComPtr<ID3D12Resource>         m_fogCB;
    uint8_t* m_fogCBMapped = nullptr;
    UINT                           m_fogCBStride = 0;

    // ---- Fog parameters (UI)
    bool  m_fogEnabled = true;
    float m_fogDensity = 6.0f;
    float m_fogStart = 2.0f;
    float m_fogEnd = 25.0f;
    float m_fogColor[3] = { 0.6f, 0.7f, 0.9f };

    // Must match your projection near/far (set these to your camera values)
    float m_nearZ = 0.1f;
    float m_farZ = 100.0f;

    enum class SceneKind : int { Triangle = 0, Whirligig = 1, Jelly = 2 };
    SceneKind                      m_sceneKind;
    TriangleScene                  m_triangle;
    WhirligigScene                 m_whirligig;
    JellyScene                     m_jelly;

    std::chrono::steady_clock::time_point m_prev;

    float m_menuWidth;
};
