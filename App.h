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
    HWND hwnd;
    UINT width;
    UINT height;

    ComPtr<IDXGIFactory6>          factory;
    ComPtr<ID3D12Device>           device;
    ComPtr<ID3D12CommandQueue>     commandQueue;
    ComPtr<IDXGISwapChain3>        swapChain;
    ComPtr<ID3D12DescriptorHeap>   rtvHeap;
    ComPtr<ID3D12Resource>         renderTargets[FrameCount];
    UINT                           rtvDescriptorSize;
    UINT                           frameIndex;

    ComPtr<ID3D12DescriptorHeap>   dsvHeap;
    ComPtr<ID3D12Resource>         depth;   // typeless resource (R32_TYPELESS)
    D3D12_CPU_DESCRIPTOR_HANDLE    dsv;     // DSV view (D32_FLOAT)

    ComPtr<ID3D12CommandAllocator> commandAllocators[FrameCount];
    ComPtr<ID3D12GraphicsCommandList> commandList;

    ComPtr<ID3D12Fence>            fence;
    UINT64                         fenceValues[FrameCount];
    HANDLE                         fenceEvent;

    D3D12_VIEWPORT                 viewport;
    D3D12_RECT                     scissor;

    // ---- Shared shader-visible heap: ImGui + postprocess SRVs
    ComPtr<ID3D12DescriptorHeap>   srvHeap;
    UINT                           srvDescSize = 0;

    // ---- Offscreen scene render target (color)
    ComPtr<ID3D12DescriptorHeap>   sceneRtvHeap;
    ComPtr<ID3D12Resource>         sceneColor;
    D3D12_CPU_DESCRIPTOR_HANDLE    sceneRtv{};

    // ---- Postprocess fog pipeline
    ComPtr<ID3D12RootSignature>    fogRootSig;
    ComPtr<ID3D12PipelineState>    fogPSO;

    ComPtr<ID3D12Resource>         fogCB;
    uint8_t* fogCBMapped = nullptr;
    UINT                           fogCBStride = 0;

    // ---- Fog parameters (UI)
    bool  fogEnabled = true;
    float fogDensity = 6.0f;
    float fogStart = 2.0f;
    float fogEnd = 25.0f;
    float fogColor[3] = { 0.6f, 0.7f, 0.9f };

    // Must match your projection near/far (set these to your camera values)
    float nearZ = 0.1f;
    float farZ = 100.0f;

    enum class SceneKind : int { Triangle = 0, Whirligig = 1, Jelly = 2 };
    SceneKind                      sceneKind;
    TriangleScene                  triangle;
    WhirligigScene                 whirligig;
    JellyScene                     jelly;

    std::chrono::steady_clock::time_point prev;

    float menuWidth;
};
