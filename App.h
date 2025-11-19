#pragma once

#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>
#include <d3d12.h>
#include <dxgi1_6.h>
#include <chrono>

#include "Scene.h"
#include "TriangleScene.h"
#include "WhirligigScene.h"
#include "DxUtils.h"

static const UINT FrameCount = 2;

class D3D12ScenesApp {
public:
    D3D12ScenesApp(UINT w, UINT h);
    ~D3D12ScenesApp();

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
    ComPtr<ID3D12Resource>         m_depth;
    D3D12_CPU_DESCRIPTOR_HANDLE    m_dsv;

    ComPtr<ID3D12CommandAllocator> m_commandAllocators[FrameCount];
    ComPtr<ID3D12GraphicsCommandList> m_commandList;

    ComPtr<ID3D12Fence>            m_fence;
    UINT64                         m_fenceValues[FrameCount];
    HANDLE                         m_fenceEvent;

    D3D12_VIEWPORT                 m_viewport;
    D3D12_RECT                     m_scissor;

    ComPtr<ID3D12DescriptorHeap>   m_imguiSrvHeap;

    enum class SceneKind { Triangle = 0, Whirligig = 1 };
    SceneKind                      m_sceneKind;
    TriangleScene                  m_triangle;
    WhirligigScene                 m_whirligig;

    std::chrono::steady_clock::time_point m_prev;

    float m_menuWidth;
};
