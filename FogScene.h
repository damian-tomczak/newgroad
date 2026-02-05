#pragma once

#include <cstdint>

#include <d3d12.h>
#include <dxgi1_6.h>

#include "DxUtils.h"

class FogScene {
public:
    void Init(ID3D12Device* device,
        ID3D12DescriptorHeap* srvHeap,
        UINT srvDescSize,
        UINT width,
        UINT height,
        ID3D12Resource* sceneColor,
        ID3D12Resource* depth);

    void CreateDescriptors(ID3D12Resource* sceneColor, ID3D12Resource* depth);
    void OnResize(UINT width, UINT height);
    void Draw(ID3D12GraphicsCommandList* commandList, UINT frameIndex);
    void DrawUI();
    void Cleanup();

private:
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

    ID3D12Device* device = nullptr;
    ID3D12DescriptorHeap* srvHeap = nullptr;
    UINT srvDescSize = 0;
    UINT width = 1;
    UINT height = 1;

    ComPtr<ID3D12RootSignature> fogRootSig;
    ComPtr<ID3D12PipelineState> fogPSO;

    ComPtr<ID3D12Resource> fogCB;
    uint8_t* fogCBMapped = nullptr;
    UINT fogCBStride = 0;

    bool fogEnabled = true;
    float fogDensity = 6.0f;
    float fogStart = 2.0f;
    float fogEnd = 25.0f;
    float fogColor[3] = { 0.6f, 0.7f, 0.9f };
    float nearZ = 0.1f;
    float farZ = 100.0f;
};
