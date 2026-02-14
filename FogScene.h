#pragma once

#include "Scene.h"
#include "DxUtils.h"

#include <vector>
#include <cstdint>

struct FogScene : public IScene {
    void Init(ID3D12Device* device) override;
    void Render(ID3D12GraphicsCommandList* cl) override;
    void OnResize(UINT w, UINT h) override;
    void Cleanup() override;

    float rx = 1.0f;
    float ry = 0.7f;
    float rz = 0.5f;
    float shininess = 64.0f;
    float rotX = 0.3f;
    float rotY = 0.4f;
    float zoom = 1.0f;
    float panX = 0.0f;
    float panY = 0.0f;
    float camZ = 4.0f;
    float viewSize = 1.6f;
    int renderScale = 1;

private:
    void CreatePipeline();
    void CreateOrResizeTexture(UINT w, UINT h);
    void RenderCPU();

private:
    ID3D12Device* device = nullptr;
    UINT width = 1;
    UINT height = 1;
    UINT texW = 1;
    UINT texH = 1;

    ComPtr<ID3D12RootSignature> rootSig;
    ComPtr<ID3D12PipelineState> pso;
    ComPtr<ID3D12DescriptorHeap> srvHeap;

    ComPtr<ID3D12Resource> texture;
    ComPtr<ID3D12Resource> textureUpload;
    uint8_t* uploadPtr = nullptr;
    UINT uploadRowPitch = 0;
    UINT64 uploadSize = 0;

    std::vector<uint8_t> cpuPixels;
};
