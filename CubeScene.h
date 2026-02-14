#pragma once

#include <chrono>
#include <d3d12.h>
#include <DirectXMath.h>

#include "Scene.h"
#include "DxUtils.h"

struct CubeScene : public IScene {
    CubeScene() = default;
    virtual ~CubeScene() = default;

    void Init(ID3D12Device* device) override;
    void Render(ID3D12GraphicsCommandList* cl) override;
    void OnResize(UINT w, UINT h) override;
    void Cleanup() override;

    float rotationSpeed = 1.0f;

private:
    struct Vertex {
        DirectX::XMFLOAT3 position;
        DirectX::XMFLOAT3 color;
    };

    struct alignas(256) CBData {
        DirectX::XMFLOAT4X4 mvp;
    };

    ComPtr<ID3D12PipelineState> pso;
    ComPtr<ID3D12RootSignature> rootSig;

    ComPtr<ID3D12Resource> vb;
    ComPtr<ID3D12Resource> ib;
    D3D12_VERTEX_BUFFER_VIEW vbv{};
    D3D12_INDEX_BUFFER_VIEW ibv{};

    ComPtr<ID3D12Resource> cb;
    uint8_t* cbMapped = nullptr;

    UINT width = 1280;
    UINT height = 720;
    std::chrono::steady_clock::time_point startTime = std::chrono::steady_clock::now();
};

