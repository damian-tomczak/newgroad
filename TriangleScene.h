#pragma once

#include "Scene.h"
#include "DxUtils.h"
#include <d3dcompiler.h>
#include <dxgi1_6.h>
#include <cstring>

struct TriangleScene : public IScene {
    TriangleScene() = default;
    virtual ~TriangleScene() = default;

    void Init(ID3D12Device* device) override;
    void Render(ID3D12GraphicsCommandList* cl) override;
    void Cleanup() override;

private:
    ComPtr<ID3D12PipelineState>    pso;
    ComPtr<ID3D12RootSignature>    rootSig;
    ComPtr<ID3D12Resource>         vb;
    D3D12_VERTEX_BUFFER_VIEW       vbv{};
};
