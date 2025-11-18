#pragma once
#include <d3d12.h>

struct IScene {
    virtual ~IScene() = default;
    virtual void Init(ID3D12Device* device) = 0;
    virtual void Render(ID3D12GraphicsCommandList* cl) = 0;
    virtual void OnResize(UINT /*w*/, UINT /*h*/) {}
    virtual void Cleanup() = 0;
};
