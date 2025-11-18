#pragma once

#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>
#include <d3d12.h>
#include <DirectXMath.h>
#include <chrono>

#include "Scene.h"
#include "DxUtils.h"

struct WhirligigScene : public IScene {
    WhirligigScene();
    virtual ~WhirligigScene() = default;

    void Init(ID3D12Device* device) override;
    void Render(ID3D12GraphicsCommandList* cl) override;
    void OnResize(UINT w, UINT h) override;
    void Cleanup() override;

    void OnMouseMessage(UINT msg, WPARAM wParam, LPARAM lParam);
    void ResetState();

    // public UI state
    bool  running;
    bool  showCube;
    bool  showDiagonal;
    bool  showTrajectory;
    bool  showPlane;
    bool  showAxes;
    bool  useGravity;

    float cubeSize;
    float density;
    float inflectionDeg;
    float omegaMag;
    float speed;
    float dt;

    static const UINT MaxTrajPoints = 5000;
    UINT trajLength;

private:
    // pipeline
    ComPtr<ID3D12PipelineState> pso;
    ComPtr<ID3D12RootSignature> rootSig;

    // cube geometry (attached at corner in world)
    ComPtr<ID3D12Resource> vbCube, ibCube;
    D3D12_VERTEX_BUFFER_VIEW vbvCube{};
    D3D12_INDEX_BUFFER_VIEW  ibvCube{};

    // diagonal line (2 vertices)
    ComPtr<ID3D12Resource> vbDiag;
    D3D12_VERTEX_BUFFER_VIEW vbvDiag{};

    // trajectory line strip (dynamic, world-space verts, ring buffer)
    struct TrajVertex { float p[3]; float c[3]; };
    ComPtr<ID3D12Resource> vbTraj;
    D3D12_VERTEX_BUFFER_VIEW vbvTraj{};
    UINT trajCount;
    UINT trajStart;
    TrajVertex* trajVerts;

    // grid & axes (world-space lines)
    ComPtr<ID3D12Resource> vbGrid;
    D3D12_VERTEX_BUFFER_VIEW vbvGrid{};
    UINT gridVertexCount;

    ComPtr<ID3D12Resource> vbAxes;
    D3D12_VERTEX_BUFFER_VIEW vbvAxes{};
    UINT axesVertexCount;

    // view size
    UINT width;
    UINT height;

    // --- CAD camera state (right-handed) ---
    float camYaw;
    float camPitch;
    float camDist;
    DirectX::XMFLOAT3 camTarget;

    bool  rotating;
    bool  panning;
    POINT lastMouse;

    // physics state
    float                 mass;
    DirectX::XMFLOAT3X3   I;
    DirectX::XMFLOAT3X3   Iinv;
    DirectX::XMFLOAT3     rCM;
    DirectX::XMVECTOR     Q;
    DirectX::XMVECTOR     omega;

    // gravity
    const float G;

    // time accumulator
    std::chrono::steady_clock::time_point lastTime;
    bool haveLast;

private:
    static DirectX::XMFLOAT3X3 Inverse3x3(const DirectX::XMFLOAT3X3& A);
    static DirectX::XMVECTOR   MulMat3Vec(const DirectX::XMFLOAT3X3& M, DirectX::FXMVECTOR v);

    DirectX::XMVECTOR TorqueBody(DirectX::FXMVECTOR q);
    DirectX::XMVECTOR fOmega(DirectX::FXMVECTOR w, DirectX::FXMVECTOR q);
    DirectX::XMVECTOR fQ(DirectX::FXMVECTOR w, DirectX::FXMVECTOR q);
    void RK4Step(float h);
    void PushTrajectoryPoint();

    void BuildRootSignature(ID3D12Device* device);
    void BuildPSO(ID3D12Device* device);
    void BuildGeometry(ID3D12Device* device);

    DirectX::XMVECTOR GetEye() const;
    DirectX::XMVECTOR GetAt() const;
    DirectX::XMVECTOR GetUp() const;
};
