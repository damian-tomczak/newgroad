#pragma once
#ifndef NOMINMAX
#define NOMINMAX
#endif

#include <windows.h>
#include <d3d12.h>
#include <DirectXMath.h>
#include <array>
#include <vector>
#include <chrono>

#include "Scene.h"
#include "DxUtils.h"
#include "OrbitCamera.h"

using Microsoft::WRL::ComPtr;

struct JellyScene : public IScene
{
    JellyScene();
    ~JellyScene() override = default;

    void Init(ID3D12Device* device) override;
    void Render(ID3D12GraphicsCommandList* cl) override;
    void OnResize(UINT w, UINT h) override;
    void Cleanup() override;

    void ResetState();
    void OnMouseMessage(UINT msg, WPARAM wParam, LPARAM lParam);

    bool  running;

    bool  showJellyPoints;
    bool  showBezierSurface;
    bool  showControlCube;
    bool  showBoundingBox;
    bool  showAxialSprings;
    bool  showDiagonalSprings;

    // --- Deformowany obiekt ---
    bool  showDeformedObject;

    float fixedTimeStep;
    float simulationSpeed;

    float initialRandomVelocityRange;

    DirectX::XMFLOAT3 gravityAcceleration;
    float             collisionRestitutionCoefficient;
    int               applyRestitutionToWholeVelocity;

    float particleMass;
    float linearDampingCoefficient;
    float axialSpringStiffness;
    float shearSpringStiffness;

    bool  enableControlCubeCoupling;
    bool  enableSurfaceControlCubeCoupling;
    float controlCubeCornerStiffness;
    float controlCubeSurfaceStiffness;

    DirectX::XMFLOAT3 controlCubePosition;
    DirectX::XMFLOAT3 controlCubeEulerAnglesDeg;
    float             controlCubeEdgeLength;

    DirectX::XMFLOAT3 boundingBoxHalfExtent;

    int   pinchParticleIndex;
    float pinchDisplacement;
    int   punchParticleIndex;
    float punchVelocity;

private:
    static constexpr int N = 4;
    static constexpr int N3 = N * N * N;

    // Parametry siatki "kulki" (UV-sfera) w C=[0,1]^3
    static constexpr int OBJ_SLICES = 48;
    static constexpr int OBJ_STACKS = 24;

    struct Float3 {
        float x, y, z;
    };

    struct VertexPC {
        float p[3];
        float c[3];
    };

    struct VertexPN {
        float p[3];
        float n[3];
    };

    struct CB {
        DirectX::XMFLOAT4X4 mvp;
        DirectX::XMFLOAT3   lightDir;
        float               _pad0;
        DirectX::XMFLOAT3   baseColor;
        float               ambient;
    };

    std::array<Float3, N3> restUnitPositions;
    std::array<Float3, N3> particlePositions;
    std::array<Float3, N3> particleVelocities;

    OrbitCamera cam;
    UINT        width = 1280;
    UINT        height = 720;

    bool hasPreviousFrameTime;
    float simulationTimeAccumulator;
    std::chrono::steady_clock::time_point lastTime;

    ID3D12Device* device;

    ComPtr<ID3D12RootSignature> rootSig;
    ComPtr<ID3D12PipelineState> psoLines;
    ComPtr<ID3D12PipelineState> psoPoints;
    ComPtr<ID3D12PipelineState> psoSolid;

    ComPtr<ID3D12Resource> cb;
    CB* mapCB = nullptr;

    ComPtr<ID3D12Resource> vbJelly;
    D3D12_VERTEX_BUFFER_VIEW vbvJelly{};
    std::array<VertexPC, N3> jellyVerts{};
    VertexPC* mapJellyVB = nullptr;

    std::vector<uint32_t> jellyIdxAxial;
    std::vector<uint32_t> jellyIdxDiag;

    ComPtr<ID3D12Resource> ibJellyAxial;
    ComPtr<ID3D12Resource> ibJellyDiag;
    D3D12_INDEX_BUFFER_VIEW ibvJellyAxial{};
    D3D12_INDEX_BUFFER_VIEW ibvJellyDiag{};

    ComPtr<ID3D12Resource> vbCtrl;
    ComPtr<ID3D12Resource> ibCtrl;
    D3D12_VERTEX_BUFFER_VIEW vbvCtrl{};
    D3D12_INDEX_BUFFER_VIEW  ibvCtrl{};
    std::array<VertexPC, 8>  ctrlVerts{};
    VertexPC* mapCtrlVB = nullptr;
    std::vector<uint32_t>    ctrlIdx;

    ComPtr<ID3D12Resource> vbBound;
    ComPtr<ID3D12Resource> ibBound;
    D3D12_VERTEX_BUFFER_VIEW vbvBound{};
    D3D12_INDEX_BUFFER_VIEW  ibvBound{};
    std::array<VertexPC, 8>  boundVerts{};
    VertexPC* mapBoundVB = nullptr;
    std::vector<uint32_t>    boundIdx;

    int                      bezierTessellation;
    ComPtr<ID3D12Resource>   vbBezier;
    ComPtr<ID3D12Resource>   ibBezier;
    D3D12_VERTEX_BUFFER_VIEW vbvBezier{};
    D3D12_INDEX_BUFFER_VIEW  ibvBezier{};
    UINT                     bezierVtxCount = 0;
    VertexPN* mapBezierVB = nullptr;
    std::vector<uint32_t>    bezierIdx;

    // =========================================================
    // Deformowany obiekt (siatka trójkątów) w C=[0,1]^3
    // =========================================================
    std::vector<Float3>   objParam;   // (u,v,w) w [0,1]^3
    std::vector<uint32_t> objIdx;     // indeksy trójkątów

    ComPtr<ID3D12Resource> vbObj;
    ComPtr<ID3D12Resource> ibObj;
    VertexPN* mapObjVB = nullptr;

    D3D12_VERTEX_BUFFER_VIEW vbvObj{};
    D3D12_INDEX_BUFFER_VIEW  ibvObj{};
    uint32_t objVtxCount = 0;

private:
    void BuildRootSignature(ID3D12Device* device);
    void BuildPSO(ID3D12Device* device);
    void BuildGeometry(ID3D12Device* device);
    void CreateUploadBuffer(UINT byteSize,
        void** mappedPtr,
        ComPtr<ID3D12Resource>& res,
        const void* initialData);

    void UpdateDynamicVBs();
    void UpdateCB();

    int  Index(int i, int j, int k_) const;
    bool IsCorner(int i, int j, int k_) const;
    bool IsSurface(int i, int j, int k_) const;

    DirectX::XMMATRIX ControlRotation() const;
    Float3             MapRestToWorld(const Float3& rUnit) const;
    void               ControlCorners(Float3 out[8]) const;

    void ApplyInitialDisturbance();
    void StepOnce(float timeStep);
    void ApplyCollisions();

    static void Bernstein3(float t, float B[4]);
    static void Bernstein2(float t, float B[3]);
    Float3 EvalPatch(int face, float s, float t,
        Float3& dS, Float3& dT) const;

    void BuildBezierIndexBuffer();
    void UpdateBezierSurfaceVB();

    // Bezier volume + obiekt
    Float3 EvalVolume(float u, float v, float w) const;
    void   BuildDeformedObjectMesh();
    void   UpdateDeformedObjectVB();

    static Float3 F3(float x, float y, float z);
    static Float3 add(const Float3& a, const Float3& b);
    static Float3 sub(const Float3& a, const Float3& b);
    static Float3 mul(const Float3& a, float s);
    static float  dot(const Float3& a, const Float3& b);
    static float  len(const Float3& a);
    static Float3 cross3(const Float3& a, const Float3& b);
    static Float3 normalize3(const Float3& a);
};
