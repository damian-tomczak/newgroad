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

    OrbitCamera m_cam;
    UINT        width = 1280;
    UINT        height = 720;

    bool hasPreviousFrameTime;
    float simulationTimeAccumulator;
    std::chrono::steady_clock::time_point lastTime;

    ID3D12Device* m_device;

    ComPtr<ID3D12RootSignature> m_rootSig;
    ComPtr<ID3D12PipelineState> m_psoLines;
    ComPtr<ID3D12PipelineState> m_psoPoints;
    ComPtr<ID3D12PipelineState> m_psoSolid;

    ComPtr<ID3D12Resource> m_cb;
    CB* m_mapCB = nullptr;

    ComPtr<ID3D12Resource> m_vbJelly;
    D3D12_VERTEX_BUFFER_VIEW m_vbvJelly{};
    std::array<VertexPC, N3> m_jellyVerts{};
    VertexPC* m_mapJellyVB = nullptr;

    std::vector<uint32_t> m_jellyIdxAxial;
    std::vector<uint32_t> m_jellyIdxDiag;

    ComPtr<ID3D12Resource> m_ibJellyAxial;
    ComPtr<ID3D12Resource> m_ibJellyDiag;
    D3D12_INDEX_BUFFER_VIEW m_ibvJellyAxial{};
    D3D12_INDEX_BUFFER_VIEW m_ibvJellyDiag{};

    ComPtr<ID3D12Resource> m_vbCtrl;
    ComPtr<ID3D12Resource> m_ibCtrl;
    D3D12_VERTEX_BUFFER_VIEW m_vbvCtrl{};
    D3D12_INDEX_BUFFER_VIEW  m_ibvCtrl{};
    std::array<VertexPC, 8>  m_ctrlVerts{};
    VertexPC* m_mapCtrlVB = nullptr;
    std::vector<uint32_t>    m_ctrlIdx;

    ComPtr<ID3D12Resource> m_vbBound;
    ComPtr<ID3D12Resource> m_ibBound;
    D3D12_VERTEX_BUFFER_VIEW m_vbvBound{};
    D3D12_INDEX_BUFFER_VIEW  m_ibvBound{};
    std::array<VertexPC, 8>  m_boundVerts{};
    VertexPC* m_mapBoundVB = nullptr;
    std::vector<uint32_t>    m_boundIdx;

    int                      bezierTessellation;
    ComPtr<ID3D12Resource>   m_vbBezier;
    ComPtr<ID3D12Resource>   m_ibBezier;
    D3D12_VERTEX_BUFFER_VIEW m_vbvBezier{};
    D3D12_INDEX_BUFFER_VIEW  m_ibvBezier{};
    UINT                     m_bezierVtxCount = 0;
    VertexPN* m_mapBezierVB = nullptr;
    std::vector<uint32_t>    m_bezierIdx;

    // =========================================================
    // Deformowany obiekt (siatka trójkątów) w C=[0,1]^3
    // =========================================================
    std::vector<Float3>   m_objParam;   // (u,v,w) w [0,1]^3
    std::vector<uint32_t> m_objIdx;     // indeksy trójkątów

    ComPtr<ID3D12Resource> m_vbObj;
    ComPtr<ID3D12Resource> m_ibObj;
    VertexPN* m_mapObjVB = nullptr;

    D3D12_VERTEX_BUFFER_VIEW m_vbvObj{};
    D3D12_INDEX_BUFFER_VIEW  m_ibvObj{};
    uint32_t m_objVtxCount = 0;

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
