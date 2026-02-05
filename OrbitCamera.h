#pragma once
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>
#include <DirectXMath.h>
#include <algorithm>
#include <cmath>

struct OrbitCamera
{
    OrbitCamera()
        : yaw(DirectX::XM_PIDIV4)
        , pitch(DirectX::XM_PIDIV4 * 0.5f)
        , dist(4.0f)
        , target{ 0.0f, 0.0f, 0.0f }
        , rotating(false)
        , panning(false)
        , zooming(false)
        , lastMouse{ 0,0 }
        , fovY(DirectX::XM_PIDIV4)
        , zNear(0.1f)
        , zFar(100.0f)
        , width(1280)
        , height(720)
    {
    }

    void SetViewport(UINT w, UINT h) {
        width = (w == 0 ? 1u : w);
        height = (h == 0 ? 1u : h);
    }

    void Reset(float _yaw, float _pitch, float _dist, DirectX::XMFLOAT3 _target) {
        yaw = _yaw;
        pitch = _pitch;
        dist = _dist;
        target = _target;
        rotating = false;
        panning = false;
        zooming = false;
        lastMouse = { 0,0 };
    }

    // RH jak w Whirligig
    DirectX::XMVECTOR GetEye() const {
        float cy = std::cos(yaw);
        float sy = std::sin(yaw);
        float cp = std::cos(pitch);
        float sp = std::sin(pitch);

        DirectX::XMFLOAT3 fwdF{ cp * cy, sp, cp * sy };
        DirectX::XMVECTOR fwd = DirectX::XMVector3Normalize(DirectX::XMLoadFloat3(&fwdF));
        DirectX::XMVECTOR tgt = DirectX::XMLoadFloat3(&target);

        DirectX::XMVECTOR offset = DirectX::XMVectorScale(fwd, dist);
        return DirectX::XMVectorSubtract(tgt, offset);
    }

    DirectX::XMVECTOR GetAt() const {
        return DirectX::XMLoadFloat3(&target);
    }

    DirectX::XMVECTOR GetForward() const {
        DirectX::XMVECTOR eye = GetEye();
        DirectX::XMVECTOR at = GetAt();
        return DirectX::XMVector3Normalize(DirectX::XMVectorSubtract(at, eye));
    }

    DirectX::XMVECTOR GetRight() const {
        DirectX::XMVECTOR fwd = GetForward();
        DirectX::XMVECTOR worldUp = DirectX::XMVectorSet(0, 1, 0, 0);
        return DirectX::XMVector3Normalize(DirectX::XMVector3Cross(fwd, worldUp));
    }

    DirectX::XMVECTOR GetUp() const {
        DirectX::XMVECTOR fwd = GetForward();
        DirectX::XMVECTOR right = GetRight();
        return DirectX::XMVector3Normalize(DirectX::XMVector3Cross(right, fwd));
    }

    DirectX::XMMATRIX ViewRH() const {
        return DirectX::XMMatrixLookAtRH(GetEye(), GetAt(), GetUp());
    }

    DirectX::XMMATRIX ProjRH() const {
        float aspect = (height == 0) ? 1.0f : float(width) / float(height);
        return DirectX::XMMatrixPerspectiveFovRH(fovY, aspect, zNear, zFar);
    }

    // camera-style input (jak Whirligig) + zoom MMB
    void OnMouseMessage(UINT msg, WPARAM wParam, LPARAM lParam)
    {
        auto mouseX = [](LPARAM lp)->int { return (int)(short)LOWORD(lp); };
        auto mouseY = [](LPARAM lp)->int { return (int)(short)HIWORD(lp); };
        auto wheelDelta = [](WPARAM wp)->short { return (short)HIWORD(wp); };

        int x = mouseX(lParam);
        int y = mouseY(lParam);

        switch (msg)
        {
        case WM_LBUTTONDOWN:
            rotating = true;
            lastMouse = { x,y };
            break;
        case WM_LBUTTONUP:
            rotating = false;
            break;

        case WM_RBUTTONDOWN:
            panning = true;
            lastMouse = { x,y };
            break;
        case WM_RBUTTONUP:
            panning = false;
            break;

            // --- nowy tryb: przybliżanie środkowym przyciskiem ---
        case WM_MBUTTONDOWN:
            zooming = true;
            lastMouse = { x,y };
            break;
        case WM_MBUTTONUP:
            zooming = false;
            break;

        case WM_MOUSEWHEEL:
        {
            // zoom kółkiem (było już)
            short delta = wheelDelta(wParam);
            float zoomFactor = std::exp(-(float)delta / 120.0f * 0.15f);
            dist *= zoomFactor;
            dist = std::clamp(dist, 0.5f, 50.0f);
        } break;

        case WM_MOUSEMOVE:
        {
            int dx = x - lastMouse.x;
            int dy = y - lastMouse.y;
            lastMouse = { x,y };

            if (rotating)
            {
                yaw += dx * 0.01f;
                pitch += dy * 0.01f;
                const float limit = DirectX::XM_PIDIV2 - 0.1f;
                pitch = std::clamp(pitch, -limit, limit);
            }
            else if (panning)
            {
                DirectX::XMVECTOR right = GetRight();
                DirectX::XMVECTOR upv = GetUp();

                float panScale = 0.002f * dist;
                DirectX::XMVECTOR dRight = DirectX::XMVectorScale(right, -dx * panScale);
                DirectX::XMVECTOR dUp = DirectX::XMVectorScale(upv, dy * panScale);
                DirectX::XMVECTOR deltaV = DirectX::XMVectorAdd(dRight, dUp);

                DirectX::XMVECTOR at = GetAt();
                DirectX::XMVECTOR newAt = DirectX::XMVectorAdd(at, deltaV);
                DirectX::XMStoreFloat3(&target, newAt);
            }
            else if (zooming)
            {
                // zoom przy przeciąganiu MMB w pionie
                float zoomFactor = std::exp(dy * 0.01f); // mysz w dół -> oddalenie
                dist *= zoomFactor;
                dist = std::clamp(dist, 0.5f, 50.0f);
            }
        } break;

        default: break;
        }
    }

    float yaw, pitch, dist;
    DirectX::XMFLOAT3 target;

    bool rotating;
    bool panning;
    bool zooming;      // <--- NOWE
    POINT lastMouse;

    float fovY;
    float zNear, zFar;
    UINT width, height;
};
