#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>
#include <exception>

#include "DxUtils.h"
#include "App.h"

#pragma comment(lib, "d3d12.lib")
#pragma comment(lib, "dxgi.lib")
#pragma comment(lib, "d3dcompiler.lib")

int WINAPI wWinMain(HINSTANCE hInstance, HINSTANCE, PWSTR, int nCmdShow) {
    try {
        GRoad app(1280, 720);
        app.Run(hInstance, nCmdShow);
    }
    catch (const DxError& e) {
        MessageBoxA(nullptr, e.what(), "DirectX 12 Error", MB_ICONERROR | MB_OK);
        return -1;
    }
    catch (const std::exception& e) {
        MessageBoxA(nullptr, e.what(), "Error", MB_ICONERROR | MB_OK);
        return -1;
    }
    return 0;
}

int main() {
    return wWinMain(GetModuleHandle(nullptr), nullptr, GetCommandLineW(), SW_SHOWDEFAULT);
}
