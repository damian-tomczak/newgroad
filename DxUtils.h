#pragma once
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>
#include <string>
#include <stdexcept>
#include <comdef.h>
#include <wrl.h>

using Microsoft::WRL::ComPtr;

std::string Narrow(LPCWSTR ws);

struct DxError : public std::runtime_error {
    HRESULT hr;
    DxError(const char* where, HRESULT h, const std::string& extra = {});
};

void HR(HRESULT hr, const char* where, const std::string& extra = {});
