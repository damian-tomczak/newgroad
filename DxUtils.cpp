#include "DxUtils.h"

std::string Narrow(LPCWSTR ws) {
    if (!ws) return {};
    int len = WideCharToMultiByte(CP_UTF8, 0, ws, -1, nullptr, 0, nullptr, nullptr);
    if (len <= 0) return {};
    std::string out;
    out.resize(len - 1);
    WideCharToMultiByte(CP_UTF8, 0, ws, -1, &out[0], len, nullptr, nullptr);
    return out;
}

DxError::DxError(const char* where, HRESULT h, const std::string& extra)
    : std::runtime_error(std::string(where) + " failed: " +
        Narrow(_com_error(h).ErrorMessage()) +
        (extra.empty() ? "" : std::string("\nDetails:\n") + extra)),
      hr(h) {
}

void HR(HRESULT hr, const char* where, const std::string& extra) {
    if (FAILED(hr)) throw DxError(where, hr, extra);
}
