cmake_minimum_required(VERSION 3.20)

project(rigidbody LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(IMGUI_DIR "${CMAKE_CURRENT_SOURCE_DIR}/third_party/imgui")

set(IMGUI_SOURCES
    ${IMGUI_DIR}/imgui.cpp
    ${IMGUI_DIR}/imgui_demo.cpp
    ${IMGUI_DIR}/imgui_draw.cpp
    ${IMGUI_DIR}/imgui_tables.cpp
    ${IMGUI_DIR}/imgui_widgets.cpp
    ${IMGUI_DIR}/backends/imgui_impl_win32.cpp
    ${IMGUI_DIR}/backends/imgui_impl_dx12.cpp
)

add_executable(rigidbody
    main.cpp
	App.h
	App.cpp
	DxUtils.h
	DxUtils.cpp
	Scene.h
	TriangleScene.h
	TriangleScene.cpp
	WhirligigScene.h
	WhirligigScene.cpp
    ${IMGUI_SOURCES}
)

target_include_directories(rigidbody
    PRIVATE
        ${IMGUI_DIR}
        ${IMGUI_DIR}/backends
)

if(MSVC)
    target_compile_options(rigidbody PRIVATE /W4 /permissive-)
    target_compile_definitions(rigidbody PRIVATE UNICODE _UNICODE)
else()
    target_compile_options(rigidbody PRIVATE -Wall -Wextra -Wpedantic)
endif()

if(WIN32)
    target_link_libraries(rigidbody PRIVATE d3d12 dxgi d3dcompiler)
endif()

source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES main.cpp)
source_group(TREE ${IMGUI_DIR} PREFIX "ThirdParty/imgui" FILES ${IMGUI_SOURCES})

