cmake_minimum_required(VERSION 2.8.12)

project(VSharp.ClrInteraction LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CORECLR_PATH ../runtime/src/coreclr/src)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-invalid-noreturn -Wno-pragma-pack -fPIC -fms-extensions")

include_directories(.
    ${CORECLR_PATH}/pal/inc/rt
    ${CORECLR_PATH}/pal/prebuilt/inc
    ${CORECLR_PATH}/pal/inc
    ${CORECLR_PATH}/inc)

add_definitions(-DBIT64 -DPAL_STDCPP_COMPAT -DPLATFORM_UNIX)

message("Build type: ${CMAKE_BUILD_TYPE}")
if(CMAKE_BUILD_TYPE MATCHES Debug)
    add_definitions(-D_LOGGING)
    add_definitions(-D_DEBUG)
    message("Logging enabled")
endif()
add_definitions(-DHOST_64BIT)
add_definitions(-D_MIDL_USE_GUIDDEF_)

add_library(icsharpConcolic SHARED
    classFactory.cpp
    corProfiler.cpp
    dllmain.cpp
    logging.cpp
    instrumenter.cpp
    communication/unixFifoCommunicator.cpp
    communication/protocol.cpp
    memory/memory.cpp
    memory/stack.cpp
    memory/heap.cpp
    ${CORECLR_PATH}/pal/prebuilt/idl/corprof_i.cpp)
