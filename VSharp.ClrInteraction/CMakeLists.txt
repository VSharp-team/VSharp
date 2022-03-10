cmake_minimum_required(VERSION 3.12)

project(VSharp.ClrInteraction LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-invalid-noreturn -Wno-pragma-pack -fPIC -fms-extensions")

if (APPLE)
    set(CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS "${CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS} -undefined dynamic_lookup")
endif()

set(CORECLR_PATH ../runtime/src/coreclr)

include_directories(.
    ${CORECLR_PATH}/pal/inc/rt
    ${CORECLR_PATH}/pal/prebuilt/inc
    ${CORECLR_PATH}/pal/inc
    ${CORECLR_PATH}/inc)

add_definitions(-D_MIDL_USE_GUIDDEF_)
add_definitions(-DBIT64 -DHOST_64BIT)
add_definitions(-DPAL_STDCPP_COMPAT)
add_definitions(-DNOMINMAX)

message("Build type: ${CMAKE_BUILD_TYPE}")
if(CMAKE_BUILD_TYPE MATCHES Debug)
    add_definitions(-D_LOGGING)
    add_definitions(-D_DEBUG)
    message("Logging enabled")
endif()

set(sources
    classFactory.cpp
    corProfiler.cpp
    dllmain.cpp
    logging.cpp
    instrumenter.cpp
    communication/protocol.cpp
    communication/unixFifoCommunicator.cpp
    memory/memory.cpp
    memory/stack.cpp
    memory/heap.cpp
    ${CORECLR_PATH}/pal/prebuilt/idl/corprof_i.cpp)

add_library(vsharpConcolic SHARED ${sources})

add_link_options(--unresolved-symbols=ignore-in-object-files)
