cmake_minimum_required(VERSION 3.12)

project(VSharp.CoverageInstrumenter LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_COMPILER "clang")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-invalid-noreturn -Wno-pragma-pack -fPIC -fms-extensions")

if (APPLE)
    set(CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS "${CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS} -undefined dynamic_lookup")
endif()

set(CORECLR_PATH ./coreclr)

include_directories(.
    ${CORECLR_PATH}/pal/inc/rt
    ${CORECLR_PATH}/pal/prebuilt/inc
    ${CORECLR_PATH}/pal/inc
    ${CORECLR_PATH}/inc)

add_definitions(-D_MIDL_USE_GUIDDEF_)
add_definitions(-DBIT64 -DHOST_64BIT)
add_definitions(-DPAL_STDCPP_COMPAT)
add_definitions(-DNOMINMAX)
add_definitions(-DUNIX)

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
    instrumenter.cpp
    ILRewriter.cpp
    logging.cpp
    memory.cpp
    ${CORECLR_PATH}/pal/prebuilt/idl/corprof_i.cpp probes.cpp)

add_library(vsharpCoverage SHARED ${sources})

add_link_options(--unresolved-symbols=ignore-in-object-files)
