cmake_minimum_required(VERSION 3.12)

project(VSharp.Guide LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CORECLR_PATH ../runtime/src/coreclr)
set(PROFILER_PATH ../VSharp.ClrInteraction)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-invalid-noreturn -Wno-pragma-pack -fPIC -fms-extensions")

if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "AppleClang")
    set(CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS "${CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS} -undefined dynamic_lookup")
endif()

include_directories(.
        ${CORECLR_PATH}/pal/inc/rt
        ${CORECLR_PATH}/pal/prebuilt/inc
        ${CORECLR_PATH}/pal/inc
        ${CORECLR_PATH}/inc)

add_definitions(-DBIT64 -DPAL_STDCPP_COMPAT -DPLATFORM_UNIX -DHOST_64BIT)

message("Build type: ${CMAKE_BUILD_TYPE}")
if(CMAKE_BUILD_TYPE MATCHES Debug)
    add_definitions(-D_LOGGING)
    add_definitions(-D_DEBUG)
endif()

add_library(icsharpGuide SHARED
#        ${PROFILER_PATH}/classFactory.cpp
#        ${PROFILER_PATH}/corProfiler.cpp
#        ${PROFILER_PATH}/dllmain.cpp
#        ${PROFILER_PATH}/logging.cpp
        classFactory.cpp
        corProfiler.cpp
        dllmain.cpp
        logging.cpp
        communication/protocol.cpp
        instrumentation/instrumenter.cpp
        instrumentation/ilRewriter.cpp
        instrumentation/reflection.cpp
        probes.cpp
        ${CORECLR_PATH}/pal/prebuilt/idl/corprof_i.cpp)

add_link_options(--unresolved-symbols=ignore-in-object-files)
