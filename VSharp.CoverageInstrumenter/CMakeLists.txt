cmake_minimum_required(VERSION 3.12)

project(VSharp.CoverageInstrumenter LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

if(UNIX)
    set(CMAKE_CXX_COMPILER "clang++")
else()
    set(CMAKE_CXX_COMPILER "cl")
endif()

if (APPLE)
    set(CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS "${CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS} -undefined dynamic_lookup")
#    add_link_options(--unresolved-symbols=ignore-in-object-files)
#    TODO: use this only for ARM
    add_compile_definitions(HOST_ARM64) # https://github.com/dotnet/runtime/issues/78286
endif()

set(CORECLR_PATH coreclr)
set(PROFILER_PATH profiler)

message("Build type: ${CMAKE_BUILD_TYPE}")

if((CMAKE_BUILD_TYPE MATCHES Debug) OR (CMAKE_BUILD_TYPE MATCHES DebugMemory))
    add_definitions(-D_LOGGING)
    add_definitions(-D_PROFILER_DEBUG) # Don't use -D_DEBUG https://github.com/dotnet/runtime/issues/11419
    message("Logging enabled")
endif()

if(CMAKE_BUILD_TYPE MATCHES DebugMemory)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -static-libsan -O1 -fsanitize=address -fno-omit-frame-pointer -fsanitize=address")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static-libsan -fsanitize=address")
    message("Sanitizing enabled, consider you set LD_PRELOAD DYLD_INSERT_LIBRARIES")
endif()

if(UNIX)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-invalid-noreturn -Wno-pragma-pack -fPIC -fms-extensions")

    add_definitions(-DUNIX)
    add_definitions(-DPAL_STDCPP_COMPAT)
    add_definitions(-D_MIDL_USE_GUIDDEF_)
    add_definitions(-DBIT64 -DHOST_64BIT)
    add_definitions(-DNOMINMAX)
    add_definitions(-DSOS_INCLUDE)

    include_directories(.
        ${CORECLR_PATH}/pal/inc/rt
        ${CORECLR_PATH}/pal/src/include
        ${CORECLR_PATH}/pal/prebuilt/inc
        ${CORECLR_PATH}/pal/inc
        ${CORECLR_PATH}/inc
    )
    set(sources
        ${PROFILER_PATH}/classFactory.cpp
        ${PROFILER_PATH}/corProfiler.cpp
        ${PROFILER_PATH}/dllmain.cpp
        ${PROFILER_PATH}/instrumenter.cpp
        ${PROFILER_PATH}/ILRewriter.cpp
        ${PROFILER_PATH}/logging.cpp
        ${PROFILER_PATH}/memory.cpp
        ${PROFILER_PATH}/probes.cpp
        ${CORECLR_PATH}/pal/prebuilt/idl/corprof_i.cpp
        unix/os.cpp
    )

    add_library(vsharpCoverage SHARED ${sources})
else()
    add_definitions(-DWIN)
    add_definitions(-DWIN32)
    add_definitions(-DHOST_WINDOWS)
    add_definitions(-DNOMINMAX)
    add_definitions(-D_WINDOWS)
    add_definitions(-D_USRDLL)
    add_definitions(-DSOS_INCLUDE)
    add_definitions(-DSOURCE_FORMATTING)
    include_directories(.
        ${CORECLR_PATH}/pal/prebuilt/inc
        ${CORECLR_PATH}/inc
    )
    set(sources
        ${PROFILER_PATH}/classFactory.cpp
        ${PROFILER_PATH}/corProfiler.cpp
        ${PROFILER_PATH}/dllmain.cpp
        ${PROFILER_PATH}/instrumenter.cpp
        ${PROFILER_PATH}/ILRewriter.cpp
        ${PROFILER_PATH}/logging.cpp
        ${PROFILER_PATH}/memory.cpp
        ${PROFILER_PATH}/probes.cpp
        ./win/os.cpp
        ./win/vsharpCoverage.def
    )
    set(DEPENDENCIES Advapi32.lib oleaut32.lib Ole32.lib)

    add_library(libvsharpCoverage SHARED ${sources})
endif()
