cmake_minimum_required(VERSION 3.12)

project(VSharp.CoverageInstrumenter LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# ------------------ VARIABLES ------------------

# PATHS
set(CORECLR_PATH coreclr)
set(PROFILER_PATH profiler)
set(UNIX_PATH unix)
set(WINDOWS_PATH win)

# COMPILER
set(UNIX_COMPILER "clang++")
set(WINDOWS_COMPILER "cl")

# SOURCES
set(COMMON_SOURCES
    ${PROFILER_PATH}/api.cpp
    ${PROFILER_PATH}/classFactory.cpp
    ${PROFILER_PATH}/corProfiler.cpp
    ${PROFILER_PATH}/coverageTracker.cpp
    ${PROFILER_PATH}/dllmain.cpp
    ${PROFILER_PATH}/ILRewriter.cpp
    ${PROFILER_PATH}/instrumenter.cpp
    ${PROFILER_PATH}/logging.cpp
    ${PROFILER_PATH}/memory.cpp
    ${PROFILER_PATH}/probes.cpp
    ${PROFILER_PATH}/profilerState.cpp
    ${PROFILER_PATH}/threadTracker.cpp
    ${PROFILER_PATH}/threadInfo.cpp
)
set(UNIX_SOURCES
    ${CORECLR_PATH}/pal/prebuilt/idl/corprof_i.cpp
    ${UNIX_PATH}/os.cpp
)
set(WINDOWS_SOURCES
    ${WINDOWS_PATH}/os.cpp
    ${WINDOWS_PATH}/vsharpCoverage.def
)

# DIRECTORIES
set(COMMON_DIRECTORIES
    .
    ${CORECLR_PATH}/pal/prebuilt/inc
    ${CORECLR_PATH}/inc
)
set(UNIX_DIRECTORIES
    ${CORECLR_PATH}/pal/inc/rt
    ${CORECLR_PATH}/pal/src/include
    ${CORECLR_PATH}/pal/inc
)
set(WINDOWS_DIRECTORIES "")

# DEFINITIONS
set(COMMON_DEFINITIONS
    -DNOMINMAX
    -DSOS_INCLUDE
)
set(UNIX_DEFINITIONS
    -DUNIX
    -DPAL_STDCPP_COMPAT
    -D_MIDL_USE_GUIDDEF_
    -DBIT64
    -DHOST_64BIT
)
set(WINDOWS_DEFINITIONS
    -DWIN
    -DWIN32
    -DHOST_WINDOWS
    -D_WINDOWS
    -D_USRDLL
    -DSOURCE_FORMATTING
)

# DEPENDENCIES
set(COMMON_DEPENDENCIES "")
set(UNIX_DEPENDENCIES "")
set(WINDOWS_DEPENDENCIES
    Advapi32.lib
    oleaut32.lib
    Ole32.lib
)

# LIBRARY_NAME
set(UNIX_LIBRARY_NAME vsharpCoverage)
set(WINDOWS_LIBRARY_NAME libvsharpCoverage)

# CXX_FLAGS
set(COMMON_CXX_FLAGS "")
set(UNIX_CXX_FLAGS "-Wno-invalid-noreturn -Wno-pragma-pack -fPIC -fms-extensions")
set(WINDOWS_CXX_FLAGS "")

# TARGET_OS
if(UNIX)
    set(TARGET_OS UNIX)
else()
    set(TARGET_OS WINDOWS)
endif()


# ------------------ BUILD TYPE SPECIFIC ------------------

if((CMAKE_BUILD_TYPE MATCHES Debug) OR (CMAKE_BUILD_TYPE MATCHES DebugMemory))
    add_definitions(-D_LOGGING)
    add_definitions(-D_PROFILER_DEBUG) # Don't use -D_DEBUG https://github.com/dotnet/runtime/issues/11419
    message(WARNING "Logging enabled, this potentially leads to performance issue")
endif()

if(CMAKE_BUILD_TYPE MATCHES DebugMemory)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -static-libsan -O1 -fsanitize=address -fno-omit-frame-pointer -fsanitize=address")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static-libsan -fsanitize=address")
    message(WARNING "Sanitizing enabled, consider you set LD_PRELOAD DYLD_INSERT_LIBRARIES")
endif()

# ------------------ OS SPECIFIC ------------------

if (APPLE)
    set(CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS "${CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS} -undefined dynamic_lookup")
    # TODO: use this only for ARM
    add_compile_definitions(HOST_ARM64) # https://github.com/dotnet/runtime/issues/78286
    message(WARNING "TODO: On any Apple host used HOST_ARM64")
endif()

# ------------------ OS KIND SPECIFIC ------------------

set(CMAKE_CXX_COMPILER ${${TARGET_OS}_COMPILER})

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${COMMON_CXX_FLAGS} ${${TARGET_OS}_CXX_FLAGS}")

add_definitions(
        ${COMMON_DEFINITIONS}
        ${${TARGET_OS}_DEFINITIONS}
)
include_directories(
        ${COMMON_DIRECTORIES}
        ${${TARGET_OS}_DIRECTORIES}
)
set(SOURCES
        ${COMMON_SOURCES}
        ${${TARGET_OS}_SOURCES}
)

add_library(${${TARGET_OS}_LIBRARY_NAME} SHARED ${SOURCES})

# ------------------ CHECKS ------------------

# Compiler checks
function(COMPILER_CHECK)
    if(ARGV0 MATCHES CODE)
        set(TEST_CODE_PATH ${CMAKE_BINARY_DIR}/CMakeTmp/${ARGV1}.cpp)
        set(TEST_CODE ${ARGV2})
        file(
            WRITE
            ${TEST_CODE_PATH}
            "${TEST_CODE}"
        )
    else()
        set(TEST_CODE_PATH ${ARGV0})
    endif()

    try_compile(CMAKE_CXX_COMPILER_WORKS
        ${CMAKE_BINARY_DIR}
        ${TEST_CODE_PATH}
        OUTPUT_VARIABLE OUTPUT
    )
    if(NOT CMAKE_CXX_COMPILER_WORKS)
        message(STATUS "Compiler: ${CMAKE_CXX_COMPILER}")
        message(STATUS "Can't compile: ${TEST_CODE_PATH}")
        message(FATAL_ERROR "With output: \n ${OUTPUT}")
    endif()
endfunction()

COMPILER_CHECK(CODE "primitiveTest"
[=[
int main() {
    return 0;
}
]=]
)

# ------------------ INFO LOGS ------------------

get_property(INCLUDED_DIRECTORIES DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY INCLUDE_DIRECTORIES)
get_property(ADDED_COMPILE_DEFINITIONS DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY COMPILE_DEFINITIONS)

message(STATUS "Build type:    ${CMAKE_BUILD_TYPE}")
message(STATUS "OS kind:       ${TARGET_OS}")
message(STATUS "Compiler:      ${CMAKE_CXX_COMPILER}")
message(STATUS "Compile flags: ${CMAKE_CXX_FLAGS}")

foreach(src ${SOURCES})
    message(STATUS "Included source file: '${src}'")
endforeach()

foreach(dir ${INCLUDED_DIRECTORIES})
    message(STATUS "Included directory: '${dir}'")
endforeach()

foreach(def ${ADDED_COMPILE_DEFINITIONS})
    message(STATUS "Added definition: '${def}'")
endforeach()