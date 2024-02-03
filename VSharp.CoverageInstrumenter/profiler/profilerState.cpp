#include "profilerState.h"
#include "probes.h"
#include <codecvt>
#include <locale>

using namespace vsharp;

ProfilerState* vsharp::profilerState;

static std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> conv16;

void ConvertToWCHAR(const char *str, std::u16string &result) {
    result = conv16.from_bytes(str);
}

bool IsEnvVarPresent(const char *name) {
    return std::getenv(name) != nullptr;
}

char *CheckEnvVarAndGet(const char *name) {
    auto str = std::getenv(name);
    profiler_assert(str);
    return str;
}

bool ProfilerState::isCorrectFunctionId(FunctionID id) {
    return incorrectFunctionId != id;
}

ProfilerState::ProfilerState(ICorProfilerInfo8 *corProfilerInfo) {
    isPassiveRun = IsEnvVarPresent("COVERAGE_TOOL_ENABLE_PASSIVE");
    isTestExpected = IsEnvVarPresent("COVERAGE_TOOL_EXPECT_TEST_SUITE");
    collectMainOnly = IsEnvVarPresent("COVERAGE_TOOL_INSTRUMENT_MAIN_ONLY");

    mainMethodInfo.moduleName = nullptr; // mainMethod is not yet specified

#ifdef _LOGGING
    const char *name = isPassiveRun ? "lastrun.log" : "lastcoverage.log";
    open_log(name);
#endif

    InitializeProbes();

    if (isPassiveRun) {
        LOG(tout << "WORKING IN PASSIVE MODE" << std::endl);
        passiveResultPath = CheckEnvVarAndGet("COVERAGE_TOOL_RESULT_NAME");
    }

    if (isTestExpected) {
        ReadTestAssemblies(CheckEnvVarAndGet("COVERAGE_TOOL_ASSEMBLY_PATHS_FILE"));
        LOG(tout << "RECEIVED ASSEMBLIES TO INSTRUMENT" << std::endl);
    }

    if (IsEnvVarPresent("COVERAGE_TOOL_SPECIFY_MAIN_METHOD")) {
        std::u16string assemblyNameU16;
        std::u16string moduleNameU16;

        ConvertToWCHAR(CheckEnvVarAndGet("COVERAGE_TOOL_METHOD_ASSEMBLY_NAME"), assemblyNameU16);
        ConvertToWCHAR(CheckEnvVarAndGet("COVERAGE_TOOL_METHOD_MODULE_NAME"), moduleNameU16);
        int mainToken = std::stoi(CheckEnvVarAndGet("COVERAGE_TOOL_METHOD_TOKEN"));

        size_t mainAssemblyNameLength = assemblyNameU16.size();
        auto* mainAssemblyName = new WCHAR[mainAssemblyNameLength];
        memcpy(mainAssemblyName, assemblyNameU16.data(), mainAssemblyNameLength * sizeof(WCHAR));

        size_t mainModuleNameLength = moduleNameU16.size();
        auto* mainModuleName = new WCHAR[mainModuleNameLength];
        memcpy(mainModuleName, moduleNameU16.data(), mainModuleNameLength * sizeof(WCHAR));

        mainMethodInfo = {
                (mdMethodDef) mainToken,
                (ULONG) mainAssemblyNameLength,
                mainAssemblyName,
                (ULONG) mainModuleNameLength,
                mainModuleName
        };
    }

    mainFunctionId = -1;
    threadInfo = new ThreadInfo(corProfilerInfo);
    threadTracker = new ThreadTracker(threadInfo);
    coverageTracker = new CoverageTracker(threadTracker, threadInfo, collectMainOnly);
}

void vsharp::ProfilerState::ReadTestAssemblies(const char *path)
{
    std::ifstream assembliesFile;
    assembliesFile.exceptions(std::ifstream::failbit | std::ifstream::badbit);

    try {
        assembliesFile.open(path, std::ios_base::in);
        std::string assembly;

        while (assembliesFile >> assembly)
            approvedAssemblies.push_back(assembly);

        assembliesFile.close();
    }
    catch (std::ifstream::failure e) {
        LOG(tout << "FAILURE DURING THE READ OF ASSEMBLIES FILE! TOTAL RETRIEVED: " << approvedAssemblies.size()
            << "\nCONTINUING WITH RETRIEVED DATA");
    }
}

void vsharp::ProfilerState::setEntryMain(char *assemblyName, int assemblyNameLength, char *moduleName, int moduleNameLength, int methodToken) {
    auto* wcharAssemblyName = new WCHAR[assemblyNameLength];
    memcpy(wcharAssemblyName, assemblyName, assemblyNameLength * sizeof(WCHAR));

    auto* wcharModuleName = new WCHAR[moduleNameLength];
    memcpy(wcharModuleName, moduleName, moduleNameLength * sizeof(WCHAR));

    mainMethodInfo = {
            (mdMethodDef) methodToken,
            (ULONG) assemblyNameLength,
            wcharAssemblyName,
            (ULONG) moduleNameLength,
            wcharModuleName
    };
}
