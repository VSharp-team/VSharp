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

bool ProfilerState::isCorrectFunctionId(FunctionID id) {
    return incorrectFunctionId != id;
}

ProfilerState::ProfilerState(ICorProfilerInfo8 *corProfilerInfo) {
    const char* isPassive = std::getenv("COVERAGE_TOOL_ENABLE_PASSIVE");

#ifdef _LOGGING
    const char* name = isPassive == nullptr ? "lastrun.log" : "lastcoverage.log";
    open_log(name);
#endif

    InitializeProbes();
    if (isPassive != nullptr) {
        LOG(tout << "WORKING IN PASSIVE MODE" << std::endl);

        isPassiveRun = true;
        collectMainOnly = true;

        std::u16string assemblyNameU16;
        std::u16string moduleNameU16;

        ConvertToWCHAR(std::getenv("COVERAGE_TOOL_METHOD_ASSEMBLY_NAME"), assemblyNameU16);
        ConvertToWCHAR(std::getenv("COVERAGE_TOOL_METHOD_MODULE_NAME"), moduleNameU16);
        int mainToken = std::stoi(std::getenv("COVERAGE_TOOL_METHOD_TOKEN"));

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
        passiveResultPath = std::getenv("COVERAGE_TOOL_RESULT_NAME");
    }

    if (std::getenv("COVERAGE_TOOL_INSTRUMENT_MAIN_ONLY")) {
        collectMainOnly = true;
    }

    mainFunctionId = -1;
    threadInfo = new ThreadInfo(corProfilerInfo);
    threadTracker = new ThreadTracker(threadInfo);
    coverageTracker = new CoverageTracker(threadTracker, threadInfo, collectMainOnly);
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
