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
    auto value = std::getenv(name);
    return value != nullptr && value[0] == '1';
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

    // mainMethod is not yet specified
    mainMethodInfo.moduleName = nullptr;
    mainMethodInfo.assemblyName = nullptr;

    std::atomic_store(&shutdownInOrder, false);

#ifdef _LOGGING
    static const char *name = isPassiveRun ? "lastrun.log" : "lastcoverage.log";
    open_log(name);
#endif

    InitializeProbes();

    if (isPassiveRun) {
        LOG(tout << "WORKING IN PASSIVE MODE" << std::endl);
        passiveResultPath = CheckEnvVarAndGet("COVERAGE_TOOL_RESULT_NAME");
    }

    if (isTestExpected) {
        ReadAssembliesFile(CheckEnvVarAndGet("COVERAGE_TOOL_ASSEMBLY_PATHS_FILE"), &approvedAssemblies);
        ReadAssembliesFile(CheckEnvVarAndGet("COVERAGE_TOOL_MAIN_ASSEMBLY_PATHS_FILE"), &testAssemblies);
        LOG(tout << "RECEIVED ASSEMBLIES TO INSTRUMENT" << std::endl);
    }
    LOG(tout << approvedAssemblies.size() << " " << testAssemblies.size() << std::endl);

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

ProfilerState::~ProfilerState() {
    delete threadInfo;
    delete threadTracker;
    delete coverageTracker;
    DestroyProbes();
}

void vsharp::ProfilerState::ReadAssembliesFile(const char *path, std::vector<std::string> *assemblyList)
{
    std::ifstream assembliesFile;

    assembliesFile.open(path, std::ios_base::in);
    std::string assembly;
    profiler_assert(assembliesFile.good());

    while (getline(assembliesFile, assembly)) {
        assemblyList->push_back(assembly);
        LOG(tout << assembly << std::endl);
    }

    assembliesFile.close();
    LOG(tout << std::endl);
}

void vsharp::ProfilerState::setEntryMain(char *assemblyName, int assemblyNameLength, char *moduleName, int moduleNameLength, int methodToken) {
    auto* wcharAssemblyName = new WCHAR[assemblyNameLength];
    memcpy(wcharAssemblyName, assemblyName, assemblyNameLength * sizeof(WCHAR));

    auto* wcharModuleName = new WCHAR[moduleNameLength];
    memcpy(wcharModuleName, moduleName, moduleNameLength * sizeof(WCHAR));

    mainMethodInfo.Dispose(); // deleting previously allocated names
    mainMethodInfo = {
            (mdMethodDef) methodToken,
            (ULONG) assemblyNameLength,
            wcharAssemblyName,
            (ULONG) moduleNameLength,
            wcharModuleName
    };
}

void vsharp::ProfilerState::printMethod(std::string message, int methodId) {
    LOG(
        coverageTracker->collectedMethodsMutex.lock();
        auto method = coverageTracker->collectedMethods[methodId];
        auto wl = method.assemblyNameLength;
        auto ws = method.assemblyName;
        tout << message << ' ' << std::string(ws, ws + wl - 1) << '.' << method.methodName << std::endl;
        coverageTracker->collectedMethodsMutex.unlock();
    );
}
