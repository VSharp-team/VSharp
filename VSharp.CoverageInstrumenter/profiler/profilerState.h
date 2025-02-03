#ifndef VSHARP_COVERAGEINSTRUMENTER_PROFILERSTATE_H
#define VSHARP_COVERAGEINSTRUMENTER_PROFILERSTATE_H

#include "threadTracker.h"
#include "coverageTracker.h"
#include <set>

namespace vsharp {

class ProfilerState {
private:
    static const FunctionID incorrectFunctionId = 0;

    void ReadAssembliesFile(const char *path, std::vector<std::string> *assemblyList);
public:
    ThreadTracker* threadTracker;
    CoverageTracker* coverageTracker;
    ThreadInfo* threadInfo;

    bool isPassiveRun = false;
    bool collectMainOnly = false;
    bool isTestExpected = false;
    char *passiveResultPath = nullptr;
    std::vector<std::string> approvedAssemblies;
    std::vector<std::string> testAssemblies;

    // saves every FunctionID instrumented, and the corresponding MethodID within the coverage tool
    std::map<int, int> funcIdToMethodId;

    std::mutex finalizator;

    MethodInfo mainMethodInfo;
    FunctionID mainFunctionId;

    bool isCorrectFunctionId(FunctionID id);

    void setEntryMain(char* assemblyName, int assemblyNameLength, char* moduleName, int moduleNameLength, int methodToken);

    explicit ProfilerState(ICorProfilerInfo8 *corProfilerInfo);
    ~ProfilerState();

    void printMethod(std::string message, int methodId);
};

extern ProfilerState* profilerState;

}

#endif //VSHARP_COVERAGEINSTRUMENTER_PROFILERSTATE_H
