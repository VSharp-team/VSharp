#ifndef VSHARP_COVERAGEINSTRUMENTER_PROFILERSTATE_H
#define VSHARP_COVERAGEINSTRUMENTER_PROFILERSTATE_H

#include "threadTracker.h"
#include "coverageTracker.h"

namespace vsharp {

class ProfilerState {
private:
    static const FunctionID incorrectFunctionId = 0;
public:
    ThreadTracker* threadTracker;
    CoverageTracker* coverageTracker;
    ThreadInfo* threadInfo;

    bool isPassiveRun = false;
    bool collectMainOnly = true;
    bool isFinished = false;
    char *passiveResultPath = nullptr;
    MethodInfo mainMethodInfo;
    FunctionID mainFunctionId;

    bool isCorrectFunctionId(FunctionID id);

    void setEntryMain(char* assemblyName, int assemblyNameLength, char* moduleName, int moduleNameLength, int methodToken);

    explicit ProfilerState(ICorProfilerInfo8 *corProfilerInfo);
};

extern ProfilerState* profilerState;

}

#endif //VSHARP_COVERAGEINSTRUMENTER_PROFILERSTATE_H
