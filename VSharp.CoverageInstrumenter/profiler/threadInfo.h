#include "cor.h"
#include "corprof.h"

#ifndef VSHARP_COVERAGEINSTRUMENTER_THREADINFO_H
#define VSHARP_COVERAGEINSTRUMENTER_THREADINFO_H

class ThreadInfo {
private:
    ICorProfilerInfo8* corProfilerInfo;
public:
    explicit ThreadInfo(ICorProfilerInfo8* corProfilerInfo_);
    ThreadID getCurrentThread();
};

#endif //VSHARP_COVERAGEINSTRUMENTER_THREADINFO_H
