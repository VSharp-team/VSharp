#include "threadInfo.h"
#include "logging.h"
#include "profilerDebug.h"

ThreadInfo::ThreadInfo(ICorProfilerInfo8* corProfilerInfo_) {
    corProfilerInfo = corProfilerInfo_;
}

ThreadID ThreadInfo::getCurrentThread() {
    return std::hash<std::thread::id>{}(std::this_thread::get_id());
}
