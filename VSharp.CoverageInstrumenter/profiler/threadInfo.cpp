#include "threadInfo.h"
#include "logging.h"
#include "profilerDebug.h"

ThreadInfo::ThreadInfo(ICorProfilerInfo8* corProfilerInfo_) {
    corProfilerInfo = corProfilerInfo_;
}

ThreadID ThreadInfo::getCurrentThread() {
//    ThreadID result;
//    profiler_assert(corProfilerInfo != nullptr);
//    HRESULT hr = corProfilerInfo->GetCurrentThreadID(&result);
//    if (hr != S_OK) {
//        LOG_ERROR(tout << "getting current thread failed with HRESULT = " << std::hex << hr);
//        std::abort();
//    }
    return std::hash<std::thread::id>{}(std::this_thread::get_id());
}
