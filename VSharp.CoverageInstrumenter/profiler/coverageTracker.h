#ifndef VSHARP_COVERAGEINSTRUMENTER_COVERAGETRACKER_H
#define VSHARP_COVERAGEINSTRUMENTER_COVERAGETRACKER_H

#include "logging.h"
#include "memory.h"
#include "threadTracker.h"
#include <vector>
#include <algorithm>
#include <mutex>

namespace vsharp {

enum CoverageEvent {
    EnterMain,
    Enter,
    LeaveMain,
    Leave,
    BranchHit,
    Call,
    Tailcall,
    TrackCoverage,
    StsfldHit,
    ThrowLeave,
};

struct MethodInfo {
    mdMethodDef token;
    ULONG assemblyNameLength;
    WCHAR *assemblyName;
    ULONG moduleNameLength;
    WCHAR *moduleName;
    std::string methodName;

    void serialize(std::vector<char>& buffer) const;

    // frees previously allocated resources for it; the object is not supposed to be used afterwards
    void Dispose();
};

struct CoverageRecord {
    OFFSET offset;
    CoverageEvent event;
    ThreadID thread;
    int methodId;
    long long timestamp;

    void serialize(std::vector<char>& buffer) const;
};

class CoverageHistory {
private:
    std::vector<CoverageRecord*> records{};
public:
    explicit CoverageHistory(OFFSET offset, int methodId);
    void addCoverage(OFFSET offset, CoverageEvent event, int methodId);
    void serialize(std::vector<char>& buffer) const;
    ~CoverageHistory();

    std::set<int> visitedMethods;
};

class CoverageTracker {

private:
    bool collectMainOnly;
    std::mutex visitedMethodsMutex;
    std::set<int> visitedMethods;
    ThreadStorage<CoverageHistory*>* trackedCoverage;
    ThreadTracker* threadTracker;
    std::mutex serializedCoverageMutex;
    std::vector<int> serializedCoverageThreadIds;
public:
    std::vector<std::vector<char>> serializedCoverage;
    std::mutex collectedMethodsMutex;
    std::vector<MethodInfo> collectedMethods;
    explicit CoverageTracker(ThreadTracker* threadTracker, ThreadInfo* threadInfo, bool collectMainOnly);
    bool isCollectMainOnly() const;
    void addCoverage(OFFSET offset, CoverageEvent event, int methodId);
    void invocationAborted();
    void invocationFinished();
    size_t collectMethod(MethodInfo info);
    char* serializeCoverageReport(size_t* size);
    void clear();
    ~CoverageTracker();
};

}
#endif //VSHARP_COVERAGEINSTRUMENTER_COVERAGETRACKER_H
