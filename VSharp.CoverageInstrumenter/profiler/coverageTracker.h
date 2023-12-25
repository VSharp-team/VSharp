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
    StsfldHit
};

struct MethodInfo {
    mdMethodDef token;
    ULONG assemblyNameLength;
    WCHAR *assemblyName;
    ULONG moduleNameLength;
    WCHAR *moduleName;

    void serialize(std::vector<char>& buffer) const;
};

struct CoverageRecord {
    OFFSET offset;
    CoverageEvent event;
    ThreadID thread;
    int methodId;

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
    std::mutex collectedMethodsMutex;
    std::vector<MethodInfo> collectedMethods;
    std::mutex visitedMethodsMutex;
    std::set<int> visitedMethods;
    ThreadStorage<CoverageHistory*>* trackedCoverage;
    ThreadTracker* threadTracker;
    std::mutex serializedCoverageMutex;
    std::vector<std::vector<char>> serializedCoverage;
    std::vector<int> serializedCoverageThreadIds;
public:
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
