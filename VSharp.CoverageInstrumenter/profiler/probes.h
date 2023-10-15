#ifndef PROBES_H_
#define PROBES_H_

#include "logging.h"
#include "memory.h"
#include <vector>
#include <algorithm>
#include <mutex>

namespace vsharp {

class ProbeCall {
    std::map<ThreadID, mdSignature> threadMapping;
    std::mutex threadMappingLock;

public:
    INT_PTR addr;
    mdSignature getSig();
    void setSig(mdSignature sig);
    explicit ProbeCall(INT_PTR addr);
};

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
    ThreadStorage<CoverageHistory*> trackedCoverage;
public:
    explicit CoverageTracker(bool collectMainOnly);
    bool isCollectMainOnly() const;
    void addCoverage(OFFSET offset, CoverageEvent event, int methodId);
    void invocationAborted();
    size_t collectMethod(MethodInfo info);
    char* serializeCoverageReport(size_t* size);
    void clear();
    ~CoverageTracker();
};
/// ------------------------------ Commands ---------------------------


extern CoverageTracker* coverageTracker;

/// ------------------------------ Probes declarations ---------------------------

void Track_Coverage(OFFSET offset, int methodId);

void Track_Stsfld(OFFSET offset, int methodId);

void Branch(OFFSET offset, int methodId);

void Track_Call(OFFSET offset, int methodId);

void Track_Tailcall(OFFSET offset, int methodId);

void Track_Enter(OFFSET offset, int methodId, int isSpontaneous);

void Track_EnterMain(OFFSET offset, int methodId, int isSpontaneous);

void Track_Leave(OFFSET offset, int methodId);

void Track_LeaveMain(OFFSET offset, int methodId);

void Track_Throw(OFFSET offset, int methodId);

void Finalize_Call(OFFSET offset);

struct CoverageProbes {
    ProbeCall* Coverage;
    ProbeCall* Stsfld;
    ProbeCall* Branch;
    ProbeCall* Enter;
    ProbeCall* EnterMain;
    ProbeCall* Leave;
    ProbeCall* LeaveMain;
    ProbeCall* Finalize_Call;
    ProbeCall* Call;
    ProbeCall* Tailcall;
    ProbeCall* Throw;
};

extern CoverageProbes coverageProbes;

CoverageProbes* getProbes();
void InitializeProbes();

}
#endif // PROBES_H_
