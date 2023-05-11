#ifndef PROBES_H_
#define PROBES_H_

#include "logging.h"
#include "memory/memory.h"
#include <vector>
#include <algorithm>
#include <mutex>

namespace vsharp {

class ThreadSignature {
    std::map<ThreadID, mdSignature> threadMapping;
    std::mutex mutex;

public:
    mdSignature getSig();
    void setSig(mdSignature sig);
};

enum CoverageEvents {
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

struct CoverageRecord {
    OFFSET offset;
    CoverageEvents event;
    CoverageRecord* next;
    ThreadID thread;
    int methodId;

    size_t sizeNode() const;
    size_t size() const;
    void serialize(char *&buffer) const;
};

class CoverageHistory {
private:
    std::set<int> visitedMethods;
    CoverageRecord *head;
    CoverageRecord *current;
public:
    explicit CoverageHistory(OFFSET offset, int methodId);
    void AddCoverage(OFFSET offset, CoverageEvents event, int methodId);
    size_t size() const;
    void serialize(char *&buffer) const;
    ~CoverageHistory();
};

struct MethodInfo {
    mdMethodDef token;
    ULONG assemblyNameLength;
    WCHAR *assemblyName;
    ULONG moduleNameLength;
    WCHAR *moduleName;

    size_t size() const;
    void serialize(char *&buffer) const;
};

/// ------------------------------ Commands ---------------------------

extern std::vector<MethodInfo> collectedMethods;
extern std::vector<CoverageHistory*> coverageHistory;
extern CoverageHistory *currentCoverage;
extern bool areProbesEnabled;
extern bool collectMainOnly;

void enableProbes();
void disableProbes();

void addCoverage(OFFSET offset, CoverageEvents event, int methodId);

void clearCoverageCollection();

void mainLeft();

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
    INT_PTR Track_Coverage_Addr;
    INT_PTR Track_Stsfld_Addr;
    INT_PTR Branch_Addr;
    INT_PTR Track_Enter_Addr;
    INT_PTR Track_EnterMain_Addr;
    INT_PTR Track_Leave_Addr;
    INT_PTR Track_LeaveMain_Addr;
    INT_PTR Finalize_Call_Addr;
    INT_PTR Track_Call_Addr;
    INT_PTR Track_Tailcall_Addr;
    INT_PTR Track_Throw_Addr;

    ThreadSignature Track_Coverage_Sig;
    ThreadSignature Track_Stsfld_Sig;
    ThreadSignature Branch_Sig;
    ThreadSignature Track_Enter_Sig;
    ThreadSignature Track_EnterMain_Sig;
    ThreadSignature Track_Leave_Sig;
    ThreadSignature Track_LeaveMain_Sig;
    ThreadSignature Finalize_Call_Sig;
    ThreadSignature Track_Call_Sig;
    ThreadSignature Track_Tailcall_Sig;
    ThreadSignature Track_Throw_Sig;
};

extern CoverageProbes coverageProbes;

CoverageProbes* getProbes();
void InitializeProbes();

}


#endif // PROBES_H_
