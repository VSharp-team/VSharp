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
void DestroyProbes();
}
#endif // PROBES_H_
