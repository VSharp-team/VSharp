#include "probes.h"
#include "memory.h"
#include "coverageTracker.h"
#include "threadTracker.h"
#include "profilerState.h"

using namespace vsharp;

ProbeCall::ProbeCall(INT_PTR methodAddr) {
    addr = methodAddr;
}

mdSignature ProbeCall::getSig() {
    ThreadID thread = profilerState->threadInfo->getCurrentThread();
    threadMappingLock.lock();
    auto pos = threadMapping.find(thread);
    if (pos == threadMapping.end()) {
        threadMappingLock.unlock();
        return 0;
    }
    threadMappingLock.unlock();
    return pos->second;
}

void ProbeCall::setSig(mdSignature sig) {
    ThreadID thread = profilerState->threadInfo->getCurrentThread();
    threadMappingLock.lock();
    threadMapping[thread] = sig;
    threadMappingLock.unlock();
}

CoverageProbes* vsharp::getProbes() {
    return &coverageProbes;
}

void vsharp::InitializeProbes() {
    auto covProbes = vsharp::getProbes();
    covProbes->Coverage          = new ProbeCall((INT_PTR) &Track_Coverage);
    covProbes->Branch            = new ProbeCall((INT_PTR) &Branch);
    covProbes->Enter             = new ProbeCall((INT_PTR) &Track_Enter);
    covProbes->EnterMain         = new ProbeCall((INT_PTR) &Track_EnterMain);
    covProbes->Leave             = new ProbeCall((INT_PTR) &Track_Leave);
    covProbes->LeaveMain         = new ProbeCall((INT_PTR) &Track_LeaveMain);
    covProbes->Finalize_Call     = new ProbeCall((INT_PTR) &Finalize_Call);
    covProbes->Call              = new ProbeCall((INT_PTR) &Track_Call);
    covProbes->Tailcall          = new ProbeCall((INT_PTR) &Track_Tailcall);
    covProbes->Stsfld            = new ProbeCall((INT_PTR) &Track_Stsfld);
    covProbes->Throw             = new ProbeCall((INT_PTR) &Track_Throw);
    LOG(tout << "probes initialized" << std::endl);
}

void vsharp::DestroyProbes() {
    auto covProbes = vsharp::getProbes();
    delete covProbes->Coverage;
    delete covProbes->Branch;
    delete covProbes->Enter;
    delete covProbes->EnterMain;
    delete covProbes->Leave;
    delete covProbes->LeaveMain;
    delete covProbes->Finalize_Call;
    delete covProbes->Call;
    delete covProbes->Tailcall;
    delete covProbes->Stsfld;
    delete covProbes->Throw;
    LOG(tout << "probes destroyed" << std::endl);
}

CoverageProbes vsharp::coverageProbes;

//region Probes declarations
void vsharp::Track_Coverage(OFFSET offset, int methodId) {
    if (!profilerState->threadTracker->isCurrentThreadTracked()) return;
    profilerState->printMethod("Track_Coverage", methodId);
    profilerState->coverageTracker->addCoverage(offset, TrackCoverage, methodId);
}

void vsharp::Track_Stsfld(OFFSET offset, int methodId) {
    if (!profilerState->threadTracker->isCurrentThreadTracked()) return;
    profilerState->printMethod("Track_Stsfld", methodId);
    profilerState->coverageTracker->addCoverage(offset, StsfldHit, methodId);
}

void vsharp::Branch(OFFSET offset, int methodId) {
    if (!profilerState->threadTracker->isCurrentThreadTracked()) return;
    profilerState->printMethod("Branch", methodId);
    profilerState->coverageTracker->addCoverage(offset, BranchHit, methodId);
}

void vsharp::Track_Call(OFFSET offset, int methodId) {
    if (!profilerState->threadTracker->isCurrentThreadTracked()) return;
    profilerState->printMethod("Track_Call", methodId);
    profilerState->coverageTracker->addCoverage(offset, Call, methodId);
}

void vsharp::Track_Tailcall(OFFSET offset, int methodId) {
    if (!profilerState->threadTracker->isCurrentThreadTracked()) return;
    profilerState->printMethod("Track_Tailcall", methodId);
    // popping frame before tailcall execution
    profilerState->threadTracker->stackBalanceDown();
    profilerState->coverageTracker->addCoverage(offset, Tailcall, methodId);
}

void vsharp::Track_Enter(OFFSET offset, int methodId, int isSpontaneous) {
    if (!profilerState->threadTracker->isCurrentThreadTracked()) return;
    profilerState->printMethod("Entered", methodId);
    if (profilerState->threadTracker->isPossibleStackOverflow()) {
        LOG(tout << "Possible stack overflow: " << methodId);
    }
    if (!profilerState->coverageTracker->isCollectMainOnly())
        profilerState->coverageTracker->addCoverage(offset, Enter, methodId);
    profilerState->threadTracker->stackBalanceUp();
}

void vsharp::Track_EnterMain(OFFSET offset, int methodId, int isSpontaneous) {
    if (profilerState->threadTracker->isCurrentThreadTracked()) {
        // Recursive enter
        profilerState->printMethod("Entered main (recursive)", methodId);
        profilerState->threadTracker->stackBalanceUp();
        profilerState->coverageTracker->addCoverage(offset, Enter, methodId);
        return;
    }
    profilerState->printMethod("Entered Main", methodId);
    profilerState->threadTracker->trackCurrentThread();
    profilerState->threadTracker->stackBalanceUp();
    profilerState->coverageTracker->addCoverage(offset, EnterMain, methodId);
}

void vsharp::Track_Leave(OFFSET offset, int methodId) {
    if (!profilerState->threadTracker->isCurrentThreadTracked()) return;
    profilerState->printMethod("Left", methodId);
    if (!profilerState->coverageTracker->isCollectMainOnly())
        profilerState->coverageTracker->addCoverage(offset, Leave, methodId);
    profilerState->threadTracker->stackBalanceDown();
}

void vsharp::Track_LeaveMain(OFFSET offset, int methodId) {
    if (!profilerState->threadTracker->isCurrentThreadTracked()) return;
    profilerState->printMethod("Left main", methodId);
    if (profilerState->threadTracker->stackBalanceDown()) {
        // first main frame has not yet been reached
        profilerState->coverageTracker->addCoverage(offset, Leave, methodId);
        return;
    }
    profilerState->coverageTracker->addCoverage(offset, LeaveMain, methodId);
    profilerState->threadTracker->loseCurrentThread();
}

void vsharp::Track_Throw(OFFSET offset, int methodId) {
    if (!profilerState->threadTracker->isCurrentThreadTracked()) return;
    profilerState->printMethod("Throw", methodId);
    profilerState->coverageTracker->addCoverage(offset, ThrowLeave, methodId);
}

void vsharp::Finalize_Call(OFFSET offset) {
    if (!profilerState->threadTracker->isCurrentThreadTracked()) return;
}
//endregion
