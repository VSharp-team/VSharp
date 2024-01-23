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
    covProbes->Coverage = new ProbeCall((INT_PTR) &Track_Coverage);
    covProbes->Branch = new ProbeCall((INT_PTR) &Branch);
    covProbes->Enter = new ProbeCall((INT_PTR) &Track_Enter);
    covProbes->EnterMain = new ProbeCall((INT_PTR) &Track_EnterMain);
    covProbes->Leave = new ProbeCall((INT_PTR) &Track_Leave);
    covProbes->LeaveMain = new ProbeCall((INT_PTR) &Track_LeaveMain);
    covProbes->Finalize_Call = new ProbeCall((INT_PTR) &Finalize_Call);
    covProbes->Call = new ProbeCall((INT_PTR) &Track_Call);
    covProbes->Tailcall = new ProbeCall((INT_PTR) &Track_Tailcall);
    covProbes->Stsfld = new ProbeCall((INT_PTR) &Track_Stsfld);
    covProbes->Throw = new ProbeCall((INT_PTR) &Track_Throw);
    LOG(tout << "probes initialized" << std::endl);
}

CoverageProbes vsharp::coverageProbes;

//region Probes declarations
void vsharp::Track_Coverage(OFFSET offset, int methodId) {
    if (!profilerState->threadTracker->isCurrentThreadTracked()) return;
    LOG(tout << "Track_Coverage: method = " << methodId << ", offset = " << HEX(offset));
    profilerState->coverageTracker->addCoverage(offset, TrackCoverage, methodId);
}

void vsharp::Track_Stsfld(OFFSET offset, int methodId) {
    if (!profilerState->threadTracker->isCurrentThreadTracked()) return;
    LOG(tout << "Track_Stsfld: method = " << methodId << ", offset = " << HEX(offset));
    profilerState->coverageTracker->addCoverage(offset, StsfldHit, methodId);
}

void vsharp::Branch(OFFSET offset, int methodId) {
    if (!profilerState->threadTracker->isCurrentThreadTracked()) return;
    LOG(tout << "Branch: method = " << methodId << ", offset = " << HEX(offset));
    profilerState->coverageTracker->addCoverage(offset, BranchHit, methodId);
}

void vsharp::Track_Call(OFFSET offset, int methodId) {
    if (!profilerState->threadTracker->isCurrentThreadTracked()) return;
    LOG(tout << "Track_Call: method = " << methodId << ", offset = " << HEX(offset));
    profilerState->coverageTracker->addCoverage(offset, Call, methodId);
}

void vsharp::Track_Tailcall(OFFSET offset, int methodId) {
    if (!profilerState->threadTracker->isCurrentThreadTracked()) return;
    LOG(tout << "Track_Tailcall: method = " << methodId << ", offset = " << HEX(offset));
    // popping frame before tailcall execution
    profilerState->threadTracker->stackBalanceDown();
    profilerState->coverageTracker->addCoverage(offset, Tailcall, methodId);
}

void vsharp::Track_Enter(OFFSET offset, int methodId, int isSpontaneous) {
    if (!profilerState->threadTracker->isCurrentThreadTracked()) return;
    if (profilerState->threadTracker->isPossibleStackOverflow()) {
        LOG(tout << "Possible stack overflow: " << methodId);
    }
    LOG(tout << "Track_Enter: " << methodId);
    if (!profilerState->coverageTracker->isCollectMainOnly())
        profilerState->coverageTracker->addCoverage(offset, Enter, methodId);
    profilerState->threadTracker->stackBalanceUp();
}

void vsharp::Track_EnterMain(OFFSET offset, int methodId, int isSpontaneous) {
    if (profilerState->threadTracker->isCurrentThreadTracked()) {
        // Recursive enter
        LOG(tout << "(recursive) Track_EnterMain: " << methodId);
        profilerState->threadTracker->stackBalanceUp();
        return;
    }
    LOG(tout << "Track_EnterMain: " << methodId);
    profilerState->threadTracker->trackCurrentThread();
    profilerState->threadTracker->stackBalanceUp();
    profilerState->coverageTracker->addCoverage(offset, EnterMain, methodId);
}

void vsharp::Track_Leave(OFFSET offset, int methodId) {
    if (!profilerState->threadTracker->isCurrentThreadTracked()) return;
    LOG(tout << "Track_Leave: " << methodId);
    if (!profilerState->coverageTracker->isCollectMainOnly())
        profilerState->coverageTracker->addCoverage(offset, Leave, methodId);
    profilerState->threadTracker->stackBalanceDown();
}

void vsharp::Track_LeaveMain(OFFSET offset, int methodId) {
    if (!profilerState->threadTracker->isCurrentThreadTracked()) return;
    profilerState->coverageTracker->addCoverage(offset, LeaveMain, methodId);
    LOG(tout << "Track_LeaveMain: " << methodId);
    if (profilerState->threadTracker->stackBalanceDown()) {
        // first main frame is not yet reached
        return;
    }
    profilerState->threadTracker->loseCurrentThread();
}

void vsharp::Track_Throw(OFFSET offset, int methodId) {
    if (!profilerState->threadTracker->isCurrentThreadTracked()) return;
    LOG(tout << "Track_Throw: method = " << methodId << ", offset = " << HEX(offset));
    profilerState->coverageTracker->addCoverage(offset, Leave, methodId);
}

void vsharp::Finalize_Call(OFFSET offset) {
    if (!profilerState->threadTracker->isCurrentThreadTracked()) return;
}
//endregion
