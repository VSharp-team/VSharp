#include "probes.h"
#include "memory.h"
#include "profiler_assert.h"

using namespace vsharp;

ProbeCall::ProbeCall(INT_PTR methodAddr) {
    addr = methodAddr;
}

mdSignature ProbeCall::getSig() {
    ThreadID thread = threadInfo->getCurrentThread();
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
    ThreadID thread = threadInfo->getCurrentThread();
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
CoverageTracker* vsharp::coverageTracker;

//region MethodInfo
void MethodInfo::serialize(std::vector<char>& buffer) const {
    serializePrimitive(token, buffer);
    serializePrimitive(assemblyNameLength, buffer);
    serializePrimitiveArray(assemblyName, assemblyNameLength, buffer);
    serializePrimitive(moduleNameLength, buffer);
    serializePrimitiveArray(moduleName, moduleNameLength, buffer);
}
//endregion

//region CoverageRecord
void CoverageRecord::serialize(std::vector<char>& buffer) const {
    serializePrimitive(offset, buffer);
    serializePrimitive(event, buffer);
    serializePrimitive(methodId, buffer);
    serializePrimitive(thread, buffer);
}
//endregion

//region CoverageHistory
CoverageHistory::CoverageHistory(OFFSET offset, int methodId) {
    auto insertResult = visitedMethods.insert(methodId);
    LOG(if (insertResult.second) {
       tout << "Visit method: " << methodId;
    });
    auto record = new CoverageRecord({offset, EnterMain, threadInfo->getCurrentThread(), methodId});
    records.push_back(record);
}

void CoverageHistory::addCoverage(OFFSET offset, CoverageEvent event, int methodId) {
    auto insertResult = visitedMethods.insert(methodId);
    LOG(
        if (insertResult.second) {
            tout << "Visit method: " << methodId;
        }
    );
    auto record = new CoverageRecord({offset, event, threadInfo->getCurrentThread(), methodId});
    records.push_back(record);
}

void CoverageHistory::serialize(std::vector<char>& buffer) const {
    serializePrimitive(static_cast<int> (records.size()), buffer);
    LOG(tout << "Serialize reports count: " << static_cast<int> (records.size()));
    for (auto r: records) {
        r->serialize(buffer);
    }
}

CoverageHistory::~CoverageHistory() {
    records.clear();
}
//endregion

//region CoverageTracker
CoverageTracker::CoverageTracker(bool collectMainOnly_) {
    collectMainOnly = collectMainOnly_;
}

void CoverageTracker::addCoverage(UINT32 offset, CoverageEvent event, int methodId) {
    profiler_assert(threadTracker->isCurrentThreadTracked());
    bool mainOnly = coverageTracker->isCollectMainOnly();
    if ((event == EnterMain && mainOnly || !mainOnly) && !trackedCoverage.exist()) {
        trackedCoverage.store(new CoverageHistory(offset, methodId));
    } else {
        trackedCoverage.load()->addCoverage(offset, event, methodId);
    }
}

char* CoverageTracker::serializeCoverageReport(size_t* size) {
    auto coverage = trackedCoverage.items();
    auto threadMapping = threadTracker->getMapping();
    int coverageCount = static_cast<int>(coverage.size());

    trackedCoverage.clear();
    collectedMethodsMutex.lock();

    auto buffer = std::vector<char>();
    auto methodsToSerialize = std::vector<std::pair<int, MethodInfo>>();
    auto visitedMethodsByAllThreads = std::set<int>();

    for (int i = 0; i < coverageCount; i++) {
        if (coverage[i].second != nullptr) {
            auto visitedByI = coverage[i].second->visitedMethods;
            visitedMethodsByAllThreads.insert(visitedByI.begin(), visitedByI.end());
        }
    }

    LOG(
        for (auto x: visitedMethodsByAllThreads) {
            tout << "Visited by all: " << x << std::endl;
        }
    );

    for (int i = 0; i < collectedMethods.size(); i++) {
        if (visitedMethodsByAllThreads.count(i) > 0) {
            methodsToSerialize.emplace_back(i, collectedMethods[i]);
        }
    }

    serializePrimitive(static_cast<int> (methodsToSerialize.size()), buffer);
    for (auto el: methodsToSerialize) {
        serializePrimitive(el.first, buffer);
        el.second.serialize(buffer);
    }

    serializePrimitive(static_cast<int> (coverageCount), buffer);
    LOG(tout << "Serialize coverage count: " << coverageCount);
    for (int i = 0; i < coverageCount; i++) {
        if (threadMapping.size() == 0) {
            serializePrimitive(0, buffer);
        } else {
            for (auto &j : threadMapping) {
                if (j.first == coverage[i].first) {
                    LOG(tout << "Serialize thread id: " << j.second);
                    serializePrimitive(j.second, buffer);
                    break;
                }
            }
        }
        if (coverage[i].second != nullptr) {
            LOG(tout << "Serialize coverage: " << coverage[i].first);
            serializePrimitive(0, buffer);
            coverage[i].second->serialize(buffer);
        } else {
            LOG(tout << "Serialize coverage (aborted): " << coverage[i].first);
            serializePrimitive(1, buffer);
        }
    }

    methodsToSerialize.clear();
    visitedMethodsByAllThreads.clear();
    collectedMethodsMutex.unlock();

    *size = buffer.size();
    char* array = new char[*size];
    std::memcpy(array, &buffer[0], *size);
    return array;
}

size_t CoverageTracker::collectMethod(MethodInfo info) {
    collectedMethodsMutex.lock();
    size_t result = collectedMethods.size();
    collectedMethods.push_back(info);
    collectedMethodsMutex.unlock();
    return result;
}

bool CoverageTracker::isCollectMainOnly() const {
    return collectMainOnly;
}

void CoverageTracker::clear()  {
    trackedCoverage.clear();
}

CoverageTracker::~CoverageTracker(){
    clear();
}

void CoverageTracker::invocationAborted() {
    trackedCoverage.update([](CoverageHistory *cov) {
        return (CoverageHistory*) nullptr;
    });
}
//endregion

//region Probes declarations
void vsharp::Track_Coverage(OFFSET offset, int methodId) {
    if (!threadTracker->isCurrentThreadTracked()) return;
    LOG(tout << "Track_Coverage: method = " << methodId << ", offset = " << HEX(offset));
    coverageTracker->addCoverage(offset, TrackCoverage, methodId);
}

void vsharp::Track_Stsfld(OFFSET offset, int methodId) {
    if (!threadTracker->isCurrentThreadTracked()) return;
    LOG(tout << "Track_Stsfld: method = " << methodId << ", offset = " << HEX(offset));
    coverageTracker->addCoverage(offset, StsfldHit, methodId);
}

void vsharp::Branch(OFFSET offset, int methodId) {
    if (!threadTracker->isCurrentThreadTracked()) return;
    LOG(tout << "Branch: method = " << methodId << ", offset = " << HEX(offset));
    coverageTracker->addCoverage(offset, BranchHit, methodId);
}

void vsharp::Track_Call(OFFSET offset, int methodId) {
    if (!threadTracker->isCurrentThreadTracked()) return;
    LOG(tout << "Track_Call: method = " << methodId << ", offset = " << HEX(offset));
    coverageTracker->addCoverage(offset, Call, methodId);
}

void vsharp::Track_Tailcall(OFFSET offset, int methodId) {
    if (!threadTracker->isCurrentThreadTracked()) return;
    LOG(tout << "Track_Tailcall: method = " << methodId << ", offset = " << HEX(offset));
    // popping frame before tailcall execution
    threadTracker->stackBalanceDown();
    coverageTracker->addCoverage(offset, Tailcall, methodId);
}

void vsharp::Track_Enter(OFFSET offset, int methodId, int isSpontaneous) {
    if (!threadTracker->isCurrentThreadTracked()) return;
    if (isPossibleStackOverflow()) {
        LOG(tout << "Possible stack overflow: " << methodId);
    }
    LOG(tout << "Track_Enter: " << methodId);
    if (!coverageTracker->isCollectMainOnly())
        coverageTracker->addCoverage(offset, Enter, methodId);
    threadTracker->stackBalanceUp();
}

void vsharp::Track_EnterMain(OFFSET offset, int methodId, int isSpontaneous) {
    if (threadTracker->isCurrentThreadTracked()) {
        // Recursive enter
        LOG(tout << "(recursive) Track_EnterMain: " << methodId);
        threadTracker->stackBalanceUp();
        return;
    }
    LOG(tout << "Track_EnterMain: " << methodId);
    threadTracker->trackCurrentThread();
    threadTracker->stackBalanceUp();
    coverageTracker->addCoverage(offset, EnterMain, methodId);
}

void vsharp::Track_Leave(OFFSET offset, int methodId) {
    if (!threadTracker->isCurrentThreadTracked()) return;
    LOG(tout << "Track_Leave: " << methodId);
    if (!coverageTracker->isCollectMainOnly())
        coverageTracker->addCoverage(offset, Leave, methodId);
    threadTracker->stackBalanceDown();
}

void vsharp::Track_LeaveMain(OFFSET offset, int methodId) {
    if (!threadTracker->isCurrentThreadTracked()) return;
    coverageTracker->addCoverage(offset, LeaveMain, methodId);
    LOG(tout << "Track_LeaveMain: " << methodId);
    if (threadTracker->stackBalanceDown()) {
        // first main frame is not yet reached
        return;
    }
    threadTracker->loseCurrentThread();
}

void vsharp::Track_Throw(OFFSET offset, int methodId) {
    if (!threadTracker->isCurrentThreadTracked()) return;
    LOG(tout << "Track_Throw: method = " << methodId << ", offset = " << HEX(offset));
    coverageTracker->addCoverage(offset, Leave, methodId);
}

void vsharp::Finalize_Call(OFFSET offset) {
    if (!threadTracker->isCurrentThreadTracked()) return;
}
//endregion