#include "coverageTracker.h"
#include "serialization.h"
#include "threadTracker.h"
#include "profilerState.h"

using namespace vsharp;

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
    auto record = new CoverageRecord({offset, EnterMain, profilerState->threadInfo->getCurrentThread(), methodId});
    records.push_back(record);
}

void CoverageHistory::addCoverage(OFFSET offset, CoverageEvent event, int methodId) {
    auto insertResult = visitedMethods.insert(methodId);
    LOG(
    if (insertResult.second) {
        tout << "Visit method: " << methodId;
    }
    );
    auto record = new CoverageRecord({offset, event, profilerState->threadInfo->getCurrentThread(), methodId});
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
CoverageTracker::CoverageTracker(ThreadTracker* threadTracker_,ThreadInfo* threadInfo, bool collectMainOnly_) {
    threadTracker = threadTracker_;
    collectMainOnly = collectMainOnly_;
    trackedCoverage = new ThreadStorage<CoverageHistory*>(threadInfo);
}

void CoverageTracker::addCoverage(UINT32 offset, CoverageEvent event, int methodId) {
    profiler_assert(threadTracker->isCurrentThreadTracked());
    bool mainOnly = isCollectMainOnly();
    if ((event == EnterMain && mainOnly || !mainOnly) && !trackedCoverage->exist()) {
        trackedCoverage->store(new CoverageHistory(offset, methodId));
    } else {
        trackedCoverage->load()->addCoverage(offset, event, methodId);
    }
}

void CoverageTracker::invocationFinished() {
    auto buffer = std::vector<char>();
    auto coverage = trackedCoverage->load();

    int threadId = 0;
    if (threadTracker->hasMapping()) {
        threadId = threadTracker->getCurrentThreadMappedId();
    }

    visitedMethodsMutex.lock();
    visitedMethods.insert(coverage->visitedMethods.begin(), coverage->visitedMethods.end());
    visitedMethodsMutex.unlock();

    LOG(tout << "Serialize thread id: " << threadId);
    serializePrimitive(threadId, buffer);

    if (coverage == nullptr) {
        serializePrimitive(1, buffer);
    } else {
        serializePrimitive(0, buffer);
        coverage->serialize(buffer);
    }



    trackedCoverage->remove();

    serializedCoverageMutex.lock();
    serializedCoverage.push_back(buffer);
    serializedCoverageMutex.unlock();
}

char* CoverageTracker::serializeCoverageReport(size_t* size) {
    int coverageCount = static_cast<int>(serializedCoverage.size());

    collectedMethodsMutex.lock();

    auto buffer = std::vector<char>();
    auto methodsToSerialize = std::vector<std::pair<int, MethodInfo>>();

    for (int i = 0; i < collectedMethods.size(); i++) {
        if (visitedMethods.count(i) > 0) {
            methodsToSerialize.emplace_back(i, collectedMethods[i]);
        }
    }

    LOG(tout << "Serialize methods count: " << methodsToSerialize.size());
    serializePrimitive(static_cast<int> (methodsToSerialize.size()), buffer);
    for (auto el: methodsToSerialize) {
        serializePrimitive(el.first, buffer);
        el.second.serialize(buffer);
    }

    serializePrimitive(static_cast<int> (coverageCount), buffer);
    LOG(tout << "Serialize coverage count: " << coverageCount);
    for (int i = 0; i < coverageCount; i++) {
        serializePrimitiveArray(&serializedCoverage[i][0], serializedCoverage[i].size(), buffer);
    }

    trackedCoverage->clear();
    serializedCoverage.clear();
    methodsToSerialize.clear();
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
    trackedCoverage->clear();
}

CoverageTracker::~CoverageTracker(){
    clear();
}

void CoverageTracker::invocationAborted() {
    trackedCoverage->update([](CoverageHistory *cov) {
        return (CoverageHistory*) nullptr;
    });
}
//endregion
