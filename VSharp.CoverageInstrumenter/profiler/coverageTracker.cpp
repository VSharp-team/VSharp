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

char* CoverageTracker::serializeCoverageReport(size_t* size) {
    auto coverage = trackedCoverage->items();
    auto threadMapping = threadTracker->getMapping();
    int coverageCount = static_cast<int>(coverage.size());

    trackedCoverage->clear();
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
        if (threadMapping.empty()) {
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
