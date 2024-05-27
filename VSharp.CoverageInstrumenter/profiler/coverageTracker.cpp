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

void MethodInfo::Dispose() {
    delete moduleName;
    delete assemblyName;
}
//endregion

//region CoverageRecord
void CoverageRecord::serialize(std::vector<char>& buffer) const {
    serializePrimitive(offset, buffer);
    serializePrimitive(event, buffer);
    serializePrimitive(methodId, buffer);
    serializePrimitive(thread, buffer);
    serializePrimitive(timestamp, buffer);
}
//endregion

long long GetMicrosecondTime() {
    using namespace std::chrono;
    return duration_cast<microseconds>(system_clock::now().time_since_epoch()).count();
}

//region CoverageHistory
CoverageHistory::CoverageHistory(OFFSET offset, int methodId) {
    auto insertResult = visitedMethods.insert(methodId);
    if (insertResult.second) {
        LOG(tout << "Visit method: " << methodId);
    }
    auto record = new CoverageRecord({offset, EnterMain, profilerState->threadInfo->getCurrentThread(), methodId, GetMicrosecondTime()});
    records.push_back(record);
}

void CoverageHistory::addCoverage(OFFSET offset, CoverageEvent event, int methodId) {
    auto insertResult = visitedMethods.insert(methodId);
    if (insertResult.second) {
        LOG(tout << "Visit method: " << methodId);
    }
    auto record = new CoverageRecord({offset, event, profilerState->threadInfo->getCurrentThread(), methodId, GetMicrosecondTime()});
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
    for (auto r : records)
        delete r;
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

    serializePrimitive(threadId, buffer);

    if (coverage == nullptr) {
        LOG(tout << "Serialize empty coverage (aborted) for thread id: " << threadId);
        serializePrimitive(1, buffer);
    } else {
        LOG(tout << "Serialize coverage for thread id: " << threadId);
        serializePrimitive(0, buffer);
        coverage->serialize(buffer);
    }

    delete coverage;
    trackedCoverage->remove();

    serializedCoverageMutex.lock();
    serializedCoverageThreadIds.push_back(threadId);
    serializedCoverage.push_back(buffer);
    serializedCoverageMutex.unlock();
}

char* CoverageTracker::serializeCoverageReport(size_t* size) {
    collectedMethodsMutex.lock();
    serializedCoverageMutex.lock();
    printf("got locks, converting info\n");

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
    printf("converted methods: %lld\n", buffer.size());

    auto threadMapping = profilerState->threadTracker->getMapping();
    for (auto mapping: threadMapping) {
        auto threadId = mapping.second;
        if (std::count(serializedCoverageThreadIds.begin(), serializedCoverageThreadIds.end(), threadId) == 0) {
            auto coverageBuffer = std::vector<char>();
            // Thread may not enter main, but we still need empty coverage for this thread
            LOG(tout << "Serialize empty coverage (not entered) for thread id: " << threadId);
            serializePrimitive(threadId, coverageBuffer);
            serializePrimitive(1, coverageBuffer);
            serializedCoverageThreadIds.push_back(threadId);
            serializedCoverage.push_back(coverageBuffer);
        }
    }

    int coverageCount = static_cast<int>(serializedCoverage.size());
    serializePrimitive(static_cast<int> (coverageCount), buffer);
    LOG(tout << "Serialize coverage count: " << coverageCount);
    for (int i = 0; i < coverageCount; i++) {
        serializePrimitiveArray(&serializedCoverage[i][0], serializedCoverage[i].size(), buffer);
    }

    printf("converted reports: %lld\n", buffer.size());

    for (auto cov : trackedCoverage->items())
        delete cov.second;
    trackedCoverage->clear();
    serializedCoverage.clear();
    methodsToSerialize.clear();

    serializedCoverageMutex.unlock();
    collectedMethodsMutex.unlock();

    printf("cleared and unlocked data\n");

    *size = buffer.size();
    char* array = new char[*size];
    printf("successfully allocated\n");
    std::memcpy(array, &buffer[0], *size);

    printf("total bytes: %lld", *size);
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
    for (int i = 0; i < collectedMethods.size(); i++)
        collectedMethods[i].Dispose();
    collectedMethods.clear();
    delete trackedCoverage;
}

void CoverageTracker::invocationAborted() {
    auto abortedCov = trackedCoverage->load();
    delete abortedCov;
    trackedCoverage->update([](CoverageHistory *cov) {
        return (CoverageHistory*) nullptr;
    });
}
//endregion
