#include "threadTracker.h"
#include "profilerState.h"

using namespace vsharp;

void ThreadTracker::trackCurrentThread() {
    LOG(tout << "<<Thread tracked>>");
    stackBalances->store(0);
    inFilterMapping->store(0);
}

void ThreadTracker::stackBalanceUp() {
    profiler_assert(isCurrentThreadTracked());
    LOG(tout << "Stack up");
    stackBalances->update( [] (int value) {
        return value + 1;
    });
}

int ThreadTracker::stackBalance() {
    profiler_assert(isCurrentThreadTracked());
    int balance = stackBalances->load();
    profiler_assert(balance >= 0);
    return balance;
}

bool ThreadTracker::stackBalanceDown() {
    profiler_assert(isCurrentThreadTracked());
    LOG(tout << "Stack down");
    int newBalance = stackBalances->update([] (int value) {
        return value - 1;
    });
    profiler_assert(newBalance >= 0);
    return newBalance != 0;
}

bool ThreadTracker::isCurrentThreadTracked() {
    return stackBalances->exist();
}

void ThreadTracker::onCurrentThreadFinished() {
    profilerState->coverageTracker->invocationFinished();
    stackBalances->remove();
    inFilterMapping->remove();
    if (!profilerState->isPassiveRun) {
        threadIdMapping->remove();
    }
}

void ThreadTracker::loseCurrentThread() {
    profiler_assert(isCurrentThreadTracked());
    profiler_assert(stackBalances->load() == 0);
    profiler_assert(inFilterMapping->load() == 0);
    LOG(tout << "<<Thread lost>>" << std::endl);
    onCurrentThreadFinished();
}

void ThreadTracker::abortCurrentThread() {
    LOG(tout << "<<Thread aborted>>" << std::endl);
    onCurrentThreadFinished();
}

void ThreadTracker::unwindFunctionEnter(FunctionID functionId) {
    profiler_assert(isCurrentThreadTracked());
    profiler_assert(profilerState->isCorrectFunctionId(functionId));
    LOG(tout << "Unwind enter" << std::endl);
    unwindFunctionIds->storeOrUpdate(functionId);
}

void ThreadTracker::unwindFunctionLeave() {
    profiler_assert(isCurrentThreadTracked());
    auto functionId = unwindFunctionIds->load();
    unwindFunctionIds->remove();
    LOG(tout << "Unwind leave" << std::endl);
    if ((profilerState->collectMainOnly && profilerState->mainFunctionId != functionId)
        || profilerState->funcIdToMethodId.find(functionId) == profilerState->funcIdToMethodId.end()) return;
    profilerState->coverageTracker->addCoverage(0, ThrowLeave, profilerState->funcIdToMethodId[functionId]);
    if ((!isInFilter() || stackBalance() > 0) && !stackBalanceDown()) {
        // stack is empty; function left
        loseCurrentThread();
    }
}

void ThreadTracker::filterEnter() {
    profiler_assert(isCurrentThreadTracked());
    LOG(tout << "Filter enter" << std::endl);
    inFilterMapping->update( [] (int value) {
        return value + 1;
    });
}

void ThreadTracker::filterLeave() {
    profiler_assert(isCurrentThreadTracked());
    LOG(tout << "Filter leave" << std::endl);
    inFilterMapping->update( [] (int value) {
        return value - 1;
    });
}

bool ThreadTracker::isInFilter() {
    profiler_assert(isCurrentThreadTracked());
    return inFilterMapping->load() > 0;
}

void ThreadTracker::mapCurrentThread(int mapId) {
    threadIdMapping->store(mapId);
}

bool ThreadTracker::hasMapping() {
    return threadIdMapping->exist();
}

int ThreadTracker::getCurrentThreadMappedId() {
    return threadIdMapping->load();
}

std::vector<std::pair<ThreadID, int>> ThreadTracker::getMapping() {
    return threadIdMapping->items();
}

void ThreadTracker::clear() {
    threadIdMapping->clear();
    stackBalances->clear();
    unwindFunctionIds->clear();
    inFilterMapping->clear();
}

bool ThreadTracker::isPossibleStackOverflow() {
    return false;
}

ThreadTracker::ThreadTracker(ThreadInfo *threadInfo) {
    threadIdMapping = new ThreadStorage<int>(threadInfo);
    stackBalances = new ThreadStorage<int>(threadInfo);
    unwindFunctionIds = new ThreadStorage<FunctionID>(threadInfo);
    inFilterMapping = new ThreadStorage<int>(threadInfo);
}

ThreadTracker::~ThreadTracker() {
    clear();
    delete threadIdMapping;
    delete stackBalances;
    delete unwindFunctionIds;
    delete inFilterMapping;
}
