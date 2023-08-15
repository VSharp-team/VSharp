#include "memory.h"
#include "logging.h"
#include <mutex>

using namespace vsharp;

ThreadID currentThreadNotConfigured() {
    throw std::logic_error("Current thread getter is not configured!");
}

std::function<ThreadID()> vsharp::currentThread(&currentThreadNotConfigured);
static std::map<ThreadID, int> vsharp::stackBalances;
static ThreadID vsharp::mainThread = 0;
static FunctionID mainFunctionId = incorrectFunctionId;

void vsharp::assertCorrectFunctionId(FunctionID id) {
    if (id == incorrectFunctionId) {
        FAIL_LOUD("Incorrect function id")
    }
}

void vsharp::setMainFunctionId(FunctionID id) {
    assertCorrectFunctionId(id);
    mainFunctionId = id;
}

bool vsharp::isMainFunction(FunctionID id) {
    assertCorrectFunctionId(mainFunctionId);
    assertCorrectFunctionId(id);
    return id == mainFunctionId;
}

bool vsharp::isMainThread() {
    return currentThread() == ::mainThread;
}

void vsharp::stackBalanceUp() {
    LOG(tout << "stackBalanceUp" << std::endl);
    ThreadID thread = currentThread();
    if (!isMainThread()) return;
    getLock();
    auto pos = stackBalances.find(thread);
    if (pos == ::stackBalances.end())
        ::stackBalances[thread] = 1;
    else
        ::stackBalances[thread]++;
    LOG(tout << "stackBalance[" << thread << "] = " << ::stackBalances[thread] << std::endl);
    freeLock();
}

bool vsharp::stackBalanceDown() {
    LOG(tout << "stackBalanceDown" << std::endl);
    ThreadID thread = currentThread();
    if (!isMainThread()) return true;
    getLock();
    auto pos = stackBalances.find(thread);
    if (pos == ::stackBalances.end()) FAIL_LOUD("stack balance down on thread without stack!")
    ::stackBalances[thread]--;
    auto newBalance = ::stackBalances[thread];
    LOG(tout << "stackBalance[" << thread << "] = " << ::stackBalances[thread] << std::endl);
    freeLock();
    return newBalance != 0;
}

void vsharp::emptyStacks() {
    getLock();
    ::stackBalances.empty();
    freeLock();
}

void vsharp::setMainThread() {
    ::mainThread = currentThread();
}

void vsharp::unsetMainThread() {
    ::mainThread = 0;
}

std::mutex mutex;

void vsharp::getLock() {
    mutex.lock();
}

void vsharp::freeLock() {
    mutex.unlock();
}
