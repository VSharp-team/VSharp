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

void vsharp::stackBalanceUp() {
    ThreadID thread = currentThread();
    getLock();
    auto pos = stackBalances.find(thread);
    if (pos == ::stackBalances.end())
        ::stackBalances[thread] = 1;
    else
        ::stackBalances[thread]++;
    freeLock();
}

bool vsharp::stackBalanceDown() {
    ThreadID thread = currentThread();
    getLock();
    auto pos = stackBalances.find(thread);
    if (pos == ::stackBalances.end()) FAIL_LOUD("stack balance down on thread without stack!")
    ::stackBalances[thread]--;
    auto newBalance = ::stackBalances[thread];
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

bool vsharp::isMainThread() {
    return currentThread() == ::mainThread;
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

