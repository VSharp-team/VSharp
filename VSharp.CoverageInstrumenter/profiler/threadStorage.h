#ifndef VSHARP_COVERAGEINSTRUMENTER_THREADSTORAGE_H
#define VSHARP_COVERAGEINSTRUMENTER_THREADSTORAGE_H

#include "cor.h"
#include "corprof.h"
#include "logging.h"
#include "profilerDebug.h"
#include <map>
#include <vector>
#include "threadInfo.h"


template <typename T> class ThreadStorage {
private:
    std::map<ThreadID, T> innerStorage;
    std::mutex innerStorageLock;
    ThreadInfo* threadInfo;

    bool exist_(ThreadID thread) {
        auto e = innerStorage.find(thread);
        return e != innerStorage.end();
    }

public:
    explicit ThreadStorage(ThreadInfo* threadInfo_) {
        innerStorage = std::map<ThreadID, T>();
        threadInfo = threadInfo_;
    }

    void store(T data) {
        ThreadID thread = threadInfo->getCurrentThread();
        innerStorageLock.lock();
        profiler_assert(!exist_(thread));
        innerStorage[thread] = data;
        innerStorageLock.unlock();
    }

    void storeOrUpdate(T data) {
        ThreadID thread = threadInfo->getCurrentThread();
        innerStorageLock.lock();
        innerStorage[thread] = data;
        innerStorageLock.unlock();
    }

    T load() {
        ThreadID thread = threadInfo->getCurrentThread();
        innerStorageLock.lock();
        profiler_assert(exist_(thread));
        T result = innerStorage[thread];
        innerStorageLock.unlock();
        return result;
    }

    T update(T (*f)(T)) {
        ThreadID thread = threadInfo->getCurrentThread();
        innerStorageLock.lock();
        profiler_assert(exist_(thread));
        auto result = f(innerStorage[thread]);
        innerStorage[thread] = result;
        innerStorageLock.unlock();
        return result;
    }

    size_t size() {
        innerStorageLock.lock();
        auto result = innerStorage.size();
        innerStorageLock.unlock();
        return result;
    }

    bool exist() {
        ThreadID thread = threadInfo->getCurrentThread();
        innerStorageLock.lock();
        auto e = innerStorage.find(thread);
        auto result = e != innerStorage.end();
        innerStorageLock.unlock();
        return result;
    }

    void remove() {
        ThreadID thread = threadInfo->getCurrentThread();
        innerStorageLock.lock();
        profiler_assert(exist_(thread));
        innerStorage.erase(thread);
        innerStorageLock.unlock();
    }

    void clear() {
        innerStorageLock.lock();
        innerStorage.clear();
        innerStorageLock.unlock();
    }

    std::vector<std::pair<ThreadID, T>> items() {
        auto result = std::vector<std::pair<ThreadID, T>>();
        innerStorageLock.lock();
        for (auto e: innerStorage) {
            result.emplace_back(e.first, e.second);
        }
        innerStorageLock.unlock();
        return result;
    }

    ~ThreadStorage() {
        clear();
    }
};

#endif //VSHARP_COVERAGEINSTRUMENTER_THREADSTORAGE_H
