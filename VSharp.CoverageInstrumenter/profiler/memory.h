#ifndef MEMORY_H_
#define MEMORY_H_

#include "cor.h"
#include "corprof.h"
#include "logging.h"
#include "cstring"
#include "profiler_assert.h"
#include <map>
#include <set>
#include <vector>
#include <functional>
#include <shared_mutex>
#include <mutex>
#include <atomic>



typedef UINT_PTR ThreadID;

namespace vsharp {

template <typename T> void serializePrimitive(const T obj, std::vector<char>& v) {
    static_assert(std::is_fundamental<T>::value || std::is_enum<T>::value,"Can only serialize primitive objects.");
    auto size = v.size();
    v.resize(size + sizeof(T));
    std::memcpy(&v[size], &obj, sizeof(T));
}

template <typename T> void serializePrimitiveArray(const T *obj, size_t len, std::vector<char>& v) {
    static_assert(std::is_fundamental<T>::value || std::is_enum<T>::value,"Can only serialize primitive objects.");
    auto size = v.size();
    v.resize(size + sizeof(T) * len);
    std::memcpy(&v[size], obj, sizeof(T) * len);
}


class ThreadInfo {
private:
    ICorProfilerInfo8 *corProfilerInfo;
public:
    explicit ThreadInfo(ICorProfilerInfo8 *corProfilerInfo);
    ThreadID getCurrentThread();
};
extern ThreadInfo* threadInfo;


template <typename T> class ThreadStorage {
private:
    std::map<ThreadID, T> innerStorage;
    std::mutex innerStorageLock;

    bool exist_(ThreadID thread) {
        auto e = innerStorage.find(thread);
        return e != innerStorage.end();
    }

public:
    explicit ThreadStorage() {
        innerStorage = std::map<ThreadID, T>();
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

};

class ThreadTracker {
private:
    ThreadStorage<int> threadIdMapping;
    ThreadStorage<int> stackBalances;
    ThreadStorage<FunctionID> unwindFunctionIds;
    ThreadStorage<int> inFilterMapping;
public:
    void mapCurrentThread(int mapId);
    int getCurrentThreadMappedId();
    std::vector<std::pair<ThreadID, int>> getMapping();
    bool isCurrentThreadTracked();
    void trackCurrentThread();
    void loseCurrentThread();
    // returns 'true' if the stack is not empty
    bool stackBalanceDown();
    void stackBalanceUp();
    // returns current stack size
    int stackBalance();
    void clear();

    void unwindFunctionEnter(FunctionID functionId);
    void unwindFunctionLeave();
    void filterEnter();
    void filterLeave();
    bool isInFilter();
};

#define OFFSET UINT32
const size_t defaultStackLimitByteSize = 1000000;
extern size_t stackBottom;
extern ThreadTracker* threadTracker;
extern std::mutex shutdownLock;
static const FunctionID incorrectFunctionId = 0;
extern std::atomic<int> shutdownBlockingRequestsCount;

void dumpUncatchableException(const std::string& exceptionName);
bool isPossibleStackOverflow();
void setMainFunctionId(FunctionID id);
bool isMainFunction(FunctionID id);
}

#endif // MEMORY_H_