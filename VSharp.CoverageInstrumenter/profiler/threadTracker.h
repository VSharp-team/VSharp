#ifndef VSHARP_COVERAGEINSTRUMENTER_THREADTRACKER_H
#define VSHARP_COVERAGEINSTRUMENTER_THREADTRACKER_H

#include "threadStorage.h"

namespace vsharp {

class ThreadTracker {
private:
    ThreadStorage<int>* threadIdMapping;
    ThreadStorage<int>* stackBalances;
    ThreadStorage<FunctionID>* unwindFunctionIds;
    ThreadStorage<int>* inFilterMapping;

    void onCurrentThreadFinished();
public:
    void mapCurrentThread(int mapId);
    bool hasMapping();
    int getCurrentThreadMappedId();
    std::vector<std::pair<ThreadID, int>> getMapping();
    bool isCurrentThreadTracked();
    void trackCurrentThread();
    void loseCurrentThread();
    void abortCurrentThread();
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

    bool isPossibleStackOverflow();

    explicit ThreadTracker(ThreadInfo* threadInfo);
    ~ThreadTracker();
};

}
#endif //VSHARP_COVERAGEINSTRUMENTER_THREADTRACKER_H
