#include "memory.h"
#include "stack.h"

using namespace vsharp;

ThreadID currentThreadNotConfigured() {
    throw std::logic_error("Current thread getter is not configured!");
}

std::function<ThreadID()> vsharp::currentThread(&currentThreadNotConfigured);

Storage vsharp::heap = Storage();

#ifdef _DEBUG
std::map<unsigned, const char*> vsharp::stringsPool;
int topStringIndex = 0;
#endif

ThreadID lastThreadID = 0;
Stack *currentStack = nullptr;

inline void switchContext() {
    ThreadID tid = currentThread();
    if (tid != lastThreadID) {
        lastThreadID = tid;
        Stack *&s = stacks[tid];
        if (!s) s = new Stack(heap);
        currentStack = s;
    }
}

Stack &vsharp::stack() {
    switchContext();
    return *currentStack;
}

StackFrame &vsharp::topFrame() {
    switchContext();
    return currentStack->topFrame();
}

void vsharp::validateStackEmptyness() {
#ifdef _DEBUG
    for (auto &kv : stacks) {
        if (!kv.second->isEmpty()) {
            FAIL_LOUD("Stack is not empty after program termination!");
        }
        if (!kv.second->opmemIsEmpty()) {
            FAIL_LOUD("Opmem is not empty after program termination!");
        }
    }
#endif
}

#ifdef _DEBUG
unsigned vsharp::allocateString(const char *s) {
    unsigned currentIndex = topStringIndex;
    // Place s into intern pool
    stringsPool[currentIndex] = s;
//    LOG(tout << "Allocated string '" << s << "' with index '" << currentIndex << "'");
    // Increment top index
    topStringIndex++;
    // Return string's index
    return currentIndex;
}
#endif

bool _mainLeft = false;

void vsharp::mainLeft() {
    _mainLeft = true;
}

bool vsharp::isMainLeft() {
    return _mainLeft;
}

bool instrumentationEnabled = true;

bool vsharp::instrumentingEnabled() {
    return instrumentationEnabled;
}

void vsharp::enabledInstrumentation() {
    assert(!instrumentationEnabled);
    instrumentationEnabled = true;
}

void vsharp::disableInstrumentation() {
    assert(instrumentationEnabled);
    instrumentationEnabled = false;
}

void vsharp::resolve(INT_PTR p, VirtualAddress &address) {
    heap.physToVirtAddress(p, address);
}

void vsharp::setExpectedCoverage(const CoverageNode *expectedCoverage) {
    expectedCoverageStep = expectedCoverage;
    expectedCoverageExpirated = !expectedCoverage;
}

bool vsharp::addCoverageStep(OFFSET offset, int &lastStackPush, bool &stillExpectsCoverage) {
    int threadToken = 0; // TODO: support multithreading
    StackFrame &top = topFrame();
    int moduleToken = top.moduleToken();
    mdMethodDef methodToken = top.resolvedToken();
    if (lastCoverageStep && lastCoverageStep->moduleToken == moduleToken && lastCoverageStep->methodToken == methodToken &&
            lastCoverageStep->offset == offset && lastCoverageStep->threadToken == threadToken)
    {
        stillExpectsCoverage = !expectedCoverageExpirated;
        expectedCoverageExpirated = !expectedCoverageStep;
        lastStackPush = lastCoverageStep->stackPush;
        return true;
    }
    if (expectedCoverageStep) {
        stillExpectsCoverage = true;
        if (expectedCoverageStep->moduleToken != moduleToken || expectedCoverageStep->methodToken != methodToken ||
                expectedCoverageStep->offset != offset || expectedCoverageStep->threadToken != threadToken) {
            LOG(tout << "Path divergence detected at offset " << offset << " of " << HEX(methodToken));
            return false;
        }
        lastStackPush = expectedCoverageStep->stackPush;
        expectedCoverageStep = expectedCoverageStep->next;
    } else {
        stillExpectsCoverage = false;
        expectedCoverageExpirated = true;
    }
    LOG(tout << "Cover offset " << offset << " of " << HEX(methodToken));
    CoverageNode *newStep = new CoverageNode{moduleToken, methodToken, offset, threadToken, lastStackPush, nullptr};
    if (lastCoverageStep) {
        lastCoverageStep->next = newStep;
    }
    lastCoverageStep = newStep;
    if (!newCoverageNodes) {
        newCoverageNodes = newStep;
    }
    return true;
}

int vsharp::expectedStackPush() {
    return expectedCoverageStep ? expectedCoverageStep->stackPush : 0;
}

const CoverageNode *vsharp::flushNewCoverageNodes() {
    const CoverageNode *result = newCoverageNodes;
    newCoverageNodes = nullptr;
    return result;
}

int CoverageNode::size() const {
    if (!next)
        return 1;
    return next->size() + 1;
}

void CoverageNode::serialize(char *&buffer) const {
    *(int *)buffer = moduleToken; buffer += sizeof(int);
    *(mdMethodDef *)buffer = methodToken; buffer += sizeof(mdMethodDef);
    *(OFFSET *)buffer = offset; buffer += sizeof(OFFSET);
    *(int *)buffer = threadToken; buffer += sizeof(int);
    *(int *)buffer = stackPush; buffer += sizeof(int);
}
