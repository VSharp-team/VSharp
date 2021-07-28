#include "memory/memory.h"
#include "memory/stack.h"
#include "logging.h"

using namespace icsharp;

ThreadID currentThreadNotConfigured() {
    throw std::logic_error("Current thread getter is not configured!");
}

std::function<ThreadID()> icsharp::currentThread(&currentThreadNotConfigured);
#ifdef _DEBUG
std::map<unsigned, const char*> icsharp::stringsPool;
int topStringIndex = 0;
#endif

ThreadID lastThreadID = 0;
Stack *stack = nullptr;

inline void switchContext() {
    ThreadID tid = currentThread();
    if (tid != lastThreadID) {
        lastThreadID = tid;
        Stack *&s = stacks[tid];
        if (!s) s = new Stack();
        stack = s;
    }
}

unsigned icsharp::framesCount() {
    switchContext();
    return stack->framesCount();
}

void icsharp::push1Concrete() {
    switchContext();
    stack->topFrame().push1Concrete();
}

// TODO: remove it!
void icsharp::pushConcrete(unsigned count) {
    switchContext();
    for (unsigned i = 0; i < count; ++i) {
        stack->topFrame().push1Concrete();
    }
}

void icsharp::pop1() {
    switchContext();
    stack->topFrame().pop1();
}

void icsharp::pop(unsigned count) {
    switchContext();
    stack->topFrame().pop(count);
}

void icsharp::pop2Push1() {
    switchContext();
    stack->topFrame().pop2Push1();
}

//#include<iostream>

void icsharp::call(unsigned token, unsigned argsCount) {
    switchContext();
    LOG(tout << "Setting expected token of frame " << stack->framesCount() << " to " << token << " (entered marker is " << stack->topFrame().hasEntered() << ")" << std::endl);
    stack->topFrame().setExpectedToken(token);
    stack->topFrame().setEnteredMarker(false);
    pop(argsCount);
}

void icsharp::callVirt(unsigned argsCount) {
    switchContext();
    pop(argsCount);
}


void icsharp::enter(unsigned token, unsigned maxStackSize) {
    switchContext();
    if (!stack->isEmpty()) {
        unsigned expected = stack->topFrame().expectedToken();
        LOG(tout << "Frame " << stack->framesCount() + 1 << ": entering token " << token << ", expected token is " << expected << "; resetting it!" << std::endl);
        if (!expected || expected == token) {
            stack->topFrame().setExpectedToken(0);
            stack->topFrame().setEnteredMarker(true);
            stack->topFrame().setUnmanagedContext(false);
        } else {
            stack->topFrame().setUnmanagedContext(true);
            LOG(tout << "Hmmm!! Managed code has been triggered in unmanaged context! Details: expected token " << expected << ", but entered " << token << std::endl);
        }
    }
    stack->pushFrame(maxStackSize);
}

void icsharp::leave(unsigned returnValues) {
    switchContext();
#ifdef _DEBUG
    assert(returnValues == 0 || returnValues == 1);
    if (stack->topFrame().count() != returnValues) {
        FAIL_LOUD("Corrupted stack: stack is not empty when popping frame!");
    }
#endif
    if (returnValues) {
        bool returnValue = stack->topFrame().pop1();
        stack->popFrame();
        if (!stack->isEmpty()) {
            if (!stack->topFrame().inUnmanagedContext())
                stack->topFrame().push1(returnValue);
            else
                LOG(tout << "Ignoring return type because of internal execution in unmanaged context..." << std::endl);
        } else {
            FAIL_LOUD("Function returned result, but there is no frame to push return value!")
        }
    } else {
        stack->popFrame();
    }
}

void icsharp::finalizeCall(unsigned returnValues) {
    switchContext();
    LOG(tout << "finalize call of " << stack->topFrame().expectedToken() << " (enter marker " << stack->topFrame().hasEntered() << ")" << std::endl);
    stack->topFrame().setExpectedToken(0);
    if (!stack->topFrame().hasEntered()) {
        // Extern has been called, should push return result onto stack
        LOG(tout << "Extern left! " << framesCount() << " frames remained" << std::endl);
#ifdef _DEBUG
        assert(returnValues == 0 || returnValues == 1);
        if (stack->isEmpty()) {
            FAIL_LOUD("Corrupted stack: stack is empty after executing external function!");
        }
#endif
        if (returnValues) {
            stack->topFrame().push1(1);
        }
    }
    stack->topFrame().setExpectedToken(0);
    stack->topFrame().setUnmanagedContext(false);
}

void icsharp::validateEnd() {
#ifdef _DEBUG
    for (auto &kv : stacks) {
        if (!kv.second->isEmpty()) {
            FAIL_LOUD("Stack is not empty after program termination!!");
        }
    }
#endif
}

#ifdef _DEBUG
unsigned icsharp::allocateString(const char *s) {
    unsigned currentIndex = topStringIndex;
    // Place s into intern pool
    stringsPool[currentIndex] = s;
    LOG(tout << "Allocated string '" << s << "' with index '" << currentIndex << "'");
    // Increment top index
    topStringIndex++;
    // Return string's index
    return currentIndex;
}
#endif