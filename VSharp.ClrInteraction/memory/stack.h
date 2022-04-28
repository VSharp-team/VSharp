#ifndef STACK_H_
#define STACK_H_

#include <vector>
#include <stack>
#include "storage.h"

namespace vsharp {

// NOTE: every stack cell (evaluation stack cell, local or argument) contains LocalObject class by value
struct StackCell {
    unsigned content;
    LocalObject cell;
};

class StackFrame {
private:
    StackCell *m_concreteness;
    unsigned m_capacity;
    unsigned m_concretenessTop;

    unsigned m_symbolsCount;
    unsigned m_lastSentSymbolsCount;
    unsigned m_minSymbsCountSinceLastSent;

    LocalObject *m_args;
    unsigned m_argsCount;
    LocalObject *m_locals;
    unsigned m_localsCount;
    // NOTE: used to delete from heap all stack cell, which were allocated there
    std::vector<Interval *> allocatedLocals;

    unsigned m_resolvedToken;
    unsigned m_unresolvedToken;
    bool m_enteredMarker;
    bool m_spontaneous;

    unsigned m_ip;

    Storage &m_heap;

    std::vector<std::pair<unsigned, unsigned>> m_lastPoppedSymbolics;

public:
    StackFrame(unsigned resolvedToken, unsigned unresolvedToken, const bool *args, unsigned argsCount, Storage &heap);
    ~StackFrame();

    void configure(unsigned maxStackSize, unsigned localsCount);

    inline bool isEmpty() const;
    inline bool isFull() const;

    bool peekConcreteness(unsigned idx) const;
    bool peek0() const;
    bool peek1() const;
    bool peek2() const;
    const LocalObject &peekObject(unsigned idx) const;

    void pop0();
    bool pop1();
    bool pop(unsigned count);
    void pop1Async(); // Does not track the popped symbolics, but tracks the total amount of such pops.

    void push1(const LocalObject &obj);
    void pushPrimitive(bool isConcrete);
    void push1Concrete();

    LocalObject &arg(unsigned index) const;
    LocalObject &loc(unsigned index) const;

    void addAllocatedLocal(LocalObject *local);

    bool dup();

    unsigned count() const;

    unsigned resolvedToken() const;
    unsigned unresolvedToken() const;
    unsigned ip() const;
    void setIp(unsigned ip);
    bool hasEntered() const;
    void setEnteredMarker(bool entered);
    bool isSpontaneous() const;
    void setSpontaneous(bool isUnmanaged);

    const std::vector<std::pair<unsigned, unsigned>> &poppedSymbolics() const;
    unsigned evaluationStackPops() const;
    unsigned symbolicsCount() const;
    void resetPopsTracking();
};

class Stack {
private:
    std::deque<StackFrame> m_frames;
    unsigned m_lastSentTop;
    unsigned m_minTopSinceLastSent;

    Storage &m_heap;
public:
    Stack(Storage &heap);
    void pushFrame(unsigned resolvedToken, unsigned unresolvedToken, const bool *args, unsigned argsCount);
    void popFrame();
    void popFrameUntracked();
    StackFrame &topFrame();
    inline const StackFrame &topFrame() const;

    bool isEmpty() const;
    unsigned framesCount() const;
    unsigned tokenAt(unsigned index) const;
    unsigned offsetAt(unsigned index) const;

    unsigned unsentPops() const;
    unsigned minTopSinceLastSent() const;
    void resetMinTop();
    void resetPopsTracking(int framesCount);
};

}

#endif // STACK_H_
