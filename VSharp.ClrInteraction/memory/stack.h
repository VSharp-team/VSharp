#ifndef STACK_H_
#define STACK_H_

#include <vector>
#include <stack>
#include "storage.h"

namespace vsharp {

struct StructOptional {
    bool isStruct;
    OBJID obj;
};

// NOTE: if value is struct, parameter 'content' over approximates its concreteness
//       otherwise, 'content' represents concreteness of primitive type
struct StackCell {
    unsigned content;
    StructOptional obj;
};

union ConcretenessCell {
    bool primitive;
    OBJID obj;
};

struct LocalCell {
    bool isStruct;
    ConcretenessCell concreteness;
};

class StackFrame {
private:
    StackCell *m_concreteness;
    unsigned m_capacity;
    unsigned m_concretenessTop;

    unsigned m_symbolsCount;
    unsigned m_lastSentSymbolsCount;
    unsigned m_minSymbsCountSinceLastSent;

    LocalCell *m_args;
    unsigned m_argsCount;
    LocalCell *m_locals;
    unsigned m_localsCount;

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
    void peekStruct(unsigned idx, StructOptional &structOptional) const;

    void pop0();
    bool pop1();
    bool pop(unsigned count);
    void pop1Async(); // Does not track the popped symbolics, but tracks the total amount of such pops.

    void push1(bool isConcrete, StructOptional obj);
    void pushPrimitive(bool isConcrete);
    void push1Concrete();

    void arg(unsigned index, LocalCell &cell) const;
    bool argConcreteness(unsigned index) const;
    void setArg(unsigned index, LocalCell value);
    void loc(unsigned index, LocalCell &cell) const;
    bool locConcreteness(unsigned index) const;
    void setLoc(unsigned index, LocalCell value);

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
