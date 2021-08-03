#ifndef STACK_H_
#define STACK_H_

#include <vector>
#include <stack>

namespace icsharp {

class StackFrame {
private:
    bool *m_concreteness;
    unsigned m_capacity;
    unsigned m_concretenessTop;

    bool *m_args;
    bool *m_locals;

    unsigned m_expectedToken;
    bool m_enteredMarker;
    bool m_unmanagedContext;

public:
    StackFrame(unsigned maxStackSize, char *args, unsigned argsCount, unsigned localsCount);
//    ~StackFrame();

    inline bool isEmpty() const;
    inline bool isFull() const;

    bool peek0() const;
    bool peek1() const;
    bool peek2() const;

    bool pop1();
    void pop(unsigned count);

    void push1(bool isConcrete);
    void push1Concrete();
    bool pop2Push1();

    void dup();

    unsigned count() const;

    unsigned expectedToken() const;
    void setExpectedToken(unsigned expectedToken);
    bool hasEntered() const;
    void setEnteredMarker(bool entered);
    bool inUnmanagedContext() const;
    void setUnmanagedContext(bool isUnmanaged);
};

class Stack {
private:
    std::stack<StackFrame> m_frames;

public:
//    void pushFrame(int maxStackSize, char *args, unsigned argsCount, unsigned localsCount);
    void pushFrame(unsigned maxStackSize);
    void popFrame();
    StackFrame &topFrame();
    inline const StackFrame &topFrame() const;

    bool isEmpty() const;
    unsigned framesCount() const;
};

}

#endif // STACK_H_
