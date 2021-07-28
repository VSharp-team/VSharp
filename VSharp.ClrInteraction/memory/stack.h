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
    unsigned m_expectedToken;
    bool m_enteredMarker;
    bool m_unmanagedContext;

public:
    StackFrame(unsigned maxStackSize);
//    ~StackFrame();

    inline bool isEmpty() const;
    inline bool isFull() const;

    bool pop1();
    void pop(unsigned count);

    void push1(bool isConcrete);
    void push1Concrete();
    void pop2Push1();

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
    void pushFrame(unsigned int maxStackSize);
    void popFrame();
    StackFrame &topFrame();
    inline const StackFrame &topFrame() const;

    bool isEmpty() const;
    unsigned framesCount() const;
};

}

#endif // STACK_H_
