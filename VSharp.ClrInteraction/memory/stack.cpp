#include "memory/stack.h"

#include "logging.h"
#include <cstring>

using namespace icsharp;

StackFrame::StackFrame(unsigned maxStackSize, char *args, unsigned argsCount, unsigned localsCount)
    : m_concreteness(new bool[maxStackSize])
    , m_capacity(maxStackSize)
    , m_concretenessTop(0)
    , m_args(new bool[argsCount])
    , m_locals(new bool[localsCount])
    , m_expectedToken(0)
    , m_enteredMarker(false)
    , m_unmanagedContext(false)
{
    std::memcpy(m_args, args, argsCount);
}

//StackFrame::~StackFrame()
//{
//    // TODO: why double free?
//    delete [] concreteness;
//}

bool StackFrame::isEmpty() const
{
    return m_concretenessTop == 0;
}

bool StackFrame::isFull() const
{
    return m_concretenessTop == m_capacity;
}

bool StackFrame::peek0() const
{
    return m_concreteness[m_concretenessTop - 1];
}

bool StackFrame::peek1() const
{
    return m_concreteness[m_concretenessTop - 2];
}

bool StackFrame::peek2() const
{
    return m_concreteness[m_concretenessTop - 3];
}

void StackFrame::push1(bool isConcrete)
{
#ifdef _DEBUG
    if (isFull())
        FAIL_LOUD("Stack overflow!");
#endif
    m_concreteness[m_concretenessTop++] = isConcrete;
}

void StackFrame::push1Concrete()
{
    push1(true);
}

bool StackFrame::pop1()
{
#ifdef _DEBUG
    if (isEmpty())
        FAIL_LOUD("Corrupted stack!");
#endif
    --m_concretenessTop;
    return m_concreteness[m_concretenessTop];
}


void StackFrame::pop(unsigned count)
{
#ifdef _DEBUG
    if (m_concretenessTop < count)
        FAIL_LOUD("Corrupted stack!");
#endif
    m_concretenessTop -= count;
}

bool StackFrame::pop2Push1()
{
#ifdef _DEBUG
    if (m_concretenessTop < 2)
        FAIL_LOUD("Corrupted stack!");
#endif
    --m_concretenessTop;
    return m_concreteness[m_concretenessTop - 1] &= m_concreteness[m_concretenessTop];
}

void StackFrame::dup()
{
    push1(peek0());
}

unsigned StackFrame::count() const
{
    return m_concretenessTop;
}

unsigned StackFrame::expectedToken() const
{
    return m_expectedToken;
}

void StackFrame::setExpectedToken(unsigned expectedToken)
{
    this->m_expectedToken = expectedToken;
}

bool StackFrame::hasEntered() const
{
    return m_enteredMarker;
}

void StackFrame::setEnteredMarker(bool entered)
{
    this->m_enteredMarker = entered;
}

bool StackFrame::inUnmanagedContext() const
{
    return m_unmanagedContext;
}

void StackFrame::setUnmanagedContext(bool isUnmanaged)
{
    this->m_unmanagedContext = isUnmanaged;
}

//void Stack::pushFrame(int maxStackSize, char *args, unsigned argsCount, unsigned localsCount)
//{
//    m_frames.push(StackFrame(maxStackSize, args, argsCount, localsCount));
//}
void Stack::pushFrame(unsigned maxStackSize)
{
    m_frames.push(StackFrame(maxStackSize, nullptr, 0, 0));
}

void Stack::popFrame()
{
#ifdef _DEBUG
    if (m_frames.empty()) {
        FAIL_LOUD("Stack is empty! Can't pop frame!");
    } else if (!m_frames.top().isEmpty()) {
        FAIL_LOUD("Corrupted stack: opstack is not empty when popping frame!");
    }
#endif
    m_frames.pop();
}

StackFrame &Stack::topFrame()
{
#ifdef _DEBUG
    if (m_frames.empty()) {
        FAIL_LOUD("Requesting top frame of empty stack!");
    }
#endif
    return m_frames.top();
}

const StackFrame &Stack::topFrame() const
{
#ifdef _DEBUG
    if (m_frames.empty()) {
        FAIL_LOUD("Requesting top frame of empty stack!");
    }
#endif
    return m_frames.top();
}

bool Stack::isEmpty() const
{
    return m_frames.empty();
}

unsigned Stack::framesCount() const
{
    return m_frames.size();
}
