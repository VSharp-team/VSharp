#include "stack.h"
#include "../logging.h"
#include <cstring>
#include <cassert>

using namespace vsharp;

#define CONCRETE UINT32_MAX

StackFrame::StackFrame(unsigned resolvedToken, unsigned unresolvedToken, const bool *args, unsigned argsCount)
    : m_concreteness(nullptr)
    , m_capacity(0)
    , m_concretenessTop(0)
    , m_symbolsCount(0)
    , m_args(new bool[argsCount])
    , m_locals(nullptr)
    , m_resolvedToken(resolvedToken)
    , m_unresolvedToken(unresolvedToken)
    , m_enteredMarker(false)
    , m_spontaneous(false)
{
    memcpy(m_args, args, argsCount);
    resetPopsTracking();
}

StackFrame::~StackFrame()
{
    delete [] m_concreteness;
}

void StackFrame::configure(unsigned maxStackSize, unsigned localsCount)
{
    m_capacity = maxStackSize;
    m_concreteness = new unsigned[maxStackSize];
    m_locals = new bool[localsCount];
    memset(m_locals, true, localsCount);
}

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
    return m_concreteness[m_concretenessTop - 1] == CONCRETE;
}

bool StackFrame::peek1() const
{
    return m_concreteness[m_concretenessTop - 2] == CONCRETE;
}

bool StackFrame::peek2() const
{
    return m_concreteness[m_concretenessTop - 3] == CONCRETE;
}

bool StackFrame::peek(unsigned idx) const
{
    return m_concreteness[m_concretenessTop - idx - 1] == CONCRETE;
}

void StackFrame::pop0()
{
    m_lastPoppedSymbolics.clear();
}

void StackFrame::push1(bool isConcrete)
{
#ifdef _DEBUG
    if (isFull()) {
        LOG(tout << "Frame info before stack overflow: balance = " << m_concretenessTop << ", capacity = " << m_capacity
                 << ", token = " << HEX(m_resolvedToken));
        FAIL_LOUD("Stack overflow!");
    }
#endif
    m_concreteness[m_concretenessTop++] = isConcrete ? CONCRETE : ++m_symbolsCount;
}

void StackFrame::push1Concrete()
{
    push1(true);
}

bool StackFrame::pop1()
{
#ifdef _DEBUG
    if (isEmpty()) {
        LOG(tout << "Corrupted frame info: token = " << HEX(m_resolvedToken) << ", stackSize = " << m_capacity);
        FAIL_LOUD("Corrupted stack!");
    }
#endif
    m_lastPoppedSymbolics.clear();
    --m_concretenessTop;
    unsigned cell = m_concreteness[m_concretenessTop];
    if (cell != CONCRETE) {
        --m_symbolsCount;
        m_lastPoppedSymbolics.emplace_back(cell, 0u);
        return false;
    }
    return true;
}


bool StackFrame::pop(unsigned count)
{
#ifdef _DEBUG
    if (m_concretenessTop < count) {
        LOG(tout << "Corrupted frame info: token = " << HEX(m_resolvedToken) << ", stackSize = " << m_capacity);
        FAIL_LOUD("Corrupted stack!");
    }
#endif
    m_lastPoppedSymbolics.clear();
    m_concretenessTop -= count;
    for (unsigned i = m_concretenessTop + count; i > m_concretenessTop; --i) {
        unsigned cell = m_concreteness[i - 1];
        if (cell != CONCRETE) {
            --m_symbolsCount;
            m_lastPoppedSymbolics.emplace_back(cell, m_concretenessTop + count - i);
        }
    }
    return m_lastPoppedSymbolics.empty();
}

void StackFrame::pop1Async()
{
    pop1();
    if (m_minSymbsCountSinceLastSent > m_symbolsCount)
        m_minSymbsCountSinceLastSent = m_symbolsCount;
}

bool StackFrame::arg(unsigned index) const
{
    return m_args[index];
}

void StackFrame::setArg(unsigned index, bool value)
{
    m_args[index] = value;
}

bool StackFrame::loc(unsigned index) const
{
    return m_locals[index];
}

void StackFrame::setLoc(unsigned index, bool value)
{
    m_locals[index] = value;
}

bool StackFrame::dup()
{
    bool concreteness = pop1();
    if (concreteness) {
        push1(concreteness);
        push1(concreteness);
    }
    return concreteness;
}

unsigned StackFrame::count() const
{
    return m_concretenessTop;
}

unsigned StackFrame::resolvedToken() const
{
    return m_resolvedToken;
}

unsigned StackFrame::unresolvedToken() const
{
    return m_unresolvedToken;
}

bool StackFrame::hasEntered() const
{
    return m_enteredMarker;
}

void StackFrame::setEnteredMarker(bool entered)
{
    this->m_enteredMarker = entered;
}

bool StackFrame::isSpontaneous() const
{
    return m_spontaneous;
}

void StackFrame::setSpontaneous(bool isUnmanaged)
{
    this->m_spontaneous = isUnmanaged;
}

unsigned StackFrame::evaluationStackPops() const
{
    assert(m_minSymbsCountSinceLastSent <= m_lastSentSymbolsCount);
    return m_lastSentSymbolsCount - m_minSymbsCountSinceLastSent;
}

unsigned StackFrame::symbolicsCount() const
{
    return m_symbolsCount;
}

void StackFrame::resetPopsTracking()
{
    m_lastSentSymbolsCount = m_symbolsCount;
    m_minSymbsCountSinceLastSent = m_symbolsCount;
}

const std::vector<std::pair<unsigned, unsigned>> &StackFrame::poppedSymbolics() const
{
    return m_lastPoppedSymbolics;
}

void Stack::pushFrame(unsigned resolvedToken, unsigned unresolvedToken, const bool *args, unsigned argsCount)
{
    m_frames.push_back(StackFrame(resolvedToken, unresolvedToken, args, argsCount));
}


void Stack::popFrame()
{
    popFrameUntracked();
    if (m_frames.size() < m_minTopSinceLastSent) {
        m_minTopSinceLastSent = m_frames.size();
    }
}

void Stack::popFrameUntracked()
{
#ifdef _DEBUG
    if (m_frames.empty()) {
        FAIL_LOUD("Stack is empty! Can't pop frame!");
    } else if (!m_frames.back().isEmpty()) {
        FAIL_LOUD("Corrupted stack: opstack is not empty when popping frame!");
    }
#endif
    m_frames.pop_back();
}

StackFrame &Stack::topFrame()
{
#ifdef _DEBUG
    if (m_frames.empty()) {
        FAIL_LOUD("Requesting top frame of empty stack!");
    }
#endif
    return m_frames.back();
}

const StackFrame &Stack::topFrame() const
{
#ifdef _DEBUG
    if (m_frames.empty()) {
        FAIL_LOUD("Requesting top frame of empty stack!");
    }
#endif
    return m_frames.back();
}

bool Stack::isEmpty() const
{
    return m_frames.empty();
}

unsigned Stack::framesCount() const
{
    return m_frames.size();
}

unsigned Stack::tokenAt(unsigned index) const
{
    return m_frames[index].unresolvedToken();
}

unsigned Stack::unsentPops() const
{
    return m_lastSentTop - m_minTopSinceLastSent;
}

unsigned Stack::minTopSinceLastSent() const
{
    return m_minTopSinceLastSent;
}

void Stack::resetPopsTracking(int framesCount)
{
    m_lastSentTop = framesCount;
    m_minTopSinceLastSent = m_frames.size();
    if (!m_frames.empty()) {
        m_frames.back().resetPopsTracking();
    }
}
