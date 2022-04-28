#include "stack.h"
#include <cstring>
#include <cassert>
#include <algorithm>

using namespace vsharp;

#define CONCRETE UINT32_MAX

StackFrame::StackFrame(unsigned resolvedToken, unsigned unresolvedToken, const bool *args, unsigned argsCount, Storage &heap)
    : m_concreteness(nullptr)
    , m_capacity(0)
    , m_concretenessTop(0)
    , m_symbolsCount(0)
    , m_args(new LocalObject[argsCount])
    , m_argsCount(argsCount)
    , m_locals(nullptr)
    , m_localsCount(0)
    , m_resolvedToken(resolvedToken)
    , m_unresolvedToken(unresolvedToken)
    , m_enteredMarker(false)
    , m_spontaneous(false)
    , m_heap(heap)
    , m_ip(0)
{
    for (int i = 0; i < argsCount; i++)
        m_args[i].writeConcretenessWholeObject(args[i]);
    resetPopsTracking();
}

StackFrame::~StackFrame()
{
    m_heap.deleteObjects(allocatedLocals);
    delete [] m_concreteness;
    if (m_localsCount > 0)
        delete [] m_locals;
    if (m_argsCount > 0)
        delete [] m_args;
}

void StackFrame::configure(unsigned maxStackSize, unsigned localsCount)
{
    m_capacity = maxStackSize;
    m_concreteness = new StackCell[maxStackSize];
    m_locals = new LocalObject[localsCount];
    m_localsCount = localsCount;
    for (int i = 0; i < localsCount; i++)
        m_locals[i].writeConcretenessWholeObject(true);
}

bool StackFrame::isEmpty() const
{
    return m_concretenessTop == 0;
}

bool StackFrame::isFull() const
{
    return m_concretenessTop == m_capacity;
}

bool stackCellConcreteness(const StackCell &cell) {
    return cell.content == CONCRETE;
}

bool StackFrame::peekConcreteness(unsigned idx) const
{
    StackCell cell = m_concreteness[m_concretenessTop - idx - 1];
    return stackCellConcreteness(cell);
}

bool StackFrame::peek0() const
{
    return peekConcreteness(0);
}

bool StackFrame::peek1() const
{
    return peekConcreteness(1);
}

bool StackFrame::peek2() const
{
    return peekConcreteness(2);
}

const LocalObject &StackFrame::peekObject(unsigned idx) const
{
    return m_concreteness[m_concretenessTop - idx - 1].cell;
}

void StackFrame::pop0()
{
    m_lastPoppedSymbolics.clear();
}

void StackFrame::push1(const LocalObject& obj)
{
#ifdef _DEBUG
    if (isFull()) {
        LOG(tout << "Frame info before stack overflow: balance = " << m_concretenessTop << ", capacity = " << m_capacity
                 << ", token = " << HEX(m_resolvedToken));
        FAIL_LOUD("Stack overflow!");
    }
#endif
    unsigned content = obj.isFullyConcrete() ? CONCRETE : ++m_symbolsCount;
    m_concreteness[m_concretenessTop++] = {content, LocalObject(obj)};
}

void StackFrame::pushPrimitive(bool isConcrete)
{
    LocalObject cell{};
    cell.writeConcretenessWholeObject(isConcrete);
    push1(cell);
}

void StackFrame::push1Concrete()
{
    pushPrimitive(true);
}

bool StackFrame::pop1()
{
#ifdef _DEBUG
    if (isEmpty()) {
        LOG(tout << "Corrupted frame info: token = " << HEX(m_resolvedToken) << ", stackSize = " << m_capacity);
        FAIL_LOUD("Corrupted stack!");
    }
#endif
    StackCell cell = m_concreteness[m_concretenessTop - 1];
    m_lastPoppedSymbolics.clear();
    --m_concretenessTop;
    if (!stackCellConcreteness(cell)) {
        --m_symbolsCount;
        m_lastPoppedSymbolics.emplace_back(cell.content, 0u);
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
    for (unsigned i = 0; i < count; i++) {
        StackCell cell = m_concreteness[m_concretenessTop - i - 1];
        if (!stackCellConcreteness(cell)) {
            --m_symbolsCount;
            m_lastPoppedSymbolics.emplace_back(cell.content, i);
        }
    }
    m_concretenessTop -= count;
    return m_lastPoppedSymbolics.empty();
}

void StackFrame::pop1Async()
{
    pop1();
    if (m_minSymbsCountSinceLastSent > m_symbolsCount)
        m_minSymbsCountSinceLastSent = m_symbolsCount;
}

LocalObject &StackFrame::arg(unsigned index) const
{
    return m_args[index];
}

LocalObject &StackFrame::loc(unsigned index) const
{
    return m_locals[index];
}

void StackFrame::addAllocatedLocal(LocalObject *local)
{
    if (std::find(allocatedLocals.begin(), allocatedLocals.end(), local) == allocatedLocals.end())
        allocatedLocals.push_back((Interval *)local);
}

bool StackFrame::dup()
{
    const LocalObject &cell = peekObject(0);
    bool concreteness = pop1();
    if (concreteness) {
        push1(cell);
        push1(cell);
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

unsigned StackFrame::ip() const {
    return m_ip;
}

void StackFrame::setIp(unsigned ip) {
    m_ip = ip;
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

Stack::Stack(Storage &heap)
    : m_heap(heap)
{
}

void Stack::pushFrame(unsigned resolvedToken, unsigned unresolvedToken, const bool *args, unsigned argsCount)
{
    m_frames.emplace_back(resolvedToken, unresolvedToken, args, argsCount, m_heap);
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

unsigned Stack::offsetAt(unsigned int index) const {
    return m_frames[index].ip();
}

unsigned Stack::unsentPops() const
{
    return m_lastSentTop - m_minTopSinceLastSent;
}

unsigned Stack::minTopSinceLastSent() const
{
    return m_minTopSinceLastSent;
}

void Stack::resetMinTop()
{
    m_minTopSinceLastSent = m_frames.size();
}

void Stack::resetPopsTracking(int framesCount)
{
    m_lastSentTop = framesCount;
    resetMinTop();
    if (!m_frames.empty()) {
        m_frames.back().resetPopsTracking();
    }
}
