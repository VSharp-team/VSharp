#include "stack.h"
#include <cstring>
#include <cassert>

using namespace vsharp;

#define CONCRETE UINT32_MAX

StackFrame::StackFrame(unsigned resolvedToken, unsigned unresolvedToken, const bool *args, unsigned argsCount, Storage &heap)
    : m_concreteness(nullptr)
    , m_capacity(0)
    , m_concretenessTop(0)
    , m_symbolsCount(0)
    , m_args(new LocalCell[argsCount])
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
    for (int i = 0; i < argsCount; i++) {
        ConcretenessCell concreteness{};
        concreteness.primitive = args[i];
        m_args[i] = {false, concreteness};
    }
    resetPopsTracking();
}

inline void copyUniqueLocalObjects(LocalCell *array, unsigned count, std::vector<Interval *> &objects) {
    for (int i = 0; i < count; i++) {
        LocalCell cell = array[i];
        if (cell.isStruct) {
            auto *p = (Interval *) cell.concreteness.obj;
            if (std::find(objects.begin(), objects.end(), p) == objects.end())
                objects.push_back(p);
        }
    }
}

StackFrame::~StackFrame()
{
    // NOTE: copying args and local objects to vector
    std::vector<Interval *> objects;
    copyUniqueLocalObjects(m_args, m_argsCount, objects);
    copyUniqueLocalObjects(m_locals, m_localsCount, objects);

    // NOTE: free memory of frame objects (structs)
    for (const auto *obj : objects)
        delete obj;

    // NOTE: removing objects from storage tree
    m_heap.deleteObjects(objects);

    // NOTE: free other frame specific memory
    delete [] m_concreteness;
    if (m_localsCount > 0)
        delete [] m_locals;
    if (m_argsCount > 0)
        delete [] m_args;
}

bool localCellConcreteness(const LocalCell &cell) {
    if (cell.isStruct) {
        Object *obj = (Object *) cell.concreteness.obj;
        return obj->isFullyConcrete();
    } else {
        return cell.concreteness.primitive;
    }
}

void StackFrame::configure(unsigned maxStackSize, unsigned localsCount)
{
    m_capacity = maxStackSize;
    m_concreteness = new StackCell[maxStackSize];
    m_locals = new LocalCell[localsCount];
    m_localsCount = localsCount;
    for (int i = 0; i < localsCount; i++) {
        ConcretenessCell concreteness{};
        concreteness.primitive = true;
        m_locals[i] = {false, concreteness};
    }
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
    StructOptional structOptional = cell.obj;
    if (structOptional.isStruct) {
        Object *obj = (Object *)structOptional.obj;
        return obj->isFullyConcrete();
    } else {
        return cell.content == CONCRETE;
    }
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

void StackFrame::peekStruct(unsigned idx, StructOptional &structOptional) const
{
    structOptional = m_concreteness[m_concretenessTop - idx - 1].obj;
}

void StackFrame::pop0()
{
    m_lastPoppedSymbolics.clear();
}

// NOTE: if pushing value is struct, parameter 'isConcrete' over approximates its concreteness
//       otherwise, 'isConcrete' represents concreteness of primitive type
void StackFrame::push1(bool isConcrete, StructOptional obj) // TODO: pass StructOptional via const ref?
{
#ifdef _DEBUG
    if (isFull()) {
        LOG(tout << "Frame info before stack overflow: balance = " << m_concretenessTop << ", capacity = " << m_capacity
                 << ", token = " << HEX(m_resolvedToken));
        FAIL_LOUD("Stack overflow!");
    }
#endif
    unsigned content = isConcrete ? CONCRETE : ++m_symbolsCount;
    m_concreteness[m_concretenessTop++] = {content, obj};
}

void StackFrame::pushPrimitive(bool isConcrete)
{
    StructOptional obj{false, 0};
    push1(isConcrete, obj);
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

void StackFrame::arg(unsigned index, LocalCell &cell) const
{
    cell = m_args[index];
}

bool StackFrame::argConcreteness(unsigned int index) const
{
    return localCellConcreteness(m_args[index]);
}

void StackFrame::setArg(unsigned index, LocalCell value) // TODO: pass LocalCell via const ref?
{
    m_args[index] = value;
}

void StackFrame::loc(unsigned index, LocalCell &cell) const
{
    cell = m_locals[index];
}

bool StackFrame::locConcreteness(unsigned index) const
{
    return localCellConcreteness(m_locals[index]);
}

void StackFrame::setLoc(unsigned index, LocalCell value) // TODO: pass LocalCell via const ref?
{
    m_locals[index] = value;
}

bool StackFrame::dup()
{
    StructOptional obj{};
    peekStruct(0, obj);
    bool concreteness = pop1();
    if (concreteness) {
        push1(concreteness, obj);
        push1(concreteness, obj);
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
