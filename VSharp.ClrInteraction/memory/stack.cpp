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

LocalObject &StackFrame::peekObject(unsigned idx)
{
    return m_concreteness[m_concretenessTop - idx - 1].cell;
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
    m_concreteness[m_concretenessTop++] = {content, obj};
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

void StackFrame::setResolvedToken(unsigned resolved)
{
    this->m_resolvedToken = resolved;
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

unsigned StackFrame::moduleToken() const
{
    return m_moduleToken;
}

void StackFrame::setModuleToken(unsigned token)
{
    m_moduleToken = token;
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

StackFrame &Stack::frameAt(unsigned index) {
#ifdef _DEBUG
    if (index >= m_frames.size()) {
        FAIL_LOUD("Requesting too large frame number!");
    }
#endif
    return m_frames[index];
}

const StackFrame &Stack::frameAt(unsigned index) const {
#ifdef _DEBUG
    if (index >= m_frames.size()) {
        FAIL_LOUD("Requesting too large frame number!");
    }
#endif
    return m_frames[index];
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

bool Stack::opmemIsEmpty() const
{
    return m_opmem.empty();
}

Stack::OperandMem &Stack::opmem(UINT32 offset)
{
    if (m_opmem.empty()) {
        m_opmem.emplace_back(m_frames.back(), offset);
    } else {
        const Stack::OperandMem &top = m_opmem.back();
        if (top.offset() != offset || &top.stackFrame() != &m_frames.back()) {
            m_opmem.emplace_back(m_frames.back(), offset);
        }
    }
    return m_opmem.back();
}

const Stack::OperandMem &Stack::lastOpmem() const
{
    return m_opmem.back();
}

void Stack::popOpmem()
{
    m_opmem.pop_back();
}

Stack::OperandMem::OperandMem(const StackFrame &frame, UINT32 offset)
    : m_frame(frame)
    , m_offset(offset)
    , m_entries_count(0)
    , m_data_ptr(0)
    , m_memSize(3)
{
    m_dataPtrs.resize(m_memSize);
    m_data.resize(m_memSize * (sizeof(DOUBLE) + sizeof(CorElementType)));
}

void Stack::OperandMem::mem(char *value, CorElementType t, size_t size, INT8 idx) {
    if (idx > m_memSize) {
        m_memSize = idx + 1;
        m_dataPtrs.resize(m_memSize);
        m_data.resize(m_memSize * (sizeof(DOUBLE) + sizeof(CorElementType)));
    }
    ++m_entries_count;
    m_dataPtrs[idx] = m_data_ptr;
    unsigned typeSize = sizeof(CorElementType);
    char *p = m_data.data() + m_data_ptr;
    *(CorElementType*)p = t;
    memcpy(m_data.data() + m_data_ptr + typeSize, value, size);
    m_data_ptr += size + typeSize;
}

void Stack::OperandMem::mem(char *value, CorElementType t, size_t size) {
    mem(value, t, size, (INT8)m_entries_count);
}

void Stack::OperandMem::update(char *value, size_t size, INT8 idx) {
    memcpy(m_data.data() + m_dataPtrs[idx] + sizeof(CorElementType), value, size);
}

void Stack::OperandMem::mem_i1(INT8 value) {
    LOG(tout << "mem_i1 " << (INT64) value << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I1, sizeof(INT8));
}

void Stack::OperandMem::mem_i1(INT8 value, INT8 idx) {
    LOG(tout << "mem_i1 " << value << " " << (int) idx << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I1, sizeof(INT8), idx);
}

void Stack::OperandMem::mem_i2(INT16 value) {
    LOG(tout << "mem_i2 " << (INT64) value << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I2, sizeof(INT16));
}

void Stack::OperandMem::mem_i2(INT16 value, INT8 idx) {
    LOG(tout << "mem_i2 " << value << " " << (int) idx << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I2, sizeof(INT16), idx);
}

void Stack::OperandMem::mem_i4(INT32 value) {
    LOG(tout << "mem_i4 " << (INT64) value << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I4, sizeof(INT32));
}

void Stack::OperandMem::mem_i4(INT32 value, INT8 idx) {
    LOG(tout << "mem_i4 " << (INT64) value << " " << (int) idx << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I4, sizeof(INT32), idx);
}

void Stack::OperandMem::mem_i8(INT64 value) {
    LOG(tout << "mem_i8 " << (INT64) value << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I8, sizeof(INT64));
}

void Stack::OperandMem::mem_i8(INT64 value, INT8 idx) {
    LOG(tout << "mem_i8 " << value << " " << (int) idx << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I8, sizeof(INT64), idx);
}

void Stack::OperandMem::mem_f4(FLOAT value) {
    LOG(tout << "mem_f4 " << value << std::endl);
    mem((char *) &value, ELEMENT_TYPE_R4, sizeof(FLOAT));
}

void Stack::OperandMem::mem_f4(FLOAT value, INT8 idx) {
    LOG(tout << "mem_f4 " << value << " " << (int) idx << std::endl);
    mem((char *) &value, ELEMENT_TYPE_R4, sizeof(FLOAT), idx);
}

void Stack::OperandMem::mem_f8(DOUBLE value) {
    LOG(tout << "mem_f8 " << value << std::endl);
    mem((char *) &value, ELEMENT_TYPE_R8, sizeof(DOUBLE));
}

void Stack::OperandMem::mem_f8(DOUBLE value, INT8 idx) {
    LOG(tout << "mem_f8 " << value << " " << (int) idx << std::endl);
    mem((char *) &value, ELEMENT_TYPE_R8, sizeof(DOUBLE), idx);
}

void Stack::OperandMem::mem_p(INT_PTR value) {
    LOG(tout << "mem_p " << value << std::endl);
    mem((char *) &value, ELEMENT_TYPE_PTR, sizeof(INT_PTR));
}

void Stack::OperandMem::mem_p(INT_PTR value, INT8 idx) {
    LOG(tout << "mem_p " << value << " " << (int) idx << std::endl);
    mem((char *) &value, ELEMENT_TYPE_PTR, sizeof(INT_PTR), idx);
}

void Stack::OperandMem::mem_refLikeStruct(INT_PTR ref) {
    LOG(tout << "mem_refLikeStruct " << (INT64) ref << " (offset = " << this->offset() << ", resolvedToken = " << HEX(this->stackFrame().resolvedToken()) << ")" << std::endl);
    m_refLikeStructRef = ref;
}

void Stack::OperandMem::update_i1(INT8 value, INT8 idx) {
    LOG(tout << "update_i1 " << (INT64) value << " (index = " << (int)idx << ")" << std::endl);
    update((char *) &value, sizeof(INT8), idx);
}

void Stack::OperandMem::update_i2(INT16 value, INT8 idx) {
    LOG(tout << "update_i1 " << (INT64) value << " (index = " << (int)idx << ")" << std::endl);
    update((char *) &value, sizeof(INT16), idx);
}

void Stack::OperandMem::update_i4(INT32 value, INT8 idx) {
    LOG(tout << "update_i4 " << (INT64) value << " (index = " << (int)idx << ")" << std::endl);
    update((char *) &value, sizeof(INT32), idx);
}

void Stack::OperandMem::update_i8(INT64 value, INT8 idx) {
    LOG(tout << "update_i8 " << (INT64) value << " (index = " << (int)idx << ")" << std::endl);
    update((char *) &value, sizeof(INT64), idx);
}

void Stack::OperandMem::update_f4(long long value, INT8 idx) {
    DOUBLE tmp;
    memcpy(&tmp, &value, sizeof(DOUBLE));
    auto result = (FLOAT) tmp;
    LOG(tout << "update_f4 " << result << " (index = " << (int)idx << ")" << std::endl);
    update((char *) &result, sizeof(FLOAT), idx);
}

void Stack::OperandMem::update_f8(long long value, INT8 idx) {
    DOUBLE result;
    memcpy(&result, &value, sizeof(DOUBLE));
    LOG(tout << "update_f8 " << result << " (index = " << (int)idx << ")" << std::endl);
    update((char *) &result, sizeof(DOUBLE), idx);
}

void Stack::OperandMem::update_p(INT_PTR value, INT8 idx) {
    LOG(tout << "update_p " << (INT64) value << " (index = " << (int)idx << ")" << std::endl);
    update((char *) &value, sizeof(INT_PTR), idx);
}

CorElementType Stack::OperandMem::unmemType(INT8 idx) const {
    const char *ptr = m_data.data() + m_dataPtrs[idx];
    return *(CorElementType *) ptr;
}

INT8 Stack::OperandMem::unmem_i1(INT8 idx) const {
    auto ptr = m_data.data() + m_dataPtrs[idx];
    assert(*(CorElementType *) ptr == ELEMENT_TYPE_I1);
    auto result = *((INT8*) (ptr + sizeof(CorElementType)));
    LOG(tout << "unmem_i1(" << (int)idx << ") returned " << (int) result);
    return result;
//    return *((INT8*) (data.data() + dataPtrs[idx]));
}

INT16 Stack::OperandMem::unmem_i2(INT8 idx) const {
    auto ptr = m_data.data() + m_dataPtrs[idx];
    assert(*(CorElementType *) ptr == ELEMENT_TYPE_I2);
    auto result = *((INT16*) (ptr + sizeof(CorElementType)));
    LOG(tout << "unmem_i2(" << (int)idx << ") returned " << (int) result);
    return result;
//    return *((INT16*) (data.data() + dataPtrs[idx]));
}

INT32 Stack::OperandMem::unmem_i4(INT8 idx) const {
    auto ptr = m_data.data() + m_dataPtrs[idx];
    assert(*(CorElementType *) ptr == ELEMENT_TYPE_I4);
    auto result = *((INT32*) (ptr + sizeof(CorElementType)));
    LOG(tout << "unmem_i4(" << (int)idx << ") returned " << (int) result);
    return result;
//    return *((INT32*) (data.data() + dataPtrs[idx]));
}

INT64 Stack::OperandMem::unmem_i8(INT8 idx) const {
    auto ptr = m_data.data() + m_dataPtrs[idx];
    assert(*(CorElementType *) ptr == ELEMENT_TYPE_I8);
    auto result = *((INT64*) (ptr + sizeof(CorElementType)));
    LOG(tout << "unmem_i8(" << (int)idx << ") returned " << result);
    return result;
//    return *((INT64*) (data.data() + dataPtrs[idx]));
}

FLOAT Stack::OperandMem::unmem_f4(INT8 idx) const {
    auto ptr = m_data.data() + m_dataPtrs[idx];
    assert(*(CorElementType *) ptr == ELEMENT_TYPE_R4);
    auto result = *((FLOAT*) (ptr + sizeof(CorElementType)));
    LOG(tout << "unmem_f4(" << (int)idx << ") returned " << result);
    return result;
//    return *((FLOAT*) (data.data() + dataPtrs[idx]));
}

DOUBLE Stack::OperandMem::unmem_f8(INT8 idx) const {
    auto ptr = m_data.data() + m_dataPtrs[idx];
    assert(*(CorElementType *) ptr == ELEMENT_TYPE_R8);
    auto result = *((DOUBLE*) (ptr + sizeof(CorElementType)));
    LOG(tout << "unmem_f8(" << (int)idx << ") returned " << result);
    return result;
}

INT_PTR Stack::OperandMem::unmem_p(INT8 idx) const {
    auto ptr = m_data.data() + m_dataPtrs[idx];
    assert(*(CorElementType *) ptr == ELEMENT_TYPE_PTR);
    auto result = *((INT_PTR*) (ptr + sizeof(CorElementType)));
    LOG(tout << "unmem_p(" << (int)idx << ") returned " << result);
    return result;
//    return *((INT_PTR*) (data.data() + dataPtrs[idx]));
}

INT_PTR Stack::OperandMem::unmem_refLikeStruct() const {
    LOG(tout << "unmem_refLikeStruct " << (INT64) m_refLikeStructRef << " (offset = " << this->offset() << ", resolvedToken = " << HEX(this->stackFrame().resolvedToken()) << ")" << std::endl);
    return m_refLikeStructRef;
}
