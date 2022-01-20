#include "memory/memory.h"
#include "memory/stack.h"
#include "logging.h"

using namespace icsharp;

ThreadID currentThreadNotConfigured() {
    throw std::logic_error("Current thread getter is not configured!");
}

std::function<ThreadID()> icsharp::currentThread(&currentThreadNotConfigured);

Heap icsharp::heap = Heap();

#ifdef _DEBUG
std::map<unsigned, const char*> icsharp::stringsPool;
int topStringIndex = 0;
#endif

ThreadID lastThreadID = 0;
Stack *currentStack = nullptr;

inline void switchContext() {
    ThreadID tid = currentThread();
    if (tid != lastThreadID) {
        lastThreadID = tid;
        Stack *&s = stacks[tid];
        if (!s) s = new Stack();
        currentStack = s;
    }
}

Stack &icsharp::stack() {
    switchContext();
    return *currentStack;
}

StackFrame &icsharp::topFrame() {
    switchContext();
    return currentStack->topFrame();
}

void icsharp::validateStackEmptyness() {
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
//    LOG(tout << "Allocated string '" << s << "' with index '" << currentIndex << "'");
    // Increment top index
    topStringIndex++;
    // Return string's index
    return currentIndex;
}
#endif

unsigned entries_count, data_ptr;
std::vector<char> data;
std::vector<unsigned> dataPtrs;

int memSize = 0;

void icsharp::clear_mem() {
    LOG(tout << "clear_mem()" << std::endl);
    entries_count = 0; data_ptr = 0;
    memSize = 3;
    dataPtrs.reserve(memSize);
    data.reserve(memSize * (sizeof(DOUBLE) + sizeof(CorElementType)));
}

void mem(char *value, CorElementType t, size_t size, INT8 idx) {
    if (idx > memSize) {
        memSize = idx + 1;
        dataPtrs.resize(memSize);
        data.resize(memSize * (sizeof(DOUBLE) + sizeof(CorElementType)));
    }
    ++entries_count;
    dataPtrs[idx] = data_ptr;
    unsigned typeSize = sizeof(CorElementType);
    char *p = data.data() + data_ptr;
    *(CorElementType*)p = t;
    memcpy(data.data() + data_ptr + typeSize, value, size);
    data_ptr += size + typeSize;
}

void mem(char *value, CorElementType t, size_t size) {
    mem(value, t, size, (INT8)entries_count);
}

void update(char *value, size_t size, INT8 idx) {
    memcpy(data.data() + dataPtrs[idx] + sizeof(CorElementType), value, size);
}

void icsharp::mem_i1(INT8 value) {
    LOG(tout << "mem_i1 " << (INT64) value << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I1, sizeof(INT8));
}

void icsharp::mem_i1(INT8 value, INT8 idx) {
    LOG(tout << "mem_i1 " << value << " " << idx << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I1, sizeof(INT8), idx);
}

void icsharp::mem_i2(INT16 value) {
    LOG(tout << "mem_i2 " << (INT64) value << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I2, sizeof(INT16));
}

void icsharp::mem_i2(INT16 value, INT8 idx) {
    LOG(tout << "mem_i2 " << value << " " << idx << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I2, sizeof(INT16), idx);
}

void icsharp::mem_i4(INT32 value) {
    LOG(tout << "mem_i4 " << (INT64) value << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I4, sizeof(INT32));
}

void icsharp::mem_i4(INT32 value, INT8 idx) {
    LOG(tout << "mem_i4 " << (INT64) value << " " << (INT64) idx << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I4, sizeof(INT32), idx);
}

void icsharp::mem_i8(INT64 value) {
    LOG(tout << "mem_i8 " << (INT64) value << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I8, sizeof(INT64));
}

void icsharp::mem_i8(INT64 value, INT8 idx) {
    LOG(tout << "mem_i8 " << value << " " << idx << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I8, sizeof(INT64), idx);
}

void icsharp::mem_f4(FLOAT value) {
    LOG(tout << "mem_f4 " << value << std::endl);
    mem((char *) &value, ELEMENT_TYPE_R4, sizeof(FLOAT));
}

void icsharp::mem_f4(FLOAT value, INT8 idx) {
    LOG(tout << "mem_f4 " << value << " " << idx << std::endl);
    mem((char *) &value, ELEMENT_TYPE_R4, sizeof(FLOAT), idx);
}

void icsharp::mem_f8(DOUBLE value) {
    LOG(tout << "mem_f8 " << value << std::endl);
    mem((char *) &value, ELEMENT_TYPE_R8, sizeof(DOUBLE));
}

void icsharp::mem_f8(DOUBLE value, INT8 idx) {
    LOG(tout << "mem_f8 " << value << " " << idx << std::endl);
    mem((char *) &value, ELEMENT_TYPE_R8, sizeof(DOUBLE), idx);
}

void icsharp::mem_p(INT_PTR value) {
    LOG(tout << "mem_p " << value << std::endl);
    mem((char *) &value, ELEMENT_TYPE_PTR, sizeof(INT_PTR));
}

void icsharp::mem_p(INT_PTR value, INT8 idx) {
    LOG(tout << "mem_p " << value << " " << idx << std::endl);
    mem((char *) &value, ELEMENT_TYPE_PTR, sizeof(INT_PTR), idx);
}

void icsharp::update_i1(INT8 value, INT8 idx) {
    LOG(tout << "update_i1 " << (INT64) value << " (index = " << (int)idx << ")" << std::endl);
    update((char *) &value, sizeof(INT8), idx);
}

void icsharp::update_i2(INT16 value, INT8 idx) {
    LOG(tout << "update_i1 " << (INT64) value << " (index = " << (int)idx << ")" << std::endl);
    update((char *) &value, sizeof(INT16), idx);
}

void icsharp::update_i4(INT32 value, INT8 idx) {
    LOG(tout << "update_i4 " << (INT64) value << " (index = " << (int)idx << ")" << std::endl);
    update((char *) &value, sizeof(INT32), idx);
}

void icsharp::update_i8(INT64 value, INT8 idx) {
    LOG(tout << "update_i8 " << (INT64) value << " (index = " << (int)idx << ")" << std::endl);
    update((char *) &value, sizeof(INT64), idx);
}

void icsharp::update_f4(long long value, INT8 idx) {
    DOUBLE tmp;
    std::memcpy(&tmp, &value, sizeof(DOUBLE));
    auto result = (FLOAT) tmp;
    LOG(tout << "update_f4 " << result << " (index = " << (int)idx << ")" << std::endl);
    update((char *) &result, sizeof(FLOAT), idx);
}

void icsharp::update_f8(long long value, INT8 idx) {
    DOUBLE result;
    std::memcpy(&result, &value, sizeof(DOUBLE));
    LOG(tout << "update_f8 " << result << " (index = " << (int)idx << ")" << std::endl);
    update((char *) &result, sizeof(DOUBLE), idx);
}

void icsharp::update_p(INT_PTR value, INT8 idx) {
    LOG(tout << "update_p " << (INT64) value << " (index = " << (int)idx << ")" << std::endl);
    update((char *) &value, sizeof(INT_PTR), idx);
}

CorElementType icsharp::unmemType(INT8 idx) {
    char *ptr = data.data() + dataPtrs[idx];
    return *(CorElementType *) ptr;
}

INT8 icsharp::unmem_i1(INT8 idx) {
    auto ptr = data.data() + dataPtrs[idx];
    assert(*(CorElementType *) ptr == ELEMENT_TYPE_I1);
    auto result = *((INT8*) (ptr + sizeof(CorElementType)));
    LOG(tout << "unmem_i1(" << (int)idx << ") returned " << (int) result);
    return result;
//    return *((INT8*) (data.data() + dataPtrs[idx]));
}

INT16 icsharp::unmem_i2(INT8 idx) {
    auto ptr = data.data() + dataPtrs[idx];
    assert(*(CorElementType *) ptr == ELEMENT_TYPE_I2);
    auto result = *((INT16*) (ptr + sizeof(CorElementType)));
    LOG(tout << "unmem_i2(" << (int)idx << ") returned " << (int) result);
    return result;
//    return *((INT16*) (data.data() + dataPtrs[idx]));
}

INT32 icsharp::unmem_i4(INT8 idx) {
    auto ptr = data.data() + dataPtrs[idx];
    assert(*(CorElementType *) ptr == ELEMENT_TYPE_I4);
    auto result = *((INT32*) (ptr + sizeof(CorElementType)));
    LOG(tout << "unmem_i4(" << (int)idx << ") returned " << (int) result);
    return result;
//    return *((INT32*) (data.data() + dataPtrs[idx]));
}

INT64 icsharp::unmem_i8(INT8 idx) {
    auto ptr = data.data() + dataPtrs[idx];
    assert(*(CorElementType *) ptr == ELEMENT_TYPE_I8);
    auto result = *((INT64*) (ptr + sizeof(CorElementType)));
    LOG(tout << "unmem_i8(" << (int)idx << ") returned " << result);
    return result;
//    return *((INT64*) (data.data() + dataPtrs[idx]));
}

FLOAT icsharp::unmem_f4(INT8 idx) {
    auto ptr = data.data() + dataPtrs[idx];
    assert(*(CorElementType *) ptr == ELEMENT_TYPE_R4);
    auto result = *((FLOAT*) (ptr + sizeof(CorElementType)));
    LOG(tout << "unmem_f4(" << (int)idx << ") returned " << result);
    return result;
//    return *((FLOAT*) (data.data() + dataPtrs[idx]));
}

DOUBLE icsharp::unmem_f8(INT8 idx) {
    auto ptr = data.data() + dataPtrs[idx];
    assert(*(CorElementType *) ptr == ELEMENT_TYPE_R8);
    auto result = *((DOUBLE*) (ptr + sizeof(CorElementType)));
    LOG(tout << "unmem_f8(" << (int)idx << ") returned " << result);
    return result;
}

INT_PTR icsharp::unmem_p(INT8 idx) {
    auto ptr = data.data() + dataPtrs[idx];
    assert(*(CorElementType *) ptr == ELEMENT_TYPE_PTR);
    auto result = *((INT_PTR*) (ptr + sizeof(CorElementType)));
    LOG(tout << "unmem_p(" << (int)idx << ") returned " << result);
    return result;
//    return *((INT_PTR*) (data.data() + dataPtrs[idx]));
}

bool _mainEntered = false;

void icsharp::mainEntered() {
    _mainEntered = true;
}

bool icsharp::mainLeft() {
    return _mainEntered && stack().isEmpty();
}

VirtualAddress icsharp::resolve(INT_PTR p) {
    // TODO: add stack and statics case #do
    return heap.physToVirtAddress(p);
}
