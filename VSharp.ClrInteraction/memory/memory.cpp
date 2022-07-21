#include "memory.h"
#include "stack.h"
#include <mutex>

using namespace vsharp;

ThreadID currentThreadNotConfigured() {
    throw std::logic_error("Current thread getter is not configured!");
}

std::function<ThreadID()> vsharp::currentThread(&currentThreadNotConfigured);

Heap vsharp::heap = Heap();

#ifdef _DEBUG
std::map<unsigned, const char*> vsharp::stringsPool;
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

Stack &vsharp::stack() {
    switchContext();
    return *currentStack;
}

StackFrame &vsharp::topFrame() {
    switchContext();
    return currentStack->topFrame();
}

void vsharp::validateStackEmptyness() {
#ifdef _DEBUG
    for (auto &kv : stacks) {
        if (!kv.second->isEmpty()) {
            FAIL_LOUD("Stack is not empty after program termination!!");
        }
    }
#endif
}

#ifdef _DEBUG
unsigned vsharp::allocateString(const char *s) {
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

void vsharp::clear_mem() {
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

void vsharp::mem_i1(INT8 value) {
    LOG(tout << "mem_i1 " << (INT64) value << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I1, sizeof(INT8));
}

void vsharp::mem_i1(INT8 value, INT8 idx) {
    LOG(tout << "mem_i1 " << value << " " << idx << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I1, sizeof(INT8), idx);
}

void vsharp::mem_i2(INT16 value) {
    LOG(tout << "mem_i2 " << (INT64) value << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I2, sizeof(INT16));
}

void vsharp::mem_i2(INT16 value, INT8 idx) {
    LOG(tout << "mem_i2 " << value << " " << idx << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I2, sizeof(INT16), idx);
}

void vsharp::mem_i4(INT32 value) {
    LOG(tout << "mem_i4 " << (INT64) value << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I4, sizeof(INT32));
}

void vsharp::mem_i4(INT32 value, INT8 idx) {
    LOG(tout << "mem_i4 " << (INT64) value << " " << (INT64) idx << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I4, sizeof(INT32), idx);
}

void vsharp::mem_i8(INT64 value) {
    LOG(tout << "mem_i8 " << (INT64) value << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I8, sizeof(INT64));
}

void vsharp::mem_i8(INT64 value, INT8 idx) {
    LOG(tout << "mem_i8 " << value << " " << idx << std::endl);
    mem((char *) &value, ELEMENT_TYPE_I8, sizeof(INT64), idx);
}

void vsharp::mem_f4(FLOAT value) {
    LOG(tout << "mem_f4 " << value << std::endl);
    mem((char *) &value, ELEMENT_TYPE_R4, sizeof(FLOAT));
}

void vsharp::mem_f4(FLOAT value, INT8 idx) {
    LOG(tout << "mem_f4 " << value << " " << idx << std::endl);
    mem((char *) &value, ELEMENT_TYPE_R4, sizeof(FLOAT), idx);
}

void vsharp::mem_f8(DOUBLE value) {
    LOG(tout << "mem_f8 " << value << std::endl);
    mem((char *) &value, ELEMENT_TYPE_R8, sizeof(DOUBLE));
}

void vsharp::mem_f8(DOUBLE value, INT8 idx) {
    LOG(tout << "mem_f8 " << value << " " << idx << std::endl);
    mem((char *) &value, ELEMENT_TYPE_R8, sizeof(DOUBLE), idx);
}

void vsharp::mem_p(INT_PTR value) {
    LOG(tout << "mem_p " << value << std::endl);
    mem((char *) &value, ELEMENT_TYPE_PTR, sizeof(INT_PTR));
}

void vsharp::mem_p(INT_PTR value, INT8 idx) {
    LOG(tout << "mem_p " << value << " " << idx << std::endl);
    mem((char *) &value, ELEMENT_TYPE_PTR, sizeof(INT_PTR), idx);
}

void vsharp::update_i1(INT8 value, INT8 idx) {
    LOG(tout << "update_i1 " << (INT64) value << " (index = " << (int)idx << ")" << std::endl);
    update((char *) &value, sizeof(INT8), idx);
}

void vsharp::update_i2(INT16 value, INT8 idx) {
    LOG(tout << "update_i1 " << (INT64) value << " (index = " << (int)idx << ")" << std::endl);
    update((char *) &value, sizeof(INT16), idx);
}

void vsharp::update_i4(INT32 value, INT8 idx) {
    LOG(tout << "update_i4 " << (INT64) value << " (index = " << (int)idx << ")" << std::endl);
    update((char *) &value, sizeof(INT32), idx);
}

void vsharp::update_i8(INT64 value, INT8 idx) {
    LOG(tout << "update_i8 " << (INT64) value << " (index = " << (int)idx << ")" << std::endl);
    update((char *) &value, sizeof(INT64), idx);
}

void vsharp::update_f4(long long value, INT8 idx) {
    DOUBLE tmp;
    memcpy(&tmp, &value, sizeof(DOUBLE));
    auto result = (FLOAT) tmp;
    LOG(tout << "update_f4 " << result << " (index = " << (int)idx << ")" << std::endl);
    update((char *) &result, sizeof(FLOAT), idx);
}

void vsharp::update_f8(long long value, INT8 idx) {
    DOUBLE result;
    memcpy(&result, &value, sizeof(DOUBLE));
    LOG(tout << "update_f8 " << result << " (index = " << (int)idx << ")" << std::endl);
    update((char *) &result, sizeof(DOUBLE), idx);
}

void vsharp::update_p(INT_PTR value, INT8 idx) {
    LOG(tout << "update_p " << (INT64) value << " (index = " << (int)idx << ")" << std::endl);
    update((char *) &value, sizeof(INT_PTR), idx);
}

CorElementType vsharp::unmemType(INT8 idx) {
    char *ptr = data.data() + dataPtrs[idx];
    return *(CorElementType *) ptr;
}

INT8 vsharp::unmem_i1(INT8 idx) {
    auto ptr = data.data() + dataPtrs[idx];
    assert(*(CorElementType *) ptr == ELEMENT_TYPE_I1);
    auto result = *((INT8*) (ptr + sizeof(CorElementType)));
    LOG(tout << "unmem_i1(" << (int)idx << ") returned " << (int) result);
    return result;
//    return *((INT8*) (data.data() + dataPtrs[idx]));
}

INT16 vsharp::unmem_i2(INT8 idx) {
    auto ptr = data.data() + dataPtrs[idx];
    assert(*(CorElementType *) ptr == ELEMENT_TYPE_I2);
    auto result = *((INT16*) (ptr + sizeof(CorElementType)));
    LOG(tout << "unmem_i2(" << (int)idx << ") returned " << (int) result);
    return result;
//    return *((INT16*) (data.data() + dataPtrs[idx]));
}

INT32 vsharp::unmem_i4(INT8 idx) {
    auto ptr = data.data() + dataPtrs[idx];
    assert(*(CorElementType *) ptr == ELEMENT_TYPE_I4);
    auto result = *((INT32*) (ptr + sizeof(CorElementType)));
    LOG(tout << "unmem_i4(" << (int)idx << ") returned " << (int) result);
    return result;
//    return *((INT32*) (data.data() + dataPtrs[idx]));
}

INT64 vsharp::unmem_i8(INT8 idx) {
    auto ptr = data.data() + dataPtrs[idx];
    assert(*(CorElementType *) ptr == ELEMENT_TYPE_I8);
    auto result = *((INT64*) (ptr + sizeof(CorElementType)));
    LOG(tout << "unmem_i8(" << (int)idx << ") returned " << result);
    return result;
//    return *((INT64*) (data.data() + dataPtrs[idx]));
}

FLOAT vsharp::unmem_f4(INT8 idx) {
    auto ptr = data.data() + dataPtrs[idx];
    assert(*(CorElementType *) ptr == ELEMENT_TYPE_R4);
    auto result = *((FLOAT*) (ptr + sizeof(CorElementType)));
    LOG(tout << "unmem_f4(" << (int)idx << ") returned " << result);
    return result;
//    return *((FLOAT*) (data.data() + dataPtrs[idx]));
}

DOUBLE vsharp::unmem_f8(INT8 idx) {
    auto ptr = data.data() + dataPtrs[idx];
    assert(*(CorElementType *) ptr == ELEMENT_TYPE_R8);
    auto result = *((DOUBLE*) (ptr + sizeof(CorElementType)));
    LOG(tout << "unmem_f8(" << (int)idx << ") returned " << result);
    return result;
}

INT_PTR vsharp::unmem_p(INT8 idx) {
    auto ptr = data.data() + dataPtrs[idx];
    assert(*(CorElementType *) ptr == ELEMENT_TYPE_PTR);
    auto result = *((INT_PTR*) (ptr + sizeof(CorElementType)));
    LOG(tout << "unmem_p(" << (int)idx << ") returned " << result);
    return result;
//    return *((INT_PTR*) (data.data() + dataPtrs[idx]));
}

bool _mainEntered = false;

void vsharp::mainEntered() {
    _mainEntered = true;
}

bool vsharp::mainLeft() {
    return _mainEntered && stack().isEmpty();
}

VirtualAddress vsharp::resolve(INT_PTR p) {
    // TODO: add stack and statics case #do
    return heap.physToVirtAddress(p);
}
