#ifndef MEMORY_H_
#define MEMORY_H_

#include "cor.h"
#include "stack.h"
#include "heap.h"
#include <functional>
#include <map>

typedef UINT_PTR ThreadID;

namespace vsharp {

extern std::function<ThreadID()> currentThread;
static std::map<ThreadID, Stack *> stacks;
extern Heap heap;
#ifdef _DEBUG
extern std::map<unsigned, const char*> stringsPool;
#endif

Stack &stack();
StackFrame &topFrame();

void mainEntered();
bool mainLeft();

unsigned allocateString(const char *s);

INT8 entriesCount();
void clear_mem();
void mem_i1(INT8 value);
void mem_i1(INT8 value, INT8 idx);
void mem_i2(INT16 value);
void mem_i2(INT16 value, INT8 idx);
void mem_i4(INT32 value);
void mem_i4(INT32 value, INT8 idx);
void mem_i8(INT64 value);
void mem_i8(INT64 value, INT8 idx);
void mem_f4(FLOAT value);
void mem_f4(FLOAT value, INT8 idx);
void mem_f8(DOUBLE value);
void mem_f8(DOUBLE value, INT8 idx);
void mem_p(INT_PTR value);
void mem_p(INT_PTR value, INT8 idx);
void update_i1(INT8 value, INT8 idx);
void update_i2(INT16 value, INT8 idx);
void update_i4(INT32 value, INT8 idx);
void update_i8(INT64 value, INT8 idx);
void update_f4(long long value, INT8 idx);
void update_f8(long long value, INT8 idx);
void update_p(INT_PTR value, INT8 idx);
CorElementType unmemType(INT8 idx);
INT8 unmem_i1(INT8 idx);
INT16 unmem_i2(INT8 idx);
INT32 unmem_i4(INT8 idx);
INT64 unmem_i8(INT8 idx);
FLOAT unmem_f4(INT8 idx);
DOUBLE unmem_f8(INT8 idx);
INT_PTR unmem_p(INT8 idx);

void validateStackEmptyness();

VirtualAddress resolve(INT_PTR p);

}

#endif // MEMORY_H_
