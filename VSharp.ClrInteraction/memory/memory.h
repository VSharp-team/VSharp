#ifndef MEMORY_H_
#define MEMORY_H_

#include "cor.h"
#include "heap.h"
#include <functional>
#include <map>

typedef UINT_PTR ThreadID;

namespace icsharp {

class Stack;

extern std::function<ThreadID()> currentThread;
static std::map<ThreadID, Stack *> stacks;
//static Heap heap;

unsigned framesCount();
void pushFrame(int stackSize);
void popFrame();

void push1Concrete();
void pop1();
bool pop2Push1();
void pop(unsigned count);
void dup();

// TODO: split everything into check concreteness and send command parts?

void ldarg(INT16 idx);
void ldloc(INT16 idx);
void starg(INT16 idx);
void stloc(INT16 idx);
void brtrue();
void brfalse();
void execSwitch();
void unop(INT16 op);
void ldind(INT_PTR ptr);
bool stind(INT_PTR ptr);
void conv();
void conv_ovf();
void newarr(INT_PTR ptr);
void localloc(INT_PTR ptr);
void alloc(INT_PTR ptr);
void ldobj(INT_PTR ptr);
void ldstr(INT_PTR ptr);
void stobj(INT_PTR ptr);
void initobj(INT_PTR ptr);
void ldlen(INT_PTR ptr);
bool cpobj(INT_PTR dest, INT_PTR src);
bool cpblk(INT_PTR dest, INT_PTR src);
bool initblk(INT_PTR ptr);
void castclass(mdToken typeToken, INT_PTR ptr);
void isinst(mdToken typeToken, INT_PTR ptr);
void box(INT_PTR ptr);
void unbox(mdToken typeToken, INT_PTR ptr);
void unbox_any(mdToken typeToken, INT_PTR ptr);
void ldfld(mdToken fieldToken, INT_PTR ptr);
void ldflda(mdToken fieldToken, INT_PTR ptr);
bool stfld(mdToken fieldToken, INT_PTR ptr);
void ldfsld(mdToken fieldToken);
void stsfld(mdToken fieldToken);
bool ldelema(INT_PTR ptr, INT_PTR index);
bool ldelem(INT_PTR ptr, INT_PTR index);
bool stelem(INT_PTR ptr, INT_PTR index);
void ckfinite();
void ldvirtftn(mdToken token, INT_PTR ptr);
void mkrefany();

void call(mdMethodDef token, UINT16 argsCount);
void callVirt(UINT16 argsCount);
void enter(mdMethodDef token, unsigned maxStackSize);
void leave(UINT8 returnValues);
void finalizeCall(UINT8 returnValues);
void calli(mdSignature signature);
void execThrow();
void execRethrow();

void validateEnd();

void clear_mem();
void mem_i1(INT8 value);
void mem_i2(INT16 value);
void mem_i4(INT32 value);
void mem_i8(INT64 value);
void mem_f4(FLOAT value);
void mem_f8(DOUBLE value);
void mem_p(INT_PTR value);
INT8 unmem_i1(INT8 idx);
INT16 unmem_i2(INT8 idx);
INT32 unmem_i4(INT8 idx);
INT64 unmem_i8(INT8 idx);
FLOAT unmem_f4(INT8 idx);
DOUBLE unmem_f8(INT8 idx);
INT_PTR unmem_p(INT8 idx);

}

#endif // MEMORY_H_
