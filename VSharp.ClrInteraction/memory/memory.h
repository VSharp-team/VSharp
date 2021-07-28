#ifndef MEMORY_H_
#define MEMORY_H_

#include "cor.h"
#include "heap.h"
#include <functional>
#include <map>

typedef UINT_PTR ThreadID;

namespace icsharp {

class Stack;
class Heap;

extern std::function<ThreadID()> currentThread;
static std::map<ThreadID, Stack*> stacks;
static Heap heap;
#ifdef _DEBUG
extern std::map<unsigned, const char*> stringsPool;
#endif

unsigned framesCount();

void push1Concrete();
// TODO: remove it!
void pushConcrete(unsigned count);
void pop1();
void pop(unsigned count);
void pop2Push1();

void call(unsigned token, unsigned argsCount);
void callVirt(unsigned argsCount);
void enter(unsigned token, unsigned maxStackSize);
void leave(unsigned returnValues);
void finalizeCall(unsigned returnValues);

void validateEnd();

unsigned allocateString(const char *s);
}

#endif // MEMORY_H_
