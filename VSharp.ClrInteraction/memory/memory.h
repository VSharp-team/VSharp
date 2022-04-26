#ifndef MEMORY_H_
#define MEMORY_H_

#include "cor.h"
#include "stack.h"
#include "storage.h"
#include <functional>
#include <map>

typedef UINT_PTR ThreadID;

namespace vsharp {

extern std::function<ThreadID()> currentThread;
static std::map<ThreadID, Stack *> stacks;
extern Storage heap;
#ifdef _DEBUG
extern std::map<unsigned, const char*> stringsPool;
#endif

Stack &stack();
StackFrame &topFrame();

void mainLeft();
bool isMainLeft();

bool instrumentingEnabled();
void enabledInstrumentation();
void disableInstrumentation();

unsigned allocateString(const char *s);

void validateStackEmptyness();

void resolve(INT_PTR p, VirtualAddress &vAddress);

}

#endif // MEMORY_H_
