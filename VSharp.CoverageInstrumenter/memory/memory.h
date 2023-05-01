#ifndef MEMORY_H_
#define MEMORY_H_

#include "cor.h"

#include "stack.h"

#include <map>
#include <set>
#include <vector>
#include <unordered_set>
#include <functional>

#define staticSizeOfCoverageNode (2 * sizeof(int) + sizeof(mdMethodDef) + sizeof(OFFSET))
#define READ_BYTES(src, type) *(type*)(src); (src) += sizeof(type)
#define WRITE_BYTES(type, dest, src) *(type*)(dest) = (src); (dest) += sizeof(type)

typedef UINT_PTR ThreadID;

namespace vsharp {

extern std::function<ThreadID()> currentThread;
extern std::map<ThreadID, Stack *> stacks;
extern std::map<ThreadID, int> stackBalances;
extern ThreadID mainThread;

void stackBalanceUp();

// returns true if the stack is not empty
bool stackBalanceDown();
void emptyStacks();
void setMainThread();
void unsetMainThread();
bool isMainThread();

void getLock();
void freeLock();

}

#endif // MEMORY_H_
