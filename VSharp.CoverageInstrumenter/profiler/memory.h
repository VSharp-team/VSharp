#ifndef MEMORY_H_
#define MEMORY_H_

#include "cor.h"
#include "corprof.h"
#include "logging.h"
#include "cstring"
#include "profilerDebug.h"
#include <map>
#include <set>
#include <vector>
#include <functional>
#include <shared_mutex>
#include <mutex>
#include <atomic>
#include "threadStorage.h"


typedef UINT_PTR ThreadID;

namespace vsharp {

#define OFFSET UINT32
extern std::atomic<int> shutdownBlockingRequestsCount;
extern std::atomic_bool shutdownInOrder;
}

#endif // MEMORY_H_
