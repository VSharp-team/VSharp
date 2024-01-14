#include "memory.h"

using namespace vsharp;

std::atomic<int> vsharp::shutdownBlockingRequestsCount {0};
