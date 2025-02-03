#include "memory.h"

using namespace vsharp;

std::atomic<int> vsharp::shutdownBlockingRequestsCount {0};
std::atomic_bool vsharp::shutdownInOrder{ false };
