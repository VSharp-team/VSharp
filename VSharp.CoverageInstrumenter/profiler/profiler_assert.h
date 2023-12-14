#ifndef _PROFILER_ASSERT_H
#define _PROFILER_ASSERT_H


#include <cstdio>
#include <cstdlib>
#include "os.h"

#ifdef _PROFILER_DEBUG
#define profiler_assert(e) \
        if (!(e)) { \
            std::fprintf(stderr, "Assertion failed!\nLine: %d\nFunction: %s\nFile: %s", __LINE__, __FUNCTION__, __FILE__); \
            const char* waitDebuggerAttached = std::getenv("WAIT_DEBUGGER_ATTACHED_ON_ASSERT");\
            volatile int done = waitDebuggerAttached == nullptr ? 1 : 0; \
            while (!done) OS::sleepSeconds(1); \
            std::abort(); \
        }
#else
#define profiler_assert(e)
#endif

#endif //_PROFILER_ASSERT_H
