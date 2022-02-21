#ifndef PROFILER_WIN_H_
#define PROFILER_WIN_H_

#ifndef UNIX
#include "corhdr.h"
#include "debugmacros.h"
#include <clrtypes.h>
#include <cstdlib>

#define CoTaskMemAlloc(cb) malloc(cb)
#define CoTaskMemFree(cb) free(cb)

#define UINT_PTR_FORMAT "llx"

#else
#define UINT_PTR_FORMAT "lx"
#endif

#endif // PROFILER_WIN_H_
