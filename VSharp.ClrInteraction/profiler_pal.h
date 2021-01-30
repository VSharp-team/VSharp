#ifndef PROFILER_PAL_H_
#define PROFILER_PAL_H_

#ifndef WIN32
#include <cstdlib>
#include "pal_mstypes.h"
#include "pal.h"
#include "ntimage.h"
#include "corhdr.h"

#define CoTaskMemAlloc(cb) malloc(cb)
#define CoTaskMemFree(cb) free(cb)

#define UINT_PTR_FORMAT "lx"

#else
#define UINT_PTR_FORMAT "llx"
#endif

#endif // PROFILER_PAL_H_
