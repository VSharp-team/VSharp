#ifndef _PROFILER_H
#define _PROFILER_H

#ifdef UNIX
#include <cstdlib>
#include "pal_mstypes.h"
#include "pal.h"
#include "ntimage.h"
#include "corhdr.h"
#define UINT_PTR_FORMAT "lx"
#endif //UNIX

#ifdef WIN
#include "corhdr.h"
#include <clrtypes.h>
#include <cstdlib>
#define UINT_PTR_FORMAT "llx"
#endif //WIN

#define CoTaskMemAlloc(cb) malloc(cb)
#define CoTaskMemFree(cb) free(cb)

#endif //_PROFILER_H
