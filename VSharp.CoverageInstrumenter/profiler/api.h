#ifndef VSHARP_COVERAGEINSTRUMENTER_API_H
#define VSHARP_COVERAGEINSTRUMENTER_API_H

#include "os.h"

#ifdef IMAGEHANDLER_EXPORTS
#define IMAGEHANDLER_API __declspec(dllexport)
#else
#define IMAGEHANDLER_API __declspec(dllimport)
#endif

extern "C" IMAGEHANDLER_API void SetEntryMain(char* assemblyName, int assemblyNameLength, char* moduleName, int moduleNameLength, int methodToken);
extern "C" IMAGEHANDLER_API void GetHistory(UINT_PTR size, UINT_PTR bytes);
extern "C" IMAGEHANDLER_API void SetCurrentThreadId(int mapId);
extern "C" IMAGEHANDLER_API void ClearHistory();

static char* historyBuffer = nullptr;

#endif //VSHARP_COVERAGEINSTRUMENTER_API_H
