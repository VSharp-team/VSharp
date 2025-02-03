#include "api.h"

#include "instrumenter.h"
#include "logging.h"
#include "cComPtr.h"
#include "os.h"
#include "profilerState.h"
#include <vector>

using namespace vsharp;

extern "C" void SetEntryMain(char* assemblyName, int assemblyNameLength, char* moduleName, int moduleNameLength, int methodToken) {
    profilerState->setEntryMain(assemblyName, assemblyNameLength, moduleName, moduleNameLength, methodToken);
    LOG(tout << "received entry main" << std::endl);
}

extern "C" void GetHistory(UINT_PTR size, UINT_PTR bytes) {
    LOG(tout << "GetHistory request received! serializing and writing the response");

    std::atomic_fetch_add(&shutdownBlockingRequestsCount, 1);
    size_t tmpSize;
    historyBuffer = profilerState->coverageTracker->serializeCoverageReport(&tmpSize);
    *(ULONG*)size = tmpSize;
    *(char**)bytes = historyBuffer;

    profilerState->coverageTracker->clear();
    profilerState->threadTracker->clear();

    std::atomic_fetch_sub(&shutdownBlockingRequestsCount, 1);
    LOG(tout << "GetHistory request handled!");
}

extern "C" void ClearHistory() {
    delete historyBuffer;
    historyBuffer = nullptr;
}

extern "C" void SetCurrentThreadId(int mapId) {
    LOG(tout << "Map current thread to: " << mapId);
    vsharp::profilerState->threadTracker->mapCurrentThread(mapId);
}

extern "C" void SetStackBottom() {
    LOG(tout << "Bottom marker was set");
    // TODO: Implement tracking stack size
    int stackBottomMarker;
}
