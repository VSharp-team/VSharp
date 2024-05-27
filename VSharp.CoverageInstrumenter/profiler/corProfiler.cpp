#include "corProfiler.h"
#include "logging.h"
#include "memory.h"
#include "cComPtr.h"
#include "profiler.h"
#include "os.h"
#include "coverageTracker.h"
#include "profilerState.h"
#include <locale>
#include <string>
#include <cstring>
#include <codecvt>


#define UNUSED(x) (void)x

using namespace vsharp;

CorProfiler::CorProfiler() : refCount(0), corProfilerInfo(nullptr)
{
}

CorProfiler::~CorProfiler()
{
    if (this->corProfilerInfo != nullptr)
    {
        this->corProfilerInfo->Release();
        this->corProfilerInfo = nullptr;
    }
}

HRESULT STDMETHODCALLTYPE CorProfiler::Initialize(IUnknown *pICorProfilerInfoUnk)
{
    setbuf(stdout, NULL);

    printf("PROFILER INITIALIZATION\n");
    const char* waitDebuggerAttached = std::getenv("COVERAGE_TOOL_WAIT_DEBUGGER_ATTACHED");
    volatile int done = waitDebuggerAttached == nullptr ? 1 : 0;
    while (!done) OS::sleepSeconds(1);

    HRESULT queryInterfaceResult = pICorProfilerInfoUnk->QueryInterface(__uuidof(ICorProfilerInfo8), reinterpret_cast<void **>(&this->corProfilerInfo));

    if (FAILED(queryInterfaceResult))
    {
        return E_FAIL;
    }

    DWORD eventMask =
        COR_PRF_MONITOR_JIT_COMPILATION |
        COR_PRF_DISABLE_ALL_NGEN_IMAGES |
        COR_PRF_DISABLE_OPTIMIZATIONS |
        COR_PRF_MONITOR_EXCEPTIONS |
        COR_PRF_MONITOR_CLR_EXCEPTIONS |
        COR_PRF_DISABLE_TRANSPARENCY_CHECKS_UNDER_FULL_TRUST | /* helps the case where this profiler is used on Full CLR */
        COR_PRF_DISABLE_INLINING;

    // TMP Windows fix
    #undef IfFailRet
    #define IfFailRet(EXPR) do { if (std::atomic_load(&shutdownInOrder)) return S_OK; HRESULT hr = (EXPR); if(FAILED(hr)) { return (hr); } } while (0)
    IfFailRet(this->corProfilerInfo->SetEventMask(eventMask));

    profilerState = new ProfilerState((ICorProfilerInfo8*)this);

    LOG(tout << "Initialize finished" << std::endl);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::Shutdown()
{
    printf("PROFILER SHUTDOWN\n");
    std::atomic_store(&shutdownInOrder, true);

    // waiting until all current requests are resolved
    while (std::atomic_load(&shutdownBlockingRequestsCount) > 0) {}

    LOG(tout << "SHUTDOWN");
    if (profilerState->isPassiveRun) {
        printf("serializing information\n");

        size_t tmpSize;
        auto tmpBytes = profilerState->coverageTracker->serializeCoverageReport(&tmpSize);

        printf("saving to %s\n", profilerState->passiveResultPath);

        std::ofstream fout;
        fout.open(profilerState->passiveResultPath, std::ios::out|std::ios::binary);
        if (!fout.write(tmpBytes, static_cast<long>(tmpSize)))
            printf("failure while saving the file\n");
        fout.close();

        delete tmpBytes;
    }

#ifdef _LOGGING
    close_log();
#endif

    delete profilerState;

    if (this->corProfilerInfo != nullptr)
    {
        this->corProfilerInfo->Release();
        this->corProfilerInfo = nullptr;
    }

    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::AppDomainCreationStarted(AppDomainID appDomainId)
{
    UNUSED(appDomainId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::AppDomainCreationFinished(AppDomainID appDomainId, HRESULT hrStatus)
{
    UNUSED(appDomainId);
    UNUSED(hrStatus);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::AppDomainShutdownStarted(AppDomainID appDomainId)
{
    UNUSED(appDomainId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::AppDomainShutdownFinished(AppDomainID appDomainId, HRESULT hrStatus)
{
    UNUSED(appDomainId);
    UNUSED(hrStatus);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::AssemblyLoadStarted(AssemblyID assemblyId)
{
    UNUSED(assemblyId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::AssemblyLoadFinished(AssemblyID assemblyId, HRESULT hrStatus)
{
    UNUSED(assemblyId);
    UNUSED(hrStatus);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::AssemblyUnloadStarted(AssemblyID assemblyId)
{
    UNUSED(assemblyId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::AssemblyUnloadFinished(AssemblyID assemblyId, HRESULT hrStatus)
{
    UNUSED(assemblyId);
    UNUSED(hrStatus);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ModuleLoadStarted(ModuleID moduleId)
{
    UNUSED(moduleId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ModuleLoadFinished(ModuleID moduleId, HRESULT hrStatus)
{
    UNUSED(moduleId);
    UNUSED(hrStatus);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ModuleUnloadStarted(ModuleID moduleId)
{
    UNUSED(moduleId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ModuleUnloadFinished(ModuleID moduleId, HRESULT hrStatus)
{
    UNUSED(moduleId);
    UNUSED(hrStatus);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ModuleAttachedToAssembly(ModuleID moduleId, AssemblyID assemblyId)
{
    UNUSED(moduleId);
    UNUSED(assemblyId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ClassLoadStarted(ClassID classId)
{
    UNUSED(classId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ClassLoadFinished(ClassID classId, HRESULT hrStatus)
{
    UNUSED(classId);
    UNUSED(hrStatus);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ClassUnloadStarted(ClassID classId)
{
    UNUSED(classId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ClassUnloadFinished(ClassID classId, HRESULT hrStatus)
{
    UNUSED(classId);
    UNUSED(hrStatus);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::FunctionUnloadStarted(FunctionID functionId)
{
    UNUSED(functionId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::JITCompilationStarted(FunctionID functionId, BOOL fIsSafeToBlock)
{
    // the process was finished, ignoring all firther requests
    if (std::atomic_load(&shutdownInOrder)) return S_OK;

    std::atomic_fetch_add(&shutdownBlockingRequestsCount, 1);

    UNUSED(fIsSafeToBlock);
    auto instrument = new Instrumenter(*corProfilerInfo);
    auto methodName = GetFunctionName(functionId);
    HRESULT hr = instrument->instrument(functionId, methodName);
    delete instrument;

    std::atomic_fetch_sub(&shutdownBlockingRequestsCount, 1);
    return hr;
}

HRESULT STDMETHODCALLTYPE CorProfiler::JITCompilationFinished(FunctionID functionId, HRESULT hrStatus, BOOL fIsSafeToBlock)
{
    UNUSED(functionId);
    UNUSED(hrStatus);
    UNUSED(fIsSafeToBlock);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::JITCachedFunctionSearchStarted(FunctionID functionId, BOOL *pbUseCachedFunction)
{
    UNUSED(functionId);
    UNUSED(pbUseCachedFunction);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::JITCachedFunctionSearchFinished(FunctionID functionId, COR_PRF_JIT_CACHE result)
{
    UNUSED(functionId);
    UNUSED(result);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::JITFunctionPitched(FunctionID functionId)
{
    UNUSED(functionId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::JITInlining(FunctionID callerId, FunctionID calleeId, BOOL *pfShouldInline)
{
    UNUSED(callerId);
    UNUSED(calleeId);
    UNUSED(pfShouldInline);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ThreadCreated(ThreadID threadId)
{
    UNUSED(threadId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ThreadDestroyed(ThreadID threadId)
{
    UNUSED(threadId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ThreadAssignedToOSThread(ThreadID managedThreadId, DWORD osThreadId)
{
    UNUSED(managedThreadId);
    UNUSED(osThreadId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::RemotingClientInvocationStarted()
{
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::RemotingClientSendingMessage(GUID *pCookie, BOOL fIsAsync)
{
    UNUSED(pCookie);
    UNUSED(fIsAsync);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::RemotingClientReceivingReply(GUID *pCookie, BOOL fIsAsync)
{
    UNUSED(pCookie);
    UNUSED(fIsAsync);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::RemotingClientInvocationFinished()
{
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::RemotingServerReceivingMessage(GUID *pCookie, BOOL fIsAsync)
{
    UNUSED(pCookie);
    UNUSED(fIsAsync);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::RemotingServerInvocationStarted()
{
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::RemotingServerInvocationReturned()
{
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::RemotingServerSendingReply(GUID *pCookie, BOOL fIsAsync)
{
    UNUSED(pCookie);
    UNUSED(fIsAsync);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::UnmanagedToManagedTransition(FunctionID functionId, COR_PRF_TRANSITION_REASON reason)
{
    UNUSED(functionId);
    UNUSED(reason);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ManagedToUnmanagedTransition(FunctionID functionId, COR_PRF_TRANSITION_REASON reason)
{
    UNUSED(functionId);
    UNUSED(reason);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::RuntimeSuspendStarted(COR_PRF_SUSPEND_REASON suspendReason)
{
    UNUSED(suspendReason);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::RuntimeSuspendFinished()
{
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::RuntimeSuspendAborted()
{
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::RuntimeResumeStarted()
{
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::RuntimeResumeFinished()
{
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::RuntimeThreadSuspended(ThreadID threadId)
{
    UNUSED(threadId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::RuntimeThreadResumed(ThreadID threadId)
{
    UNUSED(threadId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::MovedReferences(ULONG cMovedObjectIDRanges, ObjectID oldObjectIDRangeStart[], ObjectID newObjectIDRangeStart[], ULONG cObjectIDRangeLength[])
{
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ObjectAllocated(ObjectID objectId, ClassID classId)
{
    LOG(tout << "ObjectAllocated: " << GetObjectTypeName(objectId));
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ObjectsAllocatedByClass(ULONG cClassCount, ClassID classIds[], ULONG cObjects[])
{
    UNUSED(cClassCount);
    UNUSED(classIds);
    UNUSED(cObjects);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ObjectReferences(ObjectID objectId, ClassID classId, ULONG cObjectRefs, ObjectID objectRefIds[])
{
    LOG(tout << "ObjectReferences: " << GetObjectTypeName(objectId));
    UNUSED(objectId);
    UNUSED(classId);
    UNUSED(cObjectRefs);
    UNUSED(objectRefIds);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::RootReferences(ULONG cRootRefs, ObjectID rootRefIds[])
{
    UNUSED(cRootRefs);
    UNUSED(rootRefIds);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ExceptionThrown(ObjectID thrownObjectId)
{
    if (std::atomic_load(&shutdownInOrder)) return S_OK;
    auto exceptionName = GetObjectTypeName(thrownObjectId);
    LOG(
        if(profilerState->threadTracker->hasMapping()) {
            tout << "EXCEPTION THROWN: " << exceptionName << " on mapped thread " << profilerState->threadTracker->getCurrentThreadMappedId();
        } else {
            tout << "EXCEPTION THROWN: " << exceptionName;
        }
    );
    if (profilerState->threadTracker->isCurrentThreadTracked()) {
        if (exceptionName == "System.Threading.ThreadAbortException") {
            LOG(tout << "Invocation aborted");
            profilerState->coverageTracker->invocationAborted();
            profilerState->threadTracker->abortCurrentThread();
        }
    }
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ExceptionSearchFunctionEnter(FunctionID functionId)
{
    LOG(tout << "EXCEPTION Search function enter " << GetFunctionName(functionId));
    UNUSED(functionId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ExceptionSearchFunctionLeave()
{
    LOG(tout << "EXCEPTION Search function leave");
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ExceptionSearchFilterEnter(FunctionID functionId)
{
    if (std::atomic_load(&shutdownInOrder) || !profilerState->threadTracker->isCurrentThreadTracked()) return S_OK;
    LOG(tout << "EXCEPTION Search filter enter");
    profilerState->threadTracker->filterEnter();
    UNUSED(functionId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ExceptionSearchFilterLeave()
{
    if (std::atomic_load(&shutdownInOrder) || !profilerState->threadTracker->isCurrentThreadTracked()) return S_OK;
    LOG(tout << "EXCEPTION Search filter leave");
    profilerState->threadTracker->filterLeave();
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ExceptionSearchCatcherFound(FunctionID functionId)
{
    LOG(tout << "EXCEPTION Search catcher found " << GetFunctionName(functionId));
    UNUSED(functionId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ExceptionOSHandlerEnter(UINT_PTR ptr)
{
    LOG(tout << "EXCEPTION OS HANDLER ENTER!");
    UNUSED(ptr);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ExceptionOSHandlerLeave(UINT_PTR ptr)
{
    LOG(tout << "EXCEPTION OS HANDLER LEAVE!");
    UNUSED(ptr);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ExceptionUnwindFunctionEnter(FunctionID functionId)
{
    LOG(tout << "EXCEPTION UNWIND FUNCTION ENTER: " << GetFunctionName(functionId));
    if (profilerState->threadTracker->isCurrentThreadTracked())
        profilerState->threadTracker->unwindFunctionEnter(functionId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ExceptionUnwindFunctionLeave()
{
    LOG(tout << "EXCEPTION UNWIND FUNCTION LEAVE");
    // the process was finished, ignoring all further requests
    if (std::atomic_load(&shutdownInOrder)
        || !profilerState->threadTracker->isCurrentThreadTracked()) return S_OK;
    profilerState->threadTracker->unwindFunctionLeave();
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ExceptionUnwindFinallyEnter(FunctionID functionId)
{
    LOG(tout << "EXCEPTION UNWIND FINALLY ENTER!");
    UNUSED(functionId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ExceptionUnwindFinallyLeave()
{
    LOG(tout << "EXCEPTION UNWIND FINALLY LEAVE!");
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ExceptionCatcherEnter(FunctionID functionId, ObjectID objectId)
{
    LOG(tout << "EXCEPTION CATCHER ENTER!");
    UNUSED(functionId);
    UNUSED(objectId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ExceptionCatcherLeave()
{
    LOG(tout << "EXCEPTION CATCHER Leave!");
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::COMClassicVTableCreated(ClassID wrappedClassId, REFGUID implementedIID, void *pVTable, ULONG cSlots)
{
    UNUSED(wrappedClassId);
    UNUSED(implementedIID);
    UNUSED(pVTable);
    UNUSED(cSlots);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::COMClassicVTableDestroyed(ClassID wrappedClassId, REFGUID implementedIID, void *pVTable)
{
    UNUSED(wrappedClassId);
    UNUSED(implementedIID);
    UNUSED(pVTable);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ExceptionCLRCatcherFound()
{
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ExceptionCLRCatcherExecute()
{
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ThreadNameChanged(ThreadID threadId, ULONG cchName, WCHAR name[])
{
    UNUSED(threadId);
    UNUSED(cchName);
    UNUSED(name);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::GarbageCollectionStarted(int cGenerations, BOOL generationCollected[], COR_PRF_GC_REASON reason)
{
    UNUSED(cGenerations);
    UNUSED(generationCollected);
    UNUSED(reason);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::SurvivingReferences(ULONG cSurvivingObjectIDRanges, ObjectID objectIDRangeStart[], ULONG cObjectIDRangeLength[])
{
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::GarbageCollectionFinished()
{
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::FinalizeableObjectQueued(DWORD finalizerFlags, ObjectID objectId)
{
    UNUSED(finalizerFlags);
    UNUSED(objectId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::RootReferences2(ULONG cRootRefs, ObjectID rootRefIds[], COR_PRF_GC_ROOT_KIND rootKinds[], COR_PRF_GC_ROOT_FLAGS rootFlags[], UINT_PTR rootIds[])
{
    UNUSED(cRootRefs);
    UNUSED(rootRefIds);
    UNUSED(rootKinds);
    UNUSED(rootFlags);
    UNUSED(rootIds);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::HandleCreated(GCHandleID handleId, ObjectID initialObjectId)
{
    UNUSED(handleId);
    UNUSED(initialObjectId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::HandleDestroyed(GCHandleID handleId)
{
    UNUSED(handleId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::InitializeForAttach(IUnknown *pCorProfilerInfoUnk, void *pvClientData, UINT cbClientData)
{
    UNUSED(pCorProfilerInfoUnk);
    UNUSED(pvClientData);
    UNUSED(cbClientData);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ProfilerAttachComplete()
{
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ProfilerDetachSucceeded()
{
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ReJITCompilationStarted(FunctionID functionId, ReJITID rejitId, BOOL fIsSafeToBlock)
{
    UNUSED(functionId);
    UNUSED(rejitId);
    UNUSED(fIsSafeToBlock);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::GetReJITParameters(ModuleID moduleId, mdMethodDef methodId, ICorProfilerFunctionControl *pFunctionControl)
{
    UNUSED(moduleId);
    UNUSED(methodId);
    UNUSED(pFunctionControl);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ReJITCompilationFinished(FunctionID functionId, ReJITID rejitId, HRESULT hrStatus, BOOL fIsSafeToBlock)
{
    UNUSED(functionId);
    UNUSED(rejitId);
    UNUSED(hrStatus);
    UNUSED(fIsSafeToBlock);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ReJITError(ModuleID moduleId, mdMethodDef methodId, FunctionID functionId, HRESULT hrStatus)
{
    UNUSED(moduleId);
    UNUSED(methodId);
    UNUSED(functionId);
    UNUSED(hrStatus);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::MovedReferences2(ULONG cMovedObjectIDRanges, ObjectID oldObjectIDRangeStart[], ObjectID newObjectIDRangeStart[], SIZE_T cObjectIDRangeLength[])
{
    UNUSED(cMovedObjectIDRanges);
    UNUSED(oldObjectIDRangeStart);
    UNUSED(newObjectIDRangeStart);
    UNUSED(cObjectIDRangeLength);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::SurvivingReferences2(ULONG cSurvivingObjectIDRanges, ObjectID objectIDRangeStart[], SIZE_T cObjectIDRangeLength[])
{
    UNUSED(cSurvivingObjectIDRanges);
    UNUSED(objectIDRangeStart);
    UNUSED(cObjectIDRangeLength);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ConditionalWeakTableElementReferences(ULONG cRootRefs, ObjectID keyRefIds[], ObjectID valueRefIds[], GCHandleID rootIds[])
{
    UNUSED(cRootRefs);
    UNUSED(keyRefIds);
    UNUSED(valueRefIds);
    UNUSED(rootIds);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::GetAssemblyReferences(const WCHAR *wszAssemblyPath, ICorProfilerAssemblyReferenceProvider *pAsmRefProvider)
{
    UNUSED(wszAssemblyPath);
    UNUSED(pAsmRefProvider);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::ModuleInMemorySymbolsUpdated(ModuleID moduleId)
{
    UNUSED(moduleId);
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::DynamicMethodJITCompilationStarted(FunctionID functionId, BOOL fIsSafeToBlock, LPCBYTE ilHeader, ULONG cbILHeader)
{
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CorProfiler::DynamicMethodJITCompilationFinished(FunctionID functionId, HRESULT hrStatus, BOOL fIsSafeToBlock)
{
    return S_OK;
}

std::string CorProfiler::GetObjectTypeName(ObjectID objectId) {
    if (std::atomic_load(&shutdownInOrder)) return "";
    ClassID classId;
    corProfilerInfo->GetClassFromObject(objectId, &classId);

    ModuleID moduleId;
    mdTypeDef type;
    corProfilerInfo->GetClassIDInfo(classId, &moduleId, &type);

    CComPtr<IMetaDataImport> spMetadata;
    corProfilerInfo->GetModuleMetaData(moduleId, ofRead, IID_IMetaDataImport, reinterpret_cast<IUnknown**>(&spMetadata));

    WCHAR name[256];
    ULONG nameSize = 256;
    DWORD flags;
    mdTypeDef baseType;
    spMetadata->GetTypeDefProps(type, name, 256, &nameSize, &flags, &baseType);

    return OS::unicodeToAnsi(name);
}

std::string CorProfiler::GetFunctionName(FunctionID functionId) {
    if (std::atomic_load(&shutdownInOrder)) return "";
    ClassID classId;
    ModuleID moduleId;
    mdToken token;
    corProfilerInfo->GetFunctionInfo(functionId, &classId, &moduleId, &token);

    CComPtr<IMetaDataImport> spMetadata;
    corProfilerInfo->GetModuleMetaData(moduleId, ofRead, IID_IMetaDataImport, reinterpret_cast<IUnknown**>(&spMetadata));

    WCHAR name[256];
    ULONG nameSize = 256;
    DWORD flags;
    mdTypeDef baseType;

    spMetadata->GetMethodProps(token, NULL, name, nameSize, &flags, NULL, NULL, NULL, NULL, NULL);

    return OS::unicodeToAnsi(name);
}
