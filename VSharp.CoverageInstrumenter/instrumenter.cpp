#include "instrumenter.h"
#include "logging.h"
#include "cComPtr.h"
#include <vector>
#include <assert.h>

using namespace vsharp;

#define TOKENPASTE(x, y) x ## y
#define TOKENPASTE2(x, y) TOKENPASTE(x, y)
#define UNIQUE TOKENPASTE2(Sig, __LINE__)
#define SIG_DEF(...) \
    constexpr COR_SIGNATURE UNIQUE[] = {IMAGE_CEE_CS_CALLCONV_STDCALL, __VA_ARGS__};\
    IfFailRet(metadataEmit->GetTokenFromSig(UNIQUE, sizeof(UNIQUE), &signatureToken));\
    tokens.push_back(signatureToken);

#define ELEMENT_TYPE_COND ELEMENT_TYPE_I
#define ELEMENT_TYPE_TOKEN ELEMENT_TYPE_U4
#define ELEMENT_TYPE_OFFSET ELEMENT_TYPE_I4
#define ELEMENT_TYPE_SIZE ELEMENT_TYPE_U

WCHAR* vsharp::mainAssemblyName = nullptr;
int vsharp::mainAssemblyNameLength = 0;
WCHAR* vsharp::mainModuleName = nullptr;
int vsharp::mainModuleNameLength = 0;
mdMethodDef vsharp::mainToken = 0;
bool vsharp::rewriteMainOnly = false;

void SetEntryMain(char* assemblyName, int assemblyNameLength, char* moduleName, int moduleNameLength, int methodToken) {
    mainAssemblyNameLength = assemblyNameLength;
    mainAssemblyName = new WCHAR[assemblyNameLength];
    memcpy(mainAssemblyName, assemblyName, assemblyNameLength * sizeof(WCHAR));

    mainModuleNameLength = moduleNameLength;
    mainModuleName = new WCHAR[moduleNameLength];
    memcpy(mainModuleName, moduleName, moduleNameLength * sizeof(WCHAR));

    mainToken = methodToken;

    LOG(tout << "received entry main" << std::endl);
}

void GetHistory(UINT_PTR size, UINT_PTR bytes) {
    LOG(tout << "GetHistory request received! serializing and writing the response");

    auto sizeBytes = sizeof(int);
    for (auto el : coverageHistory) {
        sizeBytes += el->size();
    }

    char *buffer = (char*)malloc(sizeBytes); // the buffer pointer moves further after each serialization
    auto beginning = buffer; // remembering the first point to check the sizes were counted correctly
    WRITE_BYTES(int, buffer, coverageHistory.size());
    for (auto el : coverageHistory) {
        el->serialize(buffer);
    }
    assert(buffer - beginning == sizeBytes);
    *(ULONG*)size = sizeBytes;
    *(char**)bytes = beginning;

    clearCoverageCollection(); // freeing up the history
}

std::set<std::pair<FunctionID, ModuleID>> vsharp::instrumentedMethods;

HRESULT initTokens(const CComPtr<IMetaDataEmit> &metadataEmit, std::vector<mdSignature> &tokens) {
    auto covProb = getProbes();
    mdSignature signatureToken;
    SIG_DEF(0x01, ELEMENT_TYPE_VOID, ELEMENT_TYPE_OFFSET)
    covProb->Finalize_Call->setSig(signatureToken);
    SIG_DEF(0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_OFFSET, ELEMENT_TYPE_I4)
    covProb->Branch->setSig(signatureToken);
    covProb->Call->setSig(signatureToken);
    covProb->Leave->setSig(signatureToken);
    covProb->Throw->setSig(signatureToken);
    covProb->Stsfld->setSig(signatureToken);
    covProb->Coverage->setSig(signatureToken);
    covProb->Tailcall->setSig(signatureToken);
    covProb->LeaveMain->setSig(signatureToken);
    SIG_DEF(0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_OFFSET, ELEMENT_TYPE_I4, ELEMENT_TYPE_I4)
    covProb->EnterMain->setSig(signatureToken);
    covProb->Enter->setSig(signatureToken);
    return S_OK;
}

Instrumenter::Instrumenter(ICorProfilerInfo8 &profilerInfo)
    : m_profilerInfo(profilerInfo)
    , m_moduleId(0)
    , m_signatureTokens(nullptr)
{
}

Instrumenter::~Instrumenter()
{
    delete[] m_signatureTokens;
}


bool Instrumenter::currentMethodIsMain(const WCHAR *moduleName, int moduleSize, mdMethodDef method) const {
    // NOTE: decrementing 'moduleSize', because of null terminator
    if (mainModuleName == nullptr)
        return false;
    if (mainModuleNameLength != moduleSize - 1 || mainToken != method)
        return false;
    for (int i = 0; i < mainModuleNameLength; i++)
        if (mainModuleName[i] != moduleName[i]) return false;
    return true;
}

HRESULT Instrumenter::doInstrumentation(ModuleID oldModuleId, int methodId, const WCHAR *moduleName, ULONG moduleNameLength) {
    HRESULT hr;
    CComPtr<IMetaDataImport> metadataImport;
    CComPtr<IMetaDataEmit> metadataEmit;
    IfFailRet(m_profilerInfo.GetModuleMetaData(m_moduleId, ofRead | ofWrite, IID_IMetaDataImport, reinterpret_cast<IUnknown **>(&metadataImport)));
    IfFailRet(metadataImport->QueryInterface(IID_IMetaDataEmit, reinterpret_cast<void **>(&metadataEmit)));

    if (oldModuleId != m_moduleId) {
        delete[] m_signatureTokens;
        std::vector<mdSignature> tokens;
        initTokens(metadataEmit, tokens);
        m_signatureTokensLength = tokens.size() * sizeof(mdSignature);
        m_signatureTokens = new char[m_signatureTokensLength];
        memcpy(m_signatureTokens, (char *)&tokens[0], m_signatureTokensLength);
    }

    RewriteIL(&m_profilerInfo, nullptr, m_moduleId, m_jittedToken, methodId, currentMethodIsMain(moduleName, moduleNameLength, m_jittedToken), rewriteMainOnly);

    return S_OK;
}

HRESULT Instrumenter::instrument(FunctionID functionId) {
    HRESULT hr = S_OK;
    ModuleID newModuleId;
    ClassID classId;
    IfFailRet(m_profilerInfo.GetFunctionInfo(functionId, &classId, &newModuleId, &m_jittedToken));
    assert((m_jittedToken & 0xFF000000L) == mdtMethodDef);

    LPCBYTE baseLoadAddress;
    ULONG moduleNameLength;
    AssemblyID assembly;
    IfFailRet(m_profilerInfo.GetModuleInfo(newModuleId, &baseLoadAddress, 0, &moduleNameLength, nullptr, &assembly));
    WCHAR *moduleName = new WCHAR[moduleNameLength];
    IfFailRet(m_profilerInfo.GetModuleInfo(newModuleId, &baseLoadAddress, moduleNameLength, &moduleNameLength, moduleName, &assembly));
    ULONG assemblyNameLength;
    AppDomainID appDomainId;
    ModuleID startModuleId;
    IfFailRet(m_profilerInfo.GetAssemblyInfo(assembly, 0, &assemblyNameLength, nullptr, &appDomainId, &startModuleId));
    WCHAR *assemblyName = new WCHAR[assemblyNameLength];
    IfFailRet(m_profilerInfo.GetAssemblyInfo(assembly, assemblyNameLength, &assemblyNameLength, assemblyName, &appDomainId, &startModuleId));

    // checking if this method was rewritten before
    if (instrumentedMethods.find({ m_jittedToken, newModuleId }) != instrumentedMethods.end()) {
        LOG(tout << "repeated JIT of " << m_jittedToken << "! skipped" << std::endl);
        return S_OK;
    }

    getLock();
    int currentMethodId = collectedMethods.size();
    collectedMethods.push_back({m_jittedToken, assemblyNameLength, assemblyName, moduleNameLength, moduleName});
    instrumentedMethods.insert({m_jittedToken, newModuleId});
    freeLock();
    ModuleID oldModuleId = m_moduleId;
    m_moduleId = newModuleId;
    hr = doInstrumentation(oldModuleId, currentMethodId, moduleName, moduleNameLength);

    return hr;
}
