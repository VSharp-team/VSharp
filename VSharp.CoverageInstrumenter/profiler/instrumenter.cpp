#include "instrumenter.h"
#include "logging.h"
#include "cComPtr.h"
#include "os.h"
#include "profilerState.h"
#include <vector>


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

std::set<std::pair<FunctionID, ModuleID>> vsharp::instrumentedMethods;

HRESULT initTokens(const CComPtr<IMetaDataEmit> &metadataEmit, std::vector<mdSignature> &tokens) {
    auto covProb = getProbes();
    mdSignature signatureToken;
    SIG_DEF(0x01, ELEMENT_TYPE_VOID, ELEMENT_TYPE_OFFSET)
    covProb->Finalize_Call                  ->setSig(signatureToken);

    SIG_DEF(0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_OFFSET, ELEMENT_TYPE_I4)
    covProb->Branch                         ->setSig(signatureToken);
    covProb->Call                           ->setSig(signatureToken);
    covProb->Leave                          ->setSig(signatureToken);
    covProb->Throw                          ->setSig(signatureToken);
    covProb->Stsfld                         ->setSig(signatureToken);
    covProb->Coverage                       ->setSig(signatureToken);
    covProb->Tailcall                       ->setSig(signatureToken);
    covProb->LeaveMain                      ->setSig(signatureToken);

    SIG_DEF(0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_OFFSET, ELEMENT_TYPE_I4, ELEMENT_TYPE_I4)
    covProb->EnterMain                      ->setSig(signatureToken);
    covProb->Enter                          ->setSig(signatureToken);

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

bool vsharp::IsMain(const WCHAR *moduleName, int moduleSize, mdMethodDef method) {
    // NOTE: decrementing 'moduleSize', because of null terminator
    if (profilerState->mainMethodInfo.moduleName == nullptr)
        return false;
    if (profilerState->mainMethodInfo.moduleNameLength != moduleSize - 1 || profilerState->mainMethodInfo.token != method)
        return false;
    for (int i = 0; i < profilerState->mainMethodInfo.moduleNameLength; i++)
        if (profilerState->mainMethodInfo.moduleName[i] != moduleName[i]) return false;

    return true;
}

bool doesContainAssembly(const WCHAR *assemblyName, int assemblyNameLength, std::vector<std::string> &assemblyList) {
    auto asmName = std::string(assemblyName, assemblyName + assemblyNameLength - 1);

    // safety failcheck as instrumenting System.Private results in an incorrect exit from the profiler
    if (asmName.find("System.Private") == 0) return false;

    for (auto approved : assemblyList) {
        if (assemblyNameLength - 1 == approved.length() && asmName == approved)
            return true;
    }
    return false;
}

bool vsharp::InstrumentationIsNeeded(const WCHAR* assemblyName, int assemblySize, const WCHAR *moduleName, int moduleSize, mdMethodDef method) {
    bool shouldInstrumentAll = !profilerState->isTestExpected && !profilerState->collectMainOnly;
    bool fromTestAssembly = profilerState->isTestExpected && doesContainAssembly(assemblyName, assemblySize, profilerState->approvedAssemblies);
    return IsMain(moduleName, moduleSize, method)
        || shouldInstrumentAll
        || fromTestAssembly;
}

HRESULT Instrumenter::doInstrumentation(ModuleID oldModuleId, size_t methodId, const WCHAR *moduleName, ULONG moduleNameLength) {
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

    RewriteIL(
            &m_profilerInfo,
            nullptr,
            m_moduleId,
            m_jittedToken,
            methodId,
            IsMain(moduleName, moduleNameLength, m_jittedToken),
            profilerState->isTestExpected
    );

    return S_OK;
}

HRESULT Instrumenter::instrument(FunctionID functionId, std::string methodName) {
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

    // checking if this method was rewritten before and if it is in need of rewriting
    if (instrumentedMethods.find({ m_jittedToken, newModuleId }) != instrumentedMethods.end()
        || !InstrumentationIsNeeded(assemblyName, assemblyNameLength, moduleName, moduleNameLength, m_jittedToken)) {
        delete[] moduleName;
        delete[] assemblyName;
        return S_OK;
    }

    if (profilerState->collectMainOnly) {
        profilerState->mainFunctionId = functionId;
    }

    mutex.lock();
    size_t currentMethodId = profilerState->coverageTracker->collectMethod({
            m_jittedToken,
            assemblyNameLength,
            assemblyName,
            moduleNameLength,
            moduleName,
            methodName}
        );
    instrumentedMethods.insert({m_jittedToken, newModuleId});
    mutex.unlock();

    profilerState->funcIdToMethodId[functionId] = currentMethodId;

    ModuleID oldModuleId = m_moduleId;
    m_moduleId = newModuleId;
    hr = doInstrumentation(oldModuleId, currentMethodId, moduleName, moduleNameLength);

    return hr;
}
