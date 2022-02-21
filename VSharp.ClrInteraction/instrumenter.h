#ifndef INSTRUMENTER_H_
#define INSTRUMENTER_H_

#include <set>
#include "corProfiler.h"
#include "cComPtr.h"

struct COR_ILMETHOD_SECT_EH;
struct IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT;

namespace vsharp {

class Protocol;

struct MethodInfo {
    unsigned token;
    char *bytecode;
    unsigned codeLength;
    unsigned maxStackSize;
    char *ehs;
    unsigned ehsLength;
};

class Instrumenter {
private:
    ICorProfilerInfo8 &m_profilerInfo;  // Does not have ownership
    IMethodMalloc *m_methodMalloc;  // Does not have ownership

    Protocol &m_protocol;

    WCHAR *m_mainModuleName;
    int m_mainModuleSize;
    mdMethodDef m_mainMethod;
    bool m_mainReached;

    mdMethodDef m_jittedToken;
    ModuleID m_moduleId;

    char *m_signatureTokens;
    unsigned m_signatureTokensLength;

    mdToken     m_tkLocalVarSig;
    unsigned    m_maxStack;
    unsigned    m_flags;
    bool        m_generateTinyHeader;

    unsigned    m_nEH;
    IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT *m_pEH;

    char *m_code;
    unsigned    m_codeSize;

    std::map<std::pair<ModuleID, mdMethodDef>, MethodInfo> instrumentedFunctions;
    std::set<std::pair<ModuleID, mdMethodDef>> skippedBeforeMain;

    bool m_reJitInstrumentedStarted;

    unsigned codeSize() const;
    char *code() const;
    unsigned ehCount() const;
    IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT *ehs() const;
    unsigned maxStackSize() const;

    HRESULT setILFunctionBody(LPCBYTE pBody);
    LPBYTE allocateILMemory(unsigned size);

    HRESULT importIL();
    HRESULT importEH(const COR_ILMETHOD_SECT_EH* pILEH, unsigned nEH);
    HRESULT exportIL(char *bytecode, unsigned codeLength, unsigned maxStackSize, char *ehs, unsigned ehsLength);

    HRESULT startReJitInstrumented();
    HRESULT startReJitSkipped();
    HRESULT undoInstrumentation(FunctionID functionId);
    HRESULT doInstrumentation(ModuleID oldModuleId, const WCHAR *assemblyName, ULONG assemblyNameLength, const WCHAR *moduleName, ULONG moduleNameLength);

    bool currentMethodIsMain(const WCHAR *moduleName, int moduleSize, mdMethodDef method) const;

public:
    explicit Instrumenter(ICorProfilerInfo8 &profilerInfo, Protocol &protocol);
    ~Instrumenter();

    const char *signatureTokens() const { return m_signatureTokens; }
    unsigned signatureTokensLength() const { return m_signatureTokensLength; }

    void configureEntryPoint();

    HRESULT instrument(FunctionID functionId);
    HRESULT reInstrument(FunctionID functionId);
};

}

#endif // INSTRUMENTER_H_
