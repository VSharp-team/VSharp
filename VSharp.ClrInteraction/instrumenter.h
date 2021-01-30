#ifndef INSTRUMENTER_H_
#define INSTRUMENTER_H_

#include <set>
#include "corProfiler.h"

struct COR_ILMETHOD_SECT_EH;
struct IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT;

namespace icsharp {

class Protocol;
class ILRewriter;
struct ILInstr;

class Instrumenter {
private:
    ICorProfilerInfo9 &m_profilerInfo;  // Does not have ownership
    IMethodMalloc *m_methodMalloc;  // Does not have ownership

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

    std::set<std::pair<ModuleID, mdMethodDef>> instrumentedFunctions;

    unsigned codeSize() const;
    char *code() const;
    unsigned ehCount() const;
    IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT *ehs() const;
    unsigned maxStackSize() const;

    HRESULT setILFunctionBody(LPBYTE pBody);
    LPBYTE allocateILMemory(unsigned size);

    HRESULT importIL();
    HRESULT importEH(const COR_ILMETHOD_SECT_EH* pILEH, unsigned nEH);
    HRESULT exportIL(char *bytecode, unsigned codeLength, unsigned maxStackSize, char *ehs, unsigned ehsLength);


public:
    explicit Instrumenter(ICorProfilerInfo9 &profilerInfo);
    ~Instrumenter();

    const char *signatureTokens() const { return m_signatureTokens; }
    unsigned signatureTokensLength() const { return m_signatureTokensLength; }

    HRESULT instrument(FunctionID functionId, Protocol &protocol);
};

}

#endif // INSTRUMENTER_H_
