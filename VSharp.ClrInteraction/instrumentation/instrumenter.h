#ifndef INSTRUMENTER_H_
#define INSTRUMENTER_H_

#include "reflection.h"
#include <set>

namespace icsharp {

class Protocol;
class ILRewriter;
struct ILInstr;

class Instrumenter {
private:
    ICorProfilerInfo9 &profilerInfo;  // Does not have ownership
    ILRewriter *rewriter;  // Has ownership
    Reflection reflection;
    mdMethodDef jittedToken;
    ModuleID moduleId;
    std::set<std::pair<ModuleID, mdMethodDef>> instrumentedFunctions;
    char *m_signatureTokens;
    unsigned m_signatureTokensLength;

    const char *ilInstrToString(ILInstr *pInstr) const;
    void printInstructions(const char *heading);

    void mkCalli(ILInstr *&instr, mdSignature sig);
    void mkCalli_void(ILInstr *&instr);
    void mkCalli_void_int(ILInstr *&instr);
    void mkCalli_void_uint(ILInstr *&instr);
    void mkCalli_void_ulong(ILInstr *&instr);
    void mkCalli_void_bool_uint(ILInstr *&instr);
    void mkCalli_void_uint_uint(ILInstr *&instr);
    void mkCalli_void_uint_uint_ulong(ILInstr *&instr);

    HRESULT prependProbe_void(UINT_PTR methodAddress, ILInstr *&pBeforeInstr);
    HRESULT appendProbe_void(UINT_PTR methodAddress, ILInstr *pAfterInstr);
    HRESULT prependProbe_void_int(UINT_PTR methodAddress, int arg, ILInstr *&pBeforeInstr);
    HRESULT prependProbe_void_uint(UINT_PTR methodAddress, unsigned arg, ILInstr *&pBeforeInstr);
    HRESULT prependProbe_void_ulong(UINT_PTR methodAddress, unsigned long arg, ILInstr *&pBeforeInstr);
    HRESULT prependProbe_void_bool_uint(UINT_PTR methodAddress, bool arg1, unsigned arg2, ILInstr *&pBeforeInstr);
    HRESULT prependProbe_void_uint_uint(UINT_PTR methodAddress, unsigned arg1, unsigned arg2, ILInstr *&pBeforeInstr);
    HRESULT prependProbe_void_uint_uint_ulong(UINT_PTR methodAddress, unsigned arg1, unsigned arg2, unsigned long arg3, ILInstr *&pBeforeInstr);
    HRESULT appendProbe_void_int(UINT_PTR methodAddress, int arg, ILInstr *pAfterInstr);
    HRESULT appendProbe_void_uint(UINT_PTR methodAddress, unsigned arg, ILInstr *pAfterInstr);

    HRESULT placeEnterProbe(ILInstr *&pFirstInstr);
    HRESULT placeEnterProbe(ILInstr *&pFirstInstr, const char *name);
    HRESULT placeLeaveProbe(ILInstr *&retInstr);
    HRESULT placeProbes();

public:
    explicit Instrumenter(ICorProfilerInfo9 &profilerInfo);
    ~Instrumenter();

    const char *signatureTokens() const { return m_signatureTokens; }
    unsigned signatureTokensLength() const { return m_signatureTokensLength; }

    HRESULT instrument(FunctionID functionId, Protocol &protocol);
};

}

#endif // INSTRUMENTER_H_
