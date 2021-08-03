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

    void InitTokens();
    void Initialize();

    ILInstr mkCalli(mdSignature sig);
    ILInstr mkCalli_int();
    ILInstr mkLDC_I(UINT_PTR methodAddress);
    ILInstr mkRet();

    HRESULT appendProbe_int(UINT_PTR methodAddress);

    HRESULT placeProbes();

public:
    explicit Instrumenter(ICorProfilerInfo9 &profilerInfo);

    HRESULT placeConsoleReadProbe();
    HRESULT placeRet();

    HRESULT instrument(FunctionID functionId);
};

}

#endif // INSTRUMENTER_H_
