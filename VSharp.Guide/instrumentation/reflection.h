#ifndef REFLECTION_H_
#define REFLECTION_H_

#include "logging.h"
#include "cor.h"
#include "corProfiler.h"
#include "cComPtr.h"

namespace icsharp {

class Reflection {
private:
    ICorProfilerInfo9 &profilerInfo;
    CComPtr<IMetaDataEmit> metadataEmit;

public:
    explicit Reflection(ICorProfilerInfo9 &profilerInfo);
    HRESULT configure(FunctionID functionId, ModuleID &moduleId, mdMethodDef &token);
    mdSignature signatureToken(PCCOR_SIGNATURE sig, ULONG size) const;
};

}

#endif // REFLECTION_H_
