#ifndef REFLECTION_H_
#define REFLECTION_H_

#include "logging.h"
#include "cor.h"
#include "corProfiler.h"
#include "cComPtr.h"

namespace icsharp {

class Reflection {
private:
    ICorProfilerInfo9 &profilerInfo;  // Does not have ownership
    CComPtr<IMetaDataImport> metadataImport;
#ifdef INSTRUMENTATION
    CComPtr<IMetaDataEmit> metadataEmit;
#endif
    mdToken jittedToken;
    ModuleID moduleId;
    mdMethodDef currentToken;

private:
    HRESULT isIntrinsic(mdMethodDef methodDef, bool &result) const;

public:
    explicit Reflection(ICorProfilerInfo9 &profilerInfo);

#ifdef INSTRUMENTATION
    HRESULT configure(FunctionID functionId, ModuleID &moduleId, mdMethodDef &token);
#else
    HRESULT configure(FunctionID functionId, ModuleID &moduleId, mdMethodDef &token, CComPtr<IMetaDataEmit> &metadataEmit);
#endif

    // TODO: rewrite it to manage memory more accurately!
    const char *className(mdMethodDef methodDef) const;
    const char *methodName(mdMethodDef methodDef) const;
    const char *moduleName(ModuleID module) const;

    mdSignature signatureToken(PCCOR_SIGNATURE sig, ULONG size) const;
    HRESULT getFunctionInfo(mdMethodDef &methodDef, const char *&name, mdTypeDef &declaringClass,
                            unsigned &count, bool &returnsSomething,
                            bool &hasThis, bool &isInternalCall) const;
    HRESULT getReturnValueInfo(mdMethodDef methodDef, bool &returnsSomething) const;

    HRESULT resolveMethodRefOrSpec(mdMemberRef methodRef, ModuleID module, mdTypeDef &declaringType, mdMethodDef &methodDef) const;

};

}

#endif // REFLECTION_H_
