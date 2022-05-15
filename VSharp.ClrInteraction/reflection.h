#ifndef REFLECTION_H_
#define REFLECTION_H_

#include "logging.h"
#include "cor.h"
#include "corProfiler.h"
#include "cComPtr.h"

namespace vsharp {

    class Reflection {
    private:
        ICorProfilerInfo8 &profilerInfo;  // Does not have ownership
        CComPtr<IMetaDataImport2> metadataImport;
        CComPtr<IMetaDataEmit> metadataEmit;
#ifdef INSTRUMENTATION
        CComPtr<IMetaDataEmit> metadataEmit;
#endif
        mdToken m_jittedToken;
        ModuleID m_moduleId;

    private:
        mdTypeRef getStringTypeToken() const;

    public:
        explicit Reflection(ICorProfilerInfo8 &profilerInfo);

#ifdef INSTRUMENTATION
        HRESULT configure(FunctionID functionId, ModuleID &moduleId, mdMethodDef &token);
        mdSignature signatureToken(PCCOR_SIGNATURE sig, ULONG size) const;
#else
        HRESULT configure(ModuleID moduleId, mdMethodDef token);
#endif
        mdToken getTypeRefByName(WCHAR *typeName) const;
        mdToken getTypeSpecByName(WCHAR *typeName) const;
        std::vector<mdToken> getTypeInfoFromSignature(PCCOR_SIGNATURE pvSigBlob, ULONG cbSigBlob) const;
        std::vector<mdToken> getTypeInfoFromMethod(mdToken method) const;
        mdToken getTypeTokenFromFieldRef(mdToken fieldRef) const;
        mdToken getTypeTokenFromFieldDef(mdToken fieldDef) const;
        void getSigAndDeclaringTypeFromMethod(mdToken method, PCCOR_SIGNATURE &sig, ULONG &count, mdToken &declaringType) const;
        mdToken getTypeTokenFromParameter(mdToken method, INT32 argIndex) const;
        mdToken getTypeTokenFromLocal(mdToken localsToken, INT32 argIndex) const;
        mdToken getTypeTokenOfReturnType(mdToken method) const;
        mdToken getTypeTokenOfDeclaringType(mdToken method) const;
    };

}

#endif // REFLECTION_H_