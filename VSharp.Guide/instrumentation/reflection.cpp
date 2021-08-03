#include "reflection.h"
#include <vector>

using namespace icsharp;

#undef IfFailRet
#define IfFailRet(EXPR) do { HRESULT hr = (EXPR); if(FAILED(hr)) { return (hr); } } while (0)

#undef IfNullRet
#define IfNullRet(EXPR) do { if ((EXPR) == NULL) return E_OUTOFMEMORY; } while (0)

Reflection::Reflection(ICorProfilerInfo9 &profilerInfo)
    : profilerInfo(profilerInfo)
{
}

HRESULT Reflection::configure(FunctionID functionId, ModuleID &moduleId, mdMethodDef &methodDef)
{
    // TODO: should we dispose metadataImport and metadataEmit?
    ClassID classId;
    IfFailRet(profilerInfo.GetFunctionInfo(functionId, &classId, &moduleId, &methodDef));
    CComPtr<IMetaDataImport> metadataImport;
    IfFailRet(profilerInfo.GetModuleMetaData(moduleId, ofRead | ofWrite, IID_IMetaDataImport, reinterpret_cast<IUnknown **>(&metadataImport)));
    IfFailRet(metadataImport->QueryInterface(IID_IMetaDataEmit, reinterpret_cast<void **>(&this->metadataEmit)));
    assert((methodDef & 0xFF000000L) == mdtMethodDef);
    return S_OK;
}

mdSignature Reflection::signatureToken(PCCOR_SIGNATURE sig, ULONG size) const
{
    mdSignature signatureToken;
    HRESULT hr = metadataEmit->GetTokenFromSig(sig, size, &signatureToken);
    if (FAILED(hr))
        throw std::logic_error("Failed to get signature token");
    return signatureToken;
}
