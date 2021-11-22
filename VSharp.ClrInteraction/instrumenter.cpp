#include "instrumenter.h"
#include "communication/protocol.h"
#include "logging.h"
#include "cComPtr.h"
#include <vector>
#include <stdexcept>
#include <corhlpr.cpp>
#include <memory/memory.h>

using namespace icsharp;


#define TOKENPASTE(x, y) x ## y
#define TOKENPASTE2(x, y) TOKENPASTE(x, y)
#define UNIQUE TOKENPASTE2(Sig, __LINE__)
#define SIG_DEF(...) \
    constexpr COR_SIGNATURE UNIQUE[] = {__VA_ARGS__};\
    IfFailRet(metadataEmit->GetTokenFromSig(UNIQUE, sizeof(UNIQUE), &signatureToken));\
    tokens.push_back(signatureToken);

#define ELEMENT_TYPE_COND ELEMENT_TYPE_I
#define ELEMENT_TYPE_TOKEN ELEMENT_TYPE_U4
#define ELEMENT_TYPE_OFFSET ELEMENT_TYPE_I4

struct MethodBodyInfo {
    unsigned token;
    unsigned codeLength;
    unsigned assemblyNameLength;
    unsigned moduleNameLength;
    unsigned maxStackSize;
    unsigned ehsLength;
    unsigned signatureTokensLength;
    char *signatureTokens;
    char16_t *assemblyName;
    char16_t *moduleName;
    char *bytecode;
    char *ehs;

    void serialize(char *&bytes, unsigned &count) const {
        count = codeLength + 6 * sizeof(unsigned) + ehsLength + assemblyNameLength + moduleNameLength + signatureTokensLength;
        bytes = new char[count];
        char *buffer = bytes;
        unsigned size = sizeof(unsigned);
        *(unsigned *)buffer = token; buffer += size;
        *(unsigned *)buffer = codeLength; buffer += size;
        *(unsigned *)buffer = assemblyNameLength; buffer += size;
        *(unsigned *)buffer = moduleNameLength; buffer += size;
        *(unsigned *)buffer = maxStackSize; buffer += size;
        *(unsigned *)buffer = signatureTokensLength;
        buffer += size; size = signatureTokensLength;
        memcpy(buffer, signatureTokens, size);
        buffer += size; size = assemblyNameLength;
        memcpy(buffer, (char*)assemblyName, size);
        buffer += size; size = moduleNameLength;
        memcpy(buffer, (char*)moduleName, size);
        buffer += size; size = codeLength;
        memcpy(buffer, bytecode, size);
        buffer += size; size = ehsLength;
        memcpy(buffer, ehs, size);
    }
};

HRESULT initTokens(const CComPtr<IMetaDataEmit> &metadataEmit, std::vector<mdSignature> &tokens) {
    HRESULT hr;
    mdSignature signatureToken;
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x00, ELEMENT_TYPE_VOID)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x00, ELEMENT_TYPE_COND)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x01, ELEMENT_TYPE_VOID, ELEMENT_TYPE_U1)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x01, ELEMENT_TYPE_VOID, ELEMENT_TYPE_U4)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x01, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x01, ELEMENT_TYPE_COND, ELEMENT_TYPE_I)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x01, ELEMENT_TYPE_COND, ELEMENT_TYPE_U2)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x01, ELEMENT_TYPE_I1, ELEMENT_TYPE_I1)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x01, ELEMENT_TYPE_I2, ELEMENT_TYPE_I1)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x01, ELEMENT_TYPE_I4, ELEMENT_TYPE_I1)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x01, ELEMENT_TYPE_I8, ELEMENT_TYPE_I1)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x01, ELEMENT_TYPE_R4, ELEMENT_TYPE_I1)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x01, ELEMENT_TYPE_R8, ELEMENT_TYPE_I1)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x01, ELEMENT_TYPE_I, ELEMENT_TYPE_I1)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I1)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I2)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_U2)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I4)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I8)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_R4)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_R8)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I4, ELEMENT_TYPE_I4)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I4, ELEMENT_TYPE_I)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I8, ELEMENT_TYPE_I4)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I8, ELEMENT_TYPE_I8)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_R4, ELEMENT_TYPE_R4)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_R8, ELEMENT_TYPE_R8)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_COND, ELEMENT_TYPE_I, ELEMENT_TYPE_I4)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_COND, ELEMENT_TYPE_I, ELEMENT_TYPE_I)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I, ELEMENT_TYPE_I)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I, ELEMENT_TYPE_I1)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I, ELEMENT_TYPE_I2)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I, ELEMENT_TYPE_I4)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I, ELEMENT_TYPE_I8)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I, ELEMENT_TYPE_R4)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I, ELEMENT_TYPE_R8)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I1, ELEMENT_TYPE_I)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I1, ELEMENT_TYPE_I1, ELEMENT_TYPE_I1)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I2, ELEMENT_TYPE_I1, ELEMENT_TYPE_I1)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I4, ELEMENT_TYPE_I1, ELEMENT_TYPE_I1)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I8, ELEMENT_TYPE_I1, ELEMENT_TYPE_I1)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_R4, ELEMENT_TYPE_I1, ELEMENT_TYPE_I1)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_R8, ELEMENT_TYPE_I1, ELEMENT_TYPE_I1)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I1, ELEMENT_TYPE_I1)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x05, ELEMENT_TYPE_VOID, ELEMENT_TYPE_TOKEN, ELEMENT_TYPE_U2, ELEMENT_TYPE_BOOLEAN, ELEMENT_TYPE_U4, ELEMENT_TYPE_U4)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x01, ELEMENT_TYPE_VOID, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_U1, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_U2, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I4, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I8, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_R4, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_R8, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_TOKEN, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I1, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I2, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I4, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I8, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_R4, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_R8, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_TOKEN, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I4, ELEMENT_TYPE_I4, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_U2, ELEMENT_TYPE_I4, ELEMENT_TYPE_I4, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_U2, ELEMENT_TYPE_I4, ELEMENT_TYPE_I, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_U2, ELEMENT_TYPE_I8, ELEMENT_TYPE_I4, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_U2, ELEMENT_TYPE_I8, ELEMENT_TYPE_I8, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_U2, ELEMENT_TYPE_R4, ELEMENT_TYPE_R4, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_U2, ELEMENT_TYPE_R8, ELEMENT_TYPE_R8, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_U2, ELEMENT_TYPE_I, ELEMENT_TYPE_I, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_U2, ELEMENT_TYPE_I, ELEMENT_TYPE_I4, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I, ELEMENT_TYPE_I, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I, ELEMENT_TYPE_I1, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I, ELEMENT_TYPE_I2, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I, ELEMENT_TYPE_I4, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I, ELEMENT_TYPE_I8, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I, ELEMENT_TYPE_R4, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I, ELEMENT_TYPE_R8, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I, ELEMENT_TYPE_I1, ELEMENT_TYPE_I, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_TOKEN, ELEMENT_TYPE_U4, ELEMENT_TYPE_U4, ELEMENT_TYPE_U4)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_TOKEN, ELEMENT_TYPE_I, ELEMENT_TYPE_I, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_TOKEN, ELEMENT_TYPE_I, ELEMENT_TYPE_I4, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_TOKEN, ELEMENT_TYPE_I, ELEMENT_TYPE_I8, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_TOKEN, ELEMENT_TYPE_I, ELEMENT_TYPE_R4, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x04, ELEMENT_TYPE_VOID, ELEMENT_TYPE_TOKEN, ELEMENT_TYPE_I, ELEMENT_TYPE_R8, ELEMENT_TYPE_OFFSET)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x05, ELEMENT_TYPE_VOID, ELEMENT_TYPE_TOKEN, ELEMENT_TYPE_TOKEN, ELEMENT_TYPE_BOOLEAN, ELEMENT_TYPE_U2, ELEMENT_TYPE_OFFSET)
    return S_OK;
}


Instrumenter::Instrumenter(ICorProfilerInfo9 &profilerInfo)
    : m_profilerInfo(profilerInfo)
    , m_methodMalloc(nullptr)
    , m_moduleId(0)
    , m_signatureTokens(nullptr)
    , m_generateTinyHeader(false)
    , m_pEH(nullptr)
{
}

Instrumenter::~Instrumenter()
{
    if (m_signatureTokens)
        delete[] m_signatureTokens;
}

unsigned Instrumenter::codeSize() const
{
    return m_codeSize;
}

char *Instrumenter::code() const
{
    return m_code;
}

unsigned Instrumenter::ehCount() const
{
    return m_nEH * sizeof(IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT);
}

IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT *Instrumenter::ehs() const
{
    return m_pEH;
}

unsigned Instrumenter::maxStackSize() const
{
    return m_maxStack;
}

HRESULT Instrumenter::setILFunctionBody(LPBYTE pBody)
{
    return m_profilerInfo.SetILFunctionBody(m_moduleId, m_jittedToken, pBody);
}

LPBYTE Instrumenter::allocateILMemory(unsigned size)
{
    if (FAILED(m_profilerInfo.GetILFunctionBodyAllocator(m_moduleId, &m_methodMalloc)))
        return nullptr;

    return (LPBYTE)m_methodMalloc->Alloc(size);
}

HRESULT Instrumenter::importIL()
{
    HRESULT hr;
    LPCBYTE pMethodBytes;

    IfFailRet(m_profilerInfo.GetILFunctionBody(m_moduleId, m_jittedToken, &pMethodBytes, NULL));

    COR_ILMETHOD_DECODER decoder((COR_ILMETHOD*)pMethodBytes);

    // Import the header flags
    m_tkLocalVarSig = decoder.GetLocalVarSigTok();
    m_maxStack = decoder.GetMaxStack();
    m_flags = (decoder.GetFlags() & CorILMethod_InitLocals);

    m_codeSize = decoder.GetCodeSize();
    m_code = new char[m_codeSize];
    memcpy(m_code, decoder.Code, m_codeSize);

    IfFailRet(importEH(decoder.EH, decoder.EHCount()));

    return S_OK;
}

HRESULT Instrumenter::importEH(const COR_ILMETHOD_SECT_EH* pILEH, unsigned nEH)
{
    assert(m_pEH == nullptr);

    m_nEH = nEH;

    if (nEH == 0)
        return S_OK;

    IfNullRet(m_pEH = new IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT[m_nEH]);
    for (unsigned iEH = 0; iEH < m_nEH; iEH++)
    {
        // If the EH clause is in tiny form, the call to pILEH->EHClause() below will
        // use this as a scratch buffer to expand the EH clause into its fat form.
        COR_ILMETHOD_SECT_EH_CLAUSE_FAT scratch;
        const COR_ILMETHOD_SECT_EH_CLAUSE_FAT* ehInfo;
        ehInfo = (COR_ILMETHOD_SECT_EH_CLAUSE_FAT*)pILEH->EHClause(iEH, &scratch);
        memcpy(m_pEH + iEH, ehInfo, sizeof (IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT));
    }

    return S_OK;
}


HRESULT Instrumenter::exportIL(char *bytecode, unsigned codeLength, unsigned maxStackSize, char *ehs, unsigned ehsLength)
{
    HRESULT hr;

    // Use FAT header
    m_nEH = ehsLength / sizeof(IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT);
    m_maxStack = maxStackSize;

    unsigned alignedCodeSize = (codeLength + 3) & ~3;

    unsigned totalSize = sizeof(IMAGE_COR_ILMETHOD_FAT) + alignedCodeSize +
        (m_nEH ? (sizeof(IMAGE_COR_ILMETHOD_SECT_FAT) + sizeof(IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT) * m_nEH) : 0);

    LPBYTE pBody = allocateILMemory(totalSize);
    IfNullRet(pBody);

    BYTE * pCurrent = pBody;

    IMAGE_COR_ILMETHOD_FAT *pHeader = (IMAGE_COR_ILMETHOD_FAT *)pCurrent;
    pHeader->Flags = m_flags | (m_nEH ? CorILMethod_MoreSects : 0) | CorILMethod_FatFormat;
    pHeader->Size = sizeof(IMAGE_COR_ILMETHOD_FAT) / sizeof(DWORD);
    pHeader->MaxStack = m_maxStack;
    pHeader->CodeSize = codeLength;
    pHeader->LocalVarSigTok = m_tkLocalVarSig;

    pCurrent = (BYTE*)(pHeader + 1);

    CopyMemory(pCurrent, bytecode, codeLength);
    pCurrent += alignedCodeSize;

    if (m_nEH != 0)
    {
        IMAGE_COR_ILMETHOD_SECT_FAT *pEH = (IMAGE_COR_ILMETHOD_SECT_FAT *)pCurrent;
        pEH->Kind = CorILMethod_Sect_EHTable | CorILMethod_Sect_FatFormat;
        pEH->DataSize = (unsigned)(sizeof(IMAGE_COR_ILMETHOD_SECT_FAT) + sizeof(IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT) * m_nEH);

        pCurrent = (BYTE*)(pEH + 1);

        for (unsigned iEH = 0; iEH < m_nEH; iEH++)
        {
            IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT * pDst = (IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT *)pCurrent;
            *pDst = *(IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT *)(ehs + iEH * sizeof(IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT));
            pCurrent = (BYTE*)(pDst + 1);
        }
    }

    IfFailRet(setILFunctionBody(pBody));

    if (m_pEH) {
        delete[] m_pEH;
        m_pEH = nullptr;
    }

    if (m_methodMalloc)
        m_methodMalloc->Release();

    return S_OK;
}

HRESULT Instrumenter::instrument(FunctionID functionId, Protocol &protocol) {
    HRESULT hr;
    ModuleID oldModuleId = m_moduleId;
    CComPtr<IMetaDataImport> metadataImport;
    CComPtr<IMetaDataEmit> metadataEmit;
    ClassID classId;
    IfFailRet(m_profilerInfo.GetFunctionInfo(functionId, &classId, &m_moduleId, &m_jittedToken));
    IfFailRet(m_profilerInfo.GetModuleMetaData(m_moduleId, ofRead | ofWrite, IID_IMetaDataImport, reinterpret_cast<IUnknown **>(&metadataImport)));
    IfFailRet(metadataImport->QueryInterface(IID_IMetaDataEmit, reinterpret_cast<void **>(&metadataEmit)));
    assert((m_jittedToken & 0xFF000000L) == mdtMethodDef);

    // TODO: analyze the IL code instead to understand that we've injected functions?
    if (instrumentedFunctions.find({m_moduleId, m_jittedToken}) != instrumentedFunctions.end()) {
        LOG(tout << "Duplicate jitting of " << HEX(m_jittedToken) << std::endl);
        return S_OK;
    }
    if (mainLeft()) {
        LOG(tout << "Main left! Skipping instrumentation of " << HEX(m_jittedToken) << std::endl);
        return S_OK;
    }
    instrumentedFunctions.insert({m_moduleId, m_jittedToken});

    LPCBYTE baseLoadAddress;
    ULONG moduleNameLength;
    AssemblyID assembly;
    IfFailRet(m_profilerInfo.GetModuleInfo(m_moduleId, &baseLoadAddress, 0, &moduleNameLength, nullptr, &assembly));
    WCHAR *moduleName = new WCHAR[moduleNameLength];
    IfFailRet(m_profilerInfo.GetModuleInfo(m_moduleId, &baseLoadAddress, moduleNameLength, &moduleNameLength, moduleName, &assembly));
    ULONG assemblyNameLength;
    AppDomainID appDomainId;
    ModuleID startModuleId;
    IfFailRet(m_profilerInfo.GetAssemblyInfo(assembly, 0, &assemblyNameLength, nullptr, &appDomainId, &startModuleId));
    WCHAR *assemblyName = new WCHAR[assemblyNameLength];
    IfFailRet(m_profilerInfo.GetAssemblyInfo(assembly, assemblyNameLength, &assemblyNameLength, assemblyName, &appDomainId, &startModuleId));

    if (oldModuleId != m_moduleId) {
        if (m_signatureTokens)
            delete[] m_signatureTokens;
        std::vector<mdSignature> *tokens = new std::vector<mdSignature>;
        initTokens(metadataEmit, *tokens);
        m_signatureTokensLength = tokens->size() * sizeof(mdSignature);
        m_signatureTokens = (char *) &(*tokens)[0];
    }

    LOG(tout << "Instrumenting token " << HEX(m_jittedToken) << "..." << std::endl);

    IfFailRet(importIL());
    MethodBodyInfo info{
        (unsigned)m_jittedToken,
        (unsigned)codeSize(),
        (unsigned)(assemblyNameLength - 1) * sizeof(WCHAR),
        (unsigned)(moduleNameLength - 1) * sizeof(WCHAR),
        (unsigned)maxStackSize(),
        (unsigned)ehCount(),
        m_signatureTokensLength,
        m_signatureTokens,
        assemblyName,
        moduleName,
        code(),
        (char*)ehs()
    };
    if (!protocol.sendSerializable(InstrumentCommand, info)) return false;
    LOG(tout << "Successfully sent method body!");
    char *bytecode; int length; unsigned maxStackSize; char *ehs; unsigned ehsLength;
#ifdef _DEBUG
    CommandType command;
    do {
        if (!protocol.acceptCommand(command)) return false;
        switch (command) {
            case ReadString: {
                char *string;
                if (!protocol.acceptString(string)) return false;
                unsigned index = allocateString(string);
                if (!protocol.sendStringsPoolIndex(index)) return false;
                break;
            }
            default:
                break;
        }
    } while (command != ReadMethodBody);
#endif
    LOG(tout << "Reading method body back...");
    if (!protocol.acceptMethodBody(bytecode, length, maxStackSize, ehs, ehsLength)) return false;
    LOG(tout << "Exporting " << length << " IL bytes!");
    IfFailRet(exportIL(bytecode, length, maxStackSize, ehs, ehsLength));

    return S_OK;
}
