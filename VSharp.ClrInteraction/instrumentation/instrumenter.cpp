#include "instrumentation/instrumenter.h"
#include "instrumentation/ilRewriter.h"
#include "instrumentation/instructions.h"
#include "communication/protocol.h"
#include "logging.h"
#include <vector>
#include <stdexcept>

#include "cComPtr.h"

#ifdef INSTRUMENTATION
#include "instrumentation/probes.h"
#endif

using namespace icsharp;


#undef IfFailRet
#define IfFailRet(EXPR) do { HRESULT hr = (EXPR); if(FAILED(hr)) { return (hr); } } while (0)

#undef IfNullRet
#define IfNullRet(EXPR) do { if ((EXPR) == NULL) return E_OUTOFMEMORY; } while (0)

const auto *ignoredInstructions  = new std::vector<OPCODE> {
    CEE_NOP,
    CEE_BREAK,
    CEE_UNUSED49,
    CEE_JMP,
    CEE_UNUSED58,
    CEE_UNUSED1,
    CEE_UNUSED5,
    CEE_UNUSED6,
    CEE_UNUSED7,
    CEE_UNUSED8,
    CEE_UNUSED9,
    CEE_UNUSED10,
    CEE_UNUSED11,
    CEE_UNUSED12,
    CEE_UNUSED13,
    CEE_UNUSED14,
    CEE_UNUSED15,
    CEE_UNUSED16,
    CEE_UNUSED17,
    CEE_UNUSED50,
    CEE_UNUSED18,
    CEE_UNUSED19,
    CEE_UNUSED20,
    CEE_UNUSED21,
    CEE_UNUSED22,
    CEE_UNUSED23,
    CEE_UNUSED24,
    CEE_UNUSED25,
    CEE_UNUSED59,
    CEE_UNUSED60,
    CEE_UNUSED61,
    CEE_UNUSED62,
    CEE_UNUSED63,
    CEE_UNUSED64,
    CEE_UNUSED65,
    CEE_UNUSED66,
    CEE_UNUSED67,
    CEE_ENDFINALLY,
    CEE_LEAVE,
    CEE_LEAVE_S,
    CEE_UNUSED26,
    CEE_UNUSED27,
    CEE_UNUSED28,
    CEE_UNUSED29,
    CEE_UNUSED30,
    CEE_UNUSED31,
    CEE_UNUSED32,
    CEE_UNUSED33,
    CEE_UNUSED34,
    CEE_UNUSED35,
    CEE_UNUSED36,
    CEE_UNUSED37,
    CEE_UNUSED38,
    CEE_UNUSED39,
    CEE_UNUSED40,
    CEE_UNUSED41,
    CEE_UNUSED42,
    CEE_UNUSED43,
    CEE_UNUSED44,
    CEE_UNUSED45,
    CEE_UNUSED46,
    CEE_UNUSED47,
    CEE_UNUSED48,
    CEE_PREFIX7,
    CEE_PREFIX6,
    CEE_PREFIX5,
    CEE_PREFIX4,
    CEE_PREFIX3,
    CEE_PREFIX2,
    CEE_PREFIX1,
    CEE_PREFIXREF,
    CEE_UNUSED56,
    CEE_UNUSED57,
    CEE_ENDFILTER,
    CEE_UNUSED69,
    CEE_UNUSED51,
    CEE_UNUSED53,
    CEE_UNUSED54,
    CEE_UNUSED55,
    CEE_UNUSED70,
    CEE_ILLEGAL,
    CEE_MACRO_END,
    CEE_CODE_LABEL,
    CEE_SWITCH_ARG,
};
const auto concreteInstructions = new std::vector<OPCODE> {
    CEE_LDARGA_S,
    CEE_LDLOCA_S,
    CEE_LDNULL,
    CEE_LDC_I4_M1,
    CEE_LDC_I4_0,
    CEE_LDC_I4_1,
    CEE_LDC_I4_2,
    CEE_LDC_I4_3,
    CEE_LDC_I4_4,
    CEE_LDC_I4_5,
    CEE_LDC_I4_6,
    CEE_LDC_I4_7,
    CEE_LDC_I4_8,
    CEE_LDC_I4_S,
    CEE_LDC_I4,
    CEE_LDC_I8,
    CEE_LDC_R4,
    CEE_LDC_R8,
    CEE_POP,
    CEE_LDTOKEN,
    CEE_ARGLIST,
    CEE_LDFTN,
    CEE_LDARGA,
    CEE_LDLOCA,
    CEE_SIZEOF,
};
const auto symbolicStackInstructions = new std::vector<OPCODE> {
    CEE_LDARG_0,
    CEE_LDARG_1,
    CEE_LDARG_2,
    CEE_LDARG_3,
    CEE_LDLOC_0,
    CEE_LDLOC_1,
    CEE_LDLOC_2,
    CEE_LDLOC_3,
    CEE_STLOC_0,
    CEE_STLOC_1,
    CEE_STLOC_2,
    CEE_STLOC_3,
    CEE_LDARG_S,
    CEE_STARG_S,
    CEE_LDLOC_S,
    CEE_STLOC_S,
    CEE_DUP,
    CEE_ADD,
    CEE_SUB,
    CEE_MUL,
    CEE_DIV,
    CEE_DIV_UN,
    CEE_REM,
    CEE_REM_UN,
    CEE_AND,
    CEE_OR,
    CEE_XOR,
    CEE_SHL,
    CEE_SHR,
    CEE_SHR_UN,
    CEE_NEG,
    CEE_NOT,
    CEE_CONV_I1,
    CEE_CONV_I2,
    CEE_CONV_I4,
    CEE_CONV_I8,
    CEE_CONV_R4,
    CEE_CONV_R8,
    CEE_CONV_U4,
    CEE_CONV_U8,
    CEE_CONV_R_UN,
    CEE_REFANYVAL,
    CEE_MKREFANY,
    CEE_CONV_U2,
    CEE_CONV_U1,
    CEE_CONV_I,
    CEE_CONV_U,
    CEE_CEQ,
    CEE_CGT,
    CEE_CGT_UN,
    CEE_CLT,
    CEE_CLT_UN,
    CEE_LDARG,
    CEE_STARG,
    CEE_LDLOC,
    CEE_STLOC,
    CEE_LOCALLOC,
    CEE_REFANYTYPE,
};
const auto prefixes = new std::vector<OPCODE> {
    CEE_UNALIGNED,
    CEE_VOLATILE,
    CEE_TAILCALL,
    CEE_CONSTRAINED,
    CEE_READONLY,
};

const auto branchings = new std::vector<OPCODE> {
    CEE_BR_S,
    CEE_BRFALSE_S,
    CEE_BRTRUE_S,
    CEE_BEQ_S,
    CEE_BGE_S,
    CEE_BGT_S,
    CEE_BLE_S,
    CEE_BLT_S,
    CEE_BNE_UN_S,
    CEE_BGE_UN_S,
    CEE_BGT_UN_S,
    CEE_BLE_UN_S,
    CEE_BLT_UN_S,
    CEE_BR,
    CEE_BRFALSE,
    CEE_BRTRUE,
    CEE_BEQ,
    CEE_BGE,
    CEE_BGT,
    CEE_BLE,
    CEE_BLT,
    CEE_BNE_UN,
    CEE_BGE_UN,
    CEE_BGT_UN,
    CEE_BLE_UN,
    CEE_BLT_UN,
    CEE_SWITCH
};

bool *isIgnoredInstruction = nullptr;
bool *isConcreteInstruction = nullptr;
bool *isSymbolicStackInstruction = nullptr;
bool *isPrefixInstruction = nullptr;
bool *isBranchingInstruction = nullptr;

void initInstructionBuffers() {
    if (isIgnoredInstruction) return;
    isIgnoredInstruction = new bool[CEE_COUNT];
    isConcreteInstruction = new bool[CEE_COUNT];
    isSymbolicStackInstruction = new bool[CEE_COUNT];
    isPrefixInstruction = new bool[CEE_COUNT];
    isBranchingInstruction = new bool[CEE_COUNT];
    memset(isIgnoredInstruction, false, CEE_COUNT);
    memset(isConcreteInstruction, false, CEE_COUNT);
    memset(isSymbolicStackInstruction, false, CEE_COUNT);
    memset(isPrefixInstruction, false, CEE_COUNT);
    memset(isBranchingInstruction, false, CEE_COUNT);
    for (unsigned opcode = 0; opcode < ignoredInstructions->size(); ++opcode)
        isIgnoredInstruction[ignoredInstructions->at(opcode)] = true;
    for (unsigned opcode = 0; opcode < concreteInstructions->size(); ++opcode)
        isConcreteInstruction[concreteInstructions->at(opcode)] = true;
    for (unsigned opcode = 0; opcode < symbolicStackInstructions->size(); ++opcode)
        isSymbolicStackInstruction[symbolicStackInstructions->at(opcode)] = true;
    for (unsigned opcode = 0; opcode < prefixes->size(); ++opcode)
        isPrefixInstruction[prefixes->at(opcode)] = true;
    for (unsigned opcode = 0; opcode < branchings->size(); ++opcode)
        isBranchingInstruction[branchings->at(opcode)] = true;
}

//CEE_CALL,
//CEE_CALLI,
//CEE_RET,
//CEE_LDIND_I1,
//CEE_LDIND_U1,
//CEE_LDIND_I2,
//CEE_LDIND_U2,
//CEE_LDIND_I4,
//CEE_LDIND_U4,
//CEE_LDIND_I8,
//CEE_LDIND_I,
//CEE_LDIND_R4,
//CEE_LDIND_R8,
//CEE_LDIND_REF,
//CEE_STIND_REF,
//CEE_STIND_I1,
//CEE_STIND_I2,
//CEE_STIND_I4,
//CEE_STIND_I8,
//CEE_STIND_R4,
//CEE_STIND_R8,
//CEE_CALLVIRT,
//CEE_CPOBJ,
//CEE_LDOBJ,
//CEE_LDSTR,
//CEE_NEWOBJ,
//CEE_CASTCLASS,
//CEE_ISINST,
//CEE_UNBOX,
//CEE_THROW,
//CEE_LDFLD,
//CEE_LDFLDA,
//CEE_STFLD,
//CEE_LDSFLD,
//CEE_LDSFLDA,
//CEE_STSFLD,
//CEE_STOBJ,
//CEE_CONV_OVF_I1_UN,
//CEE_CONV_OVF_I2_UN,
//CEE_CONV_OVF_I4_UN,
//CEE_CONV_OVF_I8_UN,
//CEE_CONV_OVF_U1_UN,
//CEE_CONV_OVF_U2_UN,
//CEE_CONV_OVF_U4_UN,
//CEE_CONV_OVF_U8_UN,
//CEE_CONV_OVF_I_UN,
//CEE_CONV_OVF_U_UN,
//CEE_BOX,
//CEE_NEWARR,
//CEE_LDLEN,
//CEE_LDELEMA,
//CEE_LDELEM_I1,
//CEE_LDELEM_U1,
//CEE_LDELEM_I2,
//CEE_LDELEM_U2,
//CEE_LDELEM_I4,
//CEE_LDELEM_U4,
//CEE_LDELEM_I8,
//CEE_LDELEM_I,
//CEE_LDELEM_R4,
//CEE_LDELEM_R8,
//CEE_LDELEM_REF,
//CEE_STELEM_I,
//CEE_STELEM_I1,
//CEE_STELEM_I2,
//CEE_STELEM_I4,
//CEE_STELEM_I8,
//CEE_STELEM_R4,
//CEE_STELEM_R8,
//CEE_STELEM_REF,
//CEE_LDELEM,
//CEE_STELEM,
//CEE_UNBOX_ANY,
//CEE_CONV_OVF_I1,
//CEE_CONV_OVF_U1,
//CEE_CONV_OVF_I2,
//CEE_CONV_OVF_U2,
//CEE_CONV_OVF_I4,
//CEE_CONV_OVF_U4,
//CEE_CONV_OVF_I8,
//CEE_CONV_OVF_U8,
//CEE_CKFINITE,
//CEE_CONV_OVF_I,
//CEE_CONV_OVF_U,
//CEE_ADD_OVF,
//CEE_ADD_OVF_UN,
//CEE_MUL_OVF,
//CEE_MUL_OVF_UN,
//CEE_SUB_OVF,
//CEE_SUB_OVF_UN,
//CEE_STIND_I,
//CEE_LDVIRTFTN,
//CEE_INITOBJ,
//CEE_CPBLK,
//CEE_INITBLK,
//CEE_RETHROW,

constexpr auto CEE_LDC_I = sizeof(size_t) == 8 ? CEE_LDC_I8 : sizeof(size_t) == 4 ? CEE_LDC_I4 : throw std::logic_error("size_t must be defined as 8 or 4");

// TODO: more fancy error reporting via COM! see https://searchcode.com/codesearch/view/30154548/

#include<iostream>
//#include <locale>
//#include <codecvt>// TODOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOo
//char *wStrToCharStr(WCHAR *wstr) {
//    std::u16string u16str(wstr);
//    std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> codecvt;
//    std::string u8str = codecvt.to_bytes(u16str);
//    char *result = new char[u8str.length() + 1];
//    u8str.copy(result, u8str.length());
//    result[u8str.length()] = '\0';
//    return result;
//}

#define TOKENPASTE(x, y) x ## y
#define TOKENPASTE2(x, y) TOKENPASTE(x, y)
#define UNIQUE TOKENPASTE2(Sig, __LINE__)
#define SIG_DEF(...) \
    constexpr COR_SIGNATURE UNIQUE[] = {__VA_ARGS__};\
    IfFailRet(metadataEmit->GetTokenFromSig(UNIQUE, sizeof(UNIQUE), &signatureToken));\
    tokens.push_back(signatureToken);

HRESULT initTokens(const CComPtr<IMetaDataEmit> &metadataEmit, std::vector<mdSignature> &tokens) {
    HRESULT hr;
    mdSignature signatureToken;
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x00, ELEMENT_TYPE_VOID)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x01, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x01, ELEMENT_TYPE_VOID, ELEMENT_TYPE_U)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x01, ELEMENT_TYPE_VOID, ELEMENT_TYPE_U8)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_BOOLEAN, ELEMENT_TYPE_U)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_U, ELEMENT_TYPE_U)
    SIG_DEF(IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_U, ELEMENT_TYPE_U, ELEMENT_TYPE_U8);
    return S_OK;
}


//struct SignatureTokens {
//    mdSignature void_Signature_token;
//    mdSignature void_int_Signature_token;
//    mdSignature void_uint_Signature_token;
//    mdSignature void_ulong_Signature_token;
//    mdSignature void_bool_uint_Signature;
//    mdSignature void_uint_uint_Signature;
//    mdSignature void_uint_uint_ulong_Signature;
//};

//void initTokens(const CComPtr<IMetaDataEmit> &metadataEmit, SignatureTokens &tokens) {
//    token.void_Signature_token = signatureToken(void_Signature, sizeof(void_Signature), metadataEmit);
//    token.void_int_Signature_token = signatureToken(void_int_Signature, sizeof(void_int_Signature), metadataEmit);
//    token.void_uint_Signature_token = signatureToken(void_int_Signature, sizeof(void_int_Signature), metadataEmit);
//    token.void_ulong_Signature_token = signatureToken(void_int_Signature, sizeof(void_int_Signature), metadataEmit);
//    token.void_bool_uint_Signature = signatureToken(void_int_Signature, sizeof(void_int_Signature), metadataEmit);
//    token.void_uint_uint_Signature = signatureToken(void_int_Signature, sizeof(void_int_Signature), metadataEmit);
//    token.void_uint_uint_ulong_Signature = signatureToken(void_int_Signature, sizeof(void_int_Signature), metadataEmit);
//}

#ifdef INSTRUMENTATION
const char *probeAddressToString(INT64 address) {
    if ((unsigned long long) address == (unsigned long long) Push1ConcreteAddress) return "probe_push1Concrete";
    if ((unsigned long long) address == (unsigned long long) Pop1Address) return "probe_pop1";
    if ((unsigned long long) address == (unsigned long long) Pop2Push1Address) return "probe_pop2push1";
    if ((unsigned long long) address == (unsigned long long) EnterProbeAddress) return "probe_enter";
    if ((unsigned long long) address == (unsigned long long) Enter1ProbeAddress) return "probe_enter1";
    if ((unsigned long long) address == (unsigned long long) LeaveProbeAddress) return "probe_leave";
    if ((unsigned long long) address == (unsigned long long) FinalizeCallProbeAddress) return "probe_finalizeCall";
    if ((unsigned long long) address == (unsigned long long) DumpInstructionAddress) return "probe_dump";
    if ((unsigned long long) address == (unsigned long long) CallProbeAddress) return "probe_call";
    if ((unsigned long long) address == (unsigned long long) CallVirtProbeAddress) return "probe_callVirt";
    if ((unsigned long long) address == (unsigned long long) PopAddress) return "probe_pop";
    if ((unsigned long long) address == (unsigned long long) PushAddress) return "probe_push";
    auto s = std::to_string(address);
    char *res = new char[s.length()];
    strcpy(res, s.c_str());
    return res;
}

const char *Instrumenter::ilInstrToString(ILInstr *pInstr) const {
    const char *opcodeName = s_OpCodeNames[pInstr->m_opcode];
    const char *arg;
    if (pInstr->m_opcode == CEE_CALL || pInstr->m_opcode == CEE_CALLVIRT || pInstr->m_opcode == CEE_NEWOBJ) {
        arg = reflection.methodName((mdMethodDef)pInstr->m_Arg32);
    } else if (pInstr->m_opcode == CEE_CALLI) {
        arg = "";
    } else {
        BYTE flags = s_OpCodeFlags[pInstr->m_opcode];
        switch (flags)
        {
        case 0:
            arg = "";
            break;
        case 1:
            arg = std::to_string(pInstr->m_Arg8).c_str();
            break;
        case 2:
            arg = std::to_string(pInstr->m_Arg16).c_str();
            break;
        case 4:
            arg = std::to_string(pInstr->m_Arg32).c_str();
            break;
        case 8:
            arg = probeAddressToString(pInstr->m_Arg64);
            break;
        case 1 | OPCODEFLAGS_BranchTarget:
            arg = s_OpCodeNames[pInstr->m_pTarget->m_opcode];
            break;
        case 4 | OPCODEFLAGS_BranchTarget:
            arg = s_OpCodeNames[pInstr->m_pTarget->m_opcode];
            break;
        case 0 | OPCODEFLAGS_Switch:
            arg = "<switch args>";
            break;
        default:
            assert(false);
            break;
        }

    }
    size_t len = strlen(opcodeName) + strlen(arg) + 2;
    char *result = new char[len];
    strcpy(result, opcodeName);
    result[strlen(opcodeName)] = ' ';
    strcpy(result + strlen(opcodeName) + 1, arg);
    result[len - 1] = '\0';
    return result;
    return opcodeName;
}
#endif

void Instrumenter::printInstructions(const char *heading)
{
   std::cout << "==============" << heading << ": =============" << std::endl;
   for (ILInstr * pInstr = rewriter->GetILList()->m_pNext; pInstr != rewriter->GetILList(); pInstr = pInstr->m_pNext) {
       std::cout << ilInstrToString(pInstr) << std::endl;
   }
   std::cout << "===================================================" << std::endl;
}

void Instrumenter::mkCalli(ILInstr *&instr, mdSignature sig)
{
    instr = rewriter->NewILInstr();
    instr->m_opcode = CEE_CALLI;
    instr->m_Arg32 = sig;
}

#ifdef INSTRUMENTATION
COR_SIGNATURE void_Signature                    [] = { IMAGE_CEE_CS_CALLCONV_STDCALL, 0x00, ELEMENT_TYPE_VOID };
COR_SIGNATURE void_int_Signature                [] = { IMAGE_CEE_CS_CALLCONV_STDCALL, 0x01, ELEMENT_TYPE_VOID, ELEMENT_TYPE_I };
COR_SIGNATURE void_uint_Signature               [] = { IMAGE_CEE_CS_CALLCONV_STDCALL, 0x01, ELEMENT_TYPE_VOID, ELEMENT_TYPE_U };
COR_SIGNATURE void_ulong_Signature              [] = { IMAGE_CEE_CS_CALLCONV_STDCALL, 0x01, ELEMENT_TYPE_VOID, ELEMENT_TYPE_U8 };
COR_SIGNATURE void_bool_uint_Signature          [] = { IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_BOOLEAN, ELEMENT_TYPE_U };
COR_SIGNATURE void_uint_uint_Signature          [] = { IMAGE_CEE_CS_CALLCONV_STDCALL, 0x02, ELEMENT_TYPE_VOID, ELEMENT_TYPE_U, ELEMENT_TYPE_U };
COR_SIGNATURE void_uint_uint_ulong_Signature    [] = { IMAGE_CEE_CS_CALLCONV_STDCALL, 0x03, ELEMENT_TYPE_VOID, ELEMENT_TYPE_U, ELEMENT_TYPE_U, ELEMENT_TYPE_U8 };

void Instrumenter::mkCalli_void(ILInstr *&instr)
{
    mdSignature sig = reflection.signatureToken(void_Signature, sizeof(void_Signature));
    mkCalli(instr, sig);
}

void Instrumenter::mkCalli_void_int(ILInstr *&instr)
{
    mdSignature sig = reflection.signatureToken(void_int_Signature, sizeof(void_int_Signature));
    mkCalli(instr, sig);
}

void Instrumenter::mkCalli_void_uint(ILInstr *&instr)
{
    mdSignature sig = reflection.signatureToken(void_uint_Signature, sizeof(void_uint_Signature));
    mkCalli(instr, sig);
}

void Instrumenter::mkCalli_void_ulong(ILInstr *&instr)
{
    mdSignature sig = reflection.signatureToken(void_uint_Signature, sizeof(void_uint_Signature));
    mkCalli(instr, sig);
}

void Instrumenter::mkCalli_void_bool_uint(ILInstr *&instr)
{
    mdSignature sig = reflection.signatureToken(void_bool_uint_Signature, sizeof(void_bool_uint_Signature));
    mkCalli(instr, sig);
}

void Instrumenter::mkCalli_void_uint_uint(ILInstr *&instr)
{
    mdSignature sig = reflection.signatureToken(void_uint_uint_Signature, sizeof(void_uint_uint_Signature));
    mkCalli(instr, sig);
}

void Instrumenter::mkCalli_void_uint_uint_ulong(ILInstr *&instr)
{
    mdSignature sig = reflection.signatureToken(void_uint_uint_ulong_Signature, sizeof(void_uint_uint_ulong_Signature));
    mkCalli(instr, sig);
}
#endif

HRESULT Instrumenter::prependProbe_void(UINT_PTR methodAddress, ILInstr *& pBeforeInstr)
{
    ILInstr * pNewInstr = rewriter->CopyILInstr(pBeforeInstr);
    rewriter->InsertAfter(pBeforeInstr, pNewInstr);
    std::swap(pNewInstr, pBeforeInstr);

    pNewInstr->m_opcode = CEE_LDC_I;
    pNewInstr->m_Arg64 = methodAddress;

    mkCalli_void(pNewInstr);
    rewriter->InsertBefore(pBeforeInstr, pNewInstr);

    return S_OK;
}

HRESULT Instrumenter::appendProbe_void(UINT_PTR methodAddress, ILInstr * pAfterInstr)
{
    ILInstr * pNewInstr = nullptr;

    mkCalli_void(pNewInstr);
    rewriter->InsertAfter(pAfterInstr, pNewInstr);

    pNewInstr = rewriter->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I;
    pNewInstr->m_Arg64 = methodAddress;
    rewriter->InsertAfter(pAfterInstr, pNewInstr);

    return S_OK;
}

HRESULT Instrumenter::prependProbe_void_int(UINT_PTR methodAddress, int arg, ILInstr *&pBeforeInstr)
{
    ILInstr * pNewInstr = rewriter->CopyILInstr(pBeforeInstr);
    rewriter->InsertAfter(pBeforeInstr, pNewInstr);
    std::swap(pNewInstr, pBeforeInstr);

    pNewInstr->m_opcode = CEE_LDC_I4;
    pNewInstr->m_Arg32 = arg;

    pNewInstr = rewriter->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I;
    pNewInstr->m_Arg64 = methodAddress;
    rewriter->InsertBefore(pBeforeInstr, pNewInstr);

    mkCalli_void_int(pNewInstr);
    rewriter->InsertBefore(pBeforeInstr, pNewInstr);

    return S_OK;
}

HRESULT Instrumenter::prependProbe_void_uint(UINT_PTR methodAddress, unsigned arg, ILInstr *&pBeforeInstr)
{
    ILInstr * pNewInstr = rewriter->CopyILInstr(pBeforeInstr);
    rewriter->InsertAfter(pBeforeInstr, pNewInstr);
    std::swap(pNewInstr, pBeforeInstr);

    pNewInstr->m_opcode = CEE_LDC_I4;
    pNewInstr->m_Arg32 = (int)arg;

    pNewInstr = rewriter->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I;
    pNewInstr->m_Arg64 = methodAddress;
    rewriter->InsertBefore(pBeforeInstr, pNewInstr);

    mkCalli_void_uint(pNewInstr);
    rewriter->InsertBefore(pBeforeInstr, pNewInstr);

    return S_OK;
}

HRESULT Instrumenter::prependProbe_void_ulong(UINT_PTR methodAddress, unsigned long arg, ILInstr *&pBeforeInstr)
{
    ILInstr * pNewInstr = rewriter->CopyILInstr(pBeforeInstr);
    rewriter->InsertAfter(pBeforeInstr, pNewInstr);
    std::swap(pNewInstr, pBeforeInstr);

    pNewInstr->m_opcode = CEE_LDC_I8;
    pNewInstr->m_Arg64 = (long)arg;

    pNewInstr = rewriter->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I;
    pNewInstr->m_Arg64 = methodAddress;
    rewriter->InsertBefore(pBeforeInstr, pNewInstr);

    mkCalli_void_ulong(pNewInstr);
    rewriter->InsertBefore(pBeforeInstr, pNewInstr);

    return S_OK;
}

HRESULT Instrumenter::prependProbe_void_bool_uint(UINT_PTR methodAddress, bool arg1, unsigned arg2, ILInstr *&pBeforeInstr)
{
    ILInstr * pNewInstr = rewriter->CopyILInstr(pBeforeInstr);
    rewriter->InsertAfter(pBeforeInstr, pNewInstr);
    std::swap(pNewInstr, pBeforeInstr);

    pNewInstr->m_opcode = arg1 ? CEE_LDC_I4_1 : CEE_LDC_I4_0;

    pNewInstr = rewriter->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I4;
    pNewInstr->m_Arg32 = (int)arg2;
    rewriter->InsertBefore(pBeforeInstr, pNewInstr);

    pNewInstr = rewriter->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I;
    pNewInstr->m_Arg64 = methodAddress;
    rewriter->InsertBefore(pBeforeInstr, pNewInstr);

    mkCalli_void_bool_uint(pNewInstr);
    rewriter->InsertBefore(pBeforeInstr, pNewInstr);

    return S_OK;
}

HRESULT Instrumenter::prependProbe_void_uint_uint(UINT_PTR methodAddress, unsigned arg1, unsigned arg2, ILInstr *&pBeforeInstr)
{
    ILInstr * pNewInstr = rewriter->CopyILInstr(pBeforeInstr);
    rewriter->InsertAfter(pBeforeInstr, pNewInstr);
    std::swap(pNewInstr, pBeforeInstr);

    pNewInstr->m_opcode = CEE_LDC_I4;
    pNewInstr->m_Arg32 = (int)arg1;

    pNewInstr = rewriter->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I4;
    pNewInstr->m_Arg32 = (int)arg2;
    rewriter->InsertBefore(pBeforeInstr, pNewInstr);

    pNewInstr = rewriter->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I;
    pNewInstr->m_Arg64 = methodAddress;
    rewriter->InsertBefore(pBeforeInstr, pNewInstr);

    mkCalli_void_uint_uint(pNewInstr);
    rewriter->InsertBefore(pBeforeInstr, pNewInstr);

    return S_OK;
}

HRESULT Instrumenter::prependProbe_void_uint_uint_ulong(UINT_PTR methodAddress, unsigned arg1, unsigned arg2, unsigned long arg3, ILInstr *&pBeforeInstr)
{
    ILInstr * pNewInstr = rewriter->CopyILInstr(pBeforeInstr);
    rewriter->InsertAfter(pBeforeInstr, pNewInstr);
    std::swap(pNewInstr, pBeforeInstr);

    pNewInstr->m_opcode = CEE_LDC_I4;
    pNewInstr->m_Arg32 = (int)arg1;

    pNewInstr = rewriter->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I4;
    pNewInstr->m_Arg32 = (int)arg2;
    rewriter->InsertBefore(pBeforeInstr, pNewInstr);

    pNewInstr = rewriter->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I8;
    pNewInstr->m_Arg64 = (long)arg3;
    rewriter->InsertBefore(pBeforeInstr, pNewInstr);

    pNewInstr = rewriter->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I;
    pNewInstr->m_Arg64 = methodAddress;
    rewriter->InsertBefore(pBeforeInstr, pNewInstr);

    mkCalli_void_uint_uint_ulong(pNewInstr);
    rewriter->InsertBefore(pBeforeInstr, pNewInstr);

    return S_OK;
}

HRESULT Instrumenter::appendProbe_void_int(UINT_PTR methodAddress, int arg, ILInstr *pAfterInstr)
{
    ILInstr * pNewInstr = nullptr;

    mkCalli_void_int(pNewInstr);
    rewriter->InsertAfter(pAfterInstr, pNewInstr);

    pNewInstr = rewriter->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I;
    pNewInstr->m_Arg64 = methodAddress;
    rewriter->InsertAfter(pAfterInstr, pNewInstr);

    pNewInstr = rewriter->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I4;
    pNewInstr->m_Arg32 = arg;
    rewriter->InsertAfter(pAfterInstr, pNewInstr);

    return S_OK;
}

HRESULT Instrumenter::appendProbe_void_uint(UINT_PTR methodAddress, unsigned arg, ILInstr *pAfterInstr)
{
    ILInstr * pNewInstr = nullptr;

    mkCalli_void_uint(pNewInstr);
    rewriter->InsertAfter(pAfterInstr, pNewInstr);

    pNewInstr = rewriter->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I;
    pNewInstr->m_Arg64 = methodAddress;
    rewriter->InsertAfter(pAfterInstr, pNewInstr);

    pNewInstr = rewriter->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I4;
    pNewInstr->m_Arg32 = (int)arg;
    rewriter->InsertAfter(pAfterInstr, pNewInstr);

    return S_OK;
}

Instrumenter::Instrumenter(ICorProfilerInfo9 &profilerInfo)
    : profilerInfo(profilerInfo)
    , rewriter(nullptr)
    , reflection(profilerInfo)
    , moduleId(0)
    , m_signatureTokens(nullptr)
{
    initInstructionBuffers();
}

Instrumenter::~Instrumenter()
{
    if (m_signatureTokens) delete[] m_signatureTokens;
}

void saveInstructions(ILRewriter &rewriter, std::vector<ILInstr *> &instructions, unsigned &count)
{
    count = rewriter.GetInstructionsCount();
    instructions.reserve(count);
    unsigned i = 0;
    for (ILInstr * pInstr = rewriter.GetILList()->m_pNext; pInstr != rewriter.GetILList(); pInstr = pInstr->m_pNext, ++i) {
        instructions[i] = pInstr;
    }
    assert(i == count);
}

void *unexpectedInstruction(unsigned opcode) {
    ERROR(tout << "Unexpected instruction " << s_OpCodeNames[opcode]);
    throw std::logic_error("Unexpected instruction!");
}

#ifdef INSTRUMENTATION
HRESULT Instrumenter::placeEnterProbe(ILInstr *&pFirstInstr) {
    unsigned maxStackSize = rewriter->MaxStackSize();
    return prependProbe_void_uint_uint(reinterpret_cast<ULONGLONG>(EnterProbeAddress), jittedToken, maxStackSize, pFirstInstr);
}


HRESULT Instrumenter::placeEnterProbe(ILInstr *&pFirstInstr, const char *name) {
    unsigned maxStackSize = rewriter->MaxStackSize();
    return prependProbe_void_uint_uint_ulong(reinterpret_cast<ULONGLONG>(Enter1ProbeAddress), jittedToken, maxStackSize, (unsigned long)name, pFirstInstr);
}

HRESULT Instrumenter::placeLeaveProbe(ILInstr *&pInstr) {
    bool returnsSomething;
    reflection.getReturnValueInfo(jittedToken, returnsSomething);
    return prependProbe_void_uint(reinterpret_cast<ULONGLONG>(LeaveProbeAddress), returnsSomething ? 1 : 0, pInstr);
}

HRESULT Instrumenter::placeProbes()
{
    unsigned instructionsCount = 0;
    std::vector<ILInstr *> instructions;
//    printInstructions("before instrumentation");
    saveInstructions(*rewriter, instructions, instructionsCount);

    bool atLeastOneReturnFound = false;
    ILInstr **prefix = nullptr;
    IfFailRet(placeEnterProbe(instructions[0], reflection.methodName(jittedToken)));
    for (unsigned i = 0; i < instructionsCount; ++i) {
        ILInstr *&pInstr = instructions[i];
        if (isPrefixInstruction[pInstr->m_opcode]) {
            prefix = &instructions[i];
            continue;
        }

//        const char *dumpedInfo = ilInstrToString(pInstr);
//        if (pInstr->m_opcode != CEE_SWITCH_ARG) {
//            prependProbe_void_ulong(reinterpret_cast<ULONGLONG>(DumpInstructionAddress), (unsigned long)dumpedInfo, prefix ? *prefix : pInstr);
//        }
        // TODO: generate switch by defines?
        if (isConcreteInstruction[pInstr->m_opcode]) {
            int pushes = k_rgnStackPushes[pInstr->m_opcode];
            int pops = k_rgnStackPops[pInstr->m_opcode];
            auto probe =
                pushes == 1 && pops == 0 ? Push1ConcreteAddress :
                pushes == 0 && pops == 1 ? Pop1Address :
                reinterpret_cast<void(*)()>(unexpectedInstruction(pInstr->m_opcode));
                IfFailRet(appendProbe_void(reinterpret_cast<ULONGLONG>(probe), pInstr));
//        } else if (isSymbolicStackInstruction[pInstr->m_opcode]) {
        } else if (pInstr->m_opcode == CEE_CALL || pInstr->m_opcode == CEE_CALLI || pInstr->m_opcode == CEE_CALLVIRT || pInstr->m_opcode == CEE_NEWOBJ) {
            bool isTailCall = prefix && (*prefix)->m_opcode == CEE_TAILCALL;
            switch (pInstr->m_opcode) {
            case CEE_CALL: {
            case CEE_CALLVIRT:
            case CEE_NEWOBJ:
                mdMethodDef token = pInstr->m_Arg32;
                const char *name;
                unsigned count;
                mdTypeDef declaringClass;
                bool returnsSomething;
                bool hasThis;
                bool isExtern;
                reflection.getFunctionInfo(token, name, declaringClass, count, returnsSomething, hasThis, isExtern);
                if (hasThis && pInstr->m_opcode != CEE_NEWOBJ) ++count;
                prependProbe_void_uint_uint(reinterpret_cast<ULONGLONG>(CallProbeAddress), (pInstr->m_opcode == CEE_CALLVIRT ? 0 : token), count, prefix ? *prefix : pInstr);
                appendProbe_void_uint(reinterpret_cast<ULONGLONG>(FinalizeCallProbeAddress), returnsSomething ? 1 : 0, pInstr);
//                if (isExtern && returnsSomething) {
//                    appendProbe_void_uint(reinterpret_cast<ULONGLONG>(ExternLeftProbeAddress), 1, pInstr);
//                }

                if (pInstr->m_opcode == CEE_NEWOBJ) {
                    appendProbe_void(reinterpret_cast<ULONGLONG>(Push1ConcreteAddress), pInstr);
                }
//                std::cout << "FUNCTION INFO RETURNED "<< count << "; returns something: " << returnsSomething << std::endl;
            }
                break;
            case CEE_CALLI:
                break;
            }
            // TODO: drop opstack! push result!
        } else if (pInstr->m_opcode == CEE_RET) {
            atLeastOneReturnFound = true;
            IfFailRet(placeLeaveProbe(pInstr));
        } else if (pInstr->m_opcode == CEE_THROW) {
            atLeastOneReturnFound = true;
        } else if (isBranchingInstruction[pInstr->m_opcode]) {
            int pops = k_rgnStackPops[pInstr->m_opcode];
            IfFailRet(prependProbe_void_int(reinterpret_cast<ULONGLONG>(PopAddress), pops, prefix ? *prefix : pInstr));
        } else if (!isIgnoredInstruction[pInstr->m_opcode] && !isPrefixInstruction[pInstr->m_opcode]) {
            // TODO: this clause should be removed!
            int pushes = k_rgnStackPushes[pInstr->m_opcode];
            int pops = k_rgnStackPops[pInstr->m_opcode];
            if (!rewriter->isLastEHInstr(pInstr) && pInstr->m_pNext != rewriter->GetILList()) {
                if (pushes > 0)
                    appendProbe_void_int(reinterpret_cast<ULONGLONG>(PushAddress), pushes, pInstr);
                if (pops > 0)
                    appendProbe_void_int(reinterpret_cast<ULONGLONG>(PopAddress), pops, pInstr);
            } else {
                std::cout << "detected last EH instr: " << ilInstrToString(pInstr) << std::endl;
            }
        } else {
            //assert(isIgnoredInstruction[pInstr->m_opcode]);
        }

        if (prefix && !isPrefixInstruction[pInstr->m_opcode]) {
            prefix = nullptr;
        }
    }
    return atLeastOneReturnFound ? S_OK : E_FAIL;
}
#endif

HRESULT Instrumenter::instrument(FunctionID functionId, Protocol &protocol) {
    HRESULT hr;
    ModuleID oldModuleId = moduleId;
#ifdef INSTRUMENTATION
    IfFailRet(reflection.configure(functionId, moduleId, jittedToken));
#else
    CComPtr<IMetaDataEmit> metadataEmit;
    IfFailRet(reflection.configure(functionId, moduleId, jittedToken, metadataEmit));
#endif

    // TODO: analyze the IL code instead to understand that we've injected functions?
    if (instrumentedFunctions.find({moduleId, jittedToken}) != instrumentedFunctions.end()) {
//        std::cout << "DUPLICATE JITTING OF " << reflection.methodName(jittedToken) << std::endl;
        return S_OK;
    }
    instrumentedFunctions.insert({moduleId, jittedToken});

#ifndef INSTRUMENTATION
    LPCBYTE baseLoadAddress;
    ULONG moduleNameLength;
    AssemblyID assembly;
    IfFailRet(profilerInfo.GetModuleInfo(moduleId, &baseLoadAddress, 0, &moduleNameLength, nullptr, &assembly));
    WCHAR *moduleName = new WCHAR[moduleNameLength];
    IfFailRet(profilerInfo.GetModuleInfo(moduleId, &baseLoadAddress, moduleNameLength, &moduleNameLength, moduleName, &assembly));
    ULONG assemblyNameLength;
    AppDomainID appDomainId;
    ModuleID startModuleId;
    IfFailRet(profilerInfo.GetAssemblyInfo(assembly, 0, &assemblyNameLength, nullptr, &appDomainId, &startModuleId));
    WCHAR *assemblyName = new WCHAR[assemblyNameLength];
    IfFailRet(profilerInfo.GetAssemblyInfo(assembly, assemblyNameLength, &assemblyNameLength, assemblyName, &appDomainId, &startModuleId));

    if (oldModuleId != moduleId) {
        if (m_signatureTokens)
            delete[] m_signatureTokens;
        std::vector<mdSignature> *tokens = new std::vector<mdSignature>;
        initTokens(metadataEmit, *tokens);
        m_signatureTokensLength = tokens->size() * sizeof(mdSignature);
        m_signatureTokens = (char *) &(*tokens)[0];
    }

//    CComPtr<IMetaDataImport> metadataImport;
#endif

#ifdef INSTRUMENTATION
    assert(!rewriter);
#endif
    rewriter = new ILRewriter(&profilerInfo, nullptr, moduleId, jittedToken);

//    LOG(tout << "Instrumenting token " << jittedToken << "..." << std::endl);

#ifndef INSTRUMENTATION
    IfFailRet(rewriter->Import());
    MethodBodyInfo info{
        (unsigned)jittedToken,
        (unsigned)rewriter->CodeSize(),
        (unsigned)(assemblyNameLength - 1) * sizeof(WCHAR),
        (unsigned)(moduleNameLength - 1) * sizeof(WCHAR),
        (unsigned)rewriter->MaxStackSize(),
        (unsigned)rewriter->EHCount(),
        m_signatureTokensLength,
        m_signatureTokens,
        assemblyName,
        moduleName,
        rewriter->Code(),
        (char*)rewriter->EHs()
    };
    if (!protocol.sendMethodBody(info)) return false;
    LOG(tout << "Successfully sent method body!");
    char *bytecode; int length; unsigned maxStackSize; char *ehs; unsigned ehsLength;
    LOG(tout << "Reading method body back...");
    if (!protocol.acceptMethodBody(bytecode, length, maxStackSize, ehs, ehsLength)) return false;
    LOG(tout << "Exporting " << length << " IL bytes!");
    IfFailRet(rewriter->Export(bytecode, length, maxStackSize, ehs, ehsLength));
#endif

#ifdef INSTRUMENTATION
    IfFailRet(rewriter->Import());
//    LOG(tout << "Instrumenting " << methodName(jittedToken) << " (token " << jittedToken << ")...");
//    std::cout << "Instrumenting "  << reflection.methodName(jittedToken) << " (functionId " << functionId << ", moduleId " << reflection.moduleName(moduleId) << ", token " << jittedToken << ")..." << std::endl;
    std::cout << "Instrumenting "  << reflection.methodName(jittedToken) << " (token = " << jittedToken << ")" << std::endl;

    IfFailRet(placeProbes());
////    LOG(tout << "Exporting " << functionId);
    printInstructions("after instrumentation");
//    std::cout << "Exporting..." << std::endl;
    IfFailRet(rewriter->Export());
    std::cout << "Exported!" << std::endl;
    LOG(tout << "Exported!" << std::endl);

    delete rewriter;
    rewriter = nullptr;
#endif
    return S_OK;
}
