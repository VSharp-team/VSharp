#include "instrumentation/instrumenter.h"
#include "instrumentation/ilRewriter.h"
#include "instrumentation/instructions.h"
#include "logging.h"
#include <vector>
#include <stdexcept>

#include "probes.h"

using namespace icsharp;

#undef IfFailRet
#define IfFailRet(EXPR) do { HRESULT hr = (EXPR); if(FAILED(hr)) { return (hr); } } while (0)

#undef IfNullRet
#define IfNullRet(EXPR) do { if ((EXPR) == NULL) return E_OUTOFMEMORY; } while (0)

constexpr auto CEE_LDC_I = sizeof(size_t) == 8 ? CEE_LDC_I8 : sizeof(size_t) == 4 ? CEE_LDC_I4 : throw std::logic_error("size_t must be defined as 8 or 4");

#include<iostream>

ILInstr Instrumenter::mkCalli(mdSignature sig)
{
    ILInstr call = ILInstr();
    call.m_opcode = CEE_CALLI;
    call.m_Arg32 = sig;
    return call;
}

COR_SIGNATURE int_Signature[] = { IMAGE_CEE_CS_CALLCONV_STDCALL, 0x00, ELEMENT_TYPE_I };

mdSignature intSignatureToken;

void Instrumenter::InitTokens() {
    intSignatureToken = reflection.signatureToken(int_Signature, sizeof(int_Signature));
}

void Instrumenter::Initialize() {
    InitTokens();
    InitializeProbes();
    assert(!rewriter);
    rewriter = new ILRewriter(&profilerInfo, nullptr, moduleId, jittedToken);
}

ILInstr Instrumenter::mkCalli_int()
{
    return mkCalli(intSignatureToken);
}

ILInstr Instrumenter::mkLDC_I(UINT_PTR methodAddress)
{
    ILInstr ldc_i = ILInstr();
    ldc_i.m_opcode = CEE_LDC_I;
    ldc_i.m_Arg64 = methodAddress;
    return ldc_i;
}

ILInstr Instrumenter::mkRet()
{
    ILInstr ret = ILInstr();
    ret.m_opcode = CEE_RET;
    return ret;
}

HRESULT Instrumenter::appendProbe_int(UINT_PTR methodAddress)
{
    ILInstr call = mkCalli_int();
    ILInstr ldc_i = mkLDC_I(methodAddress);
    rewriter->AddInstruction(ldc_i);
    rewriter->AddInstruction(call);
    return S_OK;
}

Instrumenter::Instrumenter(ICorProfilerInfo9 &profilerInfo)
    : profilerInfo(profilerInfo)
    , rewriter(nullptr)
    , reflection(profilerInfo)
    , moduleId(0)
{ }

HRESULT Instrumenter::placeConsoleReadProbe() {
    return appendProbe_int(reinterpret_cast<ULONGLONG>(ConsoleReadAddress));
}

HRESULT Instrumenter::placeRet() {
    ILInstr ret = mkRet();
    rewriter->AddInstruction(ret);
    return S_OK;
}

HRESULT Instrumenter::placeProbes()
{
    IfFailRet(placeConsoleReadProbe());
    IfFailRet(placeRet());
    return S_OK;
}

HRESULT Instrumenter::instrument(FunctionID functionId) {
    IfFailRet(reflection.configure(functionId, moduleId, jittedToken));
    LOG(tout << "Got token: " << std::hex << jittedToken << std::endl);
    if (jittedToken == 0x6000077) {
        LOG(tout << "Substituting System.Console.Read" << std::endl);
        if (instrumentedFunctions.find({moduleId, jittedToken}) != instrumentedFunctions.end()) {
            return S_OK;
        }
        instrumentedFunctions.insert({moduleId, jittedToken});

        Initialize();

        IfFailRet(placeProbes());
        IfFailRet(rewriter->Export());
    }

    delete rewriter;
    rewriter = nullptr;

    return S_OK;
}
