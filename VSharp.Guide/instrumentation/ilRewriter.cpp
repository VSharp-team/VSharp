#include "instrumentation/ilRewriter.h"
#include "instructions.h"
#include <corhlpr.cpp>
#include <cassert>

using namespace icsharp;

#undef IfFailRet
#define IfFailRet(EXPR) do { HRESULT hr = (EXPR); if(FAILED(hr)) { return (hr); } } while (0)

#undef IfNullRet
#define IfNullRet(EXPR) do { if ((EXPR) == NULL) return E_OUTOFMEMORY; } while (0)

ILRewriter::ILRewriter(ICorProfilerInfo * pICorProfilerInfo, ICorProfilerFunctionControl * pICorProfilerFunctionControl, ModuleID moduleID, mdToken tkMethod)
    : m_pICorProfilerInfo(pICorProfilerInfo), m_pICorProfilerFunctionControl(pICorProfilerFunctionControl),
    m_moduleId(moduleID), m_tkMethod(tkMethod), m_pOutputBuffer(nullptr), m_pIMethodMalloc(nullptr), m_nInstrs(0) { }

ILRewriter::~ILRewriter()
{
    delete[] m_pOutputBuffer;

    if (m_pIMethodMalloc)
        m_pIMethodMalloc->Release();
}

void ILRewriter::AddInstruction(const ILInstr &instr)
{
    m_IL.push_back(instr);
    m_nInstrs++;
}

/////////////////////////////////////////////////////////////////////////////////////////////////
//
// E X P O R T
//
////////////////////////////////////////////////////////////////////////////////////////////////


HRESULT ILRewriter::Export()
{
    // One instruction produces 2 + sizeof(native int) bytes in the worst case which can be 10 bytes for 64-bit.
    // For simplification we just use 10 here.
    unsigned maxSize = m_nInstrs * 10;

    m_pOutputBuffer = new BYTE[maxSize];
    IfNullRet(m_pOutputBuffer);

    BYTE * pIL = m_pOutputBuffer;
    unsigned offset = 0;

    // Go over all instructions and produce code for them
    for (ILInstr &pInstr : m_IL)
    {
        assert(offset < maxSize);
        pInstr.m_offset = offset;

        unsigned opcode = pInstr.m_opcode;
        if (opcode < CEE_COUNT)
        {
            // CEE_PREFIX1 refers not to instruction prefixes (like tail.), but to
            // the lead byte of multi-byte opcodes. For now, the only lead byte
            // supported is CEE_PREFIX1 = 0xFE.
            if (opcode >= 0x100)
                m_pOutputBuffer[offset++] = CEE_PREFIX1;

            // This appears to depend on an implicit conversion from
            // unsigned opcode down to BYTE, to deliberately lose data and have
            // opcode >= 0x100 wrap around to 0.
            m_pOutputBuffer[offset++] = (opcode & 0xFF);
        }

        BYTE flags = s_OpCodeFlags[pInstr.m_opcode];
        switch (flags)
        {
        case 0:
            break;
        case 1:
            *(UNALIGNED INT8 *)&(pIL[offset]) = pInstr.m_Arg8;
            break;
        case 2:
            *(UNALIGNED INT16 *)&(pIL[offset]) = pInstr.m_Arg16;
            break;
        case 4:
            *(UNALIGNED INT32 *)&(pIL[offset]) = pInstr.m_Arg32;
            break;
        case 8:
            *(UNALIGNED INT64 *)&(pIL[offset]) = pInstr.m_Arg64;
            break;
        case 0 | OPCODEFLAGS_Switch:
            *(UNALIGNED INT32 *)&(pIL[offset]) = pInstr.m_Arg32;
            offset += sizeof(INT32);
            break;
        default:
            assert(false);
            break;
        }
        offset += (flags & OPCODEFLAGS_SizeMask);
    }

    unsigned codeSize = offset;
    assert(codeSize < 64);
    unsigned totalSize = sizeof(IMAGE_COR_ILMETHOD_TINY) + codeSize;
    LPBYTE pBody = AllocateILMemory(totalSize);
    IfNullRet(pBody);

    BYTE * pCurrent = pBody;

    // Here's the tiny header
    *pCurrent = (BYTE)(CorILMethod_TinyFormat | (codeSize << 2));
    pCurrent += sizeof(IMAGE_COR_ILMETHOD_TINY);

    // And the body
    CopyMemory(pCurrent, m_pOutputBuffer, codeSize);

    IfFailRet(SetILFunctionBody(totalSize, pBody));
    DeallocateILMemory(pBody);

    return S_OK;
}

HRESULT ILRewriter::SetILFunctionBody(unsigned size, LPBYTE pBody)
{
    if (m_pICorProfilerFunctionControl != NULL)
    {
        // We're supplying IL for a rejit, so use the rejit mechanism
        IfFailRet(m_pICorProfilerFunctionControl->SetILFunctionBody(size, pBody));
    }
    else
    {
        // "classic-style" instrumentation on first JIT, so use old mechanism
        IfFailRet(m_pICorProfilerInfo->SetILFunctionBody(m_moduleId, m_tkMethod, pBody));
    }

    return S_OK;
}

LPBYTE ILRewriter::AllocateILMemory(unsigned size)
{
    if (m_pICorProfilerFunctionControl != NULL)
    {
        // We're supplying IL for a rejit, so we can just allocate from
        // the heap
        return new BYTE[size];
    }

    // Else, this is "classic-style" instrumentation on first JIT, and
    // need to use the CLR's IL allocator

    if (FAILED(m_pICorProfilerInfo->GetILFunctionBodyAllocator(m_moduleId, &m_pIMethodMalloc)))
        return NULL;

    return (LPBYTE)m_pIMethodMalloc->Alloc(size);
}

void ILRewriter::DeallocateILMemory(LPBYTE pBody)
{
    if (m_pICorProfilerFunctionControl == NULL)
    {
        // Old-style instrumentation does not provide a way to free up bytes
        return;
    }

    delete[] pBody;
}


