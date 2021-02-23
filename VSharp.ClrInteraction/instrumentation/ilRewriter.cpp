#include "instrumentation/ilRewriter.h"
#include "instructions.h"
#include <corhlpr.cpp>
#include <cassert>
#include <stdexcept>

using namespace icsharp;

#undef IfFailRet
#define IfFailRet(EXPR) do { HRESULT hr = (EXPR); if(FAILED(hr)) { return (hr); } } while (0)

#undef IfNullRet
#define IfNullRet(EXPR) do { if ((EXPR) == NULL) return E_OUTOFMEMORY; } while (0)

ILRewriter::ILRewriter(ICorProfilerInfo * pICorProfilerInfo, ICorProfilerFunctionControl * pICorProfilerFunctionControl, ModuleID moduleID, mdToken tkMethod)
    : m_pICorProfilerInfo(pICorProfilerInfo), m_pICorProfilerFunctionControl(pICorProfilerFunctionControl),
    m_moduleId(moduleID), m_tkMethod(tkMethod), m_fGenerateTinyHeader(false),
    m_pEH(nullptr), m_pOffsetToInstr(nullptr), m_pOutputBuffer(nullptr), m_pIMethodMalloc(nullptr)
{
    m_IL.m_pNext = &m_IL;
    m_IL.m_pPrev = &m_IL;

    m_nInstrs = 0;
}

ILRewriter::~ILRewriter()
{
    ILInstr * p = m_IL.m_pNext;
    while (p != &m_IL)
    {
        ILInstr * t = p->m_pNext;
        delete p;
        p = t;
    }
    delete[] m_pEH;
    delete[] m_pOffsetToInstr;
    delete[] m_pOutputBuffer;

    if (m_pIMethodMalloc)
        m_pIMethodMalloc->Release();
}

/////////////////////////////////////////////////////////////////////////////////////////////////
//
// I M P O R T
//
////////////////////////////////////////////////////////////////////////////////////////////////

HRESULT ILRewriter::Import()
{
    LPCBYTE pMethodBytes;

    IfFailRet(m_pICorProfilerInfo->GetILFunctionBody(
        m_moduleId, m_tkMethod, &pMethodBytes, NULL));

    COR_ILMETHOD_DECODER decoder((COR_ILMETHOD*)pMethodBytes);

    // Import the header flags
    m_tkLocalVarSig = decoder.GetLocalVarSigTok();
    m_maxStack = decoder.GetMaxStack();
    m_flags = (decoder.GetFlags() & CorILMethod_InitLocals);

    m_CodeSize = decoder.GetCodeSize();
#ifndef INSTRUMENTATION
    m_Code = new char[m_CodeSize];
    memcpy(m_Code, decoder.Code, m_CodeSize);
#endif



#ifdef INSTRUMENTATION
    IfFailRet(ImportIL(decoder.Code));
#endif

    IfFailRet(ImportEH(decoder.EH, decoder.EHCount()));

    return S_OK;
}

HRESULT ILRewriter::ImportIL(LPCBYTE pIL)
{
    m_pOffsetToInstr = new ILInstr*[m_CodeSize + 1];
    IfNullRet(m_pOffsetToInstr);

    ZeroMemory(m_pOffsetToInstr, m_CodeSize * sizeof(ILInstr*));

    // Set the sentinel instruction
    m_pOffsetToInstr[m_CodeSize] = &m_IL;
    m_IL.m_opcode = -1;

    bool fBranch = false;
    unsigned offset = 0;
    while (offset < m_CodeSize)
    {
        unsigned startOffset = offset;
        unsigned opcode = pIL[offset++];

        if (opcode == CEE_PREFIX1)
        {
            if (offset >= m_CodeSize)
            {
                assert(false);
                return COR_E_INVALIDPROGRAM;
            }
            opcode = 0x100 + pIL[offset++];
        }

        if ((CEE_PREFIX7 <= opcode) && (opcode <= CEE_PREFIX2))
        {
            // NOTE: CEE_PREFIX2-7 are currently not supported
            assert(false);
            return COR_E_INVALIDPROGRAM;
        }

        if (opcode >= CEE_COUNT)
        {
            assert(false);
            return COR_E_INVALIDPROGRAM;
        }

        BYTE flags = s_OpCodeFlags[opcode];

        int size = (flags & OPCODEFLAGS_SizeMask);
        if (offset + size > m_CodeSize)
        {
            assert(false);
            return COR_E_INVALIDPROGRAM;
        }

        ILInstr * pInstr = NewILInstr();
        IfNullRet(pInstr);

        pInstr->m_opcode = opcode;

        InsertBefore(&m_IL, pInstr);

        m_pOffsetToInstr[startOffset] = pInstr;

        switch (flags)
        {
        case 0:
            break;
        case 1:
            pInstr->m_Arg8 = *(UNALIGNED INT8 *)&(pIL[offset]);
            break;
        case 2:
            pInstr->m_Arg16 = *(UNALIGNED INT16 *)&(pIL[offset]);
            break;
        case 4:
            pInstr->m_Arg32 = *(UNALIGNED INT32 *)&(pIL[offset]);
            break;
        case 8:
            pInstr->m_Arg64 = *(UNALIGNED INT64 *)&(pIL[offset]);
            break;
        case 1 | OPCODEFLAGS_BranchTarget:
            pInstr->m_Arg32 = offset + 1 + *(UNALIGNED INT8 *)&(pIL[offset]);
            fBranch = true;
            break;
        case 4 | OPCODEFLAGS_BranchTarget:
            pInstr->m_Arg32 = offset + 4 + *(UNALIGNED INT32 *)&(pIL[offset]);
            fBranch = true;
            break;
        case 0 | OPCODEFLAGS_Switch:
        {
            if (offset + sizeof(INT32) > m_CodeSize)
            {
                assert(false);
                return COR_E_INVALIDPROGRAM;
            }

            unsigned nTargets = *(UNALIGNED INT32 *)&(pIL[offset]);
            pInstr->m_Arg32 = nTargets;
            offset += sizeof(INT32);

            unsigned base = offset + nTargets * sizeof(INT32);

            for (unsigned iTarget = 0; iTarget < nTargets; iTarget++)
            {
                if (offset + sizeof(INT32) > m_CodeSize)
                {
                    assert(false);
                    return COR_E_INVALIDPROGRAM;
                }

                pInstr = NewILInstr();
                IfNullRet(pInstr);

                pInstr->m_opcode = CEE_SWITCH_ARG;

                pInstr->m_Arg32 = base + *(UNALIGNED INT32 *)&(pIL[offset]);
                offset += sizeof(INT32);

                InsertBefore(&m_IL, pInstr);
            }
            fBranch = true;
            break;
        }
        default:
            assert(false);
            break;
        }
        offset += size;
    }
    assert(offset == m_CodeSize);

    if (fBranch)
    {
        // Go over all control flow instructions and resolve the targets
        for (ILInstr * pInstr = m_IL.m_pNext; pInstr != &m_IL; pInstr = pInstr->m_pNext)
        {
            if (s_OpCodeFlags[pInstr->m_opcode] & OPCODEFLAGS_BranchTarget)
                pInstr->m_pTarget = GetInstrFromOffset(pInstr->m_Arg32);
        }
    }

    return S_OK;
}

HRESULT ILRewriter::ImportEH(const COR_ILMETHOD_SECT_EH* pILEH, unsigned nEH)
{
    assert(m_pEH == NULL);

    m_nEH = nEH;

    if (nEH == 0)
        return S_OK;

#ifndef INSTRUMENTATION
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
#else
    IfNullRet(m_pEH = new EHClause[m_nEH]);
    for (unsigned iEH = 0; iEH < m_nEH; iEH++)
    {
        // If the EH clause is in tiny form, the call to pILEH->EHClause() below will
        // use this as a scratch buffer to expand the EH clause into its fat form.
        COR_ILMETHOD_SECT_EH_CLAUSE_FAT scratch;

        const COR_ILMETHOD_SECT_EH_CLAUSE_FAT* ehInfo;
        ehInfo = (COR_ILMETHOD_SECT_EH_CLAUSE_FAT*)pILEH->EHClause(iEH, &scratch);

        EHClause* clause = &(m_pEH[iEH]);
        clause->m_Flags = ehInfo->GetFlags();

        clause->m_pTryBegin = GetInstrFromOffset(ehInfo->GetTryOffset());
        clause->m_pTryEnd = GetInstrFromOffset(ehInfo->GetTryOffset() + ehInfo->GetTryLength());
        clause->m_pHandlerBegin = GetInstrFromOffset(ehInfo->GetHandlerOffset());
        clause->m_pHandlerEnd = GetInstrFromOffset(ehInfo->GetHandlerOffset() + ehInfo->GetHandlerLength())->m_pPrev;
        if ((clause->m_Flags & COR_ILEXCEPTION_CLAUSE_FILTER) == 0)
            clause->m_ClassToken = ehInfo->GetClassToken();
        else
            clause->m_pFilter = GetInstrFromOffset(ehInfo->GetFilterOffset());
    }
#endif

    return S_OK;
}

unsigned ILRewriter::CodeSize() const
{
    return m_CodeSize;
}

char *ILRewriter::Code() const
{
    return m_Code;
}

unsigned ILRewriter::EHCount() const
{
    return m_nEH * sizeof(IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT);
}

#ifndef INSTRUMENTATION
IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT *ILRewriter::EHs() const
{
    return m_pEH;
}
#else
EHClause *ILRewriter::EHs() const
{
    return m_pEH;
}
#endif

ILInstr* ILRewriter::NewILInstr()
{
    m_nInstrs++;
    return new ILInstr();
}

ILInstr *ILRewriter::CopyILInstr(ILInstr *instr)
{
    m_nInstrs++;
    return new ILInstr(*instr);
}

ILInstr* ILRewriter::GetInstrFromOffset(unsigned offset)
{
    ILInstr * pInstr = NULL;

    if (offset <= m_CodeSize)
        pInstr = m_pOffsetToInstr[offset];

    assert(pInstr != NULL);
    return pInstr;
}

unsigned ILRewriter::MaxStackSize() const
{
    return m_maxStack;
}

void ILRewriter::InsertBefore(ILInstr * pWhere, ILInstr * pWhat)
{
    pWhat->m_pNext = pWhere;
    pWhat->m_pPrev = pWhere->m_pPrev;

    pWhat->m_pNext->m_pPrev = pWhat;
    pWhat->m_pPrev->m_pNext = pWhat;

    AdjustState(pWhat);
}

void ILRewriter::InsertAfter(ILInstr * pWhere, ILInstr * pWhat)
{
    pWhat->m_pNext = pWhere->m_pNext;
    pWhat->m_pPrev = pWhere;

    pWhat->m_pNext->m_pPrev = pWhat;
    pWhat->m_pPrev->m_pNext = pWhat;

#ifdef INSTRUMENTATION
    for (unsigned iEH = 0; iEH < m_nEH; iEH++) {
        EHClause *pEH = &(m_pEH[iEH]);
        if (pEH->m_pHandlerEnd == pWhere)
            pEH->m_pHandlerEnd = pWhat;
    }
#endif

    AdjustState(pWhat);
}

void ILRewriter::AdjustState(ILInstr * pNewInstr)
{
    m_maxStack += k_rgnStackPushes[pNewInstr->m_opcode];
}

unsigned ILRewriter::GetInstructionsCount() const
{
    return m_nInstrs;
}

#ifdef INSTRUMENTATION
bool ILRewriter::isLastEHInstr(ILInstr *pInstr) const
{
    for (unsigned iEH = 0; iEH < m_nEH; iEH++) {
        if (m_pEH[iEH].m_pHandlerEnd == pInstr)
            return true;
    }
    return false;
}
#endif

ILInstr * ILRewriter::GetILList()
{
    return &m_IL;
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

again:
    BYTE * pIL = m_pOutputBuffer;

    bool fBranch = false;
    unsigned offset = 0;

    // Go over all instructions and produce code for them
    for (ILInstr * pInstr = m_IL.m_pNext; pInstr != &m_IL; pInstr = pInstr->m_pNext)
    {
        assert(offset < maxSize);
        pInstr->m_offset = offset;

        unsigned opcode = pInstr->m_opcode;
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

        BYTE flags = s_OpCodeFlags[pInstr->m_opcode];
        switch (flags)
        {
        case 0:
            break;
        case 1:
            *(UNALIGNED INT8 *)&(pIL[offset]) = pInstr->m_Arg8;
            break;
        case 2:
            *(UNALIGNED INT16 *)&(pIL[offset]) = pInstr->m_Arg16;
            break;
        case 4:
            *(UNALIGNED INT32 *)&(pIL[offset]) = pInstr->m_Arg32;
            break;
        case 8:
            *(UNALIGNED INT64 *)&(pIL[offset]) = pInstr->m_Arg64;
            break;
        case 1 | OPCODEFLAGS_BranchTarget:
            fBranch = true;
            break;
        case 4 | OPCODEFLAGS_BranchTarget:
            fBranch = true;
            break;
        case 0 | OPCODEFLAGS_Switch:
            *(UNALIGNED INT32 *)&(pIL[offset]) = pInstr->m_Arg32;
            offset += sizeof(INT32);
            break;
        default:
            assert(false);
            break;
        }
        offset += (flags & OPCODEFLAGS_SizeMask);
    }
    m_IL.m_offset = offset;

    if (fBranch)
    {
        bool fTryAgain = false;
        unsigned switchBase = 0;

        // Go over all control flow instructions and resolve the targets
        for (ILInstr * pInstr = m_IL.m_pNext; pInstr != &m_IL; pInstr = pInstr->m_pNext)
        {
            unsigned opcode = pInstr->m_opcode;

            if (pInstr->m_opcode == CEE_SWITCH)
            {
                switchBase = pInstr->m_offset + 1 + sizeof(INT32) * (pInstr->m_Arg32 + 1);
                continue;
            }
            if (opcode == CEE_SWITCH_ARG)
            {
                // Switch args are special
                *(UNALIGNED INT32 *)&(pIL[pInstr->m_offset]) = pInstr->m_pTarget->m_offset - switchBase;
                continue;
            }

            BYTE flags = s_OpCodeFlags[pInstr->m_opcode];

            if (flags & OPCODEFLAGS_BranchTarget)
            {
                int delta = pInstr->m_pTarget->m_offset - pInstr->m_pNext->m_offset;

                switch (flags)
                {
                case 1 | OPCODEFLAGS_BranchTarget:
                    // Check if delta is too big to fit into an INT8.
                    //
                    // (see #pragma at top of file)
                    if ((INT8)delta != delta)
                    {
                        if (opcode == CEE_LEAVE_S)
                        {
                            pInstr->m_opcode = CEE_LEAVE;
                        }
                        else
                        {
                            assert(opcode >= CEE_BR_S && opcode <= CEE_BLT_UN_S);
                            pInstr->m_opcode = opcode - CEE_BR_S + CEE_BR;
                            assert(pInstr->m_opcode >= CEE_BR && pInstr->m_opcode <= CEE_BLT_UN);
                        }
                        fTryAgain = true;
                        continue;
                    }
                    *(UNALIGNED INT8 *)&(pIL[pInstr->m_pNext->m_offset - sizeof(INT8)]) = delta;
                    break;
                case 4 | OPCODEFLAGS_BranchTarget:
                    *(UNALIGNED INT32 *)&(pIL[pInstr->m_pNext->m_offset - sizeof(INT32)]) = delta;
                    break;
                default:
                    assert(false);
                    break;
                }
            }
        }

        // Do the whole thing again if we changed the size of some branch targets
        if (fTryAgain)
            goto again;
    }

    unsigned codeSize = offset;
    unsigned totalSize;
    LPBYTE pBody = NULL;
    if (m_fGenerateTinyHeader)
    {
        // Make sure we can fit in a tiny header
        if (codeSize >= 64)
            return E_FAIL;

        totalSize = sizeof(IMAGE_COR_ILMETHOD_TINY) + codeSize;
        pBody = AllocateILMemory(totalSize);
        IfNullRet(pBody);

        BYTE * pCurrent = pBody;

        // Here's the tiny header
        *pCurrent = (BYTE)(CorILMethod_TinyFormat | (codeSize << 2));
        pCurrent += sizeof(IMAGE_COR_ILMETHOD_TINY);

        // And the body
        CopyMemory(pCurrent, m_pOutputBuffer, codeSize);
    }
    else
    {
        // Use FAT header

        unsigned alignedCodeSize = (offset + 3) & ~3;

        totalSize = sizeof(IMAGE_COR_ILMETHOD_FAT) + alignedCodeSize +
            (m_nEH ? (sizeof(IMAGE_COR_ILMETHOD_SECT_FAT) + sizeof(IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT) * m_nEH) : 0);

        pBody = AllocateILMemory(totalSize);
        IfNullRet(pBody);

        BYTE * pCurrent = pBody;

        IMAGE_COR_ILMETHOD_FAT *pHeader = (IMAGE_COR_ILMETHOD_FAT *)pCurrent;
        pHeader->Flags = m_flags | (m_nEH ? CorILMethod_MoreSects : 0) | CorILMethod_FatFormat;
        pHeader->Size = sizeof(IMAGE_COR_ILMETHOD_FAT) / sizeof(DWORD);
        pHeader->MaxStack = m_maxStack;
        pHeader->CodeSize = offset;
        pHeader->LocalVarSigTok = m_tkLocalVarSig;

        pCurrent = (BYTE*)(pHeader + 1);

        CopyMemory(pCurrent, m_pOutputBuffer, codeSize);
        pCurrent += alignedCodeSize;

        if (m_nEH != 0)
        {
            IMAGE_COR_ILMETHOD_SECT_FAT *pEH = (IMAGE_COR_ILMETHOD_SECT_FAT *)pCurrent;
            pEH->Kind = CorILMethod_Sect_EHTable | CorILMethod_Sect_FatFormat;
            pEH->DataSize = (unsigned)(sizeof(IMAGE_COR_ILMETHOD_SECT_FAT) + sizeof(IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT) * m_nEH);

            pCurrent = (BYTE*)(pEH + 1);

            for (unsigned iEH = 0; iEH < m_nEH; iEH++)
            {
#ifdef INSTRUMENTATION
                EHClause *pSrc = &(m_pEH[iEH]);
                IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT * pDst = (IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT *)pCurrent;

                pDst->Flags = pSrc->m_Flags;
                pDst->TryOffset = pSrc->m_pTryBegin->m_offset;
                pDst->TryLength = pSrc->m_pTryEnd->m_offset - pSrc->m_pTryBegin->m_offset;
                pDst->HandlerOffset = pSrc->m_pHandlerBegin->m_offset;
                pDst->HandlerLength = pSrc->m_pHandlerEnd->m_pNext->m_offset - pSrc->m_pHandlerBegin->m_offset;
                if ((pSrc->m_Flags & COR_ILEXCEPTION_CLAUSE_FILTER) == 0)
                    pDst->ClassToken = pSrc->m_ClassToken;
                else
                    pDst->FilterOffset = pSrc->m_pFilter->m_offset;

                pCurrent = (BYTE*)(pDst + 1);
#endif
            }
        }
    }

    IfFailRet(SetILFunctionBody(totalSize, pBody));
    DeallocateILMemory(pBody);

    return S_OK;
}

HRESULT ILRewriter::Export(char *bytecode, unsigned codeLength, unsigned maxStackSize, char *ehs, unsigned ehsLength)
{
    // Use FAT header
    m_nEH = ehsLength / sizeof(IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT);
    m_maxStack = maxStackSize;

    unsigned alignedCodeSize = (codeLength + 3) & ~3;

    unsigned totalSize = sizeof(IMAGE_COR_ILMETHOD_FAT) + alignedCodeSize +
        (m_nEH ? (sizeof(IMAGE_COR_ILMETHOD_SECT_FAT) + sizeof(IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT) * m_nEH) : 0);

    LPBYTE pBody = AllocateILMemory(totalSize);
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


