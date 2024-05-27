// Copyright (c) .NET Foundation and contributors. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#include "ILRewriter.h"
#include "corhlpr.cpp"

    /////////////////////////////////////////////////////////////////////////////////////////////////
    //
    // I M P O R T
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////


typedef enum
{
#define OPDEF(c,s,pop,push,args,type,l,s1,s2,ctrl) c,
#include "opcode.def"
#undef OPDEF
    CEE_COUNT,
    CEE_SWITCH_ARG, // special internal instructions
} OPCODE;

#define OPCODEFLAGS_SizeMask        0x0F
#define OPCODEFLAGS_BranchTarget    0x10
#define OPCODEFLAGS_Switch          0x20

const char* opcodetostr(unsigned int opcode) {
#define OPDEF(c,s,pop,push,args,type,l,s1,s2,ctrl) if (opcode == c) return s;
#define OPDEF_REAL_OPCODES_ONLY

#include "opcode.def"
    return "unknwn";

#undef OPDEF_REAL_OPCODES_ONLY
#undef OPDEF
}

static const BYTE s_OpCodeFlags[] =
        {
#define InlineNone           0
#define ShortInlineVar       1
#define InlineVar            2
#define ShortInlineI         1
#define InlineI              4
#define InlineI8             8
#define ShortInlineR         4
#define InlineR              8
#define ShortInlineBrTarget  1 | OPCODEFLAGS_BranchTarget
#define InlineBrTarget       4 | OPCODEFLAGS_BranchTarget
#define InlineMethod         4
#define InlineField          4
#define InlineType           4
#define InlineString         4
#define InlineSig            4
#define InlineRVA            4
#define InlineTok            4
#define InlineSwitch         0 | OPCODEFLAGS_Switch

#define OPDEF(c,s,pop,push,args,type,l,s1,s2,flow) args,
#include "opcode.def"
#undef OPDEF

#undef InlineNone
#undef ShortInlineVar
#undef InlineVar
#undef ShortInlineI
#undef InlineI
#undef InlineI8
#undef ShortInlineR
#undef InlineR
#undef ShortInlineBrTarget
#undef InlineBrTarget
#undef InlineMethod
#undef InlineField
#undef InlineType
#undef InlineString
#undef InlineSig
#undef InlineRVA
#undef InlineTok
#undef InlineSwitch
                0,                              // CEE_COUNT
                4 | OPCODEFLAGS_BranchTarget,   // CEE_SWITCH_ARG
        };

static int k_rgnStackPushes[] = {

#define OPDEF(c,s,pop,push,args,type,l,s1,s2,ctrl) \
	 push ,

#define Push0    0
#define Push1    1
#define PushI    1
#define PushI4   1
#define PushR4   1
#define PushI8   1
#define PushR8   1
#define PushRef  1
#define VarPush  1          // Test code doesn't call vararg fcns, so this should not be used

#include "opcode.def"

#undef Push0
#undef Push1
#undef PushI
#undef PushI4
#undef PushR4
#undef PushI8
#undef PushR8
#undef PushRef
#undef VarPush
#undef OPDEF
        0,  // CEE_COUNT
        0   // CEE_SWITCH_ARG
};

ILRewriter::ILRewriter(
    ICorProfilerInfo * pICorProfilerInfo,
    ICorProfilerFunctionControl * pICorProfilerFunctionControl,
    ModuleID moduleID,
    mdToken tkMethod)
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

HRESULT ILRewriter::Import()
{
    LPCBYTE pMethodBytes;

    IfFailRet(m_pICorProfilerInfo->GetILFunctionBody(
            m_moduleId, m_tkMethod, &pMethodBytes, nullptr));

    COR_ILMETHOD_DECODER decoder((COR_ILMETHOD*)pMethodBytes);

    // Import the header flags
    m_tkLocalVarSig = decoder.GetLocalVarSigTok();
    m_maxStack = decoder.GetMaxStack();
    m_flags = (decoder.GetFlags() & CorILMethod_InitLocals);

    m_CodeSize = decoder.GetCodeSize();

    IfFailRet(ImportIL(decoder.Code));

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

    return S_OK;
}

ILInstr* ILRewriter::NewILInstr()
{
    m_nInstrs++;
    return new ILInstr();
}

ILInstr* ILRewriter::GetInstrFromOffset(unsigned offset)
{
    ILInstr * pInstr = NULL;

    if (offset <= m_CodeSize)
        pInstr = m_pOffsetToInstr[offset];

    assert(pInstr != NULL);
    return pInstr;
}

void ILRewriter::PrintEhs()
{
    LOG(
        if (m_pEH != nullptr) {
            for (int i = 0; i < m_nEH; i++) {
                tout << "handler: " << m_pEH[i].m_pHandlerBegin << " " << m_pEH[i].m_pHandlerEnd << "\n";
                tout << "    try: " << m_pEH[i].m_pTryBegin << " " << m_pEH[i].m_pTryEnd << "\n";
                tout << "\n";
            }
        }
    );
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

    AdjustState(pWhat);
}

void ILRewriter::AdjustState(ILInstr * pNewInstr)
{
    m_maxStack += k_rgnStackPushes[pNewInstr->m_opcode];
}


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
            }
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

    if (m_pICorProfilerInfo == nullptr) return NULL;

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

HRESULT AddProbe(
    ILRewriter * pilr,
    UINT_PTR methodAddress,
    ULONG32 methodSignature,
    ILInstr *pInsertProbeBeforeThisInstr)
{
    ILInstr * pNewInstr = nullptr;

    constexpr auto CEE_LDC_I = sizeof(size_t) == 8 ? CEE_LDC_I8 : sizeof(size_t) == 4 ? CEE_LDC_I4 : throw std::logic_error("size_t must be defined as 8 or 4");

    pNewInstr = pilr->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I;
    pNewInstr->m_Arg64 = methodAddress;
    pilr->InsertBefore(pInsertProbeBeforeThisInstr, pNewInstr);

    pNewInstr = pilr->NewILInstr();
    pNewInstr->m_opcode = CEE_CALLI;
    pNewInstr->m_Arg32 = methodSignature;
    pilr->InsertBefore(pInsertProbeBeforeThisInstr, pNewInstr);

    return S_OK;
}

HRESULT AddProbeAfter(
        ILRewriter * pilr,
        UINT_PTR methodAddress,
        ULONG32 methodSignature,
        ILInstr *pInsertProbeAfterThisInstr,
        ILInstr *beforehandInstr[],
        int instrSize)
{
    ILInstr * pNewInstr = nullptr;

    constexpr auto CEE_LDC_I = sizeof(size_t) == 8 ? CEE_LDC_I8 : sizeof(size_t) == 4 ? CEE_LDC_I4 : throw std::logic_error("size_t must be defined as 8 or 4");

    pNewInstr = pilr->NewILInstr();
    pNewInstr->m_opcode = CEE_CALLI;
    pNewInstr->m_Arg32 = methodSignature;
    pilr->InsertAfter(pInsertProbeAfterThisInstr, pNewInstr);

    pNewInstr = pilr->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I;
    pNewInstr->m_Arg64 = methodAddress;
    pilr->InsertAfter(pInsertProbeAfterThisInstr, pNewInstr);

    return S_OK;
}

HRESULT AddEnterProbe(
    ILRewriter * pilr,
    UINT_PTR methodAddress,
    ULONG32 methodSignature,
    int methodId)
{
    ILInstr * pFirstOriginalInstr = pilr->GetILList()->m_pNext;

    auto pNewInstr = pilr->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I4;
    pNewInstr->m_Arg32 = (INT32)pFirstOriginalInstr->m_offset;
    pilr->InsertBefore(pFirstOriginalInstr, pNewInstr);

    ILInstr * offsetInstr;
    offsetInstr = pilr->NewILInstr();
    offsetInstr->m_opcode = CEE_LDC_I4;
    offsetInstr->m_Arg32 = (INT32)methodId;
    pilr->InsertBefore(pFirstOriginalInstr, offsetInstr);

    ILInstr * spontaneousInstr;
    spontaneousInstr = pilr->NewILInstr();
    spontaneousInstr->m_opcode = CEE_LDC_I4_0;
    pilr->InsertBefore(pFirstOriginalInstr, spontaneousInstr);

    return AddProbe(pilr, methodAddress, methodSignature, pFirstOriginalInstr);
}

void countOffsets(ILRewriter *pilr) {
    unsigned offset = 0;

    ILInstr *first = pilr->GetILList();

    // Go over all instructions and produce code for them
    for (ILInstr * pInstr = first->m_pNext; pInstr != first; pInstr = pInstr->m_pNext)
    {
        pInstr->m_offset = offset;

        unsigned opcode = pInstr->m_opcode;
        if (opcode < CEE_COUNT)
        {
            // CEE_PREFIX1 refers not to instruction prefixes (like tail.), but to
            // the lead byte of multi-byte opcodes. For now, the only lead byte
            // supported is CEE_PREFIX1 = 0xFE.
            if (opcode >= 0x100)
                offset++;

            // This appears to depend on an implicit conversion from
            // unsigned opcode down to BYTE, to deliberately lose data and have
            // opcode >= 0x100 wrap around to 0.
            offset++;
        }

        BYTE flags = s_OpCodeFlags[pInstr->m_opcode];
        switch (flags)
        {
            case 0:
            case 1:
            case 2:
            case 4:
            case 8:
            case 1 | OPCODEFLAGS_BranchTarget:
            case 4 | OPCODEFLAGS_BranchTarget:
                break;
            case 0 | OPCODEFLAGS_Switch:
                offset += sizeof(INT32);
                break;
            default:
                break;
        }
        offset += (flags & OPCODEFLAGS_SizeMask);
    }
}

// returns pointer to the new instruction
ILInstr *AddLDCInstrBefore(ILRewriter *pilr, ILInstr *pInstr, INT32 arg) {
    ILInstr *pNewInstr = pilr->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I4;
    pNewInstr->m_Arg32 = arg;
    pilr->InsertBefore(pInstr, pNewInstr);

    return pNewInstr;
}

bool IsTailcallRet(ILInstr *pInstr) {
    return pInstr->m_opcode == CEE_RET && pInstr->m_pPrev->m_pPrev->m_opcode == CEE_TAILCALL;
}

bool IsPrefix(ILInstr *pInstr) {
    return pInstr->m_opcode == CEE_TAILCALL
        || pInstr->m_opcode == CEE_UNALIGNED
        || pInstr->m_opcode == CEE_VOLATILE
        || pInstr->m_opcode == CEE_READONLY
        || pInstr->m_opcode == CEE_CONSTRAINED
        ;
}

void PrintILInstructions(ILRewriter *pilr) {
    LOG(
        for (ILInstr* pInstr = pilr->GetILList()->m_pNext; pInstr != pilr->GetILList(); pInstr = pInstr->m_pNext) {
            tout << pInstr << "   " << pInstr->m_offset << " " << opcodetostr(pInstr->m_opcode);
            if (pInstr->m_opcode >= CEE_BR_S && CEE_BLT_UN >= pInstr->m_opcode)
                tout << "tg: " << pInstr->m_pTarget->m_offset;
            if (pInstr->m_opcode == 295)
                tout << "tg: " << pInstr->m_pTarget->m_offset;
            tout << "\n";
        }

        tout << "\n" << "exception handlers" << std::endl;

        tout << std::endl;
    );
    pilr->PrintEhs();
}

void CorrectHandlers(ILRewriter* pilr, ILInstr* pInstr, ILInstr* pNewInstr)
{
    // changing exception handlers bounds if we were on the end of handler block
    if (pilr->m_pEH != nullptr) {
        for (int i = 0; i < pilr->m_nEH; i++) {
            if (pilr->m_pEH[i].m_pHandlerEnd == pInstr) {
                pilr->m_pEH[i].m_pHandlerEnd = pNewInstr;
            }
        }
    }
}

// advances the instruction pointer to the instruction after the probe
HRESULT AddCoverageProbeAfter(
    ILRewriter* pilr,
    ILInstr*& pInstr,
    vsharp::ProbeCall* probe,
    int methodId)
{
    // new instruction for easier exception handling
    ILInstr* pNewInstr = pilr->NewILInstr();
    pNewInstr->m_opcode = CEE_NOP;
    pilr->InsertAfter(pInstr, pNewInstr);

    AddLDCInstrBefore(pilr, pNewInstr, (INT32)pInstr->m_offset);

    AddLDCInstrBefore(pilr, pNewInstr, methodId);

    // adding the probe
    IfFailRet(AddProbe(pilr, probe->addr, probe->getSig(), pNewInstr));

    CorrectHandlers(pilr, pInstr, pNewInstr);

    pInstr = pNewInstr;
    return S_OK;
}

// advances the instruction pointer to the copied version of the original one
HRESULT AddCoverageProbeBefore(
        ILRewriter *pilr,
        ILInstr *&pInstr,
        vsharp::ProbeCall* probe,
        int methodId)
{
    // adding the new instruction
    ILInstr * pNewInstr = pilr->NewILInstr();
    pNewInstr->m_opcode = pInstr->m_opcode;
    pNewInstr->m_Arg64 = pInstr->m_Arg64; // using the widest argument of the union to copy it
    pilr->InsertAfter(pInstr, pNewInstr);

    pInstr->m_opcode = CEE_NOP;

    AddLDCInstrBefore(pilr, pNewInstr, (INT32)pInstr->m_offset);

    AddLDCInstrBefore(pilr, pNewInstr, methodId);

    // adding the probe
    IfFailRet(AddProbe(pilr, probe->addr, probe->getSig(), pNewInstr));

    CorrectHandlers(pilr, pInstr, pNewInstr);

    pInstr = pNewInstr;
    return S_OK;
}

HRESULT AddExitProbe(
    ILRewriter* pilr,
    int methodId)
{
    BOOL isTailCall = FALSE;
    auto covProb = vsharp::getProbes();

    // TODO: create iterator over instructions
    // Find all RETs, and insert a call to the exit probe before each one.
    for (ILInstr* pInstr = pilr->GetILList()->m_pNext; pInstr != pilr->GetILList(); pInstr = pInstr->m_pNext)
    {
        // TODO: unify with 'tailcall' handling in 'RewriteIL'
        switch (pInstr->m_opcode)
        {
            case CEE_TAILCALL:
            {
                isTailCall = TRUE;
                AddCoverageProbeBefore(pilr, pInstr, covProb->Tailcall, methodId);
                break;
            }
            case CEE_RET:
            {
                if (isTailCall) {
                    isTailCall = FALSE;
                    break;
                }
                AddCoverageProbeBefore(pilr, pInstr, covProb->Leave, methodId);
                break;
            }

            default:
                break;
        }
    }

    return S_OK;
}

HRESULT MakeProbeInsertion(ILRewriter *pilr, ProbeInsertion toInsert, int methodId) {
    if (toInsert.isBeforeInstr) {
        IfFailRet(AddCoverageProbeBefore(pilr, toInsert.target, toInsert.probe, methodId));
    }
    else {
        IfFailRet(AddCoverageProbeAfter(pilr, toInsert.target, toInsert.probe, methodId));
    }
    return S_OK;
}

bool OpcodeIsBranch(unsigned opcode) {
    return
        (CEE_BR_S <= opcode && opcode <= CEE_SWITCH)
        || opcode == CEE_LEAVE || opcode == CEE_LEAVE_S;
}

// Uses the general-purpose ILRewriter class to import original
// IL, rewrite it, and send the result to the CLR
HRESULT RewriteIL(
        ICorProfilerInfo * pICorProfilerInfo,
        ICorProfilerFunctionControl * pICorProfilerFunctionControl,
        ModuleID moduleID,
        mdMethodDef methodDef,
        int methodId,
        bool isMain,
        bool isTestRun)
{
    ILRewriter rewriter(pICorProfilerInfo, pICorProfilerFunctionControl, moduleID, methodDef);
    auto pilr = &rewriter;

    auto covProb = vsharp::getProbes();

    vsharp::ProbeCall* enterMethod;
    vsharp::ProbeCall* leaveMethod;
    if (isMain || isTestRun) {
        enterMethod = covProb->EnterMain;
        leaveMethod = covProb->LeaveMain;
    }
    else {
        enterMethod = covProb->Enter;
        leaveMethod = covProb->Leave;
    }

    IfFailRet(rewriter.Import());
    countOffsets(&rewriter);

    BOOL isTailCall = FALSE;

    std::vector<ProbeInsertion> addPriorityProbe;
    std::vector<ProbeInsertion> addTargetProbe;
    std::set<unsigned> coveredInstructions;

    bool PIBeforeInstr = true;
    bool PIAfterInstr = false;

    // adding probes for coverage tracking, looking for the last instructions of basic blocks
    for (ILInstr * pInstr = pilr->GetILList()->m_pNext; pInstr != pilr->GetILList(); pInstr = pInstr->m_pNext)
    {
        unsigned opcode = pInstr->m_opcode;
        // branch coverage
        if (OpcodeIsBranch(opcode)) {
            addPriorityProbe.push_back({ pInstr, nullptr, covProb->Branch, PIBeforeInstr });

            // inserting all switch cases as possible target points
            if (opcode == CEE_SWITCH) {
                ILInstr *curSwitchArg = pInstr->m_pNext;
                for (int i = 0; i < pInstr->m_Arg32; i++) {
                    assert(curSwitchArg->m_opcode == 295); // checking switch arg constant
                    addTargetProbe.push_back({ curSwitchArg->m_pTarget->m_pPrev, pInstr, covProb->Coverage, PIAfterInstr });
                    curSwitchArg = curSwitchArg->m_pNext;
                }
            }
            else {
                // inserting instruction before target as it's the end of the block
                addTargetProbe.push_back({ pInstr->m_pTarget->m_pPrev, pInstr, covProb->Coverage, PIAfterInstr });
            }

            continue;
        }

        // rest of the coverage
        switch (pInstr->m_opcode)
        {
            case CEE_STSFLD:
            {
                ILInstr* instr;
                if (IsPrefix(pInstr->m_pPrev)) {
                    instr = pInstr->m_pPrev;
                }
                else {
                    instr = pInstr;
                }
                addPriorityProbe.push_back({ instr, nullptr, covProb->Stsfld, PIBeforeInstr });
                break;
            }
            case CEE_TAILCALL:
            {
                isTailCall = TRUE;

                // adding new instruction to redirect return probe as call and ret create their own basic blocks
                // and need different probes calls
                ILInstr* newTailcall = pilr->NewILInstr();
                newTailcall->m_opcode = CEE_TAILCALL;
                pInstr->m_opcode = CEE_NOP;
                pilr->InsertAfter(pInstr, newTailcall);

                // taking call's offset for the branch insertion as it is the end of the block not tailcall
                newTailcall->m_offset = newTailcall->m_pNext->m_offset;
                pInstr->m_offset = newTailcall->m_pNext->m_pNext->m_offset; // taking ret's offset

                addPriorityProbe.push_back({ newTailcall, nullptr, covProb->Tailcall, PIBeforeInstr });
                // covering with usual coverage probe as tailcall already takes care of stack changes
                addPriorityProbe.push_back({ pInstr, nullptr, covProb->Coverage, PIBeforeInstr });

                // advancing pInstr to avoid loops
                pInstr = newTailcall;
                break;
            }
            case CEE_CALL:
            case CEE_CALLI:
            case CEE_CALLVIRT:
            case CEE_NEWOBJ:
            {
                if (isTailCall) {
                    continue;
                }

                addPriorityProbe.push_back({ pInstr, nullptr, covProb->Call, PIAfterInstr });
                break;
            }
            case CEE_RET:
            {
                if (isTailCall) {
                    // instruction sequence is:
                    // tail.
                    // call
                    // ret <- returning flag to its default position here
                    isTailCall = FALSE;
                    break;
                }
                addPriorityProbe.push_back({ pInstr, nullptr, leaveMethod, PIBeforeInstr });
                break;
            }

            // handling exception blocks
            case CEE_ENDFINALLY:
            case CEE_ENDFILTER:
            {
                addPriorityProbe.push_back({ pInstr, nullptr, covProb->Coverage, PIBeforeInstr });
                break;
            }
            case CEE_THROW:
            case CEE_RETHROW:
            {
                addPriorityProbe.push_back({ pInstr, nullptr, covProb->Throw, PIBeforeInstr });
                break;
            }

            default:
                break;
        }
    }

    if (pilr->m_pEH != nullptr) {
        for (int i = 0; i < pilr->m_nEH; i++) {
            addPriorityProbe.push_back({ pilr->m_pEH[i].m_pHandlerBegin, nullptr, covProb->Coverage, PIBeforeInstr });
        }
    }

    for (auto &insertion : addPriorityProbe) {
        // TODO: tailcall + ret can be broken into two basic blocks; but adding two probes is impossible
        IfFailRet(MakeProbeInsertion(pilr, insertion, methodId));
        coveredInstructions.insert(insertion.target->m_offset);
    }

    // adding probes for branch targets now as they can point anywhere in the code
    for (auto &insertion : addTargetProbe) {
        ILInstr *target = insertion.target;
        ILInstr *branch = insertion.parent;

        if (target == pilr->GetILList() || coveredInstructions.find(target->m_offset) != coveredInstructions.end())
            continue;
        coveredInstructions.insert(target->m_offset);

        // targets on returns under tailcall require special treatment
        if (!IsTailcallRet(target->m_pNext)) {
            IfFailRet(AddCoverageProbeAfter(pilr, target, insertion.probe, methodId));
            continue;
        }
        // target is a ret after a tailcall:
        //
        // tail.
        // call
        // ret
        //
        // cannot add instructions before the return; changing the target of the original branch and creating new ret

        // generated IL-code:
        //
        // 0: <normal control flow before the original branch>
        // 1: original branch with target to 3
        // 2: br with target to 5
        // 3: probe call
        // 4: ret
        // 5: <normal control flow after the original branch>

        // inserting new ret after the branch
        ILInstr* pNewRet = pilr->NewILInstr();
        pNewRet->m_opcode = CEE_RET;
        pilr->InsertAfter(branch, pNewRet);

        // remembering the original ret's offset
        ILInstr* probeStart = AddLDCInstrBefore(pilr, pNewRet, (INT32)target->m_offset);

        AddLDCInstrBefore(pilr, pNewRet, methodId);

        // adding leave probe as it's a normal return without tailcall
        IfFailRet(AddProbe(pilr, leaveMethod->addr, leaveMethod->getSig(), pNewRet));

        // rerouting the original branch target to our probe
        branch->m_pTarget = probeStart;

        if (branch->m_opcode == CEE_BR || branch->m_opcode == CEE_BR_S)
            continue; // the branch is unconditional so we will always go to the target; no need for the skipping branch

        // adding unconditional branch to skip the inserted instructions in case the branch fails
        ILInstr* skipBranch = pilr->NewILInstr();
        skipBranch->m_opcode = CEE_BR;
        skipBranch->m_pTarget = pNewRet->m_pNext;
        pilr->InsertAfter(branch, skipBranch);
    }

    IfFailRet(AddEnterProbe(&rewriter, enterMethod->addr, enterMethod->getSig(), methodId));

    if (isMain) {
        LOG(tout << "rewritten main method: ");
        PrintILInstructions(pilr);
    }

    IfFailRet(rewriter.Export());

    return S_OK;
}
