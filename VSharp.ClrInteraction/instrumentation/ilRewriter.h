#ifndef ILREWRITER_H_
#define ILREWRITER_H_

#include "logging.h"
#include "cor.h"
#include "corprof.h"

struct COR_ILMETHOD_SECT_EH;
struct IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT;

namespace icsharp {

struct ILInstr
{
    ILInstr *       m_pNext;
    ILInstr *       m_pPrev;

    unsigned        m_opcode;
    unsigned        m_offset;

    union
    {
        ILInstr *   m_pTarget;
        INT8        m_Arg8;
        INT16       m_Arg16;
        INT32       m_Arg32;
        INT64       m_Arg64;
    };
};

struct EHClause
{
    CorExceptionFlag            m_Flags;
    ILInstr *                   m_pTryBegin;
    ILInstr *                   m_pTryEnd;
    ILInstr *                   m_pHandlerBegin;    // First instruction inside the handler
    ILInstr *                   m_pHandlerEnd;      // Last instruction inside the handler
    union
    {
        DWORD                   m_ClassToken;   // use for type-based exception handlers
        ILInstr *               m_pFilter;      // use for filter-based exception handlers (COR_ILEXCEPTION_CLAUSE_FILTER is set)
    };
};

class ILRewriter
{
private:
    ICorProfilerInfo * m_pICorProfilerInfo;
    ICorProfilerFunctionControl * m_pICorProfilerFunctionControl;

    ModuleID    m_moduleId;
    mdToken     m_tkMethod;

    mdToken     m_tkLocalVarSig;
    unsigned    m_maxStack;
    unsigned    m_flags;
    bool        m_fGenerateTinyHeader;

    ILInstr m_IL; // Double linked list of all il instructions

    unsigned    m_nEH;
#ifdef INSTRUMENTATION
    EHClause *  m_pEH;
#else
    IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT *m_pEH;
#endif

    // Helper table for importing.  Sparse array that maps BYTE offset of beginning of an
    // instruction to that instruction's ILInstr*.  BYTE offsets that don't correspond
    // to the beginning of an instruction are mapped to NULL.
    ILInstr **  m_pOffsetToInstr;
    char *m_Code;
    unsigned    m_CodeSize;

    unsigned    m_nInstrs;

    BYTE *      m_pOutputBuffer;

    IMethodMalloc * m_pIMethodMalloc;

public:
    ILRewriter(ICorProfilerInfo * pICorProfilerInfo, ICorProfilerFunctionControl * pICorProfilerFunctionControl, ModuleID moduleID, mdToken tkMethod);
    ~ILRewriter();

    HRESULT Import();
    HRESULT ImportIL(LPCBYTE pIL);
    HRESULT ImportEH(const COR_ILMETHOD_SECT_EH* pILEH, unsigned nEH);

    unsigned CodeSize() const;
    char *Code() const;

    unsigned EHCount() const;
#ifndef INSTRUMENTATION
    IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT *EHs() const;
#else
    EHClause *EHs() const;
#endif


    ILInstr* NewILInstr();
    ILInstr* CopyILInstr(ILInstr *instr);
    ILInstr* GetInstrFromOffset(unsigned offset);
    unsigned MaxStackSize() const;

    void InsertBefore(ILInstr * pWhere, ILInstr * pWhat);
    void InsertAfter(ILInstr * pWhere, ILInstr * pWhat);
    void AdjustState(ILInstr * pNewInstr);
    unsigned GetInstructionsCount() const;
    bool isLastEHInstr(ILInstr * pInstr) const;
    ILInstr * GetILList();

    HRESULT Export();
    HRESULT Export(char *bytecode, unsigned codeLength, unsigned maxStackSize, char *ehs, unsigned ehsLength);

    HRESULT SetILFunctionBody(unsigned size, LPBYTE pBody);
    LPBYTE AllocateILMemory(unsigned size);
    void DeallocateILMemory(LPBYTE pBody);
};

}

#endif // ILREWRITER_H_
