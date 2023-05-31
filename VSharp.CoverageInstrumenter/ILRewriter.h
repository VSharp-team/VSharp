#ifndef ILREWRITER_H_
#define ILREWRITER_H_

#include "cor.h"
#include "corprof.h"
#include <corhlpr.h>
#include <cassert>
#include <stdexcept>

#include "probes.h"

#undef IfFailRet
#define IfFailRet(EXPR) do { HRESULT hr = (EXPR); if(FAILED(hr)) { return (hr); } } while (0)

#undef IfNullRet
#define IfNullRet(EXPR) do { if ((EXPR) == NULL) return E_OUTOFMEMORY; } while (0)

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

struct ProbeInsertion {
    ILInstr* target;
    ILInstr* parent;
    vsharp::ProbeCall* probe;
    bool isBeforeInstr;
};

class ILRewriter {
private:
    ICorProfilerInfo *m_pICorProfilerInfo;
    ICorProfilerFunctionControl *m_pICorProfilerFunctionControl;

    ModuleID m_moduleId;
    mdToken m_tkMethod;

    mdToken m_tkLocalVarSig;
    unsigned m_maxStack;
    unsigned m_flags;
    bool m_fGenerateTinyHeader;

    ILInstr m_IL; // Double linked list of all il instructions

    // Helper table for importing.  Sparse array that maps BYTE offset of beginning of an
    // instruction to that instruction's ILInstr*.  BYTE offsets that don't correspond
    // to the beginning of an instruction are mapped to NULL.
    ILInstr **m_pOffsetToInstr;
    unsigned m_CodeSize;

    unsigned m_nInstrs;

    BYTE *m_pOutputBuffer;

    IMethodMalloc *m_pIMethodMalloc;

    HRESULT ImportIL(LPCBYTE pIL);
    HRESULT ImportEH(const COR_ILMETHOD_SECT_EH* pILEH, unsigned nEH);
    ILInstr* GetInstrFromOffset(unsigned offset);
    void AdjustState(ILInstr * pNewInstr);
    HRESULT SetILFunctionBody(unsigned size, LPBYTE pBody);
    LPBYTE AllocateILMemory(unsigned size);
    void DeallocateILMemory(LPBYTE pBody);

public:
    unsigned m_nEH;
    EHClause *m_pEH;

    explicit ILRewriter(
        ICorProfilerInfo *pICorProfilerInfo,
        ICorProfilerFunctionControl *pICorProfilerFunctionControl,
        ModuleID moduleID,
        mdToken tkMethod);

    HRESULT Import();
    HRESULT Export();

    ILInstr * GetILList();
    ILInstr* NewILInstr();
    void InsertBefore(ILInstr * pWhere, ILInstr * pWhat);
    void InsertAfter(ILInstr * pWhere, ILInstr * pWhat);
    void PrintEhs();

    ~ILRewriter();
};

HRESULT RewriteIL(
    ICorProfilerInfo * pICorProfilerInfo,
    ICorProfilerFunctionControl * pICorProfilerFunctionControl,
    ModuleID moduleID,
    mdMethodDef methodDef,
    int methodId,
    bool isMain,
    bool rewriteMainOnly);

#endif // ILREWRITER_H_
