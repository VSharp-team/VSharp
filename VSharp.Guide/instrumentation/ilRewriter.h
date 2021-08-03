#ifndef ILREWRITER_H_
#define ILREWRITER_H_

#include <vector>
#include "logging.h"
#include "cor.h"
#include "corprof.h"

namespace icsharp {

struct ILInstr
{
    unsigned        m_opcode;
    unsigned        m_offset; // For export only

    union
    {
        INT8        m_Arg8;
        INT16       m_Arg16;
        INT32       m_Arg32;
        INT64       m_Arg64;
    };
};

class ILRewriter
{
private:
    ICorProfilerInfo * m_pICorProfilerInfo;
    ICorProfilerFunctionControl * m_pICorProfilerFunctionControl;

    ModuleID    m_moduleId;
    mdToken     m_tkMethod;

    std::vector<ILInstr> m_IL;

    unsigned    m_nInstrs;
    BYTE *      m_pOutputBuffer;

    IMethodMalloc * m_pIMethodMalloc;

public:
    ILRewriter(ICorProfilerInfo * pICorProfilerInfo, ICorProfilerFunctionControl * pICorProfilerFunctionControl, ModuleID moduleID, mdToken tkMethod);
    ~ILRewriter();

    void AddInstruction(const ILInstr &instr);

    HRESULT Export();

    HRESULT SetILFunctionBody(unsigned size, LPBYTE pBody);
    LPBYTE AllocateILMemory(unsigned size);
    void DeallocateILMemory(LPBYTE pBody);
};

}

#endif // ILREWRITER_H_
