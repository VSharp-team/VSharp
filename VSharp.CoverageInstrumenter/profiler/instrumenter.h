#ifndef INSTRUMENTER_H_
#define INSTRUMENTER_H_

#include "ILRewriter.h"
#include <set>
#include <map>

namespace vsharp {

extern std::set<std::pair<FunctionID, ModuleID>> instrumentedMethods;

class Instrumenter {
private:
    ICorProfilerInfo8 &m_profilerInfo;  // Does not have ownership

    mdMethodDef m_jittedToken;
    ModuleID m_moduleId;

    char *m_signatureTokens;
    unsigned m_signatureTokensLength;
    std::mutex mutex;
    HRESULT doInstrumentation(ModuleID oldModuleId, size_t methodId, const WCHAR *moduleName, ULONG moduleNameLength);

public:
    explicit Instrumenter(ICorProfilerInfo8 &profilerInfo);
    ~Instrumenter();

    HRESULT instrument(FunctionID functionId, std::string methodName);
};

bool IsMain(const WCHAR *moduleName, int moduleSize, mdMethodDef method);
bool InstrumentationIsNeeded(const WCHAR* assemblyName, int assemblySize, const WCHAR *moduleName, int moduleSize, mdMethodDef method);

}

#endif // INSTRUMENTER_H_
