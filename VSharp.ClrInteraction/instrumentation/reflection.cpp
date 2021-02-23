#include "reflection.h"
#include "sigparse.h"
#include <stdlib.h>
#include <vector>
#include <sstream>

using namespace icsharp;

#undef IfFailRet
#define IfFailRet(EXPR) do { HRESULT hr = (EXPR); if(FAILED(hr)) { return (hr); } } while (0)

#undef IfNullRet
#define IfNullRet(EXPR) do { if ((EXPR) == NULL) return E_OUTOFMEMORY; } while (0)

Reflection::Reflection(ICorProfilerInfo9 &profilerInfo)
    : profilerInfo(profilerInfo)
{
}

#ifdef INSTRUMENTATION
HRESULT Reflection::configure(FunctionID functionId, ModuleID &moduleId, mdMethodDef &methodDef)
{
    // TODO: should we dispose metadataImport and metadataEmit?
    HRESULT hr;
    ClassID classId;
    IfFailRet(profilerInfo.GetFunctionInfo(functionId, &classId, &moduleId, &jittedToken));
    IfFailRet(profilerInfo.GetModuleMetaData(moduleId, ofRead | ofWrite, IID_IMetaDataImport, reinterpret_cast<IUnknown **>(&metadataImport)));
    IfFailRet(metadataImport->QueryInterface(IID_IMetaDataEmit, reinterpret_cast<void **>(&this->metadataEmit)));
    assert((jittedToken & 0xFF000000L) == mdtMethodDef);
    methodDef = jittedToken;
    this->moduleId = moduleId;
    return S_OK;
}
#else
HRESULT Reflection::configure(FunctionID functionId, ModuleID &moduleId, mdMethodDef &methodDef, CComPtr<IMetaDataEmit> &metadataEmit)
{
    // TODO: should we dispose metadataImport and metadataEmit?
    HRESULT hr;
    ClassID classId;
    IfFailRet(profilerInfo.GetFunctionInfo(functionId, &classId, &moduleId, &jittedToken));
    IfFailRet(profilerInfo.GetModuleMetaData(moduleId, ofRead | ofWrite, IID_IMetaDataImport, reinterpret_cast<IUnknown **>(&metadataImport)));
    IfFailRet(metadataImport->QueryInterface(IID_IMetaDataEmit, reinterpret_cast<void **>(&metadataEmit)));
    assert((jittedToken & 0xFF000000L) == mdtMethodDef);
    methodDef = jittedToken;
    this->moduleId = moduleId;
    return S_OK;
}
#endif


#include<iostream>
#include <locale>
#include <codecvt>// TODOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOo
char *wStrToCharStr(WCHAR *wstr) {
    std::u16string u16str(wstr);
    std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> codecvt;
    std::string u8str = codecvt.to_bytes(u16str);
    char *result = new char[u8str.length() + 1];
    u8str.copy(result, u8str.length());
    result[u8str.length()] = '\0';
    return result;
}

const char *Reflection::className(mdTypeDef typeToken) const
{
    unsigned tokenType = typeToken & 0xFF000000L;

    HRESULT hr = S_OK;
    LPWSTR szTypeDef = nullptr;
    ULONG chTypeDef;
    DWORD dwTypeDefFlags;
    mdToken tkExtends;
    switch (tokenType) {
    case mdtTypeDef:
        hr = metadataImport->GetTypeDefProps(typeToken, nullptr, 0, &chTypeDef, &dwTypeDefFlags, &tkExtends);
        break;
    case mdtTypeRef:
        hr = metadataImport->GetTypeRefProps(typeToken, &tkExtends, nullptr, 0, &chTypeDef);
        break;
    case mdtModuleRef:
        chTypeDef = 9;
        break;
    case mdtTypeSpec:
        PCCOR_SIGNATURE sig;
        ULONG csig;
        hr = metadataImport->GetTypeSpecFromToken(typeToken, &sig, &csig);
        break;
    default:
        throw std::logic_error("Unexpected class token type!");
    }

    if (FAILED(hr))
        return "<unresolved type>";

    szTypeDef = new WCHAR[chTypeDef];
    switch (tokenType) {
    case mdtTypeDef:
    case mdtTypeSpec:
        hr = metadataImport->GetTypeDefProps(typeToken, szTypeDef, chTypeDef, &chTypeDef, &dwTypeDefFlags, &tkExtends);
        break;
    case mdtTypeRef:
        hr = metadataImport->GetTypeRefProps(typeToken, &tkExtends, szTypeDef, chTypeDef, &chTypeDef);
        break;
    case mdtModuleRef:
        szTypeDef[0] = '<'; szTypeDef[1] = 'm'; szTypeDef[2] = 'o'; szTypeDef[3] = 'd';
        szTypeDef[4] = 'u'; szTypeDef[5] = 'l'; szTypeDef[6] = 'e'; szTypeDef[7] = '>';
        szTypeDef[8] = 0;
    }
    if (FAILED(hr))
        return "<unresolved type>";
    return szTypeDef ? wStrToCharStr(szTypeDef) : "<unresolved type>";
}

const char *Reflection::methodName(mdMethodDef methodToken) const
{
    HRESULT hr = S_OK;
    const char *methodName = "<unknown method>";
    mdTypeDef declaringClass;
    unsigned argsCount = 0;
    bool returnsSomething = false;
    bool hasThis = false;
    bool isInternalCall = false;
    hr = getFunctionInfo(methodToken, methodName, declaringClass, argsCount, returnsSomething, hasThis, isInternalCall);
    if (FAILED(hr)) {
        std::cout << "FAILED TO RESOLVE METHOD: TOKEN " << methodToken << ", current module " << moduleId << ", name of module = " << moduleName(moduleId) << std::endl;
    }
//    std::cout << methodName << " token type: " << (declaringClass & 0xFF000000L) << " (" << (methodDef & 0xFF000000L) << ")" << std::endl;
    const char *declaringType = className(declaringClass);
    const char *rettyp = FAILED(hr) ? "???" : (returnsSomething ? (isInternalCall ? "extern nonvoid" : "nonvoid")  : (isInternalCall ? "extern void" :"void"));
    std::string args = FAILED(hr) ? "???" : std::to_string(argsCount) + std::string(hasThis ? "+1" : "" );

    size_t len = strlen(rettyp) + strlen(methodName) + strlen(declaringType) + strlen(args.c_str()) + 5;
    char *result = new char[len];
    strcpy(result, rettyp);
    result[strlen(rettyp)] = ' ';
    strcpy(result + strlen(rettyp) + 1, declaringType);
    result[strlen(declaringType) + strlen(rettyp) + 1] = '.';
    strcpy(result + strlen(rettyp) + strlen(declaringType) + 2, methodName);
    result[strlen(rettyp) + strlen(methodName) + strlen(declaringType) + 2] = '(';
    strcpy(result + strlen(rettyp) + strlen(methodName) + strlen(declaringType) + 3, args.c_str());
    result[len - 2] = ')';
    result[len - 1] = '\0';
    return result;
}

const char *Reflection::moduleName(ModuleID moduleId) const
{
    LPCBYTE ppBaseLoadAddress;
    AssemblyID assemblyId;
    ULONG cchName;
    profilerInfo.GetModuleInfo(moduleId, &ppBaseLoadAddress, 0, &cchName, nullptr, &assemblyId);
    WCHAR *wModuleName = new WCHAR[cchName];
    profilerInfo.GetModuleInfo(moduleId, &ppBaseLoadAddress, cchName, &cchName, wModuleName, &assemblyId);
    return wStrToCharStr(wModuleName);
}

#ifdef INSTRUMENTATION
mdSignature Reflection::signatureToken(PCCOR_SIGNATURE sig, ULONG size) const
{
    mdSignature signatureToken;
    HRESULT hr = metadataEmit->GetTokenFromSig(sig, size, &signatureToken);
    if (FAILED(hr))
        throw std::logic_error("Failed to get signature token");
    return signatureToken;
}
#endif

class MethodSigParser : public SigParser
{
private:
    unsigned m_paramsCount = 0;
    bool m_hasThis = false;
    bool m_returnsSomething = true;
    bool m_methodWasParsed = false;
    bool m_retTypeExpected = false;

protected:
    void NotifyBeginMethod(sig_elem_type elem_type) override {
        m_methodWasParsed = true;
        m_hasThis = elem_type & IMAGE_CEE_CS_CALLCONV_HASTHIS;
    }

    void NotifyEndMethod() override {}

    void NotifyParamCount(sig_count count) override {
        m_paramsCount = count;
    }

    void NotifyBeginRetType() override {
        m_retTypeExpected = true;
    }

    void NotifyEndRetType() override {
        m_retTypeExpected = false;
    }

    void NotifyBeginParam() override {}
    void NotifyEndParam() override {}
    void NotifySentinal() override {}
    void NotifyGenericParamCount(sig_count count) override {}
    void NotifyBeginType() override {}
    void NotifyEndType() override {m_retTypeExpected = false;}
    void NotifyTypedByref() override {m_retTypeExpected = false;}
    void NotifyByref() override {m_retTypeExpected = false;}
    void NotifyVoid() override {
        if (m_retTypeExpected) m_returnsSomething = false;
        m_retTypeExpected = false;
    }
    void NotifyCustomMod(sig_elem_type cmod, sig_index_type indexType, sig_index index) override {m_retTypeExpected = false;}
    void NotifyTypeSimple(sig_elem_type  elem_type) override {m_retTypeExpected = false;}
    void NotifyTypeDefOrRef(sig_index_type  indexType, int index) override {m_retTypeExpected = false;}
    void NotifyTypeGenericInst(sig_elem_type elem_type, sig_index_type indexType, sig_index index, sig_mem_number number) override {m_retTypeExpected = false;}
    void NotifyTypeGenericTypeVariable(sig_mem_number number) override {m_retTypeExpected = false;}
    void NotifyTypeGenericMemberVariable(sig_mem_number number) override {m_retTypeExpected = false;}
    void NotifyTypeValueType() override {m_retTypeExpected = false;}
    void NotifyTypeClass() override {m_retTypeExpected = false;}
    void NotifyTypePointer() override {m_retTypeExpected = false;}
    void NotifyTypeFunctionPointer() override {m_retTypeExpected = false;}
    void NotifyTypeArray() override {m_retTypeExpected = false;}
    void NotifyTypeSzArray() override {m_retTypeExpected = false;}

public:
    void validateEnd() {
        assert(m_methodWasParsed);
    }

    unsigned paramsCount() const { return m_paramsCount; }
    bool hasThis() const { return m_hasThis; }
    bool returnsSomething() const { return m_returnsSomething; }
};

HRESULT Reflection::isIntrinsic(mdMethodDef methodDef, bool &result) const
{
    result = false;
    HCORENUM enumerator = nullptr;
    mdCustomAttribute attr;
    ULONG count = 0;
    mdToken tkObj;
    mdToken tkType;
    void const *pBlob;
    ULONG cbSize;
    mdTypeDef   declaringType;
    LPWSTR      szMethod = nullptr;
    ULONG       chMethod;
    PCCOR_SIGNATURE pvSigBlob;
    ULONG       cbSigBlob;
    ULONG       ulCodeRVA;
    DWORD       dwImplFlags;
    DWORD       dwAttr;
    const char *intrinsicAttributeTypeName = "System.Runtime.CompilerServices.IntrinsicAttribute";
    while (SUCCEEDED(metadataImport->EnumCustomAttributes(&enumerator, methodDef, 0, &attr, 1, &count)) && count > 0) {
        metadataImport->GetCustomAttributeProps(attr, &tkObj, &tkType, &pBlob, &cbSize);
        bool isMethodDef = (tkType & 4278190080L) == mdtMethodDef;
        assert(isMethodDef);

        IfFailRet(metadataImport->GetMethodProps(tkType, &declaringType, szMethod, 0, &chMethod, &dwAttr, &pvSigBlob, &cbSigBlob, &ulCodeRVA, &dwImplFlags));
        szMethod = new WCHAR[chMethod];
        IfFailRet(metadataImport->GetMethodProps(tkType, &declaringType, szMethod, chMethod, &chMethod, &dwAttr, &pvSigBlob, &cbSigBlob, &ulCodeRVA, &dwImplFlags));

        const char *declaringTypeName = className(declaringType);
        if (!strcmp(declaringTypeName, intrinsicAttributeTypeName)) {
            result = true;
        }
    }
    return S_OK;
}

HRESULT Reflection::getFunctionInfo(mdMethodDef &token,
                                      const char *&name,
                                      mdTypeDef &declaringClass,
                                      unsigned &count,
                                      bool &returnsSomething,
                                      bool &hasThis,
                                      bool &isExtern) const
{
    HRESULT hr = S_OK;
    count = 0;
    declaringClass = 0;
    returnsSomething = false;
    hasThis = false;
    isExtern = false;

//    unsigned tokenType = token & 4278190080L;
    bool isMethodDef;/* = tokenType == mdtMethodDef;*/
    bool isMemberRef;/* = tokenType == mdtMemberRef;*/
    bool isMethodSpec;
//    assert(isMethodDef || isMemberRef);

    do {
        unsigned tokenType = token & 4278190080L;
        isMethodDef  = tokenType == mdtMethodDef;
        isMemberRef  = tokenType == mdtMemberRef;
        isMethodSpec = tokenType == mdtMethodSpec;
        assert(isMethodDef || isMemberRef || isMethodSpec);
        if (!isMethodDef) {
            IfFailRet(resolveMethodRefOrSpec(token, moduleId, declaringClass, token));
        }
    } while (!isMethodDef);

    LPWSTR      szMethod = nullptr;
    ULONG       chMethod;
    PCCOR_SIGNATURE pvSigBlob;
    ULONG       cbSigBlob;
    ULONG       ulCodeRVA;
    DWORD       dwImplFlags;
    assert(isMethodDef);
    DWORD       dwAttr;
    IfFailRet(metadataImport->GetMethodProps(token, &declaringClass, szMethod, 0, &chMethod, &dwAttr, &pvSigBlob, &cbSigBlob, &ulCodeRVA, &dwImplFlags));
    szMethod = new WCHAR[chMethod];
    IfFailRet(metadataImport->GetMethodProps(token, &declaringClass, szMethod, chMethod, &chMethod, &dwAttr, &pvSigBlob, &cbSigBlob, &ulCodeRVA, &dwImplFlags));
//    isStatic = dwAttr & mdStatic;
//    } else if (isMemberRef) {
//        IfFailRet(metadataImport->GetMemberRefProps(token, &declaringClass, szMethod, 0, &chMethod, &pvSigBlob, &cbSigBlob));
//        szMethod = new WCHAR[chMethod];
//        IfFailRet(metadataImport->GetMemberRefProps(token, &declaringClass, szMethod, chMethod, &chMethod, &pvSigBlob, &cbSigBlob));
//        WCHAR *moduleName = new WCHAR[1000];
//        ULONG temp;
//        LPCBYTE ppBaseLoadAddress;
//        AssemblyID assId;
//        profilerInfo.GetModuleInfo(moduleId, &ppBaseLoadAddress, 1000, &temp, moduleName, &assId);

//        AppDomainID pAppDomainId;
//        ModuleID pModuleId;
////        profilerInfo.GetAssemblyInfo(assId, 1000, &temp, moduleName, &pAppDomainId, &pModuleId);
//        std::cout << "METHOD REF!!!!! module = " << moduleId << "; methodRef = " << token << "; module name = " << wStrToCharStr(moduleName) << " " << ppBaseLoadAddress << std::endl;

//        isStatic = false; // TODO wat?
//    }

//    std::cout << "getting function args of " << methodName(methodDef) << " (token " << methodDef << ")" << std::endl;
//    std::cout << "sig: ";
//    for (unsigned i = 0; i < cbSigBlob; ++i) {
//        std::cout << (int)pvSigBlob[i] << " ";
//    }
    MethodSigParser sigParser;
    sigParser.Parse((sig_byte*)pvSigBlob, cbSigBlob);
    sigParser.validateEnd();

//    count = 0;
//    do {
//        ++count;
//        mdParamDef param;
//        mdMethodDef pmd;
//        ULONG       pulSequence;
//        LPWSTR      szName = nullptr;
//        ULONG       pchName;
//        DWORD       pdwAttr;
//        DWORD       pdwCPlusTypeFlag;
//        UVCP_CONSTANT ppValue;
//        ULONG pcchValue;
//        hr = metadataImport->GetParamForMethodIndex(token, count, &param);
//        if (SUCCEEDED(hr)) {
//            IfFailRet(metadataImport->GetParamProps(param, &pmd, &pulSequence, nullptr, 0, &pchName, &pdwAttr, &pdwCPlusTypeFlag, &ppValue, &pcchValue));
//            szName = new WCHAR[pchName];
//            IfFailRet(metadataImport->GetParamProps(param, &pmd, &pulSequence, szName, pchName, &pchName, &pdwAttr, &pdwCPlusTypeFlag, &ppValue, &pcchValue));
////            std::cout << wStrToCharStr(szMethod) << " field name: " << wStrToCharStr(szName) << std::endl;
//        } else break;

//    } while (true);
//    --count;

    count = sigParser.paramsCount();
    name = wStrToCharStr(szMethod);
    hasThis = sigParser.hasThis();
    returnsSomething = sigParser.returnsSomething();

    bool isInternalCall = IsMiInternalCall(dwImplFlags);
    bool intrinsic;
    IfFailRet(isIntrinsic(token, intrinsic));
    isExtern = isInternalCall || intrinsic;

    return S_OK;
}

HRESULT Reflection::getReturnValueInfo(mdMethodDef methodDef, bool &returnsSomething) const
{
    const char *name;
    mdTypeDef declaringClass;
    unsigned argsCount = 0;
    bool hasThis = false;
    bool isInternalCall = false;
    return getFunctionInfo(methodDef, name, declaringClass, argsCount, returnsSomething, hasThis, isInternalCall);
}

void split(const std::string &str, std::vector<std::string> &cont, char delim = ' ')
{
    std::size_t current, previous = 0;
    current = str.find(delim);
    while (current != std::string::npos) {
        cont.push_back(str.substr(previous, current - previous));
        previous = current + 1;
        current = str.find(delim, previous);
    }
    cont.push_back(str.substr(previous, current - previous));
}

// Replace popen and pclose with _popen and _pclose for Windows.
std::string exec(const char* cmd)
{
//    std::cout << "starting " << cmd << std::endl;
    char buffer[128];
    std::string result = "";
    FILE* pipe = popen(cmd, "r");
    if (!pipe) throw std::runtime_error("popen() failed!");
    try {
        while (fgets(buffer, sizeof buffer, pipe) != NULL) {
            result += buffer;
        }
    } catch (...) {
        pclose(pipe);
        throw;
    }
    pclose(pipe);
    return result;
}

HRESULT Reflection::resolveMethodRefOrSpec(mdMemberRef methodRefOrSpec, ModuleID moduleId, mdTypeDef &declaringType, mdMethodDef &methodDef) const
{
    LPCBYTE ppBaseLoadAddress;
    AssemblyID assemblyId;
    ULONG cchName;
    IfFailRet(profilerInfo.GetModuleInfo(moduleId, &ppBaseLoadAddress, 0, &cchName, nullptr, &assemblyId));
    WCHAR *wModuleName = new WCHAR[cchName];
    IfFailRet(profilerInfo.GetModuleInfo(moduleId, &ppBaseLoadAddress, cchName, &cchName, wModuleName, &assemblyId));

    AppDomainID pAppDomainId;
    ModuleID pModuleId;
    IfFailRet(profilerInfo.GetAssemblyInfo(assemblyId, 0, &cchName, nullptr, &pAppDomainId, &pModuleId));
    WCHAR *wAssemblyName = new WCHAR[cchName];
    IfFailRet(profilerInfo.GetAssemblyInfo(assemblyId, cchName, &cchName, wAssemblyName, &pAppDomainId, &pModuleId));

//    LPWSTR      szMethod = nullptr;
//    ULONG       chMethod;
//    PCCOR_SIGNATURE pvSigBlob;
//    ULONG       cbSigBlob;
//    IfFailRet(metadataImport->GetMemberRefProps(methodRef, &declaringType, szMethod, 0, &chMethod, &pvSigBlob, &cbSigBlob));
//    szMethod = new WCHAR[chMethod];
//    IfFailRet(metadataImport->GetMemberRefProps(methodRef, &declaringType, szMethod, chMethod, &chMethod, &pvSigBlob, &cbSigBlob));
//    std::cout << "declaring type of " << wStrToCharStr(szMethod) << ": " <<  declaringType << "(type = " << (declaringType & 0xFF000000L) << ")" << std::endl;


    const char *path = "CORECLR_ENABLE_PROFILING=0 /home/dvvrd/dev/vsharp/VSharp.ReflectionTools/bin/Debug/netcoreapp3.1/VSharp.ReflectionTools";
    const char *moduleName = wStrToCharStr(wModuleName);
    const char *assemblyName = wStrToCharStr(wAssemblyName);
    std::stringstream ss;
    ss << path << " " <<  assemblyName << " --ref2def " << std::to_string(jittedToken) << " " << moduleName << " "<< std::to_string(methodRefOrSpec);
    std::string cmd = ss.str();
    std::string output = exec(cmd.c_str());
//    std::cout << "got output: " << output << std::endl;
    std::stoi(output);
    std::vector<std::string> tokenStrings;
    split(output, tokenStrings);
    switch(tokenStrings.capacity()) {
    case 1:
        methodDef = (ULONG32)std::stoul(tokenStrings[0]);
        break;
    case 2:
        methodDef = (ULONG32)std::stoul(tokenStrings[0]);
        declaringType = (ULONG32)std::stoul(tokenStrings[1]);
        break;
    default:
        return E_UNEXPECTED;
    }

//    std::cout << "parsed: " << methodDef << " " << declaringType << std::endl;
    return S_OK;
}

//HRESULT Reflection::resolveMethodRef(mdMemberRef methodRef, mdModule &module, mdTypeDef &declaringType, mdMethodDef &methodDef)
//{
//    IMetaDataImport *import = nullptr;

//    mdTypeRef typeRef;
//    unsigned int memberRefNameLen = 0;
//    unsigned int memberRefSigLen;
//    LPWSTR      szMethod = nullptr;
//    ULONG       chMethod;
//    PCCOR_SIGNATURE pvSigBlob;
//    ULONG       cbSigBlob;
//    ULONG       ulCodeRVA;
//    DWORD       dwImplFlags;
//    char *memberRefName = new char[1024];
//    IfFailRet(metadataImport->GetMemberRefProps(methodDef, &declaringType, szMethod, 0, &chMethod, &pvSigBlob, &cbSigBlob));
//    szMethod = new WCHAR[chMethod];
//    IfFailRet(metadataImport->GetMemberRefProps(methodDef, &declaringType, szMethod, chMethod, &chMethod, &pvSigBlob, &cbSigBlob));

//    IntPtr ptrMemberRefSig;
//    var hr = metadataImport->GetMemberRefProps(memberRef, out typeRef, memberRefName, (uint)memberRefName.Length, out memberRefNameLen, out ptrMemberRefSig, out memberRefSigLen);

//    var name = new string(memberRefName, 0, (int)memberRefNameLen - 1);

//    uint resScope;
//    uint parentNameLen;
//    hr = import.GetTypeRefProps(typeRef, out resScope, null, 0, out parentNameLen);

//    uint typeDef = mdTokenNil;
//    object newRawScope = null;
//    Guid metaDataImportGuid = new Guid(IID_IMetaDataImport);
//    hr = import.ResolveTypeRef(typeRef, ref metaDataImportGuid, out newRawScope, out typeDef);

//    var newImport = newRawScope as IMetaDataImport;

//    var hEnum = IntPtr.Zero;
//    var methodTokens = new uint[1024];
//    var methodName = new char[1024];
//    uint len;
//    hr = newImport.EnumMethods(ref hEnum, typeDef, methodTokens, (uint)methodTokens.Length, out len);

//    for (var i = 0; i < len; i++)
//    {
//        uint tmpTypeDef;
//        uint attrs;
//        var sig = IntPtr.Zero;
//        uint sigLen;
//        uint rva;
//        uint flags;
//        uint methodNameLen;
//        hr = newImport.GetMethodProps(methodTokens[i], out tmpTypeDef, methodName, (uint)methodName.Length, out methodNameLen, out attrs, out sig, out sigLen, out rva, out flags);

//        var curName = new string(methodName, 0, (int)methodNameLen - 1);

//        if (name == curName)
//        {
//            // found methodDef
//            break;
//        }
//    }
//}
