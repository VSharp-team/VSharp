#include "reflection.h"
#include "sigparse.h"
#include <cstdlib>
#include <vector>
#include <sstream>
#include <stack>
#include <algorithm>

using namespace vsharp;

#undef IfFailRet
#define IfFailRet(EXPR) do { HRESULT hr = (EXPR); if(FAILED(hr)) { return (hr); } } while (0)

#undef IfFailFail
#define IfFailFail(EXPR) do { HRESULT hr = (EXPR); if(FAILED(hr)) { FAIL_LOUD("Bad HRESULT!"); } } while (0)

#undef IfNullRet
#define IfNullRet(EXPR) do { if ((EXPR) == NULL) return E_OUTOFMEMORY; } while (0)

Reflection::Reflection(ICorProfilerInfo8 &profilerInfo)
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
HRESULT Reflection::configure(ModuleID moduleId, mdMethodDef methodDef)
{
    // TODO: should we dispose metadataImport and metadataEmit?
    m_moduleId = moduleId;
    m_jittedToken = methodDef;
    IfFailRet(profilerInfo.GetModuleMetaData(m_moduleId, ofRead | ofWrite, IID_IMetaDataImport2, reinterpret_cast<IUnknown **>(&metadataImport)));
    IfFailRet(metadataImport->QueryInterface(IID_IMetaDataEmit, reinterpret_cast<void **>(&metadataEmit)));
    assert((m_jittedToken & 0xFF000000L) == mdtMethodDef);
    return S_OK;
}
#endif


#include<iostream>
#include <locale>
#include <codecvt>// TODOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOo
char *wStrToCharStr(const WCHAR *wstr) {
    std::u16string u16str(wstr);
    std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> codecvt;
    std::string u8str = codecvt.to_bytes(u16str);
    char *result = new char[u8str.length() + 1];
    u8str.copy(result, u8str.length());
    result[u8str.length()] = '\0';
    return result;
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

class TypeSpecPrinter : public SigParser
{
private:
    std::stringstream _res;
    IMetaDataImport *_import;
    std::stack<int> expectedParams;
    int position = 0;
    char modifier = '\0';
    int modifierPosition = -1;
protected:
    void NotifyBeginType() override {
        if (expectedParams.empty() && position == 0) {
            _res.str(std::string());
            position = 1;
        } else {
            position++;
        }
    }

    void NotifyEndType() override {
        if (!expectedParams.empty()) {
            int &top = expectedParams.top();
            assert(top);
            --top;
            if (top) {
                _res << ",";
            } else {
                _res << "]";
                expectedParams.pop();
            }
        }
        if (modifierPosition == position) {
            _res << modifier;
            modifier = '\0';
            modifierPosition = -1;
        }
        position--;
    }

    void NotifyByref() override {
        // TODO: need that? delete
        // TODO: care about byref
        modifier = '&';
        modifierPosition = position;
    }
    void NotifyVoid() override {
        _res << "System.Void";
    }
    void NotifyCustomMod(sig_elem_type cmod, mdToken token) override {}
    void NotifyTypeSimple(sig_elem_type elem_type) override {
        switch (elem_type) {
            case ELEMENT_TYPE_VOID:
                _res << "System.Void";
                break;
            case ELEMENT_TYPE_BOOLEAN:
                _res << "System.Boolean";
                break;
            case ELEMENT_TYPE_CHAR:
                _res << "System.Char";
                break;
            case ELEMENT_TYPE_I1:
                _res << "System.SByte";
                break;
            case ELEMENT_TYPE_U1:
                _res << "System.Byte";
                break;
            case ELEMENT_TYPE_I2:
                _res << "System.Short";
                break;
            case ELEMENT_TYPE_U2:
                _res << "System.UShort";
                break;
            case ELEMENT_TYPE_I4:
                _res << "System.Int";
                break;
            case ELEMENT_TYPE_U4:
                _res << "System.Uint";
                break;
            case ELEMENT_TYPE_I8:
                _res << "System.Long";
                break;
            case ELEMENT_TYPE_U8:
                _res << "System.ULong";
                break;
            case ELEMENT_TYPE_R4:
                _res << "System.Float";
                break;
            case ELEMENT_TYPE_R8:
                _res << "System.Double";
                break;
            case ELEMENT_TYPE_I:
                _res << "System.IntPtr";
                break;
            case ELEMENT_TYPE_U:
                _res << "System.UIntPtr";
                break;
            case ELEMENT_TYPE_STRING:
                _res << "System.String";
                break;
            case ELEMENT_TYPE_OBJECT:
                _res << "System.Object";
                break;
            default:
                FAIL_LOUD("unexpected primitive type");
                break;
        }
    }
    void NotifyTypeDefOrRef(mdToken token) override {
        switch (token & 0xFF000000L) {
        case mdtTypeDef: {
            LPWSTR name = new WCHAR[0];
            ULONG size;
            DWORD flags;
            mdToken extends;
            IfFailFail(_import->GetTypeDefProps(token, name, 0, &size, &flags, &extends));
            name = new WCHAR[size];
            IfFailFail(_import->GetTypeDefProps(token, name, size, &size, &flags, &extends));
            _res << wStrToCharStr(name); // TODO: remove that #do
            delete[] name;
            break;
        }
        case mdtTypeRef: {
            mdToken resolutionScope;
            LPWSTR name = new WCHAR[0];
            ULONG size;
            IfFailFail(_import->GetTypeRefProps(token, &resolutionScope, name, 0, &size));
            name = new WCHAR[size];
            IfFailFail(_import->GetTypeRefProps(token, &resolutionScope, name, size, &size));
            _res << wStrToCharStr(name); // TODO: remove that #do
            delete[] name;
            break;
        }
        default:
            FAIL_LOUD("Expected typeDef or typeRef");
            break;
        }
    }
    void NotifyTypeGenericInst(sig_elem_type elem_type, mdToken token, sig_mem_number number) override {
        NotifyTypeDefOrRef(token);
//        _res << "`" << number << "[";
        _res << "[";
        expectedParams.push(number);
    }
    void NotifyTypeGenericTypeVariable(sig_mem_number number) override {
        if (number == 0)
            _res << "T";
        else
            _res << "T" << number;
    }
    void NotifyTypeGenericMemberVariable(sig_mem_number number) override {}
    void NotifyTypeValueType() override {}
    void NotifyTypeClass() override {}
    void NotifyTypePointer() override {
        modifier = '*';
        modifierPosition = position;
    }
    void NotifyTypeFunctionPointer() override {
        // TODO: it's temporary implementation, care about function pointer
        _res << "FNPTR ";
    }
    void NotifyTypeArray() override {

    }
    void NotifyTypeSzArray() override {
    }

public:
    TypeSpecPrinter(IMetaDataImport *metadataImport)
        : _import(metadataImport)
    {
    }

    ~TypeSpecPrinter()
    {
        assert(expectedParams.empty());
    }

    std::string parsedTypeName() const {
        return _res.str();
    }
    // TODO: array shape
};

class MethodSigParser : public SigParser
{
private:
    std::vector<uint32_t> types;
    uint32_t retType = 0;
    unsigned m_paramsCount = 0;
    bool m_hasThis = false;
    bool m_retTypeParsing = false;
    bool m_returnsSomething = true;
    bool m_methodWasParsed = false;
    bool m_retTypeExpected = false;
    bool m_parsingType = false;

    void addType(uint32_t type) {
        if (m_retTypeParsing) retType = type;
        else types.push_back(type);
    }

protected:
    void NotifyBeginMethod(sig_elem_type elem_type) override {
        m_methodWasParsed = true;
        // TODO: care about 'this'
        m_hasThis = elem_type & IMAGE_CEE_CS_CALLCONV_HASTHIS;
        if (m_hasThis) types.push_back(0);
    }

    void NotifyEndMethod() override {}

    void NotifyParamCount(sig_count count) override {
        m_paramsCount = count;
    }

    void NotifyBeginRetType() override {m_retTypeParsing = true;}

    void NotifyEndRetType() override {m_retTypeParsing = false;}

    void NotifyBeginParam() override {}
    void NotifyEndParam() override {}
    void NotifySentinal() override {}
    void NotifyGenericParamCount(sig_count count) override {}
    void NotifyBeginType() override {}
    void NotifyEndType() override {}
    void NotifyTypedByref() override {}
    void NotifyByref() override {
        // TODO: care about byref
    }
    void NotifyVoid() override {}
    void NotifyCustomMod(sig_elem_type cmod, mdToken token) override {}
    void NotifyTypeSimple(sig_elem_type  elem_type) override {
        addType(elem_type);
    }
    void NotifyTypeDefOrRef(mdToken token) override {
        addType(token);
    }
    void NotifyTypeGenericInst(sig_elem_type elem_type, mdToken token, sig_mem_number number) override {
        // TODO: care about generics -- use GetTokenFromTypeSpec
        addType(0);
    }
    void NotifyTypeGenericTypeVariable(sig_mem_number number) override {}
    void NotifyTypeGenericMemberVariable(sig_mem_number number) override {}
    void NotifyTypeValueType() override {}
    void NotifyTypeClass() override {}
    void NotifyTypePointer() override {}
    void NotifyTypeFunctionPointer() override {}
    void NotifyTypeArray() override {
        // TODO: care about array
        addType(0);
    }
    void NotifyTypeSzArray() override {
        // TODO: care about array
        addType(0);
    }

public:
    void validateEnd() {
        assert(m_methodWasParsed);
    }

    std::vector<mdToken> getTypes() {
        if (retType != 0) types.push_back(retType);
        return types;
    }
    unsigned paramsCount() const { return m_paramsCount; }
    bool hasThis() const { return m_hasThis; }
    bool returnsSomething() const { return m_returnsSomething; }
};

class FieldTypeParser : public SigParser
{
private:
    sig_byte *typeStart;
    sig_byte *typeEnd;
    bool m_typeParsed = false;
    IMetaDataEmit *metadataEmit;

protected:

    void NotifyBeginType() {
        typeStart = this->pbCur;
    };

    void NotifyEndType() {
        typeEnd = this->pbCur;
        m_typeParsed = true;
    };

public:

    FieldTypeParser(IMetaDataEmit *emit)
        : metadataEmit(emit)
    {
    }
    mdToken fieldType() {
        assert(m_typeParsed);
        long typeSize = typeEnd - typeStart;
        assert(typeSize <= sizeof(mdToken));
        mdToken type;
        metadataEmit->GetTokenFromTypeSpec(typeStart, typeSize, &type);
        return type;
    }
};

class MethodParamTypeParser : public SigParser
{
private:
    sig_byte *typeStart;
    sig_byte *typeEnd;
    bool m_typeParseStarted = false;
    bool m_paramParsed = false;
    bool m_typeParsed = false;
    bool m_hasThis = false;
    IMetaDataEmit *metadataEmit;
    mdToken declaringType;
    int parameterIndex;
    int currentParam = 0;

protected:

    void NotifyBeginMethod(sig_elem_type elem_type) {
        if (elem_type == SIG_HASTHIS) {
            m_hasThis = true;
            currentParam++;
            if (parameterIndex == 0) {
                m_typeParseStarted = true;
                m_paramParsed = true;
            }
        }
    }

    void NotifyBeginParam() {
        if (currentParam == parameterIndex) {
            m_typeParseStarted = true;
        }
        currentParam++;

    }
    void NotifyEndParam() {
        if (m_typeParseStarted)
            m_paramParsed = true;
    }

    void NotifyBeginType() {
        if (m_typeParseStarted && !m_paramParsed)
            typeStart = this->pbCur;
    };

    void NotifyEndType() {
        if (m_typeParseStarted && !m_paramParsed) {
            typeEnd = this->pbCur;
            m_typeParsed = true;
        }
    };

public:

    MethodParamTypeParser(IMetaDataEmit *emit, int idx, mdToken declaringType)
        : metadataEmit(emit)
        , parameterIndex(idx)
        , declaringType(declaringType)
    {
    }

    mdToken paramType() {
        assert(m_typeParseStarted && m_typeParsed);
        if (m_hasThis && parameterIndex == 0)
            return declaringType;
        long typeSize = typeEnd - typeStart;
        assert(typeSize <= sizeof(mdToken));
        mdToken type;
        metadataEmit->GetTokenFromTypeSpec(typeStart, typeSize, &type);
        return type;
    }
};

class LocalTypeParser : public SigParser
{
private:
    sig_byte *typeStart;
    sig_byte *typeEnd;
    bool m_typeParseStarted = false;
    bool m_localParsed = false;
    bool m_typeParsed = false;
    IMetaDataEmit *metadataEmit;
    int localIndex;
    int currentLocal = 0;

protected:

    void NotifyBeginLocal() {
        if (currentLocal == localIndex) {
            m_typeParseStarted = true;
        }
        currentLocal++;
    }

    void NotifyEndLocal() {
        if (m_typeParseStarted)
            m_localParsed = true;
    }

    void NotifyBeginType() {
        if (m_typeParseStarted && !m_localParsed)
            typeStart = this->pbCur;
    };

    void NotifyEndType() {
        if (m_typeParseStarted && !m_localParsed) {
            typeEnd = this->pbCur;
            m_typeParsed = true;
        }
    };

public:

    LocalTypeParser(IMetaDataEmit *emit, int idx)
        : metadataEmit(emit)
        , localIndex(idx)
    {
    }

    mdToken localType() {
        assert(m_typeParseStarted && m_typeParsed);
        long typeSize = typeEnd - typeStart;
        assert(typeSize <= sizeof(mdToken));
        mdToken type;
        HRESULT hr = metadataEmit->GetTokenFromTypeSpec(typeStart, typeSize, &type);
        if (FAILED(hr)) FAIL_LOUD("getting local type: GetTokenFromTypeSpec failed!");
        return type;
    }
};

// NOTE: returns typeRef or typeDef
mdToken Reflection::getStringTypeToken() const {
    std::wstring_convert<std::codecvt_utf8_utf16<char16_t>,char16_t> convert;

    // TODO: create function that gives info about module and assembly and cache results (use it in instrumenter.cpp) #do
    LPCBYTE baseLoadAddress;
    ULONG moduleNameLength;
    AssemblyID assembly;
    if FAILED(profilerInfo.GetModuleInfo(m_moduleId, &baseLoadAddress, 0, &moduleNameLength, nullptr, &assembly)) FAIL_LOUD("failed to get module info");
    WCHAR *moduleName = new WCHAR[moduleNameLength];
    IfFailRet(profilerInfo.GetModuleInfo(m_moduleId, &baseLoadAddress, moduleNameLength, &moduleNameLength, moduleName, &assembly));
    ULONG assemblyNameLength;
    AppDomainID appDomainId;
    ModuleID startModuleId;
    IfFailRet(profilerInfo.GetAssemblyInfo(assembly, 0, &assemblyNameLength, nullptr, &appDomainId, &startModuleId));
    WCHAR *assemblyName = new WCHAR[assemblyNameLength];
    IfFailRet(profilerInfo.GetAssemblyInfo(assembly, assemblyNameLength, &assemblyNameLength, assemblyName, &appDomainId, &startModuleId));

    if (convert.to_bytes(assemblyName) == "System.Private.CoreLib")
    {
        auto name = convert.from_bytes("System.String");
        mdTypeDef stringDef;
        if FAILED(metadataImport->FindTypeDefByName(name.data(), NULL, &stringDef)) FAIL_LOUD("failed to find string typeRef");
        return stringDef;
    } else {
        ULONG count;
        HCORENUM num = nullptr;
        mdAssemblyRef assembly;
        CComPtr<IMetaDataAssemblyImport> metadataAssemblyImport;
        if FAILED(metadataImport->QueryInterface(IID_IMetaDataAssemblyImport, reinterpret_cast<void **>(&metadataAssemblyImport))) FAIL_LOUD("failed to import assembly metadata")
        while (true) {
            metadataAssemblyImport->EnumAssemblyRefs(&num, &assembly, 1, &count);
            if (count == 0) break;
            ULONG nameLength;
            const void *publicKey = nullptr;
            ULONG publicKeyCount;
            ASSEMBLYMETADATA* assemblymetadata = new ASSEMBLYMETADATA();
            const void *hash = nullptr;
            ULONG hashCount;
            DWORD flags;
            if FAILED(metadataAssemblyImport->GetAssemblyRefProps(assembly, &publicKey, &publicKeyCount, new WCHAR[0], 0, &nameLength, assemblymetadata, &hash, &hashCount, &flags)) FAIL_LOUD("failed to get properties of assemblyRef");
            WCHAR *assemblyRefName = new WCHAR[nameLength];
            metadataAssemblyImport->GetAssemblyRefProps(assembly, &publicKey, &publicKeyCount, assemblyRefName, nameLength, &nameLength, assemblymetadata, &hash, &hashCount, &flags);
            if (convert.to_bytes(assemblyRefName) == "System.Runtime") {
                mdTypeRef stringRef;
                auto name = convert.from_bytes("System.String");
                if FAILED(metadataImport->FindTypeRef(assembly, name.data(), &stringRef)) FAIL_LOUD("failed to find string typeRef");
                return stringRef;
            }
            delete[] assemblyRefName;
        }
        FAIL_LOUD("failed to find 'System.Runtime' assembly reference");
    }
}

std::vector<mdToken> Reflection::getTypeInfoFromSignature(PCCOR_SIGNATURE pvSigBlob, ULONG cbSigBlob) const {
    MethodSigParser sigParser;
    sigParser.Parse((sig_byte *) pvSigBlob, cbSigBlob);
    sigParser.validateEnd();
    auto types = sigParser.getTypes();
    if (std::find(types.begin(), types.end(), ELEMENT_TYPE_STRING) != types.end()) {
        mdToken stringRef = getStringTypeToken();
        std::replace(types.begin(), types.end(), (unsigned) ELEMENT_TYPE_STRING, stringRef);
    }
    return types;
}

mdToken Reflection::getTypeRefByName(WCHAR *typeName) const {
    HCORENUM num = nullptr;
    mdTypeRef typeRef;
    ULONG count;
    while (true) {
        metadataImport->EnumTypeRefs(&num, &typeRef, 1, &count);
        if (count == 0) break;
        mdToken resolutionScope;
        WCHAR *typeRefName = new WCHAR[0];
        ULONG nameSize;
        metadataImport->GetTypeRefProps(typeRef, &resolutionScope, typeRefName, 0, &nameSize);
        typeRefName = new WCHAR[nameSize];
        metadataImport->GetTypeRefProps(typeRef, &resolutionScope, typeRefName, nameSize, &nameSize);
        if (!memcmp(typeName, typeRefName, nameSize * sizeof(WCHAR))) {
            LOG(tout << "resolved typeRef = " << typeRef);
            return typeRef;
        }
        delete[] typeRefName;
    }
    FAIL_LOUD("typeRef not found");
}

// TODO: array type may not be in typeSpecs, so need to emit it (test StringOfConcreteCharArray)
mdToken Reflection::getTypeSpecByName(WCHAR *typeName) const {
    TypeSpecPrinter *typeSpecPrinter = new TypeSpecPrinter(metadataImport);
    std::wstring_convert<std::codecvt_utf8_utf16<char16_t>,char16_t> convert;
    HCORENUM num = nullptr;
    mdTypeSpec typeSpec;
    ULONG count;
    while (true) {
        IfFailFail(metadataImport->EnumTypeSpecs(&num, &typeSpec, 1, &count));
        if (count == 0) break;
        PCCOR_SIGNATURE signature;
        ULONG count;
        IfFailFail(metadataImport->GetTypeSpecFromToken(typeSpec, &signature, &count));
        typeSpecPrinter->ParseType((sig_byte *)signature, count);
        std::string resultTypeName = typeSpecPrinter->parsedTypeName();
        std::u16string b = convert.from_bytes(resultTypeName);
        size_t length = std::char_traits<char16_t>::length(typeName);
        if (!memcmp(typeName, b.c_str(), length * sizeof(WCHAR))) {
            LOG(tout << "resolved typeSpec = " << typeSpec);
            return typeSpec;
        }
    }
    delete typeSpecPrinter;
    FAIL_LOUD("typeSpec not found");
}

std::vector<mdToken> Reflection::getTypeInfoFromMethod(mdToken method) const {
    LPWSTR      szMethod = nullptr;
    ULONG       chMethod;
    PCCOR_SIGNATURE pvSigBlob;
    ULONG       cbSigBlob;

    unsigned tokenType = method & 4278190080L;
    bool isMethodDef  = tokenType == mdtMethodDef;
    bool isMemberRef  = tokenType == mdtMemberRef;
    bool isMethodSpec = tokenType == mdtMethodSpec;
    assert(isMethodDef || isMemberRef || isMethodSpec);
    if (isMethodDef) {
        ULONG       ulCodeRVA;
        DWORD       dwImplFlags;
        DWORD       dwAttr;
        mdToken     declaringClass;
        ModuleID module;
//        profilerInfo.GetDynamicFunctionInfo(m_functionId, &module, &pvSigBlob, &cbSigBlob, 0, &chMethod, szMethod);
        metadataImport->GetMethodProps(method, &declaringClass, szMethod, 0, &chMethod, &dwAttr, &pvSigBlob, &cbSigBlob, &ulCodeRVA, &dwImplFlags);
    } else if (isMemberRef) {
        mdToken declaringClass;
        metadataImport->GetMemberRefProps(method, &declaringClass, szMethod, 0, &chMethod, &pvSigBlob, &cbSigBlob);
    } else {
        // TODO: implement for methodSpec
        FAIL_LOUD("method is not methodDef or memberRef!");
    }
    return getTypeInfoFromSignature(pvSigBlob, cbSigBlob);
}

mdToken Reflection::getTypeTokenFromFieldRef(mdToken fieldRef) const {
    mdToken declaringType;
    ULONG chMember;
    PCCOR_SIGNATURE signature;
    ULONG count;
    HRESULT hr = metadataImport->GetMemberRefProps(fieldRef, &declaringType, new WCHAR[0], 0, &chMember, &signature, &count);
    if (FAILED(hr)) FAIL_LOUD("getTypeTokenFromFieldRef: GetMemberRefProps failed!");
    FieldTypeParser *fieldTypeParser = new FieldTypeParser(metadataEmit);
    if (!fieldTypeParser->Parse((sig_byte *)signature, count)) FAIL_LOUD("getTypeTokenFromFieldRef: ParseField failed!");
    mdToken fieldType = fieldTypeParser->fieldType();
    delete fieldTypeParser;
    return fieldType;
}

mdToken Reflection::getTypeTokenFromFieldDef(mdToken fieldDef) const {
    mdToken declaringType;
    ULONG chField;
    DWORD dwAttr;
    PCCOR_SIGNATURE signature;
    ULONG count;
    DWORD dwCPlusTypeFlag;
    UVCP_CONSTANT constant;
    ULONG cchValue;
    HRESULT hr = metadataImport->GetFieldProps(fieldDef, &declaringType, new WCHAR[0], 0, &chField, &dwAttr, &signature, &count, &dwCPlusTypeFlag, &constant, &cchValue);
    if (FAILED(hr)) FAIL_LOUD("getTypeTokenFromFieldDef: GetFieldProps failed!");
    FieldTypeParser *fieldTypeParser = new FieldTypeParser(metadataEmit);
    if (!fieldTypeParser->Parse((sig_byte *)signature, count)) FAIL_LOUD("getTypeTokenFromFieldDef: ParseField failed!");
    mdToken fieldType = fieldTypeParser->fieldType();
    delete fieldTypeParser;
    return fieldType;
}

mdToken Reflection::getTypeTokenFromParameter(mdToken method, INT32 argIndex) const {
    mdToken declaringType;
    ULONG chMember;
    DWORD dwAttr;
    PCCOR_SIGNATURE signature;
    ULONG count;
    ULONG ulCodeRVA;
    DWORD dwImplFlags;
    HRESULT hr = metadataImport->GetMethodProps(method, &declaringType, new WCHAR[0], 0, &chMember, &dwAttr, &signature, &count, &ulCodeRVA, &dwImplFlags);
    if (FAILED(hr)) FAIL_LOUD("getTypeTokenFromParameter: GetMethodProps failed!");
    MethodParamTypeParser *methodParamTypeParser = new MethodParamTypeParser(metadataEmit, argIndex, declaringType);
    if (!methodParamTypeParser->Parse((sig_byte *)signature, count)) FAIL_LOUD("getTypeTokenFromParameter: parsing method failed!");
    mdToken paramType = methodParamTypeParser->paramType();
    delete methodParamTypeParser;
    return paramType;
}

mdToken Reflection::getTypeTokenFromLocal(mdToken localsToken, INT32 localIndex) const {
    PCCOR_SIGNATURE signature;
    ULONG count;
    HRESULT hr = metadataImport->GetSigFromToken(localsToken, &signature, &count);
    if (FAILED(hr)) FAIL_LOUD("getTypeTokenFromLocal: GetSigFromToken failed!");
    LocalTypeParser *localTypeParser = new LocalTypeParser(metadataEmit, localIndex);
    if (!localTypeParser->Parse((sig_byte *)signature, count)) FAIL_LOUD("getTypeTokenFromLocal: ParseLocals failed!");
    mdToken localType = localTypeParser->localType();
    delete localTypeParser;
    return localType;
}
