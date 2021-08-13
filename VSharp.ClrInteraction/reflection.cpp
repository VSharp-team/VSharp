#include <cor.h>
#include "reflection.h"
#include "sigparse.h"
#include "logging.h"
#include "memory/memory.h"

using namespace icsharp;

const ULONG referenceSize = sizeof(INT_PTR);

ULONG Reflection::SizeOfPrimitiveType(CorElementType elem_type) {
    switch (elem_type) {
        case ELEMENT_TYPE_BOOLEAN:
        case ELEMENT_TYPE_I1:
        case ELEMENT_TYPE_U1:
            return 1;
        case ELEMENT_TYPE_CHAR:
        case ELEMENT_TYPE_U2:
        case ELEMENT_TYPE_I2:
            return 2;
        case ELEMENT_TYPE_I4:
        case ELEMENT_TYPE_U4:
        case ELEMENT_TYPE_R4:
            return 4;
        case ELEMENT_TYPE_I8:
        case ELEMENT_TYPE_U8:
        case ELEMENT_TYPE_R8:
            return 8;
        case ELEMENT_TYPE_I:
        case ELEMENT_TYPE_U:
        case ELEMENT_TYPE_STRING:
        case ELEMENT_TYPE_OBJECT:
            return referenceSize;
        default: FAIL_LOUD("unexpected simple type!");
    }
}

// NOTE: returns CorElementType and classId if possible
class StructureTypeParser : public SigParser {
private:
    ICorProfilerInfo9 &info;
    ModuleID moduleId;
    CComPtr<IMetaDataImport> metadata;
    const ClassID *typeArgs;
    bool foundType = false;
    // TODO: use for generic type inst typeArgs (GetClassFromTokenAndTypeArgs)
    ClassID classId;
    ULONG size = 0;
    Block *block = nullptr;
    ObjectID objectId;
    bool isClass = false;

protected:
    void NotifyBeginType() override {};
    void NotifyEndType() override {};

    void gotType(ULONG s, Block *b) {
        if (!foundType) {
            foundType = true;
            size = s;
            block = b;
        }
    }

    void NotifyTypedByref() override {
        gotType(referenceSize, nullptr);
    }

    // the type has the 'byref' modifier on it -- this normally proceeds the type definition in the context
    // the type is used, so for instance a parameter might have the byref modifier on it
    // so this happens before the BeginType in that context
    void NotifyByref() override {
        gotType(referenceSize, nullptr);
    }

    // the type is "VOID" (this has limited uses, function returns and void pointer)
    void NotifyVoid() override {
        FAIL_LOUD("Unexpected type");
    }

    // the type has the indicated custom modifiers (which can be optional or required)
    // NOTE: it can be created only from managed c++, not from c#
    void NotifyCustomMod(sig_elem_type cmod, mdToken token) override {
    }

    // the type is a simple type, the elem_type defines it fully
    void NotifyTypeSimple(sig_elem_type elem_type) override {
        gotType(Reflection::SizeOfPrimitiveType((CorElementType) elem_type), nullptr);
    }

    // the type is specified by the given token of the given token type (normally a type token in the type metadata)
    // this callback is normally qualified by other ones such as NotifyTypeClass or NotifyTypeValueType
    void NotifyTypeDefOrRef(mdToken token) override {
        if (!foundType) {
            ClassID classId;
            if (isClass)
                gotType(referenceSize, nullptr);
            else {
                // TODO: mb need to resolve typeRef and get classId #do
                if (FAILED(info.GetClassFromToken(moduleId, token, &classId)))
                    FAIL_LOUD("Parsing type: getting classId from token failed!");
                auto *objectBuilder = new ObjectBuilder(info);
                ULONG count;
                if (FAILED(info.GetClassLayout(classId, new COR_FIELD_OFFSET[0], 0, &count, &size)))
                    FAIL_LOUD("Getting class layout failed!");
                block = objectBuilder->CreateBlock(objectId, classId, count, size);
            }
        }
    }

    // the type is an instance of a generic
    // elem_type indicates value_type or class
    // indexType and index indicate the metadata for the type in question
    // number indicates the number of type specifications for the generic types that will follow
    void NotifyTypeGenericInst(sig_elem_type elem_type, mdToken token, sig_mem_number number) override {
        if (!foundType) {
            // TODO: do! #do
//            NotifyTypeDefOrRef(token);
            foundType = true;
        }
    }

    // the type is the type of the nth generic type parameter for the class
    void NotifyTypeGenericTypeVariable(sig_mem_number number) override {
        if (!foundType) {
            ClassID c = typeArgs[number];
            auto *objBuilder = new ObjectBuilder(info);
            objBuilder->getSizeAndBlock(objectId, c, size, block);
            delete objBuilder;
            foundType = true;
        }
    }

    // the type is the type of the nth generic type parameter for the member
    void NotifyTypeGenericMemberVariable(sig_mem_number number) override {
        if (!foundType)
            FAIL_LOUD("unexpected generic member variable!");
    }

    // the type will be a value type
    void NotifyTypeValueType() override {
        if (!foundType) isClass = false;
    }

    // the type will be a class
    void NotifyTypeClass() override {
        if (!foundType) isClass = true;
    }

    // the type is a pointer to a type (nested type notifications follow)
    void NotifyTypePointer() override {
        gotType(referenceSize, nullptr);
    }

    // the type is a function pointer, followed by the type of the function
    void NotifyTypeFunctionPointer() override {
        gotType(referenceSize, nullptr);
    }

    // the type is an array, this is followed by the array shape, see above, as well as modifiers and element type
    void NotifyTypeArray() override {
        gotType(referenceSize, nullptr);
    }

    // the type is a simple zero-based array, this has no shape but does have custom modifiers and element type
    void NotifyTypeSzArray() override {
        gotType(referenceSize, nullptr);
    }

public:

    StructureTypeParser(ICorProfilerInfo9 &profilerInfo, ModuleID module, const ClassID genericTypeArgs[], ObjectID objectId)
        : info(profilerInfo), moduleId(module), typeArgs(genericTypeArgs), objectId(objectId)
    {
        info.GetModuleMetaData(moduleId, ofRead | ofWrite, IID_IMetaDataImport, reinterpret_cast<IUnknown **>(&metadata));
    }

    void validateEnd(bool parseResult) const {
        if (!(foundType && parseResult && size > 0))
            printf("");
        assert(foundType && parseResult && size > 0);
    }

    void getSizeAndBlock(ULONG &s, Block *&b) const {
        s = size;
        b = block;
    };
};

void Reflection::GetSizeAndBlockFromSignature(ICorProfilerInfo9 &info, ModuleID moduleId, PCCOR_SIGNATURE signature, ULONG signatureSize, ObjectID objectId, const ClassID genericTypeArgs[], ULONG size, Block *&block) {
    StructureTypeParser parser(info, moduleId, genericTypeArgs, objectId);
    parser.validateEnd(parser.Parse((sig_byte*)signature, signatureSize));
    parser.getSizeAndBlock(size, block);
}

ULONG Reflection::SizeOfCorElementType(ICorProfilerInfo9 &info, CorElementType elementType, ClassID classId) {
    if (CorIsPrimitiveType(elementType))
        return SizeOfPrimitiveType(elementType);
    if (elementType == ELEMENT_TYPE_VALUETYPE) {
        ULONG count = 0;
        ULONG size;
        HRESULT hr = info.GetClassLayout(classId, new COR_FIELD_OFFSET[0], 0, &count, &size);
        if (hr != S_OK) FAIL_LOUD("Getting class layout failed!");
        return size;
    } else {
        return referenceSize;
    }
}

// TODO: need to check: #4Dma
// TODO: if ClassID is primitive -- get size, block = nullptr
// TODO: if ClassID is Struct -- get size of struct, block = create struct block
// TODO: if ClassID is reference type -- size = referenceType, block = nullptr
void ObjectBuilder::getSizeAndBlock(ObjectID objectId, ClassID classId, ULONG &size, Block *&block) {
    HRESULT res;
    CorElementType baseElemType;
    ClassID baseClassId;
//    const wchar_t *str = L"";
//    std:cmp()
    ULONG rank;
    res = info.IsArrayClass(classId, &baseElemType, &baseClassId, &rank);
    if (res == S_OK) {
        block = nullptr;
        size = referenceSize;
    } else {
        ULONG fieldsCount = 0;
        ULONG bytesCount = 0;
        res = info.GetClassLayout(classId, new COR_FIELD_OFFSET[0], 0, &fieldsCount, &bytesCount);
        if (res ==  E_INVALIDARG) {
            ULONG lengthOffset = 0;
            ULONG contentsOffset = 0;
            res = info.GetStringLayout2(&lengthOffset, &contentsOffset);
            if (res ==  E_INVALIDARG) {
                FAIL_LOUD("Allocation unknown object: not array, class or string!\n");
            } else {
                block = nullptr;
                size = referenceSize;
            }
        } else {
            block = CreateBlock(objectId, classId, fieldsCount, bytesCount);
            size = bytesCount;
        }
    }
}

ObjectBuilder::ObjectBuilder(ICorProfilerInfo9 &info)
    : info(info)
{
//    if (FAILED(info.GetModuleMetaData(moduleId, ofRead | ofWrite, IID_IMetaDataImport, reinterpret_cast<IUnknown **>(&metadata))))
//        FAIL_LOUD("Unable to create metadataImport");
}

void ObjectBuilder::build(ObjectID objectId, ClassID classId) {
    HRESULT res;
    CorElementType baseElemType;
    ClassID baseClassId;
    ULONG rank;
    res = info.IsArrayClass(classId, &baseElemType, &baseClassId, &rank);
    if (res == S_OK) AllocateArray(objectId, baseElemType, baseClassId, rank);
    else {
        ULONG fieldsCount = 0;
        ULONG bytesCount = 0;
        res = info.GetClassLayout(classId, new COR_FIELD_OFFSET[0], 0, &fieldsCount, &bytesCount);
        if (res ==  E_INVALIDARG) {
            ULONG lengthOffset = 0;
            ULONG contentsOffset = 0;
            res = info.GetStringLayout2(&lengthOffset, &contentsOffset);
            if (res ==  E_INVALIDARG) {
                FAIL_LOUD("Allocation unknown object: not array, class or string!\n");
            }
            else AllocateString(objectId, lengthOffset, contentsOffset);
        }
        else AllocateClass(objectId, classId, fieldsCount, bytesCount);
    }
}

void ObjectBuilder::AllocateArray(ObjectID objectId, CorElementType baseElemType, ClassID baseClassId, ULONG rank) {
    auto *lengths = new ULONG[rank];
    int *lowerBounds = new int[rank];
    BYTE *p = new BYTE[100];
    if (FAILED(info.GetArrayObjectInfo(objectId, rank, lengths, lowerBounds, &p)))
        FAIL_LOUD("Unable to get array info")
    ULONG count = 1;
    for (int i = 0; i < rank; i++) count *= lengths[i];

    // TODO: create block for every element (pass right object id to createBlock for each element) #do
    ULONG elementSize;
    Block *elementBlock = nullptr;
    if (baseElemType == ELEMENT_TYPE_VALUETYPE) {
        ULONG fieldsCountOfElem = 0;
        ULONG bytesCountOfElem = 0;
        if (FAILED(info.GetClassLayout(baseClassId, new COR_FIELD_OFFSET[0], 0, &fieldsCountOfElem, &bytesCountOfElem)))
            FAIL_LOUD("Unable to get class layout");
        elementBlock = CreateBlock(0, baseClassId, fieldsCountOfElem, bytesCountOfElem);
        elementSize = bytesCountOfElem;
    } else {
        elementSize = Reflection::SizeOfCorElementType(info, baseElemType, baseClassId);
    }
//    mdTypeDef token;
//    ModuleID moduleId;
//    IfFailRet(info.GetClassIDInfo(baseClassId, &moduleId, &token));
//    std::string name = GetTypeName(token, moduleId);
//    if(!name.empty())
//        printf("Allocated array 0x%lu with elements of type %s", objectId, name.c_str());

    ULONG size = 0;
    if (FAILED(info.GetObjectSize(objectId, &size))) FAIL_LOUD("Unable to get object size of array");
    heap.allocateArray(objectId, size, elementSize, elementBlock);
}

void ObjectBuilder::AllocateString(ObjectID objectId, ULONG lengthOffset, ULONG contentsOffset) {
    ULONG size;
    if (FAILED(info.GetObjectSize(objectId, &size))) FAIL_LOUD("Unable to get object size for string");
    // TODO: what is the element size? #do
//    heap.allocateArray(objectId, size, 2);
//    printf("This is string! Length offset = %u, contents offset = %u\n", lengthOffset, contentsOffset);
//    printf("Length = %d\n", *((DWORD *)(objectId + lengthOffset)));
//    printf("Contents: ");
//    for (int i = 0; i < 100; ++i) {
//        char *p = (char*)(objectId + contentsOffset + i);
//        printf("%d", *p);
//    }
//    printf("\n");
}

// TODO: here is only derived class fields, need base class fields! #do
void ObjectBuilder::CreateFields(ObjectID objectId, ClassID classId, ULONG fieldsCount, ULONG bytesCount, COR_FIELD_OFFSET fieldOffsets[], ULONG fieldSizes[], Block *fieldStructs[]) {
    mdTypeDef token;
    ModuleID moduleId;
    ClassID parentClassId;
    ULONG32 cNumTypeArgs;
    auto *typeArgs = new ClassID[0];
    if (FAILED(info.GetClassIDInfo2(classId, &moduleId, &token, &parentClassId, 0, &cNumTypeArgs, typeArgs)))
        FAIL_LOUD("Unable to get class info");
    typeArgs = new ClassID[cNumTypeArgs];
    if (FAILED(info.GetClassIDInfo2(classId, &moduleId, &token, &parentClassId, cNumTypeArgs, &cNumTypeArgs, typeArgs)))
        FAIL_LOUD("Unable to get class info");

    CComPtr<IMetaDataImport> metadataImport;
    if (FAILED(info.GetModuleMetaData(moduleId, ofRead | ofWrite, IID_IMetaDataImport, reinterpret_cast<IUnknown **>(&metadataImport))))
        FAIL_LOUD("Unable to create metadataImport");

    // TODO: check parentClassId to add base class fields #do
//    if (!parentClassId) {
//        CreateFields(objectId)
//    }

    std::string name = GetTypeName(token, moduleId);
//    if(!name.empty())
//        printf("Allocated object 0x%lu of type %s", objectId, name.c_str());

    if (FAILED(info.GetClassLayout(classId, fieldOffsets, fieldsCount, &fieldsCount, &bytesCount)))
        FAIL_LOUD("Unable to get class layout");
    for (int i = 0; i < fieldsCount; ++i) {
        PCCOR_SIGNATURE signature;
        ULONG signatureSize;
        mdTypeDef declaringType;
        ULONG chField, constValueSize;
        DWORD dwAttr, valueTypeFlag;
        UVCP_CONSTANT constValue;
        if (FAILED(metadataImport->GetFieldProps(fieldOffsets[i].ridOfField, &declaringType, nullptr, 0, &chField, &dwAttr, &signature, &signatureSize, &valueTypeFlag, &constValue, &constValueSize)))
            FAIL_LOUD("Failed to get field properties for fieldBlock");

        ULONG size;
        Block *fieldBlock;
        ObjectID fieldObjectId = objectId + fieldOffsets[i].ulOffset;
        // TODO: refactor this #do
        Reflection::GetSizeAndBlockFromSignature(info, moduleId, signature, signatureSize, fieldObjectId, typeArgs, size, fieldBlock);
        fieldSizes[i] = size;
        fieldStructs[i] = fieldBlock;
    }

    delete[] typeArgs;
}

Block *ObjectBuilder::CreateBlock(ObjectID objectId, ClassID classId, ULONG fieldsCount, ULONG bytesCount) {
    HRESULT hr;
    auto *fieldOffsets = new COR_FIELD_OFFSET[fieldsCount];
    auto *fieldSizes = new ULONG[fieldsCount];
    auto *fieldStructs = new Block*[fieldsCount];
    CreateFields(objectId, classId, fieldsCount, bytesCount, fieldOffsets, fieldSizes, fieldStructs);
    return Heap::createBlock(objectId, bytesCount, fieldOffsets, fieldSizes, fieldStructs, fieldsCount);
}

void ObjectBuilder::AllocateClass(ObjectID objectId, ClassID classId, ULONG fieldsCount, ULONG bytesCount) {
    Block *block = CreateBlock(objectId, classId, fieldsCount, bytesCount);
    heap.allocateObject(block);
}

std::string ObjectBuilder::GetTypeName(mdTypeDef type, ModuleID module) const {
    CComPtr<IMetaDataImport> spMetadata;
    if (SUCCEEDED(info.GetModuleMetaData(module, ofRead, IID_IMetaDataImport, reinterpret_cast<IUnknown**>(&spMetadata)))) {
        WCHAR name[256];
        ULONG nameSize = 256;
        DWORD flags;
        mdTypeDef baseType;
        if (SUCCEEDED(spMetadata->GetTypeDefProps(type, name, 256, &nameSize, &flags, &baseType))) {
            std::basic_string<WCHAR> ws(name);
            return std::string(ws.begin(), ws.end());
        }
    }
    return "";
}
