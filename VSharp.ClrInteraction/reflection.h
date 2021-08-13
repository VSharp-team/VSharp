#ifndef REFLECTION_H
#define REFLECTION_H

#include <corprof.h>
#include <string>
#include <memory/heap.h>
#include "cComPtr.h"

using namespace icsharp;


class ObjectBuilder {
    ICorProfilerInfo9 &info;
//    CComPtr<IMetaDataImport> metadata;
    ClassID *typeArgs;

public:
    ObjectBuilder(ICorProfilerInfo9 &info);
    void build(ObjectID objectId, ClassID classId);
    void AllocateArray(ObjectID objectId, CorElementType baseElemType, ClassID baseClassId, ULONG rank);
    void AllocateString(ObjectID objectId, ULONG lengthOffset, ULONG contentsOffset);
    void CreateFields(ObjectID objectId, ClassID classId, ULONG fieldsCount, ULONG bytesCount, COR_FIELD_OFFSET fieldOffsets[], ULONG fieldSizes[], Block *fieldStructs[]);
    Block *CreateBlock(ObjectID objectId, ClassID classId, ULONG fieldsCount, ULONG bytesCount);
    void getSizeAndBlock(ObjectID objectId, ClassID classId, ULONG &size, Block *&block);
    void AllocateClass(ObjectID objectId, ClassID classId, ULONG fieldsCount, ULONG bytesCount);
    std::string GetTypeName(mdTypeDef type, ModuleID module) const;
};

class Reflection {
public:
    static ULONG SizeOfPrimitiveType(CorElementType elem_type);
    static ULONG SizeOfCorElementType(ICorProfilerInfo9 &info, ModuleID moduleId, PCCOR_SIGNATURE signature, ULONG signatureSize);
    static ULONG SizeOfCorElementType(ICorProfilerInfo9 &info, CorElementType elementType, ClassID classId);

    static void GetSizeAndBlockFromSignature(ICorProfilerInfo9 &info, ModuleID moduleId, PCCOR_SIGNATURE signature, ULONG signatureSize, ObjectID objectId, const ClassID genericTypeArgs[], ULONG size, Block *&block);
};


#endif //_REFLECTION_H
