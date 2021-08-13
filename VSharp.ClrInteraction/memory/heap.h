#ifndef HEAP_H_
#define HEAP_H_

#include <map>
#include <vector>
#include "memory/intervalTree.h"
#include "cor.h"
#include "corprof.h"
#include "corhdr.h"

namespace icsharp {

#define ADDR UINT_PTR
#define SIZE UINT_PTR
#define OBJID UINT_PTR
#define INDEX UINT_PTR

class Shift {
public:
    ADDR oldBase;
    ADDR newBase;
    ADDR move(ADDR addr) const;
};

class Interval {
private:
    bool marked;
    bool flushed;
public:
    ADDR left;
    ADDR right;

    Interval();
    Interval(ADDR leftValue, SIZE size);
    virtual ~Interval();

    bool operator==(const Interval &other) const;

    bool contains(ADDR point) const;
    bool includes(const Interval &other) const;
    bool intersects(const Interval &other) const;

    Interval intersect(const Interval& other) const;

    void move(const Shift &shift);

    virtual std::string toString() const;

    // TODO: move 'marked' and 'flushed' fields to Object class; traverse objects in heap class, instead of IntervalTree
    void mark();
    void unmark();
    bool isMarked() const;
    void flush();
    bool isFlushed() const;

};

enum objectType {
    CLASS,
    STRING,
    ARRAY,
};

class Object : public Interval {
public:
    objectType type;
    Object(ADDR address, SIZE size, objectType typeValue);
    std::string toString() const override;
};

class Block;

typedef std::pair<Interval, Block *> Offsets;
typedef std::map<mdToken, Offsets> FieldOffsets;

// NOTE: may be class or struct
// TODO: record #do
class Block : public Object {
private:
    // [NOTE] Empty concreteness map means fully concrete class
    // [NOTE] mdToken is mdFieldDef or mdMemberRef (for fields of type from another module)
    std::map<mdToken, bool> concreteness;
    FieldOffsets fieldOffsets;
public:
    Block(ADDR address, SIZE size, const FieldOffsets &fields);
    ~Block() override;

    void resolve(SIZE offset, mdToken &field, SIZE &offsetInsideField, Block *&structure) const;
    std::vector<std::tuple<mdToken, Interval, Block *>> resolve(const Interval &interval) const;
    void fieldOffset(const mdToken &field, SIZE &offset, Block *&fieldBlock) const;
    bool isConcrete(mdToken field) const;
    std::string toString() const override;
};

// TODO: string inherits Array? #DIMA
// TODO: string is Array? #do
class String : public Object {
private:
    // [NOTE] Empty concreteness map means fully concrete class
    std::map<INDEX, bool> charConcreteness;
    SIZE lengthOffset;
    SIZE contentsOffset;
public:
    String(ADDR address, SIZE size, SIZE lengthOffset, SIZE contentsOffsetValue);

    void resolve(SIZE offset, INDEX &index, SIZE &offsetInsideChar) const;
    std::vector<INDEX> resolve(const Interval& interval) const;
    SIZE indexOffset(INDEX index) const;
    bool isConcrete(INDEX index) const;
    std::string toString() const override;
};

class Array : public Object {
private:
    // [NOTE] Empty concreteness map means fully concrete array
    std::map<INDEX, bool> concreteness;
    // TODO: use many blocks #do
    Block *elementBlock;
    SIZE elementSize;
public:
    Array(ADDR address, SIZE size, SIZE elemSize, Block *elemBlock);

    void resolve(SIZE offset, SIZE &index, SIZE &offsetInsideElement, Block *&elemBlock) const;
    std::vector<std::tuple<INDEX, Interval>> resolve(const Interval& interval, Block *&elemBlock) const;
    void indexOffset(INDEX index, SIZE &offset, Block *&elemBlock) const;
    bool isConcrete(INDEX index) const;
    std::string toString() const override;
};

typedef IntervalTree<Interval, Shift, ADDR> IntervalTree;

enum refType {
    STRUCT_FIELD,
    CLASS_FIELD,
    ARRAY_INDEX,
};

// TODO: add destructor #do
struct VirtualAddress
{
    union
    {
        // NOTE: mdToken is mdFieldDef or mdMemberRef (for fields of type from another module)
        std::tuple<VirtualAddress *, mdToken> structField;
        std::tuple<OBJID, mdToken> classField;
        std::tuple<OBJID, INDEX> arrayIndex;
    };
    refType ctor;
};


class Heap {
private:
    IntervalTree tree;
    std::vector<OBJID> newAddresses;

//    OBJID allocateObject(Object *obj);
    bool resolve(ADDR address, OBJID &obj, SIZE &offset);
    bool isConcreteBlock(Block *block, const Interval &interval);
    void virtToPhysAddressHelper(VirtualAddress virtAddress, ADDR &address, Block *&block) const;

public:
    Heap();

    OBJID allocateObject(Object *obj);
    static Block *createBlock(ADDR address, SIZE size, COR_FIELD_OFFSET *fieldOffsets, ULONG *fieldSizes, Block *structs[], ULONG fieldsCount);
    OBJID allocateClass(ADDR address, SIZE size, COR_FIELD_OFFSET *fieldOffsets, ULONG *fieldSizes, Block *structs[], ULONG fieldsCount);
    OBJID allocateArray(ADDR address, SIZE size, SIZE elementSize, Block *elementBlock);

    void moveAndMark(ADDR oldLeft, ADDR newLeft, SIZE length);
    void markSurvivedObjects(ADDR start, SIZE length);
    void clearAfterGC();

    std::vector<Interval> flushObjects();

    VirtualAddress *physToVirtAddress(ADDR physAddress);
    ADDR virtToPhysAddress(VirtualAddress virtAddress) const;

    //    bool isConcrete(ADDR address);
    bool isConcrete(ADDR address, SIZE sizeOfPtr);

    void dump() const;
};

}

#endif // HEAP_H_
