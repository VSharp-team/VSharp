#ifndef HEAP_H_
#define HEAP_H_

#include <map>
#include <vector>
#include "intervalTree.h"
#include "cor.h"
#include "corprof.h"
#include "corhdr.h"

namespace vsharp {

#define ADDR UINT_PTR
#define SIZE UINT_PTR
#define OBJID UINT_PTR

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

typedef char cell;

struct StackKey {
    char frame;
    char idx;
};

union ObjectKey {
    StackKey stackKey;
    INT16 staticFieldKey;
    void *none;
};

enum ObjectType {
    ReferenceType = 1,
    LocalVariable = 2,
    Parameter = 3,
    Statics = 4
};

// TODO: use 'ObjectLocation' with const refs
struct ObjectLocation {
    ObjectType type;
    ObjectKey key;
};

class Object : public Interval {
private:
    // NOTE: each bit corresponds of concreteness of memory byte
    cell *concreteness = nullptr;
    const cell max = 0xFF;
    const cell min = 0x00;
    const size_t sizeofCell = sizeof(cell) * 8;
    ObjectLocation m_location;
public:
    Object(ADDR address, SIZE size, ObjectLocation location);
    ~Object() override;
    std::string toString() const override;
    bool readConcreteness(SIZE offset, SIZE size) const;
    bool isFullyConcrete() const;
    void writeConcretenessWholeObject(bool vConcreteness);
    void writeConcreteness(SIZE offset, SIZE size, bool vConcreteness);
    char *readBytes(SIZE offset, SIZE size) const;
    ObjectLocation getLocation() const;
};

typedef IntervalTree<Interval, Shift, ADDR> Intervals;

struct VirtualAddress
{
    OBJID obj;
    SIZE offset;
    ObjectLocation location;
};

class Storage {
private:
    Intervals tree;
    // TODO: store new addresses or get them from tree? #do
    std::map<OBJID, std::pair<char*, unsigned long>> newAddresses;
    std::vector<OBJID> deletedAddresses;

    bool resolve(ADDR address, VirtualAddress &vAddress) const;
    void resolveRefInHeapBytes(char *bytes) const;

public:
    Storage();

    OBJID allocateObject(ADDR address, SIZE size, char *type, unsigned long typeLength);
    // Allocate block of memory controlled by stack
    OBJID allocateLocal(ADDR address, SIZE size, ObjectLocation location, bool concreteness);
    // Allocate block of static memory
    OBJID allocateStaticField(ADDR address, INT32 size, INT16 id);

    void moveAndMark(ADDR oldLeft, ADDR newLeft, SIZE length);
    void markSurvivedObjects(ADDR start, SIZE length);
    void clearAfterGC();

    void deleteObjects(const std::vector<Interval *> &objects);

    std::map<OBJID, std::pair<char*, unsigned long>> flushObjects();

    void physToVirtAddress(ADDR physAddress, VirtualAddress &vAddress) const;
    static ADDR virtToPhysAddress(const VirtualAddress &virtAddress);

    bool readConcreteness(ADDR address, SIZE sizeOfPtr) const;
    void writeConcreteness(ADDR address, SIZE sizeOfPtr, bool vConcreteness) const;
    char *readBytes(const VirtualAddress &address, SIZE sizeOfPtr, BYTE isRef) const;
    void readWholeObject(OBJID objID, char *&buffer, SIZE &size, bool isArray, int refOffsetsLength, int *refOffsets) const;
    void unmarshall(OBJID objID, char *&buffer, SIZE &size, bool isArray, int refOffsetsLength, int *refOffsets) const;

    void dump() const;
};

}

#endif // HEAP_H_
