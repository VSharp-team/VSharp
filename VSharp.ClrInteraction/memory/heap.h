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

class Object : public Interval {
private:
    // NOTE: each bit corresponds of concreteness of memory byte
    cell *concreteness = nullptr;
    const cell max = 0xFF;
    const cell min = 0x00;
    const size_t sizeofCell = sizeof(cell) * 8;
public:
    Object(ADDR address, SIZE size);
    std::string toString() const override;
    bool read(SIZE offset, SIZE size) const;
    void write(SIZE offset, SIZE size, bool vConcreteness);
};

typedef IntervalTree<Interval, Shift, ADDR> IntervalTree;

struct VirtualAddress
{
    OBJID obj;
    SIZE offset;
};

class Heap {
private:
    IntervalTree tree;
    std::vector<OBJID> newAddresses;

    bool resolve(ADDR address, VirtualAddress &vAddress);

public:
    Heap();

    OBJID allocateObject(ADDR address, SIZE size);

    void moveAndMark(ADDR oldLeft, ADDR newLeft, SIZE length);
    void markSurvivedObjects(ADDR start, SIZE length);
    void clearAfterGC();

    std::vector<Interval> flushObjects();

    VirtualAddress physToVirtAddress(ADDR physAddress);
    static ADDR virtToPhysAddress(const VirtualAddress &virtAddress) ;

    bool isConcrete(ADDR address, SIZE sizeOfPtr);

    void dump() const;
};

}

#endif // HEAP_H_
