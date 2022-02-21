#include <iostream>
#include <algorithm>
#include <string>
#include "heap.h"

#define min(a,b) (((a) < (b)) ? (a) : (b))
#define max(a,b) (((a) > (b)) ? (a) : (b))

namespace vsharp {

// --------------------------- Shift ---------------------------

    ADDR Shift::move(ADDR addr) const {
        assert(oldBase <= addr);
        return newBase + (addr - oldBase);
    }

// --------------------------- Interval ---------------------------

    Interval::Interval()
        : left(0), right(0), marked(false), flushed(false) { }

    Interval::Interval(ADDR leftValue, SIZE size)
        : Interval()
    {
        left = leftValue;
        right = leftValue + size - 1;
    }

    Interval::~Interval() = default;

    bool Interval::operator==(const Interval &other) const {
        return left == other.left && right == other.right;
    }

    bool Interval::contains(ADDR point) const {
        return (left <= point) && (point <= right);
    }

    bool Interval::includes(const Interval &other) const {
        return (left <= other.left) && (other.right <= right);
    }

    bool Interval::intersects(const Interval &other) const {
        return (left <= other.right) && (other.right <= right) || (left <= other.left) && (other.left <= right);
    }

    Interval Interval::intersect(const Interval &other) const {
        assert(intersects(other));
        return Interval(max(left, other.left), min(right, other.right));
    }

    void Interval::move(const Shift &shift) {
        left = shift.move(left);
        right = shift.move(right);
    }

    std::string Interval::toString() const {
        return "[" + std::to_string(left) + " ... " + std::to_string(right) + "]";
    }

    void Interval::mark() {
        assert(!marked);
        marked = true;
    }

    void Interval::unmark() {
        assert(marked);
        marked = false;
    }

    bool Interval::isMarked() const {
        return marked;
    }

    void Interval::flush() {
        assert(!flushed);
        flushed = true;
    }

    bool Interval::isFlushed() const {
        return flushed;
    }

// --------------------------- Object ---------------------------

    Object::Object(ADDR address, SIZE size)
        : Interval(address, size)
    {
        assert(size > 0);
        SIZE squashedSize = (size + sizeofCell - 1) / sizeofCell;
        concreteness = new cell[squashedSize];
        // NOTE: all contents are concrete at the beginning
        for (int i = 0; i < squashedSize; ++i) concreteness[i] = max;
    }

    Object::~Object() {
        delete[] concreteness;
    }

    std::string Object::toString() const {
        return Interval::toString();
    }

    bool Object::read(SIZE offset, SIZE size) const {
        assert(size > 0);
        auto startOffset = offset % sizeofCell;
        auto startIndex = offset / sizeofCell;
        auto endOffset = (offset + size) % sizeofCell;
        auto endIndex = (offset + size) / sizeofCell;
        for (unsigned i = startIndex + (startOffset ? 1 : 0); i < endIndex; ++i) {
            if (!(concreteness[i] == max)) {
                return false;
            }
        }

        auto shift = sizeofCell - startOffset;
        cell startMask = ((cell)1 << shift) - 1; // get 00..011...1
        shift = sizeofCell - endOffset;
        cell endMask = (max >> shift) << shift; // get 11..100..0
        if (startOffset && ((concreteness[startIndex] & startMask) != startMask))
            return false;
        if (endOffset && ((concreteness[endIndex] & endMask) != endMask))
            return false;
        return true;
    }

    void Object::write(SIZE offset, SIZE size, bool vConcreteness) {
        assert(size > 0);
        auto startOffset = offset % sizeofCell;
        auto startIndex = offset / sizeofCell;
        auto endOffset = (offset + size) % sizeofCell;
        auto endIndex = (offset + size) / sizeofCell;
        for (unsigned i = startIndex + (startOffset ? 1 : 0); i < endIndex; ++i)
            this->concreteness[i] = vConcreteness ? max : min;

        auto shift = sizeofCell - startOffset;
        cell startMask = ((cell)1 << shift) - 1; // get 00..011...1
        shift = sizeofCell - endOffset;
        cell endMask = (max >> shift) << shift; // get 11..100..0
        if (startOffset) {
            if (vConcreteness)
                this->concreteness[startIndex] |= startMask;
            else
                this->concreteness[startIndex] &= ~startMask;
        }
        if (endOffset) {
            if (vConcreteness)
                this->concreteness[endIndex] |= endMask;
            else
                this->concreteness[endIndex] &= ~endMask;
        }
    }

// --------------------------- Heap ---------------------------

    Heap::Heap() = default;

    OBJID Heap::allocateObject(ADDR address, SIZE size, char *type, unsigned long typeLength) {
        auto *obj = new Object(address, size);
        tree.add(*obj);
        auto id = (OBJID) obj;
        newAddresses[id] = std::make_pair(type, typeLength);
        return id;
    }

    void Heap::moveAndMark(ADDR oldLeft, ADDR newLeft, SIZE length) {
        Interval i(oldLeft, length);
        Shift s{oldLeft, newLeft};
        tree.moveAndMark(i, s);
    }

    bool Heap::read(ADDR address, SIZE sizeOfPtr) const {
        VirtualAddress vAddress{};
        if (!resolve(address, vAddress)) {
            return false;
        }

        auto *obj = (Object *) vAddress.obj;
        return obj->read(vAddress.offset, sizeOfPtr);
    }

    void Heap::write(ADDR address, SIZE sizeOfPtr, bool vConcreteness) const {
        VirtualAddress vAddress{};
        if (!resolve(address, vAddress)) {
            FAIL_LOUD("Writing to heap: unable to resolve address");
        }

        auto *obj = (Object *) vAddress.obj;
        obj->write(vAddress.offset, sizeOfPtr, vConcreteness);
    }

    bool Heap::resolve(ADDR address, VirtualAddress &vAddress) const {
        if (const Interval *i = tree.find(address)) {
            vAddress.offset = address - i->left;
            vAddress.obj = (OBJID) i;
            return true;
        }
        return false;
    }

    void Heap::markSurvivedObjects(ADDR start, SIZE length) {
        Interval i(start, length);
        tree.mark(i);
    }

    void Heap::clearAfterGC() {
        auto deleted = tree.clearUnmarked();
        for (Interval *address : deleted)
            deletedAddresses.push_back((OBJID) address);
    }

    // TODO: store new addresses or get them from tree? #do
    std::map<OBJID, std::pair<char*, unsigned long>> Heap::flushObjects() {
//        return tree.flush();
        std::map<OBJID, std::pair<char*, unsigned long>> result;
        for (const auto &address : newAddresses)
            result[address.first] = address.second;
        newAddresses.clear();
        return result;
    }

    void Heap::dump() const {
        LOG(tout << "-------------- HEAP DUMP --------------" << std::endl);
        std::string dump = tree.dumpObjects();
        LOG(tout << dump.c_str() << std::endl);
        LOG(tout << "-------------- DUMP END ---------------" << std::endl);
    }

    VirtualAddress Heap::physToVirtAddress(ADDR physAddress) const {
        VirtualAddress vAddress{};
        if (!resolve(physAddress, vAddress)) {
            FAIL_LOUD("unable to resolve physical address!");
        }
        return vAddress;
    }

    ADDR Heap::virtToPhysAddress(const VirtualAddress &virtAddress) {
        auto object = (Object *)virtAddress.obj;
        return object->left + virtAddress.offset;
    }
}
