#include <iostream>
#include <algorithm>
#include <string>
#include "storage.h"

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

    const cell max = 0xFF;
    const cell min = 0x00;
    const size_t sizeofCell = sizeof(cell) * 8;
    inline SIZE squashSize(SIZE size) {
        return (size + sizeofCell - 1) / sizeofCell;
    }

    Object::Object(ADDR address, SIZE size, ObjectLocation location)
        : Interval(address, size)
        , m_location(location)
    {
        assert(size > 0);
        SIZE squashedSize = squashSize(size);
        concreteness = new cell[squashedSize];
        // NOTE: all contents are concrete at the beginning
        for (int i = 0; i < squashedSize; ++i) concreteness[i] = max;
        fullConcreteness = true;
    }

    Object::~Object() {
        delete[] concreteness;
    }

    std::string Object::toString() const {
        return Interval::toString();
    }

    bool Object::readConcreteness(SIZE offset, SIZE size) const {
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

    char* Object::readBytes(SIZE offset, SIZE size) const {
        assert(size > 0);
        assert(left != UNKNOWN_ADDRESS);
        char *buffer = new char[size];
        memcpy(buffer, (char *)left + offset, size);
        return buffer;
    }

    bool Object::isFullyConcrete() const {
        return fullConcreteness;
    }

    void Object::writeConcretenessWholeObject(bool vConcreteness) {
        writeConcreteness(0, sizeOf(), vConcreteness);
        fullConcreteness = vConcreteness;
    }

    void Object::writeConcreteness(SIZE offset, SIZE size, bool vConcreteness) {
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
        if (fullConcreteness)
            fullConcreteness = vConcreteness;
        else
            fullConcreteness = readConcreteness(0, sizeOf());
    }

    void Object::getLocation(ObjectLocation &location) const {
        location = m_location;
    }

    int Object::sizeOf() const {
        SIZE size = right - left + 1;
        return (int) size;
    }

// --------------------------- LocalObject ---------------------------

// TODO: think about marked and flushed flags

    LocalObject::LocalObject(int size, const ObjectLocation &location)
        : Object(UNKNOWN_ADDRESS, size, location)
    {
    }

    LocalObject::LocalObject()
        : Object(UNKNOWN_ADDRESS, 1, ObjectLocation{})
    {
    }

    LocalObject::~LocalObject() = default;

    LocalObject::LocalObject(const LocalObject &s)
        : Object(s.left, s.sizeOf(), s.m_location)
    {
        copyConcreteness(s);
    }

    void LocalObject::changeAddress(ADDR address) {
        assert(left == UNKNOWN_ADDRESS || left == address || address == UNKNOWN_ADDRESS);
        int size = sizeOf();
        left = address;
        right = address + size - 1;
    }

    void LocalObject::setSize(int size) {
        assert(sizeOf() == 1);
        delete[] concreteness;
        SIZE squashedSize = squashSize(size);
        concreteness = new cell[squashedSize];
        cell value = isFullyConcrete() ? max : min;
        for (int i = 0; i < squashedSize; ++i) concreteness[i] = value;
        right = left + size - 1;
    }

    void LocalObject::setLocation(const ObjectLocation &location) {
        m_location = ObjectLocation(location);
    }

    void LocalObject::copyConcreteness(const LocalObject &s) {
        assert(concreteness != s.concreteness);
        int size = sizeOf();
        int sizeOfOther = s.sizeOf();
        // NOTE: some local cell may have size (locs/args after setLocSize/setArgSize),
        // but some have no size:
        // - all cells, pushed via push1Concrete
        // - locs/args before setLocSize/setArgSize
        if (size == sizeOfOther) {
            // NOTE: both cells have size, or both are simplified (size = 1)
            SIZE squashedSize = squashSize(size);
            memcpy(concreteness, s.concreteness, squashedSize);
        } else {
            // NOTE: one of cells is simplified (size = 1), but another is not
            // concretizing cell with max size
            if (size > sizeOfOther) {
                writeConcretenessWholeObject(s.isFullyConcrete());
            } else {
                setSize(sizeOfOther);
                SIZE squashedSize = squashSize(sizeOfOther);
                memcpy(concreteness, s.concreteness, squashedSize);
            }
        }
        fullConcreteness = s.fullConcreteness;
    }

    // NOTE: operator needs to be overloaded, because concreteness is pointer, so it should be copied
    LocalObject &LocalObject::operator=(const LocalObject &other) {
        assert(concreteness != other.concreteness);
        if (this == &other)
            return *this;
        copyConcreteness(other);
        m_location = other.m_location;
        changeAddress(other.left);
        return *this;
    }

// --------------------------- Heap ---------------------------

    Storage::Storage() = default;

    OBJID Storage::allocateObject(ADDR address, SIZE size, char *type, unsigned long typeLength) {
        ObjectKey key;
        key.none = nullptr;
        ObjectLocation location{ReferenceType, key};
        auto *obj = new Object(address, size, location);
        tree.add(*obj);
        auto id = (OBJID) obj;
        newAddresses[id] = std::make_pair(type, typeLength);
        return id;
    }

    OBJID Storage::allocateLocal(LocalObject *local) {
        const Interval *i;
        if (!tree.find(local->left, i)) {
            tree.add(*local);
            return (OBJID) local;
        }
        return (OBJID) i;
    }

    UINT_PTR Storage::allocateStaticField(ADDR address, INT32 size, INT16 id) {
        ObjectKey key;
        key.staticFieldKey = id;
        const Interval *i;
        if (!tree.find(address, i)) {
            ObjectLocation location{Statics, key};
            auto *obj = new Object(address, size, location);
            tree.add(*obj);
            return (OBJID) obj;
        }
        return (OBJID) i;
    }

    void Storage::moveAndMark(ADDR oldLeft, ADDR newLeft, SIZE length) {
        Interval i(oldLeft, length);
        Shift s{oldLeft, newLeft};
        tree.moveAndMark(i, s);
    }

    bool Storage::readConcreteness(ADDR address, SIZE sizeOfPtr) const {
        VirtualAddress vAddress{};
        if (!resolve(address, vAddress)) {
            FAIL_LOUD("Unbound pointer!");
        }

        auto *obj = (Object *) vAddress.obj;
        return obj->readConcreteness(vAddress.offset, sizeOfPtr);
    }

    void Storage::writeConcreteness(ADDR address, SIZE sizeOfPtr, bool vConcreteness) const {
        VirtualAddress vAddress{};
        if (!resolve(address, vAddress)) {
            LOG(tout << "Unresolved address = " << address << std::endl);
            FAIL_LOUD("Writing concreteness to heap: unable to resolve address");
        }

        auto *obj = (Object *) vAddress.obj;
        obj->writeConcreteness(vAddress.offset, sizeOfPtr, vConcreteness);
    }

    char *Storage::readBytes(const VirtualAddress &address, SIZE sizeOfPtr, BYTE isRef) const {
        Object *obj = (Object *) address.obj;
        char *bytes = obj->readBytes(address.offset, sizeOfPtr);
        if (isRef) {
            assert(sizeOfPtr == sizeof(UINT_PTR));
            resolveRefInHeapBytes(bytes);
        }
        return bytes;
    }

    void Storage::resolveRefInHeapBytes(char *bytes) const {
        UINT_PTR ref;
        memcpy(&ref, bytes, sizeof(UINT_PTR));
        VirtualAddress vAddress{};
        resolve(ref, vAddress);
        assert(vAddress.offset == 0); // TODO: can ptr with offset be inside class?
        memcpy(bytes, &vAddress.obj, sizeof(UINT_PTR));
    }

    void Storage::readWholeObject(OBJID objID, char *&buffer, SIZE &size, bool isArray, int refOffsetsLength, int *refOffsets) const {
        Object *obj = (Object *) objID;
        size = obj->sizeOf();
        VirtualAddress vAddress{objID, 0};
        buffer = readBytes(vAddress, size, false);
        if (isArray && refOffsetsLength > 0) { // TODO: implement for non-vector array
            char *array = buffer + sizeof(UINT_PTR);
            INT64 length;
            memcpy(&length, array, sizeof(INT64)); array += sizeof(INT64);
            for (int j = 0; j < length; j++) {
                resolveRefInHeapBytes(array);
                array += sizeof(UINT_PTR);
            }
        } else {
            char *type = buffer + sizeof(UINT_PTR);
            for (int i = 0; i < refOffsetsLength; i++) {
                char *refAddress = type + refOffsets[i];
                resolveRefInHeapBytes(refAddress);
            }
        }

    }

    void Storage::unmarshall(OBJID objID, char *&buffer, SIZE &size, bool isArray, int refOffsetsLength, int *refOffsets) const {
        Object *obj = (Object *) objID;
        readWholeObject(objID, buffer, size, isArray, refOffsetsLength, refOffsets);
        obj->writeConcreteness(0, size, false);
    }

    bool Storage::resolve(ADDR address, VirtualAddress &vAddress) const {
        assert(address != UNKNOWN_ADDRESS);
        if (address == 0) {
            ObjectKey key{};
            key.none = nullptr;
            vAddress = {0, 0, ReferenceType, key};
            return true;
        }
        const Interval *i;
        if (tree.find(address, i)) {
            const auto *obj = dynamic_cast<const Object *>(i);
            vAddress.offset = address - obj->left;
            vAddress.obj = (OBJID) obj;
            ObjectLocation location{};
            obj->getLocation(location);
            vAddress.location = location;
            return true;
        }
        return false;
    }

    void Storage::markSurvivedObjects(ADDR start, SIZE length) {
        Interval i(start, length);
        tree.mark(i);
    }

    void Storage::clearAfterGC() {
        auto deleted = tree.clearUnmarked();
        for (Interval *address : deleted)
            deletedAddresses.push_back((OBJID) address);
    }

    // TODO: store new addresses or get them from tree? #do
    std::map<OBJID, std::pair<char*, unsigned long>> Storage::flushObjects() {
        std::map<OBJID, std::pair<char*, unsigned long>> result(newAddresses);
        newAddresses.clear();
        return result;
    }

    void Storage::dump() const {
        LOG(tout << "-------------- HEAP DUMP --------------" << std::endl);
        std::string dump = tree.dumpObjects();
        LOG(tout << dump.c_str() << std::endl);
        LOG(tout << "-------------- DUMP END ---------------" << std::endl);
    }

    void Storage::physToVirtAddress(ADDR physAddress, VirtualAddress &vAddress) const {
        if (physAddress == 0) {
            ObjectKey key{};
            key.none = nullptr;
            vAddress.obj = 0;
            vAddress.offset = 0;
            vAddress.location = {ReferenceType, key};
        }
        if (!resolve(physAddress, vAddress)) {
            LOG(tout << "Unresolved address = " << physAddress << std::endl);
            FAIL_LOUD("unable to resolve physical address!");
        }
    }

    ADDR Storage::virtToPhysAddress(const VirtualAddress &virtAddress) {
        auto object = (Object *)virtAddress.obj;
        if (object == nullptr) return 0;
        return object->left + virtAddress.offset;
    }

    void Storage::deleteObjects(const std::vector<Interval *> &objects) {
        tree.deleteIntervals(objects);
    }
}
