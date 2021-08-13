#include <iostream>
#include <algorithm>
#include "memory/heap.h"

namespace icsharp {

// TODO: questions #DIMA
// TODO: 1) how to marshall physical address to VSharp (for example, StructField has fieldId, how to create it?)

// --------------------------- Shift ---------------------------

    ADDR Shift::move(ADDR addr) const {
        assert(oldBase <= addr);
        return newBase + (addr - oldBase);
    }

// --------------------------- Interval ---------------------------

    Interval::Interval() {
        left = 0;
        right = 0;
        marked = false;
        flushed = false;
    }

    Interval::Interval(ADDR leftValue, SIZE size)
            : Interval() {
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
        return Interval(std::max(left, other.left), std::min(right, other.right));
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

// TODO: mb rename union Object and Interval classes?
    Object::Object(ADDR address, SIZE size, objectType typeValue)
            : Interval(address, size) {
        type = typeValue;
    }

    std::string Object::toString() const {
        return Interval::toString();
    }

// --------------------------- Block ---------------------------

    Block::Block(ADDR address, SIZE size, const FieldOffsets &fields)
            : Object(address, size, CLASS) {
        // [NOTE] Concreteness map is empty, because allocate creates fully concrete object
        concreteness = std::map<mdToken, bool>();
        fieldOffsets = fields;
    }

    Block::~Block() {
        for (const auto &f : fieldOffsets) {
            Interval fieldInterval;
            Block *fieldBlock;
            std::tie(fieldInterval, fieldBlock) = f.second;
            delete fieldBlock;
            fieldBlock = nullptr;
        }
    }

    void Block::resolve(SIZE offset, mdToken &field, SIZE &offsetInsideField, Block *&structure) const {
        // TODO: sorted array for fields offset, binary search here!
        for (const auto &f : fieldOffsets) {
            Interval fieldInterval;
            Block *fieldBlock;
            std::tie(fieldInterval, fieldBlock) = f.second;
            if (fieldInterval.contains(offset)) {
                field = f.first;
                offsetInsideField = offset - fieldInterval.right;
                structure = fieldBlock;
            }
        }
    }

    std::vector<std::tuple<mdToken, Interval, Block *>> Block::resolve(const Interval &interval) const {
        // TODO: sorted array for fields offset, binary search here!
        std::vector<std::tuple<mdToken, Interval, Block *>> affectedFields;
        for (const auto &f : fieldOffsets) {
            Interval fieldInterval;
            Block *fieldBlock;
            std::tie(fieldInterval, fieldBlock) = f.second;
            if (interval.intersects(fieldInterval))
                affectedFields.emplace_back(f.first, fieldInterval, fieldBlock);
        }
        return affectedFields;
    }

    void Block::fieldOffset(const mdToken &field, SIZE &offset, Block *&fieldBlock) const {
        for (const auto &f : fieldOffsets)
            if (f.first == field) {
                Interval fieldInterval;
                std::tie(fieldInterval, fieldBlock) = f.second;
                offset = fieldInterval.left;
            }
        FAIL_LOUD("Getting field offset: field not found!");
    }

    bool Block::isConcrete(mdToken field) const {
        auto fieldConcreteness = concreteness.find(field);
        if (fieldConcreteness != concreteness.end())
            return fieldConcreteness->second;
        else
            return true;
    }

    std::string Block::toString() const {
        return "(Block)" + Interval::toString();
    }

// --------------------------- String ---------------------------

    String::String(ADDR address, SIZE size, SIZE lengthOffsetValue, SIZE contentsOffsetValue)
            : Object(address, size, STRING) {
        // [NOTE] Concreteness map is empty, because allocate creates fully concrete object
        charConcreteness = std::map<INDEX, bool>();
        assert(contentsOffset > lengthOffset);
        contentsOffset = contentsOffsetValue;
        lengthOffset = lengthOffsetValue;
    }

    void String::resolve(ADDR offset, SIZE &index, SIZE &offsetInsideChar) const {
        assert(offset >= contentsOffset);
        SIZE offsetFromMetadata = offset - contentsOffset;
        index = offsetFromMetadata / 2;
        offsetInsideChar = offsetFromMetadata % 2;
    }


    std::vector<INDEX> String::resolve(const Interval &interval) const {
        std::vector<INDEX> affectedIndices;
        // NOTE: first 'contentsOffset' bytes of array is metadata
        assert(interval.left >= contentsOffset && interval.right >= contentsOffset);
        INDEX firstElement = (interval.left - contentsOffset) / 2;
        INDEX lastElement = (interval.right - contentsOffset) / 2;
        for (INDEX i = firstElement; i < lastElement; i++)
            affectedIndices.push_back(i);
        return affectedIndices;
    }

    bool String::isConcrete(INDEX index) const {
        auto fieldConcreteness = charConcreteness.find(index);
        if (fieldConcreteness != charConcreteness.end())
            return fieldConcreteness->second;
        else
            return true;
    }


    SIZE String::indexOffset(INDEX index) const {
        return contentsOffset + index * 2;
    }

    std::string String::toString() const {
        return "(String)" + Interval::toString();
    }

// --------------------------- Array ---------------------------

    Array::Array(ADDR address, SIZE size, SIZE elemSize, Block *elemBlock)
            : Object(address, size, ARRAY) {
        // [NOTE] Concreteness map is empty, because allocate creates fully concrete object
        concreteness = std::map<INDEX, bool>();
        elementSize = elemSize;
        elementBlock = elemBlock;
    }

    void Array::resolve(SIZE offset, SIZE &index, SIZE &offsetInsideElement, Block *&elemBlock) const {
        // NOTE: first 16 bytes of array is metadata
        assert(offset >= 16);
        SIZE offsetFromMetadata = offset - 16;
        index = offsetFromMetadata / elementSize;
        offsetInsideElement = offsetFromMetadata % elementSize;
        elemBlock = elementBlock;
    }

    std::vector<std::tuple<INDEX, Interval>> Array::resolve(const Interval &interval, Block *&elemBlock) const {
        std::vector<std::tuple<INDEX, Interval>> affectedIndices;
        // NOTE: first 16 bytes of array is metadata
        assert(interval.left >= 16 && interval.right >= 16);
        INDEX firstElement = (interval.left - 16) / elementSize;
        INDEX lastElement = (interval.right - 16) / elementSize;
        for (INDEX i = firstElement; i < lastElement; i++)
            affectedIndices.emplace_back(i, Interval(firstElement * elementSize, elementSize));
        elemBlock = elementBlock;
        return affectedIndices;
    }

    void Array::indexOffset(INDEX index, SIZE &offset, Block *&elemBlock) const {
        // TODO: after elements array has 8 bytes #do
        offset = 16 + index * elementSize;
        elemBlock = elementBlock;
    }

    bool Array::isConcrete(INDEX index) const {
        auto fieldConcreteness = concreteness.find(index);
        if (fieldConcreteness != concreteness.end())
            return fieldConcreteness->second;
        else
            return true;
    }

    std::string Array::toString() const {
        return "(Array)" + Interval::toString();
    }

// --------------------------- Heap ---------------------------

    Heap::Heap() = default;

    OBJID Heap::allocateObject(Object *obj) {
        tree.add(*obj);
        auto id = (OBJID) obj;
        newAddresses.push_back(id);
        return id;
    }

// TODO: care about memory leaks #do
// NOTE: creating class or struct
    Block *
    Heap::createBlock(ADDR address, SIZE size, COR_FIELD_OFFSET *fieldOffsets, ULONG *fieldSizes, Block *structs[],
                      ULONG fieldsCount) {
        std::map<mdToken, Offsets> fields;
        for (int i = 0; i < fieldsCount; ++i) {
            auto offset = std::tuple<Interval, Block *>(Interval(fieldOffsets[i].ulOffset, fieldSizes[i]), structs[i]);
            fields[fieldOffsets[i].ridOfField] = offset;
        }
        return new Block(address, size, fields);
    }

    OBJID
    Heap::allocateClass(ADDR address, SIZE size, COR_FIELD_OFFSET *fieldOffsets, ULONG *fieldSizes, Block *structs[],
                        ULONG fieldsCount) {
        Block *c = createBlock(address, size, fieldOffsets, fieldSizes, structs, fieldsCount);
        return allocateObject(c);
    }

    OBJID Heap::allocateArray(ADDR address, SIZE size, SIZE elementSize, Block *elementBlock) {
        auto *a = new Array(address, size, elementSize, elementBlock);
        return allocateObject(a);
    }

    void Heap::moveAndMark(ADDR oldLeft, ADDR newLeft, SIZE length) {
        Interval i(oldLeft, length);
        Shift s{oldLeft, newLeft};
        tree.moveAndMark(i, s);
    }

//bool Heap::isConcrete(ADDR address)
//{
//    OBJID objid;
//    SIZE offset;
//    SIZE elementOffset;
//    if (!resolve(address, objid, offset)) {
//        return false;
//    }
//
//    auto *obj = (Object *)objid;
//    assert(obj);
//    switch (obj->type) {
//        case CLASS: {
//            auto *c = dynamic_cast<Block *>(obj);
//            assert(c);
//            mdToken field;
//            Block *fieldBlock;
//            c->resolve(offset, field, elementOffset, fieldBlock);
//            if (fieldBlock == nullptr) {
//                assert(elementOffset == 0);
//                return c->isConcrete(field);
//            } else {
//                // TODO: if offset = 0, need to check concreteness of whole struct or it's first field? #do
//                return false;
//            }
//        }
//        case ARRAY: {
//            auto *a = dynamic_cast<Array *>(obj);
//            assert(a);
//            INDEX idx;
//            a->resolve(offset, idx, elementOffset);
//            assert(elementOffset == 0);
//            return a->isConcrete(idx);
//        }
//        case STRING: {
//            auto *str = dynamic_cast<String *>(obj);
//            assert(str);
//            INDEX idx;
//            str->resolve(offset, idx, elementOffset);
//            assert(elementOffset == 0);
//            return str->isConcrete(idx);
//        }
//    }
//}

    bool Heap::isConcreteBlock(Block *block, const Interval &interval) {
        std::vector<std::tuple<mdToken, Interval, Block *>> affectedFields = block->resolve(interval);
        bool res = true;
        for (const auto &field : affectedFields) {
            mdToken fieldId;
            Interval fieldInterval;
            Block *fieldBlock;
            std::tie(fieldId, fieldInterval, fieldBlock) = field;
            if (fieldBlock == nullptr || interval.includes(fieldInterval))
                // CASE: field is primitive or whole field is checked
                res = res && block->isConcrete(fieldId);
            else {
                // CASE: part of complex field is checked
                Interval affectedInterval = interval.intersect(fieldInterval);
                res = res && isConcreteBlock(fieldBlock, affectedInterval);
            }
        }
        return res;
    }

// TODO: unify #do
    bool Heap::isConcrete(ADDR address, SIZE sizeOfPtr) {
        OBJID objid;
        SIZE offset;
        if (!resolve(address, objid, offset)) {
            return false;
        }

        auto *obj = (Object *) objid;
        auto interval = Interval(offset, sizeOfPtr);
        assert(obj);
        switch (obj->type) {
            case CLASS: {
                auto *block = dynamic_cast<Block *>(obj);
                assert(block);
                return isConcreteBlock(block, interval);
            }
            case ARRAY: {
                auto *a = dynamic_cast<Array *>(obj);
                assert(a);
                Block *elementBlock;
                auto affectedIndices = a->resolve(interval, elementBlock);
                bool res = true;
                if (elementBlock == nullptr) {
                    for (const auto &i : affectedIndices) {
                        INDEX index;
                        Interval elemInterval;
                        std::tie(index, elemInterval) = i;
                        res = res && a->isConcrete(index);
                    }
                } else {
                    for (const auto &i : affectedIndices) {
                        INDEX index;
                        Interval elemInterval;
                        std::tie(index, elemInterval) = i;
                        if (interval.includes(elemInterval))
                            res = res && a->isConcrete(index);
                        else {
                            Interval affectedInterval = interval.intersect(elemInterval);
                            res = res && isConcreteBlock(elementBlock, affectedInterval);
                        }
                    }
                }
                return res;
            }
            case STRING: {
                auto *str = dynamic_cast<String *>(obj);
                assert(str);
                auto affectedIndices = str->resolve(interval);
                bool res = true;
                for (const auto &index : affectedIndices)
                    res = res && str->isConcrete(index);
                return res;
            }
        }
    }

    bool Heap::resolve(ADDR address, OBJID &obj, SIZE &offset) {
        if (Interval *i = tree.find(address)) {
            offset = address - i->left;
            obj = (OBJID) i;
            return true;
        }
        return false;
    }

    void Heap::markSurvivedObjects(ADDR start, SIZE length) {
        Interval i(start, length);
        tree.mark(i);
    }

    void Heap::clearAfterGC() {
        tree.clearUnmarked();
    }

    std::vector<Interval> Heap::flushObjects() {
        return tree.flush();
    }

    void Heap::dump() const {
        tout << "-------------- HEAP DUMP --------------" << std::endl;
        std::string dump = tree.dumpObjects();
        tout << dump.c_str() << std::endl;
        tout << "-------------- DUMP END ---------------" << std::endl;
    }

    VirtualAddress *Heap::physToVirtAddress(ADDR physAddress) {
        OBJID objid;
        SIZE offset;
        if (!resolve(physAddress, objid, offset)) {
            FAIL_LOUD("unable to resolve physical address!");
        }

        auto *obj = (Object *) objid;
        assert(obj);
        switch (obj->type) {
            case CLASS: {
                auto *block = dynamic_cast<Block *>(obj);
                assert(block);
                mdToken fieldId;
                SIZE offsetInsideField;
                Block *fieldBlock;
                block->resolve(offset, fieldId, offsetInsideField, fieldBlock);
                auto *address = new VirtualAddress{.classField = std::make_tuple(objid, fieldId), .ctor = CLASS_FIELD};
                while (fieldBlock != nullptr && offset > 0) {
                    fieldBlock->resolve(offset, fieldId, offsetInsideField, fieldBlock);
                    address = new VirtualAddress{.structField = std::make_tuple(address, fieldId), .ctor = STRUCT_FIELD};
                }
                assert(offset == 0);
                return address;
            }
                // TODO: unify with class case #do
            case ARRAY: {
                auto *a = dynamic_cast<Array *>(obj);
                assert(a);
                INDEX idx;
                SIZE elementOffset;
                Block *block;
                a->resolve(offset, idx, elementOffset, block);
                auto *address = new VirtualAddress{.arrayIndex = std::make_tuple(objid, idx), .ctor = ARRAY_INDEX};
                while (block != nullptr && offset > 0) {
                    mdToken fieldId;
                    SIZE offsetInsideField;
                    block->resolve(offset, fieldId, offsetInsideField, block);
                    address = new VirtualAddress{.structField = std::make_tuple(address, fieldId), .ctor = STRUCT_FIELD};
                }
                assert(offset == 0);
                return address;
            }
            case STRING: {
                // TODO: if offset in length, create ClassField, otherwise create ArrayIndex #do
                auto *str = dynamic_cast<String *>(obj);
                assert(str);
                INDEX idx;
                SIZE elementOffset;
                str->resolve(offset, idx, elementOffset);
//            assert()
                auto *address = new VirtualAddress{.arrayIndex = std::make_tuple(objid, idx), .ctor = ARRAY_INDEX};
                return address;
            }
        }
    }

    void Heap::virtToPhysAddressHelper(VirtualAddress virtAddress, ADDR &address, Block *&block) const {
        switch (virtAddress.ctor) {
            case STRUCT_FIELD: {
                VirtualAddress *structVirtAddressPtr;
                mdToken fieldId;
                std::tie(structVirtAddressPtr, fieldId) = virtAddress.structField;
                assert(structVirtAddressPtr);
                ADDR structAddress;
                Block *structBlock;
                virtToPhysAddressHelper(*structVirtAddressPtr, structAddress, structBlock);
                assert(structBlock);
                SIZE offset;
                structBlock->fieldOffset(fieldId, offset, block);
                address = structAddress + offset;
            }
            case CLASS_FIELD: {
                OBJID classAddress;
                mdToken fieldId;
                std::tie(classAddress, fieldId) = virtAddress.classField;
                auto *obj = (Object *) classAddress;
                auto *c = dynamic_cast<Block *>(obj);
                SIZE offset;
                c->fieldOffset(fieldId, offset, block);
                address = c->left + offset;
            }
            case ARRAY_INDEX: {
                OBJID arrayAddress;
                INDEX index;
                std::tie(arrayAddress, index) = virtAddress.arrayIndex;
                auto *obj = (Object *) arrayAddress;
                auto *a = dynamic_cast<Array *>(obj);
                SIZE offset;
                a->indexOffset(index, offset, block);
                address = a->left + offset;
            }
            default: FAIL_LOUD("Unexpected virtual address");
        }
    }

    ADDR Heap::virtToPhysAddress(VirtualAddress virtAddress) const {
        Block *block;
        ADDR address;
        virtToPhysAddressHelper(virtAddress, address, block);
        return address;
    }
}