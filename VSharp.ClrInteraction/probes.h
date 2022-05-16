#ifndef PROBES_H_
#define PROBES_H_

#include "cor.h"
#include "memory/memory.h"
#include "communication/protocol.h"
#include <vector>

#define COND INT_PTR

namespace vsharp {

/// ------------------------------ Commands ---------------------------

// TODO: sometimes ReJit may not be lazy, so need to start it in probes, so here must be ref to Instrumenter
Protocol *protocol = nullptr;
void setProtocol(Protocol *p) {
    protocol = p;
}

enum EvalStackArgType {
    OpSymbolic = 1,
    OpI4 = 2,
    OpI8 = 3,
    OpR4 = 4,
    OpR8 = 5,
    OpRef = 6,
    OpStruct = 7,
    OpEmpty = 8
};

union OperandContent {
    long long number;
    VirtualAddress address;
};

struct EvalStackOperand {
    EvalStackArgType typ;
    OperandContent content;

    size_t size() const {
        switch (typ) {
            case OpRef:
                // NOTE: evaluation stack type * base address * offset * object type * object key
                return sizeof(INT32) + sizeof(UINT_PTR) + sizeof(UINT_PTR) + sizeof(BYTE) + sizeof(BYTE) * 2;
            case OpEmpty:
                return sizeof(INT32);
            default:
                return sizeof(INT32) + sizeof(INT64);
        }
    }

    void serialize(char *&buffer) const {
        *(INT32 *)buffer = (INT32) typ;
        buffer += sizeof(INT32);
        switch (typ) {
            case OpRef: {
                VirtualAddress address = content.address;
                *(UINT_PTR *)buffer = address.obj; buffer += sizeof(UINT_PTR);
                *(UINT_PTR *)buffer = address.offset; buffer += sizeof(UINT_PTR);
                ObjectLocation location = address.location;
                ObjectType type = location.type;
                *(BYTE *)buffer = type; buffer += sizeof(BYTE);
                switch (type) {
                    case LocalVariable:
                    case Parameter: {
                        StackKey key = location.key.stackKey;
                        *(BYTE *) buffer = key.frame; buffer += sizeof(BYTE);
                        *(BYTE *) buffer = key.idx; buffer += sizeof(BYTE);
                        break;
                    }
                    case Statics:
                        *(INT16 *) buffer = location.key.staticFieldKey; buffer += sizeof(INT16);
                        break;
                    default:
                        buffer += sizeof(BYTE) * 2;
                        break;
                }
                break;
            }
            case OpEmpty:
                break;
            default:
                *(INT64 *)buffer = (INT64) content.number; buffer += sizeof(INT64);
                break;
        }
    }

    void deserialize(char *&buffer) {
        typ = *(EvalStackArgType *)buffer;
        buffer += sizeof(EvalStackArgType);
        if (typ == OpRef) {
            content.address.obj = (OBJID) *(UINT_PTR *)buffer; buffer += sizeof(UINT_PTR);
            content.address.offset = (SIZE) *(UINT_PTR *)buffer; buffer += sizeof(UINT_PTR);
            // NOTE: deserialization of object location is not needed, because updateMemory needs only address and offset
        } else {
            content.number = *(INT64 *)buffer; buffer += sizeof(INT64);
        }
    }
};

struct ExecCommand {
    unsigned isBranch;
    unsigned newCallStackFramesCount;
    unsigned ipStackCount;
    unsigned callStackFramesPops;
    unsigned evaluationStackPushesCount;
    unsigned evaluationStackPops;
    unsigned newAddressesCount;
    unsigned *newCallStackFrames;
    unsigned *ipStack;
    EvalStackOperand *evaluationStackPushes;
    // TODO: add deleted addresses
    OBJID *newAddresses;
    UINT64 *newAddressesTypeLengths;
    char *newAddressesTypes;

    void serialize(char *&bytes, unsigned &count) const {
        count = 7 * sizeof(unsigned) + sizeof(unsigned) * newCallStackFramesCount + sizeof(unsigned) * ipStackCount;
        for (unsigned i = 0; i < evaluationStackPushesCount; ++i)
            count += evaluationStackPushes[i].size();
        count += sizeof(UINT_PTR) * newAddressesCount;
        count += newAddressesCount * sizeof(UINT64);
        UINT64 fullTypesSize = 0;
        for (int i = 0; i < newAddressesCount; ++i)
            fullTypesSize += newAddressesTypeLengths[i];
        count += fullTypesSize;
        bytes = new char[count];
        char *buffer = bytes;
        unsigned size = sizeof(unsigned);
        *(unsigned *)buffer = isBranch; buffer += size;
        *(unsigned *)buffer = newCallStackFramesCount; buffer += size;
        *(unsigned *)buffer = ipStackCount; buffer += size;
        *(unsigned *)buffer = callStackFramesPops; buffer += size;
        *(unsigned *)buffer = evaluationStackPushesCount; buffer += size;
        *(unsigned *)buffer = evaluationStackPops; buffer += size;
        *(unsigned *)buffer = newAddressesCount; buffer += size;
        size = newCallStackFramesCount * sizeof(unsigned);
        memcpy(buffer, (char*)newCallStackFrames, size); buffer += size;
        size = ipStackCount * sizeof(unsigned);
        memcpy(buffer, (char*)ipStack, size); buffer += size;
        for (unsigned i = 0; i < evaluationStackPushesCount; ++i) {
            evaluationStackPushes[i].serialize(buffer);
        }
        size = newAddressesCount * sizeof(UINT_PTR);
        memcpy(buffer, (char*)newAddresses, size); buffer += size;
        size = newAddressesCount * sizeof(UINT64);
        memcpy(buffer, (char*)newAddressesTypeLengths, size); buffer += size;
        memcpy(buffer, newAddressesTypes, fullTypesSize); buffer += fullTypesSize;
    }
};

void initCommand(OFFSET offset, bool isBranch, unsigned opsCount, EvalStackOperand *ops, ExecCommand &command) {
    Stack &stack = vsharp::stack();
    StackFrame &top = stack.topFrame();
    command.isBranch = isBranch ? 1 : 0;

    unsigned minCallFrames = stack.minTopSinceLastSent();
    unsigned currCallFrames = stack.framesCount();
    assert(minCallFrames <= currCallFrames);
    command.newCallStackFramesCount = currCallFrames - minCallFrames;
    command.ipStackCount = currCallFrames;
    command.newCallStackFrames = new unsigned[command.newCallStackFramesCount];
    for (unsigned i = minCallFrames; i < currCallFrames; ++i) {
        command.newCallStackFrames[i - minCallFrames] = stack.tokenAt(i);
    }
    command.ipStack = new unsigned[command.ipStackCount];
    for (unsigned i = 0; i < currCallFrames; ++i) {
        command.ipStack[i] = stack.offsetAt(i);
    }
    command.ipStack[currCallFrames - 1] = offset;
    command.callStackFramesPops = stack.unsentPops();
    unsigned afterPop = top.symbolicsCount();
    const std::vector<std::pair<unsigned, unsigned>> &poppedSymbs = top.poppedSymbolics();
    unsigned currentSymbs = afterPop + poppedSymbs.size();
    for (auto &pair : poppedSymbs) {
        assert((int)opsCount - (int)pair.second - 1 >= 0);
        unsigned idx = opsCount - pair.second - 1;
        assert(idx < opsCount);
        ops[idx].typ = OpSymbolic;
        ops[idx].content.number = (INT64) (currentSymbs - pair.first);
    }
    command.evaluationStackPushesCount = opsCount;
    command.evaluationStackPops = top.evaluationStackPops();
    command.evaluationStackPushes = ops;
    auto newAddresses = heap.flushObjects();
    auto addressesSize = newAddresses.size();
    command.newAddressesCount = addressesSize;
    command.newAddresses = new UINT_PTR[addressesSize];
    unsigned long fullTypesSize = 0;
    for (const auto &newAddress : newAddresses)
        fullTypesSize += newAddress.second.second;
    command.newAddressesTypes = new char[fullTypesSize];
    command.newAddressesTypeLengths = new UINT64[addressesSize];
    auto begin = command.newAddressesTypes;
    int i = 0;
    for (const auto &newAddress : newAddresses) {
        command.newAddresses[i] = newAddress.first;
        auto pair = newAddress.second;
        auto typeSize = pair.second;
        command.newAddressesTypeLengths[i] = (UINT64) typeSize;
        if (typeSize != 0) memcpy(command.newAddressesTypes, pair.first, typeSize);
        command.newAddressesTypes += typeSize;
        delete pair.first;
        i++;
    }
    command.newAddressesTypes = begin;
}

bool readExecResponse(StackFrame &top, EvalStackOperand *ops, unsigned &count, int &framesCount, EvalStackOperand &result) {
    char *bytes; int messageLength;
    protocol->acceptExecResult(bytes, messageLength);
    char *start = bytes;
    framesCount = *(int*)bytes; bytes += sizeof(int);
    char lastPush = *(char*)bytes; bytes += sizeof(char);
    int opsLength = *(int*)bytes; bytes += sizeof(int);
    bool hasInternalCallResult = *(char*)bytes > 0; bytes += sizeof(char);
    bool opsConcretized = opsLength > -1;
    if (lastPush > 0) {
        bool returnValueIsConcrete = (lastPush == 2);
        // TODO: implement lastPush is struct case
        top.pushPrimitive(returnValueIsConcrete);
    }

    if (opsConcretized) {
        // NOTE: if internal call with symbolic arguments has concrete result, no arguments concretization is needed, so opsLength = 0
        assert(opsLength == count || opsLength == 0);
        for (unsigned i = 0; i < opsLength; ++i)
            ops[i].deserialize(bytes);
    }
    count = opsLength;

    if (hasInternalCallResult) {
        // NOTE: internal call with symbolic arguments but concrete result
        result.deserialize(bytes);
    }
    assert(bytes - start == messageLength);

    delete start;
    return opsConcretized;
}

void freeCommand(ExecCommand &command) {
    delete[] command.newCallStackFrames;
    delete[] command.evaluationStackPushes;
    delete[] command.newAddresses;
    delete[] command.newAddressesTypeLengths;
    delete[] command.newAddressesTypes;
}

void updateMemory(EvalStackOperand &op, Stack::OperandMem &opmem, unsigned int idx) {
    switch (op.typ) {
        case OpI4:
            opmem.update_i4((INT32) op.content.number, (INT8) idx);
            break;
        case OpI8:
            opmem.update_i8((INT64) op.content.number, (INT8) idx);
            break;
        case OpR4:
            opmem.update_f4(op.content.number, (INT8) idx);
            break;
        case OpR8:
            opmem.update_f8(op.content.number, (INT8) idx);
            break;
        case OpRef:
            opmem.update_p((INT_PTR) Storage::virtToPhysAddress(op.content.address), (INT8) idx);
            break;
        case OpStruct:
            FAIL_LOUD("Not implemented!");
        case OpEmpty:
            FAIL_LOUD("updateMemory: trying to update empty cell!");
        case OpSymbolic:
            FAIL_LOUD("updateMemory: unexpected symbolic value after concretization!");
    }
}

CommandType getAndHandleCommand() {
    CommandType command;
    if (!protocol->acceptCommand(command)) FAIL_LOUD("Accepting command failed!");
    switch (command) {
        case ReadHeapBytes: {
            VirtualAddress address{};
            INT32 size;
            BYTE isRef;
            if (!protocol->acceptHeapReadingParameters(address, size, isRef)) FAIL_LOUD("Accepting heap reading parameters failed!");

            char *buffer = heap.readBytes(address, size, isRef);
            if (!protocol->sendBytes(buffer, size)) FAIL_LOUD("Sending bytes from heap reading failed!");
            break;
        }
        case Unmarshall: {
            OBJID objID;
            bool isArray;
            int refOffsetsLength, *refOffsets;
            if (!protocol->acceptReadObjectParameters(objID, isArray, refOffsetsLength, refOffsets)) FAIL_LOUD("Accepting object ID failed!");
            char *buffer;
            SIZE size;
            heap.unmarshall(objID, buffer, size, isArray, refOffsetsLength, refOffsets);
            if (!protocol->sendBytes(buffer, (int) size)) FAIL_LOUD("Sending bytes from heap reading failed!");
            break;
        }
        case ReadWholeObject: {
            OBJID objID;
            bool isArray;
            int refOffsetsLength, *refOffsets;
            if (!protocol->acceptReadObjectParameters(objID, isArray, refOffsetsLength, refOffsets)) FAIL_LOUD("Accepting object ID failed!");
            char *buffer;
            SIZE size;
            heap.readWholeObject(objID, buffer, size, isArray, refOffsetsLength, refOffsets);
            if (!protocol->sendBytes(buffer, (int) size)) FAIL_LOUD("Sending bytes from heap reading failed!");
            break;
        }
        default:
            break;
    }
    return command;
}

bool sendCommand(OFFSET offset, unsigned opsCount, EvalStackOperand *ops) {
    ExecCommand command{};
    initCommand(offset, false, opsCount, ops, command);
    protocol->sendSerializable(ExecuteCommand, command);

    // NOTE: handling commands from SILI (ReadBytes, ...)
    CommandType commandType;
    do {
        commandType = getAndHandleCommand();
    } while (commandType != ReadExecResponse);

    Stack &stack = vsharp::stack();
    StackFrame &top = stack.topFrame();
    int framesCount;
    EvalStackOperand internalCallResult = EvalStackOperand {OpSymbolic, 0};
    unsigned oldOpsCount = opsCount;
    bool opsConcretized = readExecResponse(top, ops, opsCount, framesCount, internalCallResult);
//    if (mightFork && opsConcretized && opsCount > 0) {
//        const std::vector<std::pair<unsigned, unsigned>> &poppedSymbs = top.poppedSymbolics();
//        for (const auto &poppedSymb : poppedSymbs) {
//            assert((int)opsCount - (int)poppedSymb.second - 1 >= 0);
//            unsigned idx = opsCount - poppedSymb.second - 1;
//            assert(idx < opsCount);
//            EvalStackOperand op = ops[idx];
//            updateMemory(op, idx);
//        }
//    }
    if (internalCallResult.typ != OpSymbolic)
        updateMemory(internalCallResult, stack.opmem(offset), oldOpsCount);

    vsharp::stack().resetPopsTracking(framesCount);
    freeCommand(command);
    return opsConcretized;
}

bool sendCommand0(OFFSET offset) { return sendCommand(offset, 0, nullptr); }
bool sendCommand1(OFFSET offset) { return sendCommand(offset, 1, new EvalStackOperand[1]); }

// TODO:
EvalStackOperand mkop_4(INT32 op) { return {OpI4, (long long)op}; }
EvalStackOperand mkop_8(INT64 op) { return {OpI8, (long long)op}; }
EvalStackOperand mkop_f4(FLOAT op) {
    auto tmp = (DOUBLE) op;
    assert(sizeof(DOUBLE) == sizeof(long long));
    long long result;
    memcpy(&result, &tmp, sizeof(long long));
    return {OpR4, result};
}
EvalStackOperand mkop_f8(DOUBLE op) {
    assert(sizeof(DOUBLE) == sizeof(long long));
    long long result;
    memcpy(&result, &op, sizeof(long long));
    return {OpR8, result};
}
EvalStackOperand mkop_p(INT_PTR op) {
    OperandContent content{};
    resolve(op, content.address);
    return {OpRef, content};
}
EvalStackOperand mkop_struct(INT_PTR op) { FAIL_LOUD("not implemented"); }
EvalStackOperand mkop_refLikeStruct() {
    OperandContent content{};
    return {OpStruct, content};
}
EvalStackOperand mkop_empty() {
    return {OpEmpty, 0};
}

EvalStackOperand* createEmptyOps(int opsCount) {
    auto ops = new EvalStackOperand[opsCount];
    for (int i = 0; i < opsCount; ++i)
        ops[i] = mkop_empty();
    return ops;
}

EvalStackOperand* createOps(int opsCount, OFFSET offset) {
    const Stack::OperandMem &top = vsharp::stack().opmem(offset);
    auto ops = new EvalStackOperand[opsCount];
    for (int i = 0; i < opsCount; ++i) {
        CorElementType type = top.unmemType((INT8) i);
        switch (type) {
            case ELEMENT_TYPE_I1:
                ops[i] = mkop_4(top.unmem_i1((INT8) i));
                break;
            case ELEMENT_TYPE_I2:
                ops[i] = mkop_4(top.unmem_i2((INT8) i));
                break;
            case ELEMENT_TYPE_I4:
                ops[i] = mkop_4(top.unmem_i4((INT8) i));
                break;
            case ELEMENT_TYPE_I8:
                ops[i] = mkop_8(top.unmem_i8((INT8) i));
                break;
            case ELEMENT_TYPE_R4:
                ops[i] = mkop_f4(top.unmem_f4((INT8) i));
                break;
            case ELEMENT_TYPE_R8:
                ops[i] = mkop_f8(top.unmem_f8((INT8) i));
                break;
            case ELEMENT_TYPE_PTR:
                ops[i] = mkop_p(top.unmem_p((INT8) i));
                break;
            default:
                LOG(tout << "type = " << type);
                LOG(tout << "current frame resolved token = " << HEX(top.stackFrame().resolvedToken()) << std::endl);
                FAIL_LOUD("createOps: not implemented");
                break;
        }
    }
    return ops;
}

/// ------------------------------ Probes declarations ---------------------------

std::vector<unsigned long long> ProbesAddresses;

int registerProbe(unsigned long long probe) {
    ProbesAddresses.push_back(probe);
    return 0;
}

#define PROBE(RETTYPE, NAME, ARGS) \
    RETTYPE STDMETHODCALLTYPE NAME ARGS;\
    int NAME##_tmp = registerProbe((unsigned long long)&(NAME));\
    RETTYPE STDMETHODCALLTYPE NAME ARGS

PROBE(void, EnableInstrumentation, ()) { enabledInstrumentation(); }
PROBE(void, DisableInstrumentation, ()) { disableInstrumentation(); }

inline bool ldarg(INT16 idx) {
    StackFrame &top = vsharp::topFrame();
    top.pop0();
    LocalObject &cell = top.arg(idx);
    bool concreteness = cell.isFullyConcrete();
    if (concreteness) {
        top.push1(cell);
    }
    return concreteness;
}

PROBE(void, Track_Ldarg_0, (OFFSET offset)) { if (!ldarg(0)) sendCommand0(offset); }
PROBE(void, Track_Ldarg_1, (OFFSET offset)) { if (!ldarg(1)) sendCommand0(offset); }
PROBE(void, Track_Ldarg_2, (OFFSET offset)) { if (!ldarg(2)) sendCommand0(offset); }
PROBE(void, Track_Ldarg_3, (OFFSET offset)) { if (!ldarg(3)) sendCommand0(offset); }
PROBE(void, Track_Ldarg_S, (UINT8 idx, OFFSET offset)) { if (!ldarg(idx)) sendCommand0(offset); }
PROBE(void, Track_Ldarg, (UINT16 idx, OFFSET offset)) { if (!ldarg(idx)) sendCommand0(offset); }

PROBE(void, Track_Ldarga_Primitive, (INT_PTR ptr, UINT16 idx, SIZE size)) {
    Stack &stack = vsharp::stack();
    unsigned frame = stack.framesCount();
    StackFrame &top = stack.topFrame();
    LocalObject &cell = top.arg(idx);
    cell.setSize((int) size);
    cell.changeAddress(ptr);
    ObjectLocation location{Parameter, frame, idx};
    cell.setLocation(location);
    heap.allocateLocal(&cell);
    top.addAllocatedLocal(&cell);
    top.push1Concrete();
}

PROBE(void, Track_Ldarga_Struct, (INT_PTR ptr, UINT16 idx)) {
    StackFrame &top = topFrame();
    LocalObject &cell = top.arg(idx);
    cell.changeAddress(ptr);
    heap.allocateLocal(&cell);
    top.addAllocatedLocal(&cell);
    top.push1Concrete();
}

inline bool ldloc(INT16 idx) {
    StackFrame &top = vsharp::topFrame();
    top.pop0();
    LocalObject &cell = top.loc(idx);
    bool concreteness = cell.isFullyConcrete();
    if (concreteness) {
        top.push1(cell);
    }
    return concreteness;
}
PROBE(void, Track_Ldloc_0, (OFFSET offset)) { if (!ldloc(0)) sendCommand0(offset); }
PROBE(void, Track_Ldloc_1, (OFFSET offset)) { if (!ldloc(1)) sendCommand0(offset); }
PROBE(void, Track_Ldloc_2, (OFFSET offset)) { if (!ldloc(2)) sendCommand0(offset); }
PROBE(void, Track_Ldloc_3, (OFFSET offset)) { if (!ldloc(3)) sendCommand0(offset); }
PROBE(void, Track_Ldloc_S, (UINT8 idx, OFFSET offset)) { if (!ldloc(idx)) sendCommand0(offset); }
PROBE(void, Track_Ldloc, (UINT16 idx, OFFSET offset)) { if (!ldloc(idx)) sendCommand0(offset); }

PROBE(void, Track_Ldloca_Primitive, (INT_PTR ptr, UINT16 idx, SIZE size)) {
    Stack &stack = vsharp::stack();
    unsigned frame = stack.framesCount();
    StackFrame &top = stack.topFrame();
    LocalObject &cell = top.loc(idx);
    cell.setSize((int) size);
    cell.changeAddress(ptr);
    ObjectLocation location{LocalVariable, frame, idx};
    cell.setLocation(location);
    heap.allocateLocal(&cell);
    top.addAllocatedLocal(&cell);
    top.push1Concrete();
}

PROBE(void, Track_Ldloca_Struct, (INT_PTR ptr, UINT16 idx)) {
    Stack &stack = vsharp::stack();
    unsigned frame = stack.framesCount();
    StackFrame &top = stack.topFrame();
    LocalObject &cell = top.loc(idx);
    cell.changeAddress(ptr);
    heap.allocateLocal(&cell);
    top.addAllocatedLocal(&cell);
    top.push1Concrete();
}

inline bool starg(INT16 idx) {
    StackFrame &top = vsharp::topFrame();
    const LocalObject &cell = top.peekObject(0);
    bool concreteness = top.pop1();
    top.arg(idx) = cell;
    return concreteness;
}
PROBE(void, Track_Starg_S, (UINT8 idx, OFFSET offset)) { if (!starg(idx)) sendCommand(offset, 1, new EvalStackOperand[1]); }
PROBE(void, Track_Starg, (UINT16 idx, OFFSET offset)) { if (!starg(idx)) sendCommand(offset, 1, new EvalStackOperand[1]); }

inline bool stloc(INT16 idx) {
    StackFrame &top = vsharp::topFrame();
    const LocalObject &cell = top.peekObject(0);
    bool concreteness = top.pop1();
    top.loc(idx) = cell;
    return concreteness;
}
PROBE(void, Track_Stloc_0, (OFFSET offset)) { if (!stloc(0)) sendCommand(offset, 1, new EvalStackOperand[1]); }
PROBE(void, Track_Stloc_1, (OFFSET offset)) { if (!stloc(1)) sendCommand(offset, 1, new EvalStackOperand[1]); }
PROBE(void, Track_Stloc_2, (OFFSET offset)) { if (!stloc(2)) sendCommand(offset, 1, new EvalStackOperand[1]); }
PROBE(void, Track_Stloc_3, (OFFSET offset)) { if (!stloc(3)) sendCommand(offset, 1, new EvalStackOperand[1]); }
PROBE(void, Track_Stloc_S, (UINT8 idx, OFFSET offset)) { if (!stloc(idx)) sendCommand(offset, 1, new EvalStackOperand[1]); }
PROBE(void, Track_Stloc, (UINT16 idx, OFFSET offset)) { if (!stloc(idx)) sendCommand(offset, 1, new EvalStackOperand[1]); }

PROBE(void, Track_Ldc, ()) { topFrame().push1Concrete(); }
PROBE(void, Track_Dup, (OFFSET offset)) {
    StackFrame &top = topFrame();
    const LocalObject &cell = top.peekObject(0);
    if (!top.dup()) {
        sendCommand(offset, 1, new EvalStackOperand[1]);
        top.push1(cell);
    }
}
PROBE(void, Track_Pop, ()) { topFrame().pop1Async(); }

inline bool branch(OFFSET offset) {
    if (!topFrame().pop1())
        return sendCommand1(offset);
    // TODO
    return true;
}
// TODO: make it bool, change instrumentation
PROBE(void, BrTrue, (OFFSET offset)) { branch(offset); }
PROBE(void, BrFalse, (OFFSET offset)) { branch(offset); }
PROBE(void, Switch, (OFFSET offset)) {
    // TODO:
    topFrame().pop1();
}

PROBE(void, Track_UnOp, (UINT16 op, OFFSET offset)) {
    StackFrame &top = vsharp::topFrame();
    bool concreteness = top.pop1();
    if (concreteness)
        top.push1Concrete();
    else
        sendCommand1(offset);
}
PROBE(COND, Track_BinOp, ()) {
    StackFrame &top = vsharp::topFrame();
    bool concreteness = top.pop(2);
    if (concreteness)
        top.push1Concrete();
    return concreteness; }
// TODO: do we need op?
PROBE(void, Exec_BinOp_4, (UINT16 op, INT32 arg1, INT32 arg2, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_4(arg1), mkop_4(arg2) }); }
PROBE(void, Exec_BinOp_8, (UINT16 op, INT64 arg1, INT64 arg2, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_8(arg1), mkop_8(arg2) }); }
PROBE(void, Exec_BinOp_f4, (UINT16 op, FLOAT arg1, FLOAT arg2, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_f4(arg1), mkop_f4(arg2) }); }
PROBE(void, Exec_BinOp_f8, (UINT16 op, DOUBLE arg1, DOUBLE arg2, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_f8(arg1), mkop_f8(arg2) }); }
PROBE(void, Exec_BinOp_p, (UINT16 op, INT_PTR arg1, INT_PTR arg2, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(arg1), mkop_p(arg2) }); }
PROBE(void, Exec_BinOp_8_4, (UINT16 op, INT64 arg1, INT32 arg2, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_8(arg1), mkop_4(arg2) }); }
PROBE(void, Exec_BinOp_4_p, (UINT16 op, INT32 arg1, INT_PTR arg2, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_4(arg1), mkop_p(arg2) }); }
PROBE(void, Exec_BinOp_p_4, (UINT16 op, INT_PTR arg1, INT32 arg2, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(arg1), mkop_4(arg2) }); }
PROBE(void, Exec_BinOp_4_ovf, (UINT16 op, INT32 arg1, INT32 arg2, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_4(arg1), mkop_4(arg2) }); }
PROBE(void, Exec_BinOp_8_ovf, (UINT16 op, INT64 arg1, INT64 arg2, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_8(arg1), mkop_8(arg2) }); }
PROBE(void, Exec_BinOp_f4_ovf, (UINT16 op, FLOAT arg1, FLOAT arg2, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_f4(arg1), mkop_f4(arg2) }); }
PROBE(void, Exec_BinOp_f8_ovf, (UINT16 op, DOUBLE arg1, DOUBLE arg2, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_f8(arg1), mkop_f8(arg2) }); }
PROBE(void, Exec_BinOp_p_ovf, (UINT16 op, INT_PTR arg1, INT_PTR arg2, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(arg1), mkop_p(arg2) }); }
PROBE(void, Exec_BinOp_8_4_ovf, (UINT16 op, INT64 arg1, INT32 arg2, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_8(arg1), mkop_4(arg2) }); }
PROBE(void, Exec_BinOp_4_p_ovf, (UINT16 op, INT32 arg1, INT_PTR arg2, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_4(arg1), mkop_p(arg2) }); }
PROBE(void, Exec_BinOp_p_4_ovf, (UINT16 op, INT_PTR arg1, INT32 arg2, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(arg1), mkop_4(arg2) }); }

PROBE(void, Track_Ldind, (INT_PTR ptr, INT32 sizeOfPtr, OFFSET offset)) {
    StackFrame &top = topFrame();
    auto concreteness = top.pop1();
    if (concreteness) concreteness = heap.readConcreteness(ptr, sizeOfPtr);
    if (concreteness) top.push1Concrete();
    else sendCommand(offset, 1, new EvalStackOperand[1] { mkop_p(ptr) });
}

PROBE(COND, Track_Stind, (INT_PTR ptr, INT32 sizeOfPtr)) {
    StackFrame &top = topFrame();
    auto valueIsConcrete = top.peek0();
    auto addressIsConcrete = top.peek1();
    if (addressIsConcrete) heap.writeConcreteness(ptr, sizeOfPtr, valueIsConcrete);
    return top.pop(2);
}

PROBE(void, Exec_Stind_I1, (INT_PTR ptr, INT8 value, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(ptr), mkop_4(value) }); }
PROBE(void, Exec_Stind_I2, (INT_PTR ptr, INT16 value, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(ptr), mkop_4(value) }); }
PROBE(void, Exec_Stind_I4, (INT_PTR ptr, INT32 value, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(ptr), mkop_4(value) }); }
PROBE(void, Exec_Stind_I8, (INT_PTR ptr, INT64 value, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(ptr), mkop_8(value) }); }
PROBE(void, Exec_Stind_R4, (INT_PTR ptr, FLOAT value, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(ptr), mkop_f4(value) }); }
PROBE(void, Exec_Stind_R8, (INT_PTR ptr, DOUBLE value, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(ptr), mkop_f8(value) }); }
PROBE(void, Exec_Stind_ref, (INT_PTR ptr, INT_PTR value, OFFSET offset)) { sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(ptr), mkop_p(value) }); }

inline void conv(OFFSET offset) {
    StackFrame &top = vsharp::topFrame();
    bool concreteness = top.pop1();
    if (concreteness)
        top.push1Concrete();
    else
        sendCommand1(offset);
}
PROBE(void, Track_Conv, (OFFSET offset)) { conv(offset); }
PROBE(void, Track_Conv_Ovf, (OFFSET offset)) { conv(offset); }

PROBE(void, Track_Newarr, (INT_PTR ptr, mdToken typeToken, OFFSET offset)) {
    StackFrame &top = topFrame();
    if (!top.pop1())
        sendCommand(offset, 1, new EvalStackOperand[1]);
    else
        top.push1Concrete();
}

PROBE(void, Track_Localloc, (INT_PTR len, OFFSET offset)) { /*TODO*/ }
PROBE(void, Track_Ldobj, (INT_PTR ptr, OFFSET offset)) { /* TODO! will ptr be always concrete? */ }
PROBE(void, Track_Ldstr, (INT_PTR ptr)) { topFrame().push1Concrete(); } // TODO: do we need allocated address?
PROBE(void, Track_Ldtoken, ()) { topFrame().push1Concrete(); }

PROBE(void, Track_Stobj, (INT_PTR ptr)) {
    // TODO!
    // Will ptr be always concrete?
    topFrame().pop(2);
}

PROBE(void, Track_Initobj, (INT_PTR ptr)) {
    // TODO!
    // Will ptr be always concrete?
    topFrame().pop1();
}

PROBE(void, Track_Ldlen, (INT_PTR ptr, OFFSET offset)) {
    StackFrame &top = topFrame();
    bool concreteness = top.pop1();
    if (concreteness)
        top.push1Concrete();
    else
        sendCommand1(offset);
    // TODO: check concreteness of referenced memory
}

PROBE(COND, Track_Cpobj, (INT_PTR dest, INT_PTR src)) {
    // TODO: check concreteness of referenced memory!
    return topFrame().pop(2);
}
PROBE(void, Exec_Cpobj, (mdToken typeToken, INT_PTR dest, INT_PTR src, OFFSET offset)) {
    /*send command*/
}

PROBE(COND, Track_Cpblk, (INT_PTR dest, INT_PTR src)) {
    // TODO: check concreteness of referenced memory!
    return topFrame().pop(3);
}
PROBE(void, Exec_Cpblk, (INT_PTR dest, INT_PTR src, INT_PTR count, OFFSET offset)) {
    /*send command*/
}

PROBE(COND, Track_Initblk, (INT_PTR ptr)) {
    // TODO: check concreteness of referenced memory!
    return topFrame().pop(3);
}
PROBE(void, Exec_Initblk, (INT_PTR ptr, INT8 value, INT_PTR count, OFFSET offset)) {
    /*send command*/
}

PROBE(void, Track_Castclass, (INT_PTR ptr, mdToken typeToken, OFFSET offset)) {
    // TODO
    // TODO: if exn is thrown, no value is pushed onto the stack
//    switchContext();
    // TODO: is it true that 'castclass' contains only pop,
    // because after 'castclass' JIT calls private function 'CastHelpers.ChkCastClass',
    // that pushes result?
}

PROBE(void, Track_Isinst, (INT_PTR ptr, mdToken typeToken, OFFSET offset)) { /*TODO*/ }

PROBE(void, Track_Box, (INT_PTR ptr, OFFSET offset)) {
    StackFrame &top = vsharp::topFrame();
    if (!top.pop1()) {
        sendCommand1(offset);
    } else {
        top.push1Concrete();
    }
}
PROBE(void, Track_Unbox, (INT_PTR ptr, mdToken typeToken, OFFSET offset)) { /*TODO*/ }
PROBE(void, Track_Unbox_Any, (INT_PTR ptr, mdToken typeToken, OFFSET offset)) { /*TODO*/ }

inline bool ldfld(INT_PTR fieldPtr, INT32 fieldSize) {
    StackFrame &top = vsharp::topFrame();
    bool ptrIsConcrete = top.pop1();
    bool fieldIsConcrete = ptrIsConcrete && heap.readConcreteness(fieldPtr, fieldSize);
    return fieldIsConcrete;
}

// TODO: if objPtr = null, it's static field
PROBE(void, Track_Ldfld, (INT_PTR objPtr, INT_PTR fieldPtr, INT32 fieldSize, OFFSET offset)) {
    if (!ldfld(fieldPtr, fieldSize)) {
        sendCommand(offset, 1, new EvalStackOperand[1] { mkop_p(objPtr) });
    } else {
        vsharp::topFrame().push1Concrete();
    }
}
PROBE(void, Track_Ldfld_Struct, (INT32 fieldOffset, INT32 fieldSize, OFFSET offset)) {
    // TODO: track concreteness of each field (need to get fieldOffset for generic struct)
    StackFrame &top = vsharp::topFrame();
    if (!top.pop1()) {
        sendCommand1(offset);
    } else {
        top.push1Concrete();
    }
}
PROBE(void, Track_Ldflda, (INT_PTR objPtr, mdToken fieldToken, OFFSET offset)) { /*TODO*/ }

inline bool stfld(INT_PTR fieldPtr, INT32 fieldSize) {
    StackFrame &top = vsharp::topFrame();
    bool value = top.peek0();
    bool obj = top.peek1();
    bool memory = false;
    if (obj) memory = heap.readConcreteness(fieldPtr, fieldSize);
    if (memory) {
        heap.writeConcreteness(fieldPtr, fieldSize, value);
    }
    top.pop(2);
    return value && obj && memory;
}

PROBE(void, Track_Stfld_4, (INT_PTR fieldPtr, INT_PTR ptr, INT32 value, OFFSET offset)) {
    if (!stfld(fieldPtr, 4)) {
        sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(ptr), mkop_4(value) });
    }
}
PROBE(void, Track_Stfld_8, (INT_PTR fieldPtr, INT_PTR ptr, INT64 value, OFFSET offset)) {
    if (!stfld(fieldPtr, 8)) {
        sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(ptr), mkop_8(value) });
    }
}
PROBE(void, Track_Stfld_f4, (INT_PTR fieldPtr, INT_PTR ptr, FLOAT value, OFFSET offset)) {
    if (!stfld(fieldPtr, sizeof(FLOAT))) {
        sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(ptr), mkop_f4(value) });
    }
}
PROBE(void, Track_Stfld_f8, (INT_PTR fieldPtr, INT_PTR ptr, DOUBLE value, OFFSET offset)) {
    if (!stfld(fieldPtr, sizeof(DOUBLE))) {
        sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(ptr), mkop_f8(value) });
    }
}
PROBE(void, Track_Stfld_p, (INT_PTR fieldPtr, INT_PTR ptr, INT_PTR value, OFFSET offset)) {
    if (!stfld(fieldPtr, sizeof(INT_PTR))) {
        sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(ptr), mkop_p(value) });
    }
}
PROBE(void, Track_Stfld_struct, (INT_PTR fieldPtr, INT32 fieldSize, INT_PTR ptr, INT_PTR value, OFFSET offset)) {
    if (!stfld(fieldPtr, fieldSize)) {
        sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(ptr), mkop_struct(value) });
    }
}
PROBE(void, Track_Stfld_RefLikeStruct, (INT32 fieldOffset, INT32 fieldSize, OFFSET offset)) {
    INT_PTR ptr = vsharp::stack().opmem(offset).unmem_refLikeStruct();
    if (!stfld(ptr + fieldOffset, fieldSize)) {
        sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(ptr), mkop_refLikeStruct() });
    }
}

PROBE(void, Track_Ldsfld, (mdToken fieldToken, OFFSET offset)) {
    // TODO
    topFrame().push1Concrete();
}
PROBE(void, Track_Ldsflda, (INT_PTR fieldPtr, INT32 size, INT16 id)) {
    OBJID obj = heap.allocateStaticField(fieldPtr, size, id);
    topFrame().push1Concrete();
}
PROBE(void, Track_Stsfld, (mdToken fieldToken, OFFSET offset)) {
    // TODO
    topFrame().pop1();
}

PROBE(COND, Track_Ldelema, (INT_PTR ptr, INT_PTR index)) {
    // TODO
    StackFrame &top = vsharp::topFrame();
    return top.pop1() && top.peek0();
}
PROBE(COND, Track_Ldelem, (INT_PTR ptr, INT_PTR index, INT32 elemSize)) {
    StackFrame &top = vsharp::topFrame();
    bool iConcrete = top.peek0();
    bool ptrConcrete = top.peek1();
    int metadataSize = sizeof(INT_PTR) + sizeof(INT64);
    INT_PTR elemPtr = ptr + index * elemSize + metadataSize;
    bool memory = false;
    if (ptrConcrete) memory = heap.readConcreteness(elemPtr, elemSize);
    top.pop(2);
    bool concreteness = iConcrete && ptrConcrete && memory;
    if (concreteness) top.push1Concrete();
    return concreteness;

    return top.pop1() && top.peek0();
}
PROBE(void, Exec_Ldelema, (INT_PTR ptr, INT_PTR index, OFFSET offset)) { /*send command*/ }
PROBE(void, Exec_Ldelem, (INT_PTR ptr, INT_PTR index, OFFSET offset)) {
    sendCommand(offset, 2, new EvalStackOperand[2] {mkop_p(ptr), mkop_4(index)});
}

PROBE(COND, Track_Stelem, (INT_PTR ptr, INT_PTR index, INT32 elemSize)) {
    StackFrame &top = vsharp::topFrame();
    bool vConcrete = top.peek0();
    bool iConcrete = top.peek1();
    bool ptrConcrete = top.peek2();
    int metadataSize = sizeof(INT_PTR) + sizeof(INT64);
    INT_PTR elemPtr = ptr + index * elemSize + metadataSize;
    bool memory = false;
    if (ptrConcrete) memory = heap.readConcreteness(elemPtr, elemSize);
    if (memory) heap.writeConcreteness(elemPtr, elemSize, vConcrete);
    top.pop(3);
    return vConcrete && iConcrete && ptrConcrete && memory;
}
PROBE(void, Exec_Stelem_I, (INT_PTR ptr, INT_PTR index, INT_PTR value, OFFSET offset)) {
    sendCommand(offset, 3, new EvalStackOperand[3] {mkop_p(ptr), mkop_4(index), mkop_p(value)});
}
PROBE(void, Exec_Stelem_I1, (INT_PTR ptr, INT_PTR index, INT8 value, OFFSET offset)) {
    sendCommand(offset, 3, new EvalStackOperand[3] {mkop_p(ptr), mkop_4(index), mkop_4(value)});
}
PROBE(void, Exec_Stelem_I2, (INT_PTR ptr, INT_PTR index, INT16 value, OFFSET offset)) {
    sendCommand(offset, 3, new EvalStackOperand[3] {mkop_p(ptr), mkop_4(index), mkop_4(value)});
}
PROBE(void, Exec_Stelem_I4, (INT_PTR ptr, INT_PTR index, INT32 value, OFFSET offset)) {
    sendCommand(offset, 3, new EvalStackOperand[3] {mkop_p(ptr), mkop_4(index), mkop_4(value)});
}
PROBE(void, Exec_Stelem_I8, (INT_PTR ptr, INT_PTR index, INT64 value, OFFSET offset)) {
    sendCommand(offset, 3, new EvalStackOperand[3] {mkop_p(ptr), mkop_4(index), mkop_8(value)});
}
PROBE(void, Exec_Stelem_R4, (INT_PTR ptr, INT_PTR index, FLOAT value, OFFSET offset)) {
    sendCommand(offset, 3, new EvalStackOperand[3] {mkop_p(ptr), mkop_4(index), mkop_f4(value)});
}
PROBE(void, Exec_Stelem_R8, (INT_PTR ptr, INT_PTR index, DOUBLE value, OFFSET offset)) {
    sendCommand(offset, 3, new EvalStackOperand[3] {mkop_p(ptr), mkop_4(index), mkop_f8(value)});
}
PROBE(void, Exec_Stelem_Ref, (INT_PTR ptr, INT_PTR index, INT_PTR value, OFFSET offset)) {
    sendCommand(offset, 3, new EvalStackOperand[3] {mkop_p(ptr), mkop_4(index), mkop_p(value)});
}
PROBE(void, Exec_Stelem_Struct, (INT_PTR ptr, INT_PTR index, INT_PTR boxedValue, OFFSET offset)) {
    sendCommand(offset, 3, new EvalStackOperand[3] {mkop_p(ptr), mkop_4(index), mkop_struct(boxedValue)});
}

PROBE(void, Track_Ckfinite, ()) {
    // TODO
    // TODO: if exn is thrown, no value is pushed onto the stack
}
PROBE(void, Track_Sizeof, ()) { topFrame().push1Concrete(); }
PROBE(void, Track_Ldftn, ()) { topFrame().push1Concrete(); }
PROBE(void, Track_Ldvirtftn, (INT_PTR ptr, mdToken token, OFFSET offset)) { /*TODO*/ }
PROBE(void, Track_Arglist, ()) { topFrame().push1Concrete(); }
PROBE(void, Track_Mkrefany, ()) {
    // TODO
    topFrame().pop1();
}

PROBE(void, SetArgSize, (INT8 idx, SIZE size)) {
    assert(size > 0);
    Stack &stack = vsharp::stack();
    unsigned frame = stack.framesCount();
    StackFrame &top = stack.topFrame();
    LocalObject &cell = top.arg(idx);
    cell.setSize((int) size);
    ObjectLocation location{LocalVariable, frame, idx};
    cell.setLocation(location);
}

PROBE(void, SetLocSize, (INT8 idx, SIZE size)) {
    assert(size > 0);
    Stack &stack = vsharp::stack();
    unsigned frame = stack.framesCount();
    StackFrame &top = stack.topFrame();
    LocalObject &cell = top.loc(idx);
    cell.setSize((int) size);
    ObjectLocation location{LocalVariable, frame, idx};
    cell.setLocation(location);
}

PROBE(void, Track_Enter, (mdMethodDef token, unsigned maxStackSize, unsigned argsCount, unsigned localsCount, INT8 isSpontaneous)) {
    LOG(tout << "Track_Enter, token = " << HEX(token) << std::endl);
    Stack &stack = vsharp::stack();
    StackFrame *top = stack.isEmpty() ? nullptr : &stack.topFrame();
    unsigned expected = stack.isEmpty() ? 0xFFFFFFFFu : top->resolvedToken();
    if (expected == token || !expected && !isSpontaneous) {
        LOG(tout << "Frame " << stack.framesCount() <<
                    ": entering token " << HEX(token) <<
                    ", expected token is " << HEX(expected) << std::endl);
        if (!expected) top->setResolvedToken(token);
        top->setSpontaneous(false);
    } else {
        LOG(tout << "Spontaneous enter! Details: expected token "
                 << HEX(expected) << ", but entered " << HEX(token) << std::endl);
        auto args = new bool[argsCount];
        memset(args, true, argsCount);
        stack.pushFrame(token, token, args, argsCount);
        top = &stack.topFrame();
        top->setSpontaneous(true);
        delete[] args;
    }
    top->setEnteredMarker(true);
    top->configure(maxStackSize, localsCount);
}

PROBE(void, Track_StructCtor, (ADDR address)) {
    Stack &stack = vsharp::stack();
    unsigned frames = stack.framesCount();
    if (frames > 1 && !stack.topFrame().isSpontaneous()) {
        StackFrame &prevFrame = stack.frameAt(frames - 2);
        LocalObject &cell = prevFrame.peekObject(0);
        cell.changeAddress(address);
        heap.allocateLocal(&cell);
        prevFrame.addAllocatedLocal(&cell);
    }
}

PROBE(void, Track_EnterMain, (mdMethodDef token, UINT16 argsCount, bool argsConcreteness, unsigned maxStackSize, unsigned localsCount)) {
    Stack &stack = vsharp::stack();
    assert(stack.isEmpty());
    auto args = new bool[argsCount];
    memset(args, argsConcreteness, argsCount);
    stack.pushFrame(token, token, args, argsCount);
    Track_Enter(token, maxStackSize, argsCount, localsCount, 0);
    stack.resetPopsTracking(1);
}

PROBE(void, Track_Leave, (UINT8 returnValues, OFFSET offset)) {
    Stack &stack = vsharp::stack();
    StackFrame &top = stack.topFrame();
#ifdef _DEBUG
    assert(returnValues == 0 || returnValues == 1);
    if (top.count() != returnValues) {
        FAIL_LOUD("Corrupted stack: stack is not empty when popping frame!");
    }
#endif
    if (returnValues) {
        // TODO: implement pushing struct onto evaluation stack, when returning value is struct
        LocalObject cell = LocalObject(top.peekObject(0));
        bool returnValue = top.pop1();
        if (!stack.isEmpty()) {
            if (!top.isSpontaneous()) {
                if (!returnValue) {
                    sendCommand1(offset);
                    // NOTE: popping return value from SILI
                    top.pop1();
                }
                stack.popFrame();
                // NOTE: changing address to unknown to prevent getting address of popped frame
                cell.changeAddress(UNKNOWN_ADDRESS);
                stack.topFrame().push1(cell);
            } else {
                stack.popFrame();
                LOG(tout << "Ignoring return type because of internal execution in unmanaged context..." << std::endl);
            }
        } else {
            FAIL_LOUD("Function returned result, but there is no frame to push return value!")
        }
    } else {
        stack.popFrame();
    }
    LOG(tout << "Managed leave to frame " << stack.framesCount() << ". After popping top frame stack balance is " << top.count() << std::endl);
}

void leaveMain(OFFSET offset, UINT8 opsCount, EvalStackOperand *ops) {
    mainLeft();
    Stack &stack = vsharp::stack();
    StackFrame &top = stack.topFrame();
    LOG(tout << "Main left!");
    if (opsCount > 0) {
        // NOTE: popping return value from IL execution
        bool returnValue = top.pop1();
        LOG(tout << "Return value is " << (returnValue ? "concrete" : "symbolic") << std::endl);
    }
    sendCommand(offset, opsCount, ops);
    // NOTE: popping return value from SILI
    if (opsCount > 0) stack.topFrame().pop1();
    stack.popFrame();
    // NOTE: main left, further exploration is not needed, so only getting commands
    while (true) getAndHandleCommand();
}
PROBE(void, Track_LeaveMain_0, (OFFSET offset)) { leaveMain(offset, 0, new EvalStackOperand[0] { }); }
PROBE(void, Track_LeaveMain_4, (INT32 returnValue, OFFSET offset)) { leaveMain(offset, 1, new EvalStackOperand[1] { mkop_4(returnValue) }); }
PROBE(void, Track_LeaveMain_8, (INT64 returnValue, OFFSET offset)) { leaveMain(offset, 1, new EvalStackOperand[1] { mkop_8(returnValue) }); }
PROBE(void, Track_LeaveMain_f4, (FLOAT returnValue, OFFSET offset)) { leaveMain(offset, 1, new EvalStackOperand[1] { mkop_f4(returnValue) }); }
PROBE(void, Track_LeaveMain_f8, (DOUBLE returnValue, OFFSET offset)) { leaveMain(offset, 1, new EvalStackOperand[1] { mkop_f8(returnValue) }); }
PROBE(void, Track_LeaveMain_p, (INT_PTR returnValue, OFFSET offset)) { leaveMain(offset, 1, new EvalStackOperand[1] { mkop_p(returnValue) }); }

PROBE(void, Finalize_Call, (UINT8 returnValues)) {
    Stack &stack = vsharp::stack();
    if (!stack.topFrame().hasEntered()) {
        // Extern has been called, should pop its frame and push return result onto stack
        stack.popFrame();
        LOG(tout << "Extern left! " << stack.framesCount() << " frames remained" << std::endl);
#ifdef _DEBUG
        assert(returnValues == 0 || returnValues == 1);
        if (stack.isEmpty()) {
            FAIL_LOUD("Corrupted stack: stack is empty after executing external function!");
        }
#endif
        if (returnValues) {
            stack.topFrame().push1Concrete();
        }
    }
}

PROBE(void, Exec_Call, (INT32 argsCount, OFFSET offset)) {
    auto ops = createEmptyOps(argsCount);
    sendCommand(offset, argsCount, ops);
}
PROBE(void, Exec_ThisCall, (INT32 argsCount, OFFSET offset)) {
    auto ops = createEmptyOps(argsCount);
    const Stack::OperandMem &top = vsharp::stack().opmem(offset);
    ops[0] = mkop_p(top.unmem_p(0));
    sendCommand(offset, argsCount, ops);
}
PROBE(void, Exec_InternalCall, (INT32 argsCount, OFFSET offset)) {
    auto ops = createOps(argsCount, offset);
    sendCommand(offset, argsCount, ops);
}

// TODO: cache all structs before pop and use them in PushFrame
PROBE(COND, Track_Call, (UINT16 argsCount)) {
    return vsharp::stack().topFrame().pop(argsCount);
}

PROBE(void, PushFrame, (mdToken unresolvedToken, mdMethodDef resolvedToken, bool newobj, UINT16 argsCount, OFFSET offset)) {
    Stack &stack = vsharp::stack();
    StackFrame &top = stack.topFrame();
    argsCount = newobj ? argsCount + 1 : argsCount;
    bool *argsConcreteness = new bool[argsCount];
    memset(argsConcreteness, true, argsCount);
    LOG(tout << "Call: resolved_token = " << HEX(resolvedToken) << ", unresolved_token = " << HEX(unresolvedToken) << "\n"
             << "\t\tbalance after pop: " << top.count() << "; pushing frame " << stack.framesCount() + 1 << std::endl);
    bool callHasSymbolicArgs = false;
    const std::vector<std::pair<unsigned, unsigned>> &poppedSymbs = top.poppedSymbolics();
    for (auto &pair : poppedSymbs) {
        assert((int)argsCount - (int)pair.second - 1 >= 0);
        unsigned idx = argsCount - pair.second - 1;
        assert(idx < argsCount);
        argsConcreteness[idx] = false;
        callHasSymbolicArgs = true;
    }
    LOG(tout << "Args concreteness: ";
        for (unsigned i = 0; i < argsCount; ++i)
            tout << argsConcreteness[i];);

    top.setIp(offset);
    // TODO: push into new frame structs, popped in Track_Call
    stack.pushFrame(resolvedToken, unresolvedToken, argsConcreteness, argsCount);
    if (callHasSymbolicArgs) stack.resetMinTop();
    delete[] argsConcreteness;
}

PROBE(void, PushTemporaryAllocatedStruct, (SIZE size, OFFSET offset)) {
    Stack &stack = vsharp::stack();
    StackFrame &top = stack.topFrame();
    unsigned frame = stack.framesCount();
    ObjectLocation tempLocation{TemporaryAllocatedStruct, frame, offset};
    LocalObject allocatedStruct(size, tempLocation);
    top.push1(allocatedStruct);
}

PROBE(void, PushInternalCallResult, ()) {
    vsharp::topFrame().push1Concrete();
}

PROBE(void, Track_CallVirt, (UINT16 count, OFFSET offset)) { Track_Call(count); PushFrame(0, 0, false, count, offset); }
PROBE(void, Track_Newobj, (INT_PTR ptr)) { topFrame().push1Concrete(); }
PROBE(void, Track_Calli, (mdSignature signature, OFFSET offset)) {
    // TODO
    (void)signature;
    FAIL_LOUD("CALLI NOT IMLEMENTED!");
}

PROBE(void, Track_Throw, (OFFSET offset)) {
    StackFrame &top = topFrame();
    if (!top.pop1())
        sendCommand1(offset);
    else
        top.push1Concrete();
}
PROBE(void, Track_Rethrow, (OFFSET offset)) { /*TODO*/ }

//PROBE(void, Mem_p, (INT_PTR arg)) { clear_mem(); mem_p(arg); }

PROBE(void, Mem_1, (INT8 arg, INT8 idx, OFFSET offset)) { vsharp::stack().opmem(offset).mem_i1(arg, idx); }
PROBE(void, Mem_2, (INT16 arg, INT8 idx, OFFSET offset)) { vsharp::stack().opmem(offset).mem_i2(arg, idx); }
PROBE(void, Mem_4, (INT32 arg, INT8 idx, OFFSET offset)) { vsharp::stack().opmem(offset).mem_i4(arg, idx); }
PROBE(void, Mem_8, (INT64 arg, INT8 idx, OFFSET offset)) { vsharp::stack().opmem(offset).mem_i8(arg, idx); }
PROBE(void, Mem_f4, (FLOAT arg, INT8 idx, OFFSET offset)) { vsharp::stack().opmem(offset).mem_f4(arg, idx); }
PROBE(void, Mem_f8, (DOUBLE arg, INT8 idx, OFFSET offset)) { vsharp::stack().opmem(offset).mem_f8(arg, idx); }
PROBE(void, Mem_p, (INT_PTR arg, INT8 idx, OFFSET offset)) { vsharp::stack().opmem(offset).mem_p(arg, idx); }

PROBE(void, Mem2_4, (INT32 arg1, INT32 arg2, OFFSET offset)) { auto &opmem = vsharp::stack().opmem(offset); opmem.mem_i4(arg1); opmem.mem_i4(arg2); }
PROBE(void, Mem2_8, (INT64 arg1, INT64 arg2, OFFSET offset)) { auto &opmem = vsharp::stack().opmem(offset); opmem.mem_i8(arg1); opmem.mem_i8(arg2); }
PROBE(void, Mem2_f4, (FLOAT arg1, FLOAT arg2, OFFSET offset)) { auto &opmem = vsharp::stack().opmem(offset); opmem.mem_f4(arg1); opmem.mem_f4(arg2); }
PROBE(void, Mem2_f8, (DOUBLE arg1, DOUBLE arg2, OFFSET offset)) { auto &opmem = vsharp::stack().opmem(offset); opmem.mem_f8(arg1); opmem.mem_f8(arg2); }
//PROBE(void, Mem2_p, (INT_PTR arg1, INT_PTR arg2)) { clear_mem(); mem_p(arg1); mem_p(arg2); }
PROBE(void, Mem2_8_4, (INT64 arg1, INT32 arg2, OFFSET offset)) { auto &opmem = vsharp::stack().opmem(offset); opmem.mem_i8(arg1); opmem.mem_i4(arg2); }
//PROBE(void, Mem2_4_p, (INT32 arg1, INT_PTR arg2)) { clear_mem(); mem_i4(arg1); mem_p(arg2); }
//PROBE(void, Mem2_p_1, (INT_PTR arg1, INT8 arg2)) { clear_mem(); mem_p(arg1); mem_i1(arg2); }
//PROBE(void, Mem2_p_2, (INT_PTR arg1, INT16 arg2)) { clear_mem(); mem_p(arg1); mem_i2(arg2); }
//PROBE(void, Mem2_p_4, (INT_PTR arg1, INT32 arg2)) { clear_mem(); mem_p(arg1); mem_i4(arg2); }
//PROBE(void, Mem2_p_8, (INT_PTR arg1, INT64 arg2)) { clear_mem(); mem_p(arg1); mem_i8(arg2); }
//PROBE(void, Mem2_p_f4, (INT_PTR arg1, FLOAT arg2)) { clear_mem(); mem_p(arg1); mem_f4(arg2); }
//PROBE(void, Mem2_p_f8, (INT_PTR arg1, DOUBLE arg2)) { clear_mem(); mem_p(arg1); mem_f8(arg2); }

PROBE(void, Mem_RefLikeStruct, (INT_PTR arg, OFFSET offset)) { vsharp::stack().opmem(offset).mem_refLikeStruct(arg); }

//PROBE(void, Mem3_p_p_p, (INT_PTR arg1, INT_PTR arg2, INT_PTR arg3)) { clear_mem(); mem_p(arg1); mem_p(arg2); mem_p(arg3); }
//PROBE(void, Mem3_p_p_i1, (INT_PTR arg1, INT_PTR arg2, INT8 arg3)) { clear_mem(); mem_p(arg1); mem_p(arg2); mem_i1(arg3); }
//PROBE(void, Mem3_p_p_i2, (INT_PTR arg1, INT_PTR arg2, INT16 arg3)) { clear_mem(); mem_p(arg1); mem_p(arg2); mem_i2(arg3); }
// In instrumentation, replace Mem3_p_p_i4 probe with:
// mem_i4_idx 2 0
// convi
// mem_p_idx 1 1
// convi
// mem_p_idx 0 2
//PROBE(void, Mem3_p_p_i4, (INT_PTR arg1, INT_PTR arg2, INT32 arg3)) { clear_mem(); mem_p(arg1); mem_p(arg2); mem_i4(arg3); }
//PROBE(void, Mem3_p_p_i8, (INT_PTR arg1, INT_PTR arg2, INT64 arg3)) { clear_mem(); mem_p(arg1); mem_p(arg2); mem_i8(arg3); }
//PROBE(void, Mem3_p_p_f4, (INT_PTR arg1, INT_PTR arg2, FLOAT arg3)) { clear_mem(); mem_p(arg1); mem_p(arg2); mem_f4(arg3); }
//PROBE(void, Mem3_p_p_f8, (INT_PTR arg1, INT_PTR arg2, DOUBLE arg3)) { clear_mem(); mem_p(arg1); mem_p(arg2); mem_f8(arg3); }
//PROBE(void, Mem3_p_i1_p, (INT_PTR arg1, INT8 arg2, INT_PTR arg3)) { clear_mem(); mem_p(arg1); mem_i1(arg2); mem_p(arg3); }

PROBE(INT8, Unmem_1, (INT8 idx)) { return vsharp::stack().lastOpmem().unmem_i1(idx); }
PROBE(INT16, Unmem_2, (INT8 idx)) { return vsharp::stack().lastOpmem().unmem_i2(idx); }
PROBE(INT32, Unmem_4, (INT8 idx)) { return vsharp::stack().lastOpmem().unmem_i4(idx); }
PROBE(INT64, Unmem_8, (INT8 idx)) { return vsharp::stack().lastOpmem().unmem_i8(idx); }
PROBE(FLOAT, Unmem_f4, (INT8 idx)) { return vsharp::stack().lastOpmem().unmem_f4(idx); }
PROBE(DOUBLE, Unmem_f8, (INT8 idx)) { return vsharp::stack().lastOpmem().unmem_f8(idx); }
PROBE(INT_PTR, Unmem_p, (INT8 idx)) { return vsharp::stack().lastOpmem().unmem_p(idx); }

PROBE(void, PopOpmem, (OFFSET offset)) {
#ifdef _DEBUG
    OFFSET expectedOffset = vsharp::stack().lastOpmem().offset();
    if (offset != expectedOffset) {
        LOG(tout << "Pop opmem: expected opmem offset " << expectedOffset << ", but got offset " << offset
                 << " [token=" << HEX(vsharp::stack().lastOpmem().stackFrame().resolvedToken()) << "]");
        FAIL_LOUD("Pop: opmem validation failed!")
    }
#endif
    vsharp::stack().popOpmem();
}

PROBE(void, DumpInstruction, (UINT32 index)) {
#ifdef _DEBUG
    const char *&s = stringsPool[index];
    if (!s) {
        LOG_ERROR(tout << "Pool doesn't contain string with index " << index);
    } else {
        StackFrame &top = vsharp::topFrame();
        LOG(tout << "[Frame " << vsharp::stack().framesCount() << "] Executing " << s << " (stack balance before = " << top.count() << ")" << std::endl);
    }
#endif
}

}

#endif // PROBES_H_
