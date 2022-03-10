#ifndef PROBES_H_
#define PROBES_H_

#include "cor.h"
#include "memory/memory.h"
#include "communication/protocol.h"
#include <vector>

#define COND INT_PTR
#define OFFSET UINT32

namespace vsharp {

/// ------------------------------ Commands ---------------------------

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
    OpRef = 6
};

union OperandContent {
    long long number;
    VirtualAddress address;
};

struct EvalStackOperand {
    EvalStackArgType typ;
    OperandContent content;

    size_t size() const {
        if (typ == OpRef)
            return sizeof(EvalStackArgType) + sizeof(unsigned long) + sizeof(unsigned long);
        return sizeof(EvalStackArgType) + sizeof(long long);
    }

    void serialize(char *&buffer) const {
        *(EvalStackArgType *)buffer = typ;
        buffer += sizeof(EvalStackArgType);
        if (typ == OpRef) {
            *(unsigned long *)buffer = content.address.obj; buffer += sizeof(unsigned long);
            *(unsigned long *)buffer = content.address.offset; buffer += sizeof(unsigned long);
        } else {
            *(long long *)buffer = content.number;
            buffer += sizeof(long long);
        }
    }

    void deserialize(char *&buffer) {
        typ = *(EvalStackArgType *)buffer;
        buffer += sizeof(EvalStackArgType);
        if (typ == OpRef) {
            content.address.obj = (OBJID) *(unsigned long *)buffer; buffer += sizeof(unsigned long);
            content.address.offset = (SIZE) *(unsigned long *)buffer; buffer += sizeof(unsigned long);
        } else {
            content.number = *(long long *)buffer;
            buffer += sizeof(long long);
        }
    }
};

struct ExecCommand {
    unsigned offset;
    unsigned isBranch;
    unsigned newCallStackFramesCount;
    unsigned callStackFramesPops;
    unsigned evaluationStackPushesCount;
    unsigned evaluationStackPops;
    unsigned newAddressesCount;
    unsigned *newCallStackFrames;
    EvalStackOperand *evaluationStackPushes;
    // TODO: add deleted addresses
    OBJID *newAddresses;
    unsigned long *newAddressesTypeLengths;
    char *newAddressesTypes;

    void serialize(char *&bytes, unsigned &count) const {
        count = 7 * sizeof(unsigned) + sizeof(unsigned) * newCallStackFramesCount;
        for (unsigned i = 0; i < evaluationStackPushesCount; ++i)
            count += evaluationStackPushes[i].size();
        count += sizeof(UINT_PTR) * newAddressesCount;
        count += newAddressesCount * sizeof(unsigned long);
        unsigned long fullTypesSize = 0;
        for (int i = 0; i < newAddressesCount; ++i)
            fullTypesSize += newAddressesTypeLengths[i];
        count += fullTypesSize;
        bytes = new char[count];
        char *buffer = bytes;
        unsigned size = sizeof(unsigned);
        *(unsigned *)buffer = offset; buffer += size;
        *(unsigned *)buffer = isBranch; buffer += size;
        *(unsigned *)buffer = newCallStackFramesCount; buffer += size;
        *(unsigned *)buffer = callStackFramesPops; buffer += size;
        *(unsigned *)buffer = evaluationStackPushesCount; buffer += size;
        *(unsigned *)buffer = evaluationStackPops; buffer += size;
        *(unsigned *)buffer = newAddressesCount; buffer += size;
        size = newCallStackFramesCount * sizeof(unsigned);
        memcpy(buffer, (char*)newCallStackFrames, size); buffer += size;
        for (unsigned i = 0; i < evaluationStackPushesCount; ++i) {
            evaluationStackPushes[i].serialize(buffer);
        }
        size = newAddressesCount * sizeof(UINT_PTR);
        memcpy(buffer, (char*)newAddresses, size); buffer += size;
        size = newAddressesCount * sizeof(unsigned long);
        memcpy(buffer, (char*)newAddressesTypeLengths, size); buffer += size;
        memcpy(buffer, newAddressesTypes, fullTypesSize); buffer += fullTypesSize;
    }
};

void initCommand(OFFSET offset, bool isBranch, unsigned opsCount, EvalStackOperand *ops, ExecCommand &command) {
    Stack &stack = vsharp::stack();
    StackFrame &top = stack.topFrame();
    command.offset = offset;
    command.isBranch = isBranch ? 1 : 0;

    unsigned minCallFrames = stack.minTopSinceLastSent();
    unsigned currCallFrames = stack.framesCount();
    assert(minCallFrames <= currCallFrames);
    command.newCallStackFramesCount = currCallFrames - minCallFrames;
    command.newCallStackFrames = new unsigned[command.newCallStackFramesCount];
    for (unsigned i = minCallFrames; i < currCallFrames; ++i) {
        command.newCallStackFrames[i - minCallFrames] = stack.tokenAt(i);
    }

    command.callStackFramesPops = stack.unsentPops();
    unsigned afterPop = top.symbolicsCount();
    const std::vector<std::pair<unsigned, unsigned>> &poppedSymbs = top.poppedSymbolics();
    unsigned currentSymbs = afterPop + poppedSymbs.size();
    for (auto &pair : poppedSymbs) {
        assert((int)opsCount - (int)pair.second - 1 >= 0);
        unsigned idx = opsCount - pair.second - 1;
        assert(idx < opsCount);
        ops[idx].typ = OpSymbolic;
        ops[idx].content.number = (long long)(currentSymbs - pair.first);
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
    command.newAddressesTypeLengths = new unsigned long[addressesSize];
    auto begin = command.newAddressesTypes;
    int i = 0;
    for (const auto &newAddress : newAddresses) {
        command.newAddresses[i] = newAddress.first;
        auto pair = newAddress.second;
        auto typeSize = pair.second;
        command.newAddressesTypeLengths[i] = typeSize;
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
        top.push1(returnValueIsConcrete);
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

void updateMemory(EvalStackOperand &op, unsigned int idx) {
    switch (op.typ) {
        case OpI4:
            update_i4((INT32) op.content.number, (INT8) idx);
            break;
        case OpI8:
            update_i8((INT64) op.content.number, (INT8) idx);
            break;
        case OpR4:
            update_f4(op.content.number, (INT8) idx);
            break;
        case OpR8:
            update_f8(op.content.number, (INT8) idx);
            break;
        case OpRef:
            update_p((INT_PTR) Heap::virtToPhysAddress(op.content.address), (INT8) idx);
            break;
        case OpSymbolic:
            FAIL_LOUD("updateMemory: unexpected symbolic value after concretization!");
    }
}
bool sendCommand(OFFSET offset, unsigned opsCount, EvalStackOperand *ops) {
    ExecCommand command;
    initCommand(offset, false, opsCount, ops, command);
    protocol->sendSerializable(ExecuteCommand, command);
    StackFrame &top = vsharp::topFrame();
    int framesCount;
    EvalStackOperand internalCallResult = EvalStackOperand {OpSymbolic, 0};
    unsigned oldOpsCount = opsCount;
    bool opsConcretized = readExecResponse(top, ops, opsCount, framesCount, internalCallResult);
    if (opsConcretized && opsCount > 0) {
        const std::vector<std::pair<unsigned, unsigned>> &poppedSymbs = top.poppedSymbolics();
        for (const auto &poppedSymb : poppedSymbs) {
            assert((int)opsCount - (int)poppedSymb.second - 1 >= 0);
            unsigned idx = opsCount - poppedSymb.second - 1;
            assert(idx < opsCount);
            EvalStackOperand op = ops[idx];
            updateMemory(op, idx);
        }
    }
    if (internalCallResult.typ != OpSymbolic)
        updateMemory(internalCallResult, oldOpsCount);

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
    return {OpR4, result };
}
EvalStackOperand mkop_f8(DOUBLE op) {
    assert(sizeof(DOUBLE) == sizeof(long long));
    long long result;
    memcpy(&result, &op, sizeof(long long));
    return {OpR8, result};
}
EvalStackOperand mkop_p(INT_PTR op) {
    OperandContent content;
    content.address = resolve(op);
    return {OpRef, content};
}
EvalStackOperand mkop_struct(INT_PTR op) { FAIL_LOUD("not implemented"); }

EvalStackOperand* createOps(int opsCount) {
    auto ops = new EvalStackOperand[opsCount];
    for (int i = 0; i < opsCount; ++i) {
        CorElementType type = unmemType((INT8) i);
        switch (type) {
            case ELEMENT_TYPE_I1:
                ops[i] = mkop_4(unmem_i1((INT8) i));
                break;
            case ELEMENT_TYPE_I2:
                ops[i] = mkop_4(unmem_i2((INT8) i));
                break;
            case ELEMENT_TYPE_I4:
                ops[i] = mkop_4(unmem_i4((INT8) i));
                break;
            case ELEMENT_TYPE_I8:
                ops[i] = mkop_8(unmem_i8((INT8) i));
                break;
            case ELEMENT_TYPE_R4:
                ops[i] = mkop_f4(unmem_f4((INT8) i));
                break;
            case ELEMENT_TYPE_R8:
                ops[i] = mkop_f8(unmem_f8((INT8) i));
                break;
            case ELEMENT_TYPE_PTR:
                ops[i] = mkop_p(unmem_p((INT8) i));
                break;
            default:
                LOG(tout << "type = " << type << std::endl);
                FAIL_LOUD("Exec_Call: not implemented");
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
    int NAME##_tmp = registerProbe((unsigned long long)&NAME);\
    RETTYPE STDMETHODCALLTYPE NAME ARGS

inline bool ldarg(INT16 idx) {
    StackFrame &top = vsharp::topFrame();
    top.pop0();
    bool concreteness = top.arg(idx);
    if (concreteness) {
        top.push1Concrete();
    }
    return concreteness;
}
PROBE(void, Track_Ldarg_0, (OFFSET offset)) { if (!ldarg(0)) sendCommand0(offset); }
PROBE(void, Track_Ldarg_1, (OFFSET offset)) { if (!ldarg(1)) sendCommand0(offset); }
PROBE(void, Track_Ldarg_2, (OFFSET offset)) { if (!ldarg(2)) sendCommand0(offset); }
PROBE(void, Track_Ldarg_3, (OFFSET offset)) { if (!ldarg(3)) sendCommand0(offset); }
PROBE(void, Track_Ldarg_S, (UINT8 idx, OFFSET offset)) { if (!ldarg(idx)) sendCommand0(offset); }
PROBE(void, Track_Ldarg, (UINT16 idx, OFFSET offset)) { if (!ldarg(idx)) sendCommand0(offset); }
PROBE(void, Track_Ldarga, (INT_PTR ptr, UINT16 idx)) { topFrame().push1Concrete(); }

inline bool ldloc(INT16 idx) {
    StackFrame &top = vsharp::topFrame();
    top.pop0();
    bool concreteness = top.loc(idx);
    if (concreteness) {
        top.push1Concrete();
    }
    return concreteness;
}
PROBE(void, Track_Ldloc_0, (OFFSET offset)) { if (!ldloc(0)) sendCommand0(offset); }
PROBE(void, Track_Ldloc_1, (OFFSET offset)) { if (!ldloc(1)) sendCommand0(offset); }
PROBE(void, Track_Ldloc_2, (OFFSET offset)) { if (!ldloc(2)) sendCommand0(offset); }
PROBE(void, Track_Ldloc_3, (OFFSET offset)) { if (!ldloc(3)) sendCommand0(offset); }
PROBE(void, Track_Ldloc_S, (UINT8 idx, OFFSET offset)) { if (!ldloc(idx)) sendCommand0(offset); }
PROBE(void, Track_Ldloc, (UINT16 idx, OFFSET offset)) { if (!ldloc(idx)) sendCommand0(offset); }
PROBE(void, Track_Ldloca, (INT_PTR ptr, UINT16 idx)) { topFrame().push1Concrete(); }

inline bool starg(INT16 idx) {
    StackFrame &top = vsharp::topFrame();
    bool concreteness = top.pop1();
    top.setArg(idx, concreteness);
    return concreteness;
}
PROBE(void, Track_Starg_S, (UINT8 idx, OFFSET offset)) { if (!starg(idx)) sendCommand1(offset); }
PROBE(void, Track_Starg, (UINT16 idx, OFFSET offset)) { if (!starg(idx)) sendCommand1(offset); }

inline bool stloc(INT16 idx) {
    // TODO
    StackFrame &top = vsharp::topFrame();
    bool concreteness = top.pop1();
    top.setLoc(idx, concreteness);
    return concreteness;
}
PROBE(void, Track_Stloc_0, (OFFSET offset)) { if (!stloc(0)) sendCommand1(offset); }
PROBE(void, Track_Stloc_1, (OFFSET offset)) { if (!stloc(1)) sendCommand1(offset); }
PROBE(void, Track_Stloc_2, (OFFSET offset)) { if (!stloc(2)) sendCommand1(offset); }
PROBE(void, Track_Stloc_3, (OFFSET offset)) { if (!stloc(3)) sendCommand1(offset); }
PROBE(void, Track_Stloc_S, (UINT8 idx, OFFSET offset)) { if (!stloc(idx)) sendCommand1(offset); }
PROBE(void, Track_Stloc, (UINT16 idx, OFFSET offset)) { if (!stloc(idx)) sendCommand1(offset); }

PROBE(void, Track_Ldc, ()) { topFrame().push1Concrete(); }
PROBE(void, Track_Dup, (OFFSET offset)) {
    if (!topFrame().dup()) {
        sendCommand1(offset);
        topFrame().push1(false);
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

PROBE(void, Track_Ldind, (INT_PTR ptr, OFFSET offset)) {
    // TODO
}

PROBE(COND, Track_Stind, (INT_PTR ptr, INT32 sizeOfPtr)) {
    StackFrame &top = topFrame();
    auto valueIsConcrete = top.peek0();
    auto addressIsConcrete = top.peek1();
    if (addressIsConcrete) heap.write(ptr, sizeOfPtr, valueIsConcrete);
    return topFrame().pop(2);
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

PROBE(void, Track_Newarr, (INT_PTR ptr, mdToken typeToken, OFFSET offset)) { /*TODO! Do we need allocated address?*/ }
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
    // TODO
    StackFrame &top = vsharp::topFrame();
    top.pop1();
    top.push1Concrete();
}
PROBE(void, Track_Unbox, (INT_PTR ptr, mdToken typeToken, OFFSET offset)) { /*TODO*/ }
PROBE(void, Track_Unbox_Any, (INT_PTR ptr, mdToken typeToken, OFFSET offset)) { /*TODO*/ }

inline bool ldfld(INT_PTR fieldPtr, INT32 fieldSize) {
    StackFrame &top = vsharp::topFrame();
    bool ptrIsConcrete = top.pop1();
    bool fieldIsConcrete = false;
    if (ptrIsConcrete) fieldIsConcrete = heap.read(fieldPtr, fieldSize);
    return ptrIsConcrete && fieldIsConcrete;
}

// TODO: if objPtr = null, it's static field
PROBE(void, Track_Ldfld, (INT_PTR objPtr, INT32 fieldOffset, INT32 fieldSize, OFFSET offset)) {
    if (!ldfld(objPtr + fieldOffset, fieldSize)) {
        sendCommand(offset, 1, new EvalStackOperand[1] { mkop_p(objPtr) });
    } else {
        vsharp::topFrame().push1Concrete();
    }
}
PROBE(void, Track_Ldflda, (INT_PTR fieldPtr, mdToken fieldToken, OFFSET offset)) { /*TODO*/ }

inline bool stfld(mdToken fieldToken, INT_PTR ptr) {
    StackFrame &top = vsharp::topFrame();
    // TODO: check concreteness of memory referenced by ptr
    return top.pop(2);
}

PROBE(void, Track_Stfld_4, (mdToken fieldToken, INT_PTR ptr, INT32 value, OFFSET offset)) {
    if (!stfld(fieldToken, ptr)) {
        sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(ptr), mkop_4(value) });
    }
}
PROBE(void, Track_Stfld_8, (mdToken fieldToken, INT_PTR ptr, INT64 value, OFFSET offset)) {
    if (!stfld(fieldToken, ptr)) {
        sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(ptr), mkop_8(value) });
    }
}
PROBE(void, Track_Stfld_f4, (mdToken fieldToken, INT_PTR ptr, FLOAT value, OFFSET offset)) {
    if (!stfld(fieldToken, ptr)) {
        sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(ptr), mkop_f4(value) });
    }
}
PROBE(void, Track_Stfld_f8, (mdToken fieldToken, INT_PTR ptr, DOUBLE value, OFFSET offset)) {
    if (!stfld(fieldToken, ptr)) {
        sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(ptr), mkop_f8(value) });
    }
}
PROBE(void, Track_Stfld_p, (mdToken fieldToken, INT_PTR ptr, INT_PTR value, OFFSET offset)) {
    if (!stfld(fieldToken, ptr)) {
        sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(ptr), mkop_p(value) });
    }
}
PROBE(void, Track_Stfld_struct, (mdToken fieldToken, INT_PTR ptr, INT_PTR value, OFFSET offset)) {
    if (!stfld(fieldToken, ptr)) {
        sendCommand(offset, 2, new EvalStackOperand[2] { mkop_p(ptr), mkop_struct(value) });
    }
}
/// TODO: stfld may be called with any value type! :(

PROBE(void, Track_Ldsfld, (mdToken fieldToken, OFFSET offset)) {
    // TODO
    topFrame().push1Concrete();
}
PROBE(void, Track_Ldsflda, (INT_PTR ptr)) { topFrame().push1Concrete(); }
PROBE(void, Track_Stsfld, (mdToken fieldToken, OFFSET offset)) {
    // TODO
    topFrame().pop1();
}

PROBE(COND, Track_Ldelema, (INT_PTR ptr, INT_PTR index)) {
    // TODO
    StackFrame &top = vsharp::topFrame();
    return top.pop1() && top.peek0();
}
PROBE(COND, Track_Ldelem, (INT_PTR ptr, INT_PTR index)) {
    // TODO
    StackFrame &top = vsharp::topFrame();
    return top.pop1() && top.peek0();
}
PROBE(void, Exec_Ldelema, (INT_PTR ptr, INT_PTR index, OFFSET offset)) { /*send command*/ }
PROBE(void, Exec_Ldelem, (INT_PTR ptr, INT_PTR index, OFFSET offset)) { /*send command*/ }

PROBE(COND, Track_Stelem, (INT_PTR ptr, INT_PTR index)) {
    // TODO
    StackFrame &top = vsharp::topFrame();
    return top.pop(3);
}
PROBE(void, Exec_Stelem_I, (INT_PTR ptr, INT_PTR index, INT_PTR value, OFFSET offset)) { /*send command*/ }
PROBE(void, Exec_Stelem_I1, (INT_PTR ptr, INT_PTR index, INT8 value, OFFSET offset)) { /*send command*/ }
PROBE(void, Exec_Stelem_I2, (INT_PTR ptr, INT_PTR index, INT16 value, OFFSET offset)) { /*send command*/ }
PROBE(void, Exec_Stelem_I4, (INT_PTR ptr, INT_PTR index, INT32 value, OFFSET offset)) { /*send command*/ }
PROBE(void, Exec_Stelem_I8, (INT_PTR ptr, INT_PTR index, INT64 value, OFFSET offset)) { /*send command*/ }
PROBE(void, Exec_Stelem_R4, (INT_PTR ptr, INT_PTR index, FLOAT value, OFFSET offset)) { /*send command*/ }
PROBE(void, Exec_Stelem_R8, (INT_PTR ptr, INT_PTR index, DOUBLE value, OFFSET offset)) { /*send command*/ }
PROBE(void, Exec_Stelem_Ref, (INT_PTR ptr, INT_PTR index, INT_PTR value, OFFSET offset)) { /*send command*/ }
PROBE(void, Exec_Stelem_Struct, (INT_PTR ptr, INT_PTR index, INT_PTR boxedValue, OFFSET offset)) { /*send command*/ }

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

PROBE(void, Track_Enter, (mdMethodDef token, unsigned maxStackSize, unsigned argsCount, unsigned localsCount)) {
    Stack &stack = vsharp::stack();
    assert(!stack.isEmpty());
    StackFrame *top = &stack.topFrame();
    unsigned expected = top->resolvedToken();
    if (!expected || expected == token) {
        LOG(tout << "Frame " << stack.framesCount() <<
                    ": entering token " << HEX(token) <<
                    ", expected token is " << HEX(expected) << std::endl);
        // TODO: if expected is 0, set resolved token?
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

PROBE(void, Track_EnterMain, (mdMethodDef token, UINT16 argsCount, bool argsConcreteness, unsigned maxStackSize, unsigned localsCount)) {
    mainEntered();
    Stack &stack = vsharp::stack();
    assert(stack.isEmpty());
    auto args = new bool[argsCount];
    memset(args, argsConcreteness, argsCount);
    stack.pushFrame(token, token, args, argsCount);
    Track_Enter(token, maxStackSize, argsCount, localsCount);
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
        bool returnValue = top.pop1();
        stack.popFrame();
        if (!stack.isEmpty()) {
            if (!top.isSpontaneous())
                stack.topFrame().push1(returnValue);
            else
                LOG(tout << "Ignoring return type because of internal execution in unmanaged context..." << std::endl);
        } else {
            FAIL_LOUD("Function returned result, but there is no frame to push return value!")
        }
    } else {
        stack.popFrame();
    }
    LOG(tout << "Managed leave to frame " << stack.framesCount() << ". After popping top frame stack balance is " << top.count() << std::endl);
}

void leaveMain(OFFSET offset, UINT8 opsCount, EvalStackOperand *ops) {
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

PROBE(VOID, Exec_Call, (INT32 argsCount, OFFSET offset)) {
    auto ops = createOps(argsCount);
    sendCommand(offset, argsCount, ops);
}
PROBE(COND, Track_Call, (UINT16 argsCount)) {
    return vsharp::stack().topFrame().pop(argsCount);
}

PROBE(VOID, PushFrame, (mdToken unresolvedToken, mdMethodDef resolvedToken, bool newobj, UINT16 argsCount, OFFSET offset)) {
    Stack &stack = vsharp::stack();
    StackFrame &top = stack.topFrame();
    argsCount = newobj ? argsCount + 1 : argsCount;
    bool *argsConcreteness = new bool[argsCount];
    if (newobj) {
        argsConcreteness[0] = true;
    }
    memset(newobj ? argsConcreteness + 1 : argsConcreteness, true, argsCount);
    LOG(tout << "Call: resolved_token = " << HEX(resolvedToken) << ", unresolved_token = " << HEX(unresolvedToken) << "\n"
             << "\t\tbalance after pop: " << top.count() << "; pushing frame " << stack.framesCount() + 1 << std::endl);
    const std::vector<std::pair<unsigned, unsigned>> &poppedSymbs = top.poppedSymbolics();
    for (auto &pair : poppedSymbs) {
        assert((int)argsCount - (int)pair.second - 1 >= 0);
        unsigned idx = argsCount - pair.second - 1;
        assert(idx < argsCount);
        argsConcreteness[idx] = false;
    }
    LOG(tout << "Args concreteness: ";
        for (unsigned i = 0; i < argsCount; ++i)
            tout << argsConcreteness[i];);

    stack.pushFrame(resolvedToken, unresolvedToken, argsConcreteness, argsCount);
    delete[] argsConcreteness;
}

PROBE(void, Track_CallVirt, (UINT16 count, OFFSET offset)) { Track_Call(count); PushFrame(0, 0, false, count, offset); }
PROBE(void, Track_Newobj, (INT_PTR ptr)) { topFrame().push1Concrete(); }
PROBE(void, Track_Calli, (mdSignature signature, OFFSET offset)) {
    // TODO
    (void)signature;
    FAIL_LOUD("CALLI NOT IMLEMENTED!");
}

PROBE(void, Track_Throw, (OFFSET offset)) {
    //TODO
    StackFrame &top = vsharp::topFrame();
    top.pop1();
}
PROBE(void, Track_Rethrow, (OFFSET offset)) { /*TODO*/ }

PROBE(void, Mem_p, (INT_PTR arg)) { clear_mem(); mem_p(arg); }

PROBE(void, Mem_1_idx, (INT8 arg, INT8 idx, INT8 order)) { if (order == 0) clear_mem(); mem_i1(arg, idx); }
PROBE(void, Mem_2_idx, (INT16 arg, INT8 idx, INT8 order)) { if (order == 0) clear_mem(); mem_i2(arg, idx); }
PROBE(void, Mem_4_idx, (INT32 arg, INT8 idx, INT8 order)) { if (order == 0) clear_mem(); mem_i4(arg, idx); }
PROBE(void, Mem_8_idx, (INT64 arg, INT8 idx, INT8 order)) { if (order == 0) clear_mem(); mem_i8(arg, idx); }
PROBE(void, Mem_f4_idx, (FLOAT arg, INT8 idx, INT8 order)) { if (order == 0) clear_mem(); mem_f4(arg, idx); }
PROBE(void, Mem_f8_idx, (DOUBLE arg, INT8 idx, INT8 order)) { if (order == 0) clear_mem(); mem_f8(arg, idx); }
PROBE(void, Mem_p_idx, (INT_PTR arg, INT8 idx, INT8 order)) { if (order == 0) clear_mem(); mem_p(arg, idx); }

PROBE(void, Mem2_4, (INT32 arg1, INT32 arg2)) { clear_mem(); mem_i4(arg1); mem_i4(arg2); }
PROBE(void, Mem2_8, (INT64 arg1, INT64 arg2)) { clear_mem(); mem_i8(arg1); mem_i8(arg2); }
PROBE(void, Mem2_f4, (FLOAT arg1, FLOAT arg2)) { clear_mem(); mem_f4(arg1); mem_f4(arg2); }
PROBE(void, Mem2_f8, (DOUBLE arg1, DOUBLE arg2)) { clear_mem(); mem_f8(arg1); mem_f8(arg2); }
//PROBE(void, Mem2_p, (INT_PTR arg1, INT_PTR arg2)) { clear_mem(); mem_p(arg1); mem_p(arg2); }
PROBE(void, Mem2_8_4, (INT64 arg1, INT32 arg2)) { clear_mem(); mem_i8(arg1); mem_i4(arg2); }
//PROBE(void, Mem2_4_p, (INT32 arg1, INT_PTR arg2)) { clear_mem(); mem_i4(arg1); mem_p(arg2); }
//PROBE(void, Mem2_p_1, (INT_PTR arg1, INT8 arg2)) { clear_mem(); mem_p(arg1); mem_i1(arg2); }
//PROBE(void, Mem2_p_2, (INT_PTR arg1, INT16 arg2)) { clear_mem(); mem_p(arg1); mem_i2(arg2); }
//PROBE(void, Mem2_p_4, (INT_PTR arg1, INT32 arg2)) { clear_mem(); mem_p(arg1); mem_i4(arg2); }
//PROBE(void, Mem2_p_8, (INT_PTR arg1, INT64 arg2)) { clear_mem(); mem_p(arg1); mem_i8(arg2); }
//PROBE(void, Mem2_p_f4, (INT_PTR arg1, FLOAT arg2)) { clear_mem(); mem_p(arg1); mem_f4(arg2); }
//PROBE(void, Mem2_p_f8, (INT_PTR arg1, DOUBLE arg2)) { clear_mem(); mem_p(arg1); mem_f8(arg2); }

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

PROBE(INT8, Unmem_1, (INT8 idx)) { return unmem_i1(idx); }
PROBE(INT16, Unmem_2, (INT8 idx)) { return unmem_i2(idx); }
PROBE(INT32, Unmem_4, (INT8 idx)) { return unmem_i4(idx); }
PROBE(INT64, Unmem_8, (INT8 idx)) { return unmem_i8(idx); }
PROBE(FLOAT, Unmem_f4, (INT8 idx)) { return unmem_f4(idx); }
PROBE(DOUBLE, Unmem_f8, (INT8 idx)) { return unmem_f8(idx); }
PROBE(INT_PTR, Unmem_p, (INT8 idx)) { return unmem_p(idx); }

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
