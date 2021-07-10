#include "memory/memory.h"
#include "memory/stack.h"
#include "logging.h"

using namespace icsharp;

ThreadID currentThreadNotConfigured() {
    throw std::logic_error("Current thread getter is not configured!");
}

std::function<ThreadID()> icsharp::currentThread(&currentThreadNotConfigured);

ThreadID lastThreadID = 0;
Stack *stack = nullptr;

inline void switchContext() {
    ThreadID tid = currentThread();
    if (tid != lastThreadID) {
        lastThreadID = tid;
        Stack *&s = stacks[tid];
        if (!s) s = new Stack();
        stack = s;
    }
}

unsigned icsharp::framesCount() {
    switchContext();
    return stack->framesCount();
}

void icsharp::push1Concrete() {
    switchContext();
    stack->topFrame().push1Concrete();
}

void icsharp::pop1() {
    switchContext();
    stack->topFrame().pop1();
}

void icsharp::pop(unsigned count) {
    switchContext();
    stack->topFrame().pop(count);
}

bool icsharp::pop2Push1() {
    switchContext();
    return stack->topFrame().pop2Push1();
}

void icsharp::call(mdMethodDef token, UINT16 argsCount) {
    switchContext();
    LOG(tout << "Setting expected token of frame " << stack->framesCount() << " to "
             << std::hex << token << " (entered marker is " << stack->topFrame().hasEntered() << ")" << std::endl);
    stack->topFrame().setExpectedToken(token);
    stack->topFrame().setEnteredMarker(false);
    pop(argsCount);
}

void icsharp::callVirt(UINT16 argsCount) {
    switchContext();
    pop(argsCount);
}

void icsharp::enter(mdMethodDef token, unsigned maxStackSize/*, unsigned argsCount, unsigned localsCount*/) {
    // TODO: stack frame should be created BEFORE enter, args concreteness should be initialized there.
    // TODO: here we'll just init the evaluation stack, but only if the frame is ours (?)
    switchContext();
    if (!stack->isEmpty()) {
        unsigned expected = stack->topFrame().expectedToken();
        LOG(tout << "Entering token " << std::hex << token << ", expected token is " << std::hex << expected << "; resetting it!" << std::endl);
        if (!expected || expected == token) {
            stack->topFrame().setExpectedToken(0);
            stack->topFrame().setEnteredMarker(true);
            stack->topFrame().setUnmanagedContext(false);
        } else {
            stack->topFrame().setUnmanagedContext(true);
            LOG(tout << "Hmmm!! Managed code has been triggered in unmanaged context! Details: expected token "
                     << std::hex << expected << ", but entered " << std::hex << token << std::endl);
        }
    }
    // TODO: pass max stack size obtained BEFORE instrumentation, not after
    stack->pushFrame(maxStackSize);
}

void icsharp::leave(UINT8 returnValues) {
    switchContext();
#ifdef _DEBUG
    assert(returnValues == 0 || returnValues == 1);
    if (stack->topFrame().count() != returnValues) {
        FAIL_LOUD("Corrupted stack: stack is not empty when popping frame!");
    }
#endif
    if (returnValues) {
        bool returnValue = stack->topFrame().pop1();
        stack->popFrame();
        if (!stack->isEmpty()) {
            if (!stack->topFrame().inUnmanagedContext())
                stack->topFrame().push1(returnValue);
            else
                LOG(tout << "Ignoring return type because of internal execution in unmanaged context..." << std::endl);
        } else {
            FAIL_LOUD("Function returned result, but there is no frame to push return value!")
        }
    } else {
        stack->popFrame();
    }
}

void icsharp::finalizeCall(UINT8 returnValues) {
    switchContext();
    LOG(tout << "finalize call of " << std::hex << stack->topFrame().expectedToken() << " (enter marker " << stack->topFrame().hasEntered() << ")" << std::endl);
    stack->topFrame().setExpectedToken(0);
    if (!stack->topFrame().hasEntered()) {
        // Extern has been called, should push return result onto stack
        LOG(tout << "Extern left! " << framesCount() << " frames remained" << std::endl);
#ifdef _DEBUG
        assert(returnValues == 0 || returnValues == 1);
        if (stack->isEmpty()) {
            FAIL_LOUD("Corrupted stack: stack is empty after executing external function!");
        }
#endif
        if (returnValues) {
            stack->topFrame().push1(1);
        }
    }
    stack->topFrame().setExpectedToken(0);
    stack->topFrame().setUnmanagedContext(false);
}

void icsharp::calli(mdSignature signature) {
    // TODO
    (void)signature;
    FAIL_LOUD("CALLI NOT IMLEMENTED!");
}

void icsharp::execThrow() {
    // TODO
    switchContext();
    stack->topFrame().pop1();
    LOG(tout << "Exec THROW!" << std::endl);
}

void icsharp::execRethrow() {
    // TODO
//    switchContext();
    LOG(tout << "Exec rethrow" << std::endl);
}

void icsharp::validateEnd() {
#ifdef _DEBUG
    for (auto &kv : stacks) {
        if (!kv.second->isEmpty()) {
            FAIL_LOUD("Stack is not empty after program termination!!");
        }
    }
#endif
}

void icsharp::ldarg(INT16 idx) {
    // TODO
    switchContext();
    stack->topFrame().push1Concrete();
    LOG(tout << "Exec ldarg " << idx << std::endl);
}

void icsharp::ldloc(INT16 idx) {
    // TODO
    switchContext();
    stack->topFrame().push1Concrete();
    LOG(tout << "Exec ldloc " << idx << std::endl);
}

void icsharp::starg(INT16 idx) {
    // TODO
    switchContext();
    stack->topFrame().pop1();
    LOG(tout << "Exec starg " << idx << std::endl);
}

void icsharp::stloc(INT16 idx) {
    // TODO
    switchContext();
    stack->topFrame().pop1();
    LOG(tout << "Exec stloc " << idx << std::endl);
}

void icsharp::dup() {
    switchContext();
    stack->topFrame().dup();
    LOG(tout << "Exec dup" << std::endl);
}

void icsharp::unop(INT16 op) {
    // TODO
    switchContext();
    LOG(tout << "Exec unary operation " << op << std::endl);
}

void icsharp::ldind(INT_PTR ptr) {
    // TODO
    switchContext();
    stack->topFrame().push1Concrete();
    LOG(tout << "Exec ldind 0x" << std::hex << ptr << std::endl);
}

bool icsharp::stind(INT_PTR ptr) {
    // TODO
    switchContext();
    LOG(tout << "Exec stind 0x" << std::hex << ptr << std::endl);
    return stack->topFrame().pop1();
}

void icsharp::conv() {
    // TODO
//    switchContext();
    LOG(tout << "Exec conv" << std::endl);
}

void icsharp::conv_ovf() {
    // TODO
//    switchContext();
    LOG(tout << "Exec conv" << std::endl);
}

void newarr(INT_PTR ptr) {
    // TODO
//    switchContext();
    LOG(tout << "Exec newarr (allocated address is 0x" << std::hex << ptr << ")" << std::endl);
}

void localloc(INT_PTR ptr) {
    // TODO
//    switchContext();
    LOG(tout << "Exec localloc (allocated address is 0x" << std::hex << ptr << ")" << std::endl);
}

void icsharp::alloc(INT_PTR ptr) {

}

void ldobj(INT_PTR ptr) {
    // TODO
    // TODO: assert that ptr is concrete!
//    switchContext();
    LOG(tout << "Exec ldobj 0x" << std::hex << ptr << std::endl);
}

void ldstr(INT_PTR ptr) {
    // TODO
    switchContext();
    stack->topFrame().push1Concrete();
    LOG(tout << "Exec ldstr (allocated address is 0x" << std::hex << ptr << ")" << std::endl);
}

void stobj(INT_PTR ptr) {
    // TODO
    // TODO: assert that ptr is concrete!
    switchContext();
    stack->topFrame().pop1();
    LOG(tout << "Exec initobj 0x" << std::hex << ptr << std::endl);
}

void initobj(INT_PTR ptr) {
    // TODO
    switchContext();
    stack->topFrame().pop1();
    LOG(tout << "Exec initobj 0x" << std::hex << ptr << std::endl);
}

void ldlen(INT_PTR ptr) {
    // TODO
//    switchContext();
    LOG(tout << "Exec ldlen 0x" << std::hex << ptr << std::endl);
}

bool icsharp::cpobj(INT_PTR dest, INT_PTR src) {
    // TODO
    switchContext();
    LOG(tout << "Exec cpobj from 0x" << std::hex << src << " to 0x" << std::hex << dest << std::endl);
    return stack->topFrame().pop1() & stack->topFrame().pop1();
}

bool icsharp::cpblk(INT_PTR dest, INT_PTR src) {
    // TODO
    switchContext();
    LOG(tout << "Exec cpblk from 0x" << std::hex << src << " to 0x" << std::hex << dest << std::endl);
    return stack->topFrame().pop1() & stack->topFrame().pop1() & stack->topFrame().pop1();
}

bool icsharp::initblk(INT_PTR ptr) {
    // TODO
    switchContext();
    LOG(tout << "Exec initblk 0x" << std::hex << ptr << std::endl);
    return stack->topFrame().pop1() & stack->topFrame().pop1() & stack->topFrame().pop1();
}

void icsharp::castclass(mdToken typeToken, INT_PTR ptr) {
    // TODO
    // TODO: if exn is thrown, no value is pushed onto the stack
//    switchContext();
    LOG(tout << "Exec castclass 0x" << std::hex << ptr << " to class " << std::hex << typeToken << std::endl);
}

void icsharp::isinst(mdToken typeToken, INT_PTR ptr) {
    // TODO
//    switchContext();
    LOG(tout << "Exec isinst 0x" << std::hex << ptr << ", class token is " << std::hex << typeToken << std::endl);
}

void icsharp::box(INT_PTR ptr) {
    // TODO
    switchContext();
    stack->topFrame().pop1();
    LOG(tout << "Exec box (resulting location is 0x" << std::hex << ptr << ")" << std::endl);
}

void icsharp::unbox(mdToken typeToken, INT_PTR ptr) {
    // TODO
//    switchContext();
    LOG(tout << "Exec unbox 0x" << std::hex << ptr << "(type token 0x" << std::hex << typeToken << ")" << std::endl);
}

void icsharp::unbox_any(mdToken typeToken, INT_PTR ptr) {
    // TODO
//    switchContext();
    LOG(tout << "Exec unbox.any 0x" << std::hex << ptr << "(type token 0x" << std::hex << typeToken << ")" << std::endl);
}

void icsharp::ldfld(mdToken fieldToken, INT_PTR ptr) {
    // TODO
//    switchContext();
    LOG(tout << "Exec ldfld 0x" << std::hex << ptr << "(field token 0x" << std::hex << fieldToken << ")" << std::endl);
}

void icsharp::ldflda(mdToken fieldToken, INT_PTR ptr) {
    // TODO
//    switchContext();
    LOG(tout << "Exec ldflda 0x" << std::hex << ptr << "(field token 0x" << std::hex << fieldToken << ")" << std::endl);
}

bool icsharp::stfld(mdToken fieldToken, INT_PTR ptr) {
    // TODO
    switchContext();
    LOG(tout << "Exec stfld 0x" << std::hex << ptr << "(field token 0x" << std::hex << fieldToken << ")" << std::endl);
    return stack->topFrame().pop1() & stack->topFrame().pop1();
}

void icsharp::ldfsld(mdToken fieldToken) {
    // TODO
    switchContext();
    stack->topFrame().push1Concrete();
    LOG(tout << "Exec ldsfld, field token 0x" << std::hex << fieldToken << std::endl);
}

void icsharp::stsfld(mdToken fieldToken) {
    // TODO
    switchContext();
    stack->topFrame().pop1();
    LOG(tout << "Exec stsfld, field token 0x" << std::hex << fieldToken << std::endl);
}

bool icsharp::ldelema(INT_PTR ptr, INT_PTR index) {
    // TODO
    switchContext();
    LOG(tout << "Exec ldelema, addr 0x" << std::hex << ptr << ", index " << index << std::endl);
    return stack->topFrame().pop1() && stack->topFrame().peek0();
}

bool icsharp::ldelem(INT_PTR ptr, INT_PTR index) {
    // TODO
    switchContext();
    LOG(tout << "Exec ldelem, addr 0x" << std::hex << ptr << ", index " << index << std::endl);
    return stack->topFrame().pop1() && stack->topFrame().peek0();
}

bool icsharp::stelem(INT_PTR ptr, INT_PTR index){
    // TODO
    switchContext();
    LOG(tout << "Exec stelem, addr 0x" << std::hex << ptr << ", index " << index << std::endl);
    return stack->topFrame().pop1() & stack->topFrame().pop1() & stack->topFrame().pop1();
}

void icsharp::ckfinite() {
    // TODO
    // TODO: if exn is thrown, no value is pushed onto the stack
//    switchContext();
    LOG(tout << "Exec ckfinite" << std::endl);
}

void icsharp::ldvirtftn(mdToken token, INT_PTR ptr) {
    // TODO
    switchContext();
    LOG(tout << "Exec ldvirtftn, addr 0x" << std::hex << ptr << ", token 0x" << std::hex << token << std::endl);
}

void icsharp::mkrefany() {
    // TODO
    switchContext();
    stack->topFrame().pop1();
    LOG(tout << "Exec mkrefany" << std::endl);
}

void brtrue() {
    // TODO
    switchContext();
    stack->topFrame().pop1();
    LOG(tout << "Exec brtrue" << std::endl);
}

void brfalse() {
    // TODO
    switchContext();
    stack->topFrame().pop1();
    LOG(tout << "Exec brfalse" << std::endl);
}

void execSwitch() {
    // TODO
    switchContext();
    stack->topFrame().pop1();
    LOG(tout << "Exec switch" << std::endl);
}

unsigned data_idx, data_ptr;
std::vector<char> data;
std::vector<unsigned> dataPtrs;

void icsharp::clear_mem() {
    data_idx = 0; data_ptr = 0;
    dataPtrs.reserve(3);
}

void mem(char *value, size_t size) {
    dataPtrs[data_idx++] = data_ptr;
    data_ptr += size;
    data.reserve(data_ptr);
    memcpy(data.data() + data_ptr - size, value, size);
}

void icsharp::mem_i1(INT8 value) {
    mem((char *) &value, sizeof(INT8));
}

void icsharp::mem_i2(INT16 value) {
    mem((char *) &value, sizeof(INT16));
}

void icsharp::mem_i4(INT32 value) {
    mem((char *) &value, sizeof(INT32));
}

void icsharp::mem_i8(INT64 value) {
    mem((char *) &value, sizeof(INT64));
}

void icsharp::mem_f4(FLOAT value) {
    mem((char *) &value, sizeof(FLOAT));
}

void icsharp::mem_f8(DOUBLE value) {
    mem((char *) &value, sizeof(DOUBLE));
}

void icsharp::mem_p(INT_PTR value) {
    mem((char *) &value, sizeof(INT_PTR));
}

INT8 icsharp::unmem_i1(INT8 idx) {
    return *((INT8*) data.data() + dataPtrs[idx]);
}

INT16 icsharp::unmem_i2(INT8 idx) {
    return *((INT16*) data.data() + dataPtrs[idx]);
}

INT32 icsharp::unmem_i4(INT8 idx) {
    return *((INT32*) data.data() + dataPtrs[idx]);
}

INT64 icsharp::unmem_i8(INT8 idx) {
    return *((INT64*) data.data() + dataPtrs[idx]);
}

FLOAT icsharp::unmem_f4(INT8 idx) {
    return *((FLOAT*) data.data() + dataPtrs[idx]);
}

DOUBLE icsharp::unmem_f8(INT8 idx) {
    return *((DOUBLE*) data.data() + dataPtrs[idx]);
}

INT_PTR icsharp::unmem_p(INT8 idx) {
    return *((INT_PTR*) data.data() + dataPtrs[idx]);
}
