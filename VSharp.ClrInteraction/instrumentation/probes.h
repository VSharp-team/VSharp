#ifndef PROBES_H_
#define PROBES_H_

#include "cor.h"
#include "memory/memory.h"

namespace icsharp {

void STDMETHODCALLTYPE Push1Concrete() {
    push1Concrete();
}

void STDMETHODCALLTYPE Pop1() {
    pop1();
}

void STDMETHODCALLTYPE Pop2Push1() {
    pop2Push1();
}

void Enter(unsigned token, unsigned maxStackSize) {
    LOG(tout << "Entered!" << std::endl);
    enter(token, maxStackSize);
}

void Enter1(unsigned token, unsigned maxStackSize, unsigned long name) {
    enter(token, maxStackSize);
    LOG(tout << "Entered " << ((char*)name) << "! " << framesCount() << " frames remained" << std::endl);
}

void Leave(unsigned returnValues) {
    leave(returnValues);
    LOG(tout << "Left! " << framesCount() << " frames remained" << std::endl);
}

void FinalizeCall(unsigned returnValues) {
    finalizeCall(returnValues);
}


void DumpInstruction(unsigned long name) {
    LOG(tout << "Executing " << ((char*)name) << std::endl);
}

void Call(unsigned token, unsigned count) {
    call(token, count);
}

void CallVirt(unsigned count) {
    callVirt(count);
}

void(STDMETHODCALLTYPE *Push1ConcreteAddress)() = &Push1Concrete;
void(STDMETHODCALLTYPE *Pop1Address)() = &Pop1;
void(STDMETHODCALLTYPE *Pop2Push1Address)() = &Pop2Push1;
void(STDMETHODCALLTYPE *EnterProbeAddress)(unsigned, unsigned) = &Enter;
void(STDMETHODCALLTYPE *Enter1ProbeAddress)(unsigned, unsigned, unsigned long) = &Enter1;
void(STDMETHODCALLTYPE *LeaveProbeAddress)(unsigned) = &Leave;
void(STDMETHODCALLTYPE *FinalizeCallProbeAddress)(unsigned) = &FinalizeCall;
void(STDMETHODCALLTYPE *DumpInstructionAddress)(unsigned long) = &DumpInstruction;
void(STDMETHODCALLTYPE *CallProbeAddress)(unsigned, unsigned) = &Call;
void(STDMETHODCALLTYPE *CallVirtProbeAddress)(unsigned) = &CallVirt;

void STDMETHODCALLTYPE Pop(unsigned count) {
    pop(count);
}
void STDMETHODCALLTYPE Push(unsigned count) {
    pushConcrete(count);
}


// TODO: remove it!
void(STDMETHODCALLTYPE *PopAddress)(unsigned) = &Pop;
void(STDMETHODCALLTYPE *PushAddress)(unsigned) = &Push;

unsigned long long ProbesAddresses[] = {
    (unsigned long long) Push1ConcreteAddress,
    (unsigned long long) Pop1Address,
    (unsigned long long) Pop2Push1Address,
    (unsigned long long) EnterProbeAddress,
    (unsigned long long) Enter1ProbeAddress,                                         // TODO: remove it!
    (unsigned long long) LeaveProbeAddress,
    (unsigned long long) FinalizeCallProbeAddress,
    (unsigned long long) DumpInstructionAddress,
    (unsigned long long) CallProbeAddress,
    (unsigned long long) CallVirtProbeAddress,
    (unsigned long long) PopAddress,                                                 // TODO: remove it!
    (unsigned long long) PushAddress                                                 // TODO: remove it!
};

}

#endif // PROBES_H_
