#ifndef PROBES_H_
#define PROBES_H_

#include "cor.h"
#include "memory/memory.h"
#include <vector>

#define COND INT_PTR

// TODO: we also have ELEMENT_TYPE_BYREF and ELEMENT_TYPE_PTR. What's with them?

namespace icsharp {

std::vector<unsigned long long> ProbesAddresses;

int registerProbe(unsigned long long probe) {
    ProbesAddresses.push_back(probe);
    return 0;
}

#define PROBE(RETTYPE, NAME, ARGS) \
    RETTYPE STDMETHODCALLTYPE NAME ARGS;\
    int NAME##_tmp = registerProbe((unsigned long long)&NAME);\
    RETTYPE STDMETHODCALLTYPE NAME ARGS


PROBE(void, Track_Ldarg_0, ()) { ldarg(0); }
PROBE(void, Track_Ldarg_1, ()) { ldarg(1); }
PROBE(void, Track_Ldarg_2, ()) { ldarg(2); }
PROBE(void, Track_Ldarg_3, ()) { ldarg(3); }
PROBE(void, Track_Ldarg_S, (UINT8 idx)) { ldarg(idx); }
PROBE(void, Track_Ldarg, (UINT16 idx)) { ldarg(idx); }
PROBE(void, Track_Ldarga, ()) { push1Concrete(); }

PROBE(void, Track_Ldloc_0, ()) { ldloc(0); }
PROBE(void, Track_Ldloc_1, ()) { ldloc(1); }
PROBE(void, Track_Ldloc_2, ()) { ldloc(2); }
PROBE(void, Track_Ldloc_3, ()) { ldloc(3); }
PROBE(void, Track_Ldloc_S, (UINT8 idx)) { ldloc(idx); }
PROBE(void, Track_Ldloc, (UINT16 idx)) { ldloc(idx); }
PROBE(void, Track_Ldloca, ()) { push1Concrete(); }

PROBE(void, Track_Starg_S, (UINT8 idx)) { starg(idx); }
PROBE(void, Track_Starg, (UINT16 idx)) { starg(idx); }
PROBE(void, Track_Stloc_0, ()) { stloc(0); }
PROBE(void, Track_Stloc_1, ()) { stloc(1); }
PROBE(void, Track_Stloc_2, ()) { stloc(2); }
PROBE(void, Track_Stloc_3, ()) { stloc(3); }
PROBE(void, Track_Stloc_S, (UINT8 idx)) { stloc(idx); }
PROBE(void, Track_Stloc, (UINT16 idx)) { stloc(idx); }

PROBE(void, Track_Ldc, ()) { push1Concrete(); }
PROBE(void, Track_Dup, ()) { dup(); }
PROBE(void, Track_Pop, ()) { pop1(); }

PROBE(void, BrTrue, ()) { brtrue(); }
PROBE(void, BrFalse, ()) { brfalse(); }
PROBE(void, Switch, ()) { execSwitch(); }

PROBE(void, Track_UnOp, (UINT16 op)) { return unop(op); }
PROBE(COND, Track_BinOp, ()) { return pop2Push1(); }
PROBE(void, Exec_BinOp_4, (UINT16 op, INT32 arg1, INT32 arg2)) { /*send command*/ }
PROBE(void, Exec_BinOp_8, (UINT16 op, INT64 arg1, INT64 arg2)) { /*send command*/ }
PROBE(void, Exec_BinOp_f4, (UINT16 op, FLOAT arg1, FLOAT arg2)) { /*send command*/ }
PROBE(void, Exec_BinOp_f8, (UINT16 op, DOUBLE arg1, DOUBLE arg2)) { /*send command*/ }
PROBE(void, Exec_BinOp_p, (UINT16 op, INT_PTR arg1, INT_PTR arg2)) { /*send command*/ }
PROBE(void, Exec_BinOp_4_p, (UINT16 op, INT32 arg1, INT_PTR arg2)) { /*send command*/ }
PROBE(void, Exec_BinOp_p_4, (UINT16 op, INT_PTR arg1, INT32 arg2)) { /*send command*/ }
PROBE(void, Exec_BinOp_4_ovf, (UINT16 op, INT32 arg1, INT32 arg2)) { /*send command*/ }
PROBE(void, Exec_BinOp_8_ovf, (UINT16 op, INT64 arg1, INT64 arg2)) { /*send command*/ }
PROBE(void, Exec_BinOp_f4_ovf, (UINT16 op, FLOAT arg1, FLOAT arg2)) { /*send command*/ }
PROBE(void, Exec_BinOp_f8_ovf, (UINT16 op, DOUBLE arg1, DOUBLE arg2)) { /*send command*/ }
PROBE(void, Exec_BinOp_p_ovf, (UINT16 op, INT_PTR arg1, INT_PTR arg2)) { /*send command*/ }
PROBE(void, Exec_BinOp_4_p_ovf, (UINT16 op, INT32 arg1, INT_PTR arg2)) { /*send command*/ }
PROBE(void, Exec_BinOp_p_4_ovf, (UINT16 op, INT_PTR arg1, INT32 arg2)) { /*send command*/ }

PROBE(void, Track_Ldind, (INT_PTR ptr)) { ldind(ptr); }
PROBE(COND, Track_Stind, (INT_PTR ptr)) { return stind(ptr); }
PROBE(void, Exec_Stind_I1, (INT_PTR ptr, INT8 value)) { /*send command*/ }
PROBE(void, Exec_Stind_I2, (INT_PTR ptr, INT16 value)) { /*send command*/ }
PROBE(void, Exec_Stind_I4, (INT_PTR ptr, INT32 value)) { /*send command*/ }
PROBE(void, Exec_Stind_I8, (INT_PTR ptr, INT64 value)) { /*send command*/ }
PROBE(void, Exec_Stind_R4, (INT_PTR ptr, FLOAT value)) { /*send command*/ }
PROBE(void, Exec_Stind_R8, (INT_PTR ptr, DOUBLE value)) { /*send command*/ }
PROBE(void, Exec_Stind_ref, (INT_PTR ptr, INT_PTR value)) { /*send command*/ }

PROBE(void, Track_Conv, ()) { conv(); }
PROBE(void, Track_Conv_Ovf, ()) { conv_ovf(); }

PROBE(void, Track_Newarr, (mdToken typeToken, INT_PTR ptr)) { newarr(ptr); }
PROBE(void, Track_Localloc, (INT_PTR ptr)) { localloc(ptr); }
PROBE(void, Track_Ldobj, (INT_PTR ptr)) { ldobj(ptr); /* TODO: ptr must be always concrete? */ }
PROBE(void, Track_Ldstr, (INT_PTR ptr)) { ldstr(ptr); }
PROBE(void, Track_Ldtoken, ()) { push1Concrete(); }
PROBE(void, Track_Stobj, (INT_PTR ptr)) { stobj(ptr); /* TODO: ptr must be always concrete? */ }
PROBE(void, Track_Initobj, (INT_PTR ptr)) { initobj(ptr); }
PROBE(void, Track_Ldlen, (INT_PTR ptr)) { ldlen(ptr); }

PROBE(COND, Track_Cpobj, (INT_PTR dest, INT_PTR src)) { return cpobj(dest, src); }
PROBE(void, Exec_Cpobj, (mdToken typeToken, INT_PTR dest, INT_PTR src)) { /*send command*/ }
PROBE(COND, Track_Cpblk, (INT_PTR dest, INT_PTR src)) { return cpblk(dest, src); }
PROBE(void, Exec_Cpblk, (INT_PTR dest, INT_PTR src, INT_PTR count)) { /*send command*/ }
PROBE(COND, Track_Initblk, (INT_PTR ptr)) { return initblk(ptr); }
PROBE(void, Exec_Initblk, (INT_PTR ptr, INT8 value, INT_PTR count)) { /*send command*/ }

PROBE(void, Track_Castclass, (INT_PTR ptr, mdToken typeToken)) { castclass(typeToken, ptr); }
PROBE(void, Track_Isinst, (INT_PTR ptr, mdToken typeToken)) { isinst(typeToken, ptr); }

PROBE(void, Track_Box, (INT_PTR ptr)) { box(ptr); }
PROBE(void, Track_Unbox, (INT_PTR ptr, mdToken typeToken)) { unbox(typeToken, ptr); }
PROBE(void, Track_Unbox_Any, (INT_PTR ptr, mdToken typeToken)) { unbox_any(typeToken, ptr); }

PROBE(void, Track_Ldfld, (INT_PTR ptr, mdToken fieldToken)) { ldfld(fieldToken, ptr); }
PROBE(void, Track_Ldflda, (INT_PTR ptr, mdToken fieldToken)) { ldflda(fieldToken, ptr); }

/// TODO: stfld may be called with any value type! :(
PROBE(void, Track_Stfld_4, (mdToken fieldToken, INT_PTR ptr, INT32 value)) {
    if (!stfld(fieldToken, ptr)) {
        // Send command
    }
}
PROBE(void, Track_Stfld_8, (mdToken fieldToken, INT_PTR ptr, INT64 value)) {
    if (!stfld(fieldToken, ptr)) {
        // Send command
    }
}
PROBE(void, Track_Stfld_f4, (mdToken fieldToken, INT_PTR ptr, FLOAT value)) {
    if (!stfld(fieldToken, ptr)) {
        // Send command
    }
}
PROBE(void, Track_Stfld_f8, (mdToken fieldToken, INT_PTR ptr, DOUBLE value)) {
    if (!stfld(fieldToken, ptr)) {
        // Send command
    }
}
PROBE(void, Track_Stfld_p, (mdToken fieldToken, INT_PTR ptr, INT_PTR value)) {
    if (!stfld(fieldToken, ptr)) {
        // Send command
    }
}

PROBE(void, Track_Ldsfld, (mdToken fieldToken)) { ldfsld(fieldToken); }
PROBE(void, Track_Ldsflda, ()) { push1Concrete(); }
PROBE(void, Track_Stsfld, (mdToken fieldToken)) { stsfld(fieldToken); }

PROBE(COND, Track_Ldelema, (INT_PTR ptr, INT_PTR index)) { return ldelema(ptr, index); }
PROBE(COND, Track_Ldelem, (INT_PTR ptr, INT_PTR index)) { return ldelem(ptr, index); }
PROBE(void, Exec_Ldelema, (INT_PTR ptr, INT_PTR index)) { /*send command*/ }
PROBE(void, Exec_Ldelem, (INT_PTR ptr, INT_PTR index)) { /*send command*/ }

PROBE(COND, Track_Stelem, (INT_PTR ptr, INT_PTR index)) { return stelem(ptr, index); }
PROBE(void, Exec_Stelem_I, (INT_PTR ptr, INT_PTR index, INT_PTR value)) { /*send command*/ }
PROBE(void, Exec_Stelem_I1, (INT_PTR ptr, INT_PTR index, INT8 value)) { /*send command*/ }
PROBE(void, Exec_Stelem_I2, (INT_PTR ptr, INT_PTR index, INT16 value)) { /*send command*/ }
PROBE(void, Exec_Stelem_I4, (INT_PTR ptr, INT_PTR index, INT32 value)) { /*send command*/ }
PROBE(void, Exec_Stelem_I8, (INT_PTR ptr, INT_PTR index, INT64 value)) { /*send command*/ }
PROBE(void, Exec_Stelem_R4, (INT_PTR ptr, INT_PTR index, FLOAT value)) { /*send command*/ }
PROBE(void, Exec_Stelem_R8, (INT_PTR ptr, INT_PTR index, DOUBLE value)) { /*send command*/ }
PROBE(void, Exec_Stelem_Ref, (INT_PTR ptr, INT_PTR index, INT_PTR value)) { /*send command*/ }

PROBE(void, Track_Ckfinite, ()) { ckfinite(); }
PROBE(void, Track_Sizeof, ()) { push1Concrete(); }
PROBE(void, Track_Ldftn, ()) { push1Concrete(); }
PROBE(void, Track_Ldvirtftn, (INT_PTR ptr, mdToken token)) { ldvirtftn(token, ptr); }
PROBE(void, Track_Arglist, ()) { push1Concrete(); }
PROBE(void, Track_Mkrefany, ()) { mkrefany(); }

PROBE(void, Track_Enter, (mdMethodDef token, unsigned maxStackSize)) { enter(token, maxStackSize); }
PROBE(void, Track_Leave, (UINT8 returnValues)) { leave(returnValues); }
PROBE(void, Finalize_Call, (UINT8 returnValues)) { finalizeCall(returnValues); }
PROBE(void, Track_Call, (mdMethodDef token, UINT16 count)) { call(token, count); }
PROBE(void, Track_CallVirt, (UINT16 count)) { callVirt(count); }
PROBE(void, Track_Newobj, (INT_PTR ptr)) { alloc(ptr); }
PROBE(void, Track_Calli, (mdSignature signature)) { calli(signature); }

PROBE(void, Track_Throw, ()) { execThrow(); }
PROBE(void, Track_Rethrow, ()) { execRethrow(); }

PROBE(void, Mem2_4, (INT32 arg1, INT32 arg2)) { clear_mem(); mem_i4(arg1); mem_i4(arg2); }
PROBE(void, Mem2_8, (INT64 arg1, INT64 arg2)) { clear_mem(); mem_i8(arg1); mem_i8(arg2); }
PROBE(void, Mem2_f4, (FLOAT arg1, FLOAT arg2)) { clear_mem(); mem_f4(arg1); mem_f4(arg2); }
PROBE(void, Mem2_f8, (DOUBLE arg1, DOUBLE arg2)) { clear_mem(); mem_f8(arg1); mem_f8(arg2); }
PROBE(void, Mem2_p, (INT_PTR arg1, INT_PTR arg2)) { clear_mem(); mem_p(arg1); mem_p(arg2); }
PROBE(void, Mem2_4_p, (INT32 arg1, INT_PTR arg2)) { clear_mem(); mem_i4(arg1); mem_p(arg2); }
PROBE(void, Mem2_p_1, (INT_PTR arg1, INT8 arg2)) { clear_mem(); mem_p(arg1); mem_i1(arg2); }
PROBE(void, Mem2_p_2, (INT_PTR arg1, INT16 arg2)) { clear_mem(); mem_p(arg1); mem_i2(arg2); }
PROBE(void, Mem2_p_4, (INT_PTR arg1, INT32 arg2)) { clear_mem(); mem_p(arg1); mem_i4(arg2); }
PROBE(void, Mem2_p_8, (INT_PTR arg1, INT64 arg2)) { clear_mem(); mem_p(arg1); mem_i8(arg2); }
PROBE(void, Mem2_p_f4, (INT_PTR arg1, FLOAT arg2)) { clear_mem(); mem_p(arg1); mem_f4(arg2); }
PROBE(void, Mem2_p_f8, (INT_PTR arg1, DOUBLE arg2)) { clear_mem(); mem_p(arg1); mem_f8(arg2); }

PROBE(void, Mem3_p_p_p, (INT_PTR arg1, INT_PTR arg2, INT_PTR arg3)) { clear_mem(); mem_p(arg1); mem_p(arg2); mem_p(arg3); }
PROBE(void, Mem3_p_p_i1, (INT_PTR arg1, INT_PTR arg2, INT8 arg3)) { clear_mem(); mem_p(arg1); mem_p(arg2); mem_i1(arg3); }
PROBE(void, Mem3_p_p_i2, (INT_PTR arg1, INT_PTR arg2, INT16 arg3)) { clear_mem(); mem_p(arg1); mem_p(arg2); mem_i2(arg3); }
PROBE(void, Mem3_p_p_i4, (INT_PTR arg1, INT_PTR arg2, INT32 arg3)) { clear_mem(); mem_p(arg1); mem_p(arg2); mem_i4(arg3); }
PROBE(void, Mem3_p_p_i8, (INT_PTR arg1, INT_PTR arg2, INT64 arg3)) { clear_mem(); mem_p(arg1); mem_p(arg2); mem_i8(arg3); }
PROBE(void, Mem3_p_p_f4, (INT_PTR arg1, INT_PTR arg2, FLOAT arg3)) { clear_mem(); mem_p(arg1); mem_p(arg2); mem_f4(arg3); }
PROBE(void, Mem3_p_p_f8, (INT_PTR arg1, INT_PTR arg2, DOUBLE arg3)) { clear_mem(); mem_p(arg1); mem_p(arg2); mem_f8(arg3); }
PROBE(void, Mem3_p_i1_p, (INT_PTR arg1, INT8 arg2, INT_PTR arg3)) { clear_mem(); mem_p(arg1); mem_i1(arg2); mem_p(arg3); }

PROBE(INT8, Unmem_1, (INT8 idx)) { return unmem_i1(idx); }
PROBE(INT16, Unmem_2, (INT8 idx)) { return unmem_i2(idx); }
PROBE(INT32, Unmem_4, (INT8 idx)) { return unmem_i4(idx); }
PROBE(INT64, Unmem_8, (INT8 idx)) { return unmem_i8(idx); }
PROBE(FLOAT, Unmem_f4, (INT8 idx)) { return unmem_f4(idx); }
PROBE(DOUBLE, Unmem_f8, (INT8 idx)) { return unmem_f8(idx); }
PROBE(INT_PTR, Unmem_p, (INT8 idx)) { return unmem_p(idx); }

PROBE(void, DumpInstruction, (UINT32 index)) {
    const char *&s = stringsPool[index];
    if (!s) {
        ERROR(tout << "Pool doesn't contain string with index " << index);
    } else {
        LOG(tout << "Executing " << s << std::endl);
    }
}

}

#endif // PROBES_H_
