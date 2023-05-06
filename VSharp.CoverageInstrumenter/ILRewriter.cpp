// Copyright (c) .NET Foundation and contributors. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#include "ILRewriter.h"
#include <corhlpr.cpp>

    /////////////////////////////////////////////////////////////////////////////////////////////////
    //
    // I M P O R T
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////


typedef enum
{
#define OPDEF(c,s,pop,push,args,type,l,s1,s2,ctrl) c,
#include "opcode.def"
#undef OPDEF
    CEE_COUNT,
    CEE_SWITCH_ARG, // special internal instructions
} OPCODE;

#define OPCODEFLAGS_SizeMask        0x0F
#define OPCODEFLAGS_BranchTarget    0x10
#define OPCODEFLAGS_Switch          0x20

#define OPDEF(c,s,pop,push,args,type,l,s1,s2,ctrl) if (opcode == c) return s;

const char* opcodetostr(unsigned int opcode) {
    OPDEF(CEE_NOP,                        "nop",              Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0x00,    NEXT)
    OPDEF(CEE_BREAK,                      "break",            Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0x01,    BREAK)
    OPDEF(CEE_LDARG_0,                    "ldarg.0",          Pop0,               Push1,       InlineNone,         IMacro,      1,  0xFF,    0x02,    NEXT)
    OPDEF(CEE_LDARG_1,                    "ldarg.1",          Pop0,               Push1,       InlineNone,         IMacro,      1,  0xFF,    0x03,    NEXT)
    OPDEF(CEE_LDARG_2,                    "ldarg.2",          Pop0,               Push1,       InlineNone,         IMacro,      1,  0xFF,    0x04,    NEXT)
    OPDEF(CEE_LDARG_3,                    "ldarg.3",          Pop0,               Push1,       InlineNone,         IMacro,      1,  0xFF,    0x05,    NEXT)
    OPDEF(CEE_LDLOC_0,                    "ldloc.0",          Pop0,               Push1,       InlineNone,         IMacro,      1,  0xFF,    0x06,    NEXT)
    OPDEF(CEE_LDLOC_1,                    "ldloc.1",          Pop0,               Push1,       InlineNone,         IMacro,      1,  0xFF,    0x07,    NEXT)
    OPDEF(CEE_LDLOC_2,                    "ldloc.2",          Pop0,               Push1,       InlineNone,         IMacro,      1,  0xFF,    0x08,    NEXT)
    OPDEF(CEE_LDLOC_3,                    "ldloc.3",          Pop0,               Push1,       InlineNone,         IMacro,      1,  0xFF,    0x09,    NEXT)
    OPDEF(CEE_STLOC_0,                    "stloc.0",          Pop1,               Push0,       InlineNone,         IMacro,      1,  0xFF,    0x0A,    NEXT)
    OPDEF(CEE_STLOC_1,                    "stloc.1",          Pop1,               Push0,       InlineNone,         IMacro,      1,  0xFF,    0x0B,    NEXT)
    OPDEF(CEE_STLOC_2,                    "stloc.2",          Pop1,               Push0,       InlineNone,         IMacro,      1,  0xFF,    0x0C,    NEXT)
    OPDEF(CEE_STLOC_3,                    "stloc.3",          Pop1,               Push0,       InlineNone,         IMacro,      1,  0xFF,    0x0D,    NEXT)
    OPDEF(CEE_LDARG_S,                    "ldarg.s",          Pop0,               Push1,       ShortInlineVar,     IMacro,      1,  0xFF,    0x0E,    NEXT)
    OPDEF(CEE_LDARGA_S,                   "ldarga.s",         Pop0,               PushI,       ShortInlineVar,     IMacro,      1,  0xFF,    0x0F,    NEXT)
    OPDEF(CEE_STARG_S,                    "starg.s",          Pop1,               Push0,       ShortInlineVar,     IMacro,      1,  0xFF,    0x10,    NEXT)
    OPDEF(CEE_LDLOC_S,                    "ldloc.s",          Pop0,               Push1,       ShortInlineVar,     IMacro,      1,  0xFF,    0x11,    NEXT)
    OPDEF(CEE_LDLOCA_S,                   "ldloca.s",         Pop0,               PushI,       ShortInlineVar,     IMacro,      1,  0xFF,    0x12,    NEXT)
    OPDEF(CEE_STLOC_S,                    "stloc.s",          Pop1,               Push0,       ShortInlineVar,     IMacro,      1,  0xFF,    0x13,    NEXT)
    OPDEF(CEE_LDNULL,                     "ldnull",           Pop0,               PushRef,     InlineNone,         IPrimitive,  1,  0xFF,    0x14,    NEXT)
    OPDEF(CEE_LDC_I4_M1,                  "ldc.i4.m1",        Pop0,               PushI,       InlineNone,         IMacro,      1,  0xFF,    0x15,    NEXT)
    OPDEF(CEE_LDC_I4_0,                   "ldc.i4.0",         Pop0,               PushI,       InlineNone,         IMacro,      1,  0xFF,    0x16,    NEXT)
    OPDEF(CEE_LDC_I4_1,                   "ldc.i4.1",         Pop0,               PushI,       InlineNone,         IMacro,      1,  0xFF,    0x17,    NEXT)
    OPDEF(CEE_LDC_I4_2,                   "ldc.i4.2",         Pop0,               PushI,       InlineNone,         IMacro,      1,  0xFF,    0x18,    NEXT)
    OPDEF(CEE_LDC_I4_3,                   "ldc.i4.3",         Pop0,               PushI,       InlineNone,         IMacro,      1,  0xFF,    0x19,    NEXT)
    OPDEF(CEE_LDC_I4_4,                   "ldc.i4.4",         Pop0,               PushI,       InlineNone,         IMacro,      1,  0xFF,    0x1A,    NEXT)
    OPDEF(CEE_LDC_I4_5,                   "ldc.i4.5",         Pop0,               PushI,       InlineNone,         IMacro,      1,  0xFF,    0x1B,    NEXT)
    OPDEF(CEE_LDC_I4_6,                   "ldc.i4.6",         Pop0,               PushI,       InlineNone,         IMacro,      1,  0xFF,    0x1C,    NEXT)
    OPDEF(CEE_LDC_I4_7,                   "ldc.i4.7",         Pop0,               PushI,       InlineNone,         IMacro,      1,  0xFF,    0x1D,    NEXT)
    OPDEF(CEE_LDC_I4_8,                   "ldc.i4.8",         Pop0,               PushI,       InlineNone,         IMacro,      1,  0xFF,    0x1E,    NEXT)
    OPDEF(CEE_LDC_I4_S,                   "ldc.i4.s",         Pop0,               PushI,       ShortInlineI,       IMacro,      1,  0xFF,    0x1F,    NEXT)
    OPDEF(CEE_LDC_I4,                     "ldc.i4",           Pop0,               PushI,       InlineI,            IPrimitive,  1,  0xFF,    0x20,    NEXT)
    OPDEF(CEE_LDC_I8,                     "ldc.i8",           Pop0,               PushI8,      InlineI8,           IPrimitive,  1,  0xFF,    0x21,    NEXT)
    OPDEF(CEE_LDC_R4,                     "ldc.r4",           Pop0,               PushR4,      ShortInlineR,       IPrimitive,  1,  0xFF,    0x22,    NEXT)
    OPDEF(CEE_LDC_R8,                     "ldc.r8",           Pop0,               PushR8,      InlineR,            IPrimitive,  1,  0xFF,    0x23,    NEXT)
    OPDEF(CEE_UNUSED49,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0x24,    NEXT)
    OPDEF(CEE_DUP,                        "dup",              Pop1,               Push1+Push1, InlineNone,         IPrimitive,  1,  0xFF,    0x25,    NEXT)
    OPDEF(CEE_POP,                        "pop",              Pop1,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0x26,    NEXT)
    OPDEF(CEE_JMP,                        "jmp",              Pop0,               Push0,       InlineMethod,       IPrimitive,  1,  0xFF,    0x27,    CALL)
    OPDEF(CEE_CALL,                       "call",             VarPop,             VarPush,     InlineMethod,       IPrimitive,  1,  0xFF,    0x28,    CALL)
    OPDEF(CEE_CALLI,                      "calli",            VarPop,             VarPush,     InlineSig,          IPrimitive,  1,  0xFF,    0x29,    CALL)
    OPDEF(CEE_RET,                        "ret",              VarPop,             Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0x2A,    RETURN)
    OPDEF(CEE_BR_S,                       "br.s",             Pop0,               Push0,       ShortInlineBrTarget,IMacro,      1,  0xFF,    0x2B,    BRANCH)
    OPDEF(CEE_BRFALSE_S,                  "brfalse.s",        PopI,               Push0,       ShortInlineBrTarget,IMacro,      1,  0xFF,    0x2C,    COND_BRANCH)
    OPDEF(CEE_BRTRUE_S,                   "brtrue.s",         PopI,               Push0,       ShortInlineBrTarget,IMacro,      1,  0xFF,    0x2D,    COND_BRANCH)
    OPDEF(CEE_BEQ_S,                      "beq.s",            Pop1+Pop1,          Push0,       ShortInlineBrTarget,IMacro,      1,  0xFF,    0x2E,    COND_BRANCH)
    OPDEF(CEE_BGE_S,                      "bge.s",            Pop1+Pop1,          Push0,       ShortInlineBrTarget,IMacro,      1,  0xFF,    0x2F,    COND_BRANCH)
    OPDEF(CEE_BGT_S,                      "bgt.s",            Pop1+Pop1,          Push0,       ShortInlineBrTarget,IMacro,      1,  0xFF,    0x30,    COND_BRANCH)
    OPDEF(CEE_BLE_S,                      "ble.s",            Pop1+Pop1,          Push0,       ShortInlineBrTarget,IMacro,      1,  0xFF,    0x31,    COND_BRANCH)
    OPDEF(CEE_BLT_S,                      "blt.s",            Pop1+Pop1,          Push0,       ShortInlineBrTarget,IMacro,      1,  0xFF,    0x32,    COND_BRANCH)
    OPDEF(CEE_BNE_UN_S,                   "bne.un.s",         Pop1+Pop1,          Push0,       ShortInlineBrTarget,IMacro,      1,  0xFF,    0x33,    COND_BRANCH)
    OPDEF(CEE_BGE_UN_S,                   "bge.un.s",         Pop1+Pop1,          Push0,       ShortInlineBrTarget,IMacro,      1,  0xFF,    0x34,    COND_BRANCH)
    OPDEF(CEE_BGT_UN_S,                   "bgt.un.s",         Pop1+Pop1,          Push0,       ShortInlineBrTarget,IMacro,      1,  0xFF,    0x35,    COND_BRANCH)
    OPDEF(CEE_BLE_UN_S,                   "ble.un.s",         Pop1+Pop1,          Push0,       ShortInlineBrTarget,IMacro,      1,  0xFF,    0x36,    COND_BRANCH)
    OPDEF(CEE_BLT_UN_S,                   "blt.un.s",         Pop1+Pop1,          Push0,       ShortInlineBrTarget,IMacro,      1,  0xFF,    0x37,    COND_BRANCH)
    OPDEF(CEE_BR,                         "br",               Pop0,               Push0,       InlineBrTarget,     IPrimitive,  1,  0xFF,    0x38,    BRANCH)
    OPDEF(CEE_BRFALSE,                    "brfalse",          PopI,               Push0,       InlineBrTarget,     IPrimitive,  1,  0xFF,    0x39,    COND_BRANCH)
    OPDEF(CEE_BRTRUE,                     "brtrue",           PopI,               Push0,       InlineBrTarget,     IPrimitive,  1,  0xFF,    0x3A,    COND_BRANCH)
    OPDEF(CEE_BEQ,                        "beq",              Pop1+Pop1,          Push0,       InlineBrTarget,     IMacro,      1,  0xFF,    0x3B,    COND_BRANCH)
    OPDEF(CEE_BGE,                        "bge",              Pop1+Pop1,          Push0,       InlineBrTarget,     IMacro,      1,  0xFF,    0x3C,    COND_BRANCH)
    OPDEF(CEE_BGT,                        "bgt",              Pop1+Pop1,          Push0,       InlineBrTarget,     IMacro,      1,  0xFF,    0x3D,    COND_BRANCH)
    OPDEF(CEE_BLE,                        "ble",              Pop1+Pop1,          Push0,       InlineBrTarget,     IMacro,      1,  0xFF,    0x3E,    COND_BRANCH)
    OPDEF(CEE_BLT,                        "blt",              Pop1+Pop1,          Push0,       InlineBrTarget,     IMacro,      1,  0xFF,    0x3F,    COND_BRANCH)
    OPDEF(CEE_BNE_UN,                     "bne.un",           Pop1+Pop1,          Push0,       InlineBrTarget,     IMacro,      1,  0xFF,    0x40,    COND_BRANCH)
    OPDEF(CEE_BGE_UN,                     "bge.un",           Pop1+Pop1,          Push0,       InlineBrTarget,     IMacro,      1,  0xFF,    0x41,    COND_BRANCH)
    OPDEF(CEE_BGT_UN,                     "bgt.un",           Pop1+Pop1,          Push0,       InlineBrTarget,     IMacro,      1,  0xFF,    0x42,    COND_BRANCH)
    OPDEF(CEE_BLE_UN,                     "ble.un",           Pop1+Pop1,          Push0,       InlineBrTarget,     IMacro,      1,  0xFF,    0x43,    COND_BRANCH)
    OPDEF(CEE_BLT_UN,                     "blt.un",           Pop1+Pop1,          Push0,       InlineBrTarget,     IMacro,      1,  0xFF,    0x44,    COND_BRANCH)
    OPDEF(CEE_SWITCH,                     "switch",           PopI,               Push0,       InlineSwitch,       IPrimitive,  1,  0xFF,    0x45,    COND_BRANCH)
    OPDEF(CEE_LDIND_I1,                   "ldind.i1",         PopI,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0x46,    NEXT)
    OPDEF(CEE_LDIND_U1,                   "ldind.u1",         PopI,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0x47,    NEXT)
    OPDEF(CEE_LDIND_I2,                   "ldind.i2",         PopI,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0x48,    NEXT)
    OPDEF(CEE_LDIND_U2,                   "ldind.u2",         PopI,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0x49,    NEXT)
    OPDEF(CEE_LDIND_I4,                   "ldind.i4",         PopI,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0x4A,    NEXT)
    OPDEF(CEE_LDIND_U4,                   "ldind.u4",         PopI,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0x4B,    NEXT)
    OPDEF(CEE_LDIND_I8,                   "ldind.i8",         PopI,               PushI8,      InlineNone,         IPrimitive,  1,  0xFF,    0x4C,    NEXT)
    OPDEF(CEE_LDIND_I,                    "ldind.i",          PopI,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0x4D,    NEXT)
    OPDEF(CEE_LDIND_R4,                   "ldind.r4",         PopI,               PushR4,      InlineNone,         IPrimitive,  1,  0xFF,    0x4E,    NEXT)
    OPDEF(CEE_LDIND_R8,                   "ldind.r8",         PopI,               PushR8,      InlineNone,         IPrimitive,  1,  0xFF,    0x4F,    NEXT)
    OPDEF(CEE_LDIND_REF,                  "ldind.ref",        PopI,               PushRef,     InlineNone,         IPrimitive,  1,  0xFF,    0x50,    NEXT)
    OPDEF(CEE_STIND_REF,                  "stind.ref",        PopI+PopI,          Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0x51,    NEXT)
    OPDEF(CEE_STIND_I1,                   "stind.i1",         PopI+PopI,          Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0x52,    NEXT)
    OPDEF(CEE_STIND_I2,                   "stind.i2",         PopI+PopI,          Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0x53,    NEXT)
    OPDEF(CEE_STIND_I4,                   "stind.i4",         PopI+PopI,          Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0x54,    NEXT)
    OPDEF(CEE_STIND_I8,                   "stind.i8",         PopI+PopI8,         Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0x55,    NEXT)
    OPDEF(CEE_STIND_R4,                   "stind.r4",         PopI+PopR4,         Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0x56,    NEXT)
    OPDEF(CEE_STIND_R8,                   "stind.r8",         PopI+PopR8,         Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0x57,    NEXT)
    OPDEF(CEE_ADD,                        "add",              Pop1+Pop1,          Push1,       InlineNone,         IPrimitive,  1,  0xFF,    0x58,    NEXT)
    OPDEF(CEE_SUB,                        "sub",              Pop1+Pop1,          Push1,       InlineNone,         IPrimitive,  1,  0xFF,    0x59,    NEXT)
    OPDEF(CEE_MUL,                        "mul",              Pop1+Pop1,          Push1,       InlineNone,         IPrimitive,  1,  0xFF,    0x5A,    NEXT)
    OPDEF(CEE_DIV,                        "div",              Pop1+Pop1,          Push1,       InlineNone,         IPrimitive,  1,  0xFF,    0x5B,    NEXT)
    OPDEF(CEE_DIV_UN,                     "div.un",           Pop1+Pop1,          Push1,       InlineNone,         IPrimitive,  1,  0xFF,    0x5C,    NEXT)
    OPDEF(CEE_REM,                        "rem",              Pop1+Pop1,          Push1,       InlineNone,         IPrimitive,  1,  0xFF,    0x5D,    NEXT)
    OPDEF(CEE_REM_UN,                     "rem.un",           Pop1+Pop1,          Push1,       InlineNone,         IPrimitive,  1,  0xFF,    0x5E,    NEXT)
    OPDEF(CEE_AND,                        "and",              Pop1+Pop1,          Push1,       InlineNone,         IPrimitive,  1,  0xFF,    0x5F,    NEXT)
    OPDEF(CEE_OR,                         "or",               Pop1+Pop1,          Push1,       InlineNone,         IPrimitive,  1,  0xFF,    0x60,    NEXT)
    OPDEF(CEE_XOR,                        "xor",              Pop1+Pop1,          Push1,       InlineNone,         IPrimitive,  1,  0xFF,    0x61,    NEXT)
    OPDEF(CEE_SHL,                        "shl",              Pop1+Pop1,          Push1,       InlineNone,         IPrimitive,  1,  0xFF,    0x62,    NEXT)
    OPDEF(CEE_SHR,                        "shr",              Pop1+Pop1,          Push1,       InlineNone,         IPrimitive,  1,  0xFF,    0x63,    NEXT)
    OPDEF(CEE_SHR_UN,                     "shr.un",           Pop1+Pop1,          Push1,       InlineNone,         IPrimitive,  1,  0xFF,    0x64,    NEXT)
    OPDEF(CEE_NEG,                        "neg",              Pop1,               Push1,       InlineNone,         IPrimitive,  1,  0xFF,    0x65,    NEXT)
    OPDEF(CEE_NOT,                        "not",              Pop1,               Push1,       InlineNone,         IPrimitive,  1,  0xFF,    0x66,    NEXT)
    OPDEF(CEE_CONV_I1,                    "conv.i1",          Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0x67,    NEXT)
    OPDEF(CEE_CONV_I2,                    "conv.i2",          Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0x68,    NEXT)
    OPDEF(CEE_CONV_I4,                    "conv.i4",          Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0x69,    NEXT)
    OPDEF(CEE_CONV_I8,                    "conv.i8",          Pop1,               PushI8,      InlineNone,         IPrimitive,  1,  0xFF,    0x6A,    NEXT)
    OPDEF(CEE_CONV_R4,                    "conv.r4",          Pop1,               PushR4,      InlineNone,         IPrimitive,  1,  0xFF,    0x6B,    NEXT)
    OPDEF(CEE_CONV_R8,                    "conv.r8",          Pop1,               PushR8,      InlineNone,         IPrimitive,  1,  0xFF,    0x6C,    NEXT)
    OPDEF(CEE_CONV_U4,                    "conv.u4",          Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0x6D,    NEXT)
    OPDEF(CEE_CONV_U8,                    "conv.u8",          Pop1,               PushI8,      InlineNone,         IPrimitive,  1,  0xFF,    0x6E,    NEXT)
    OPDEF(CEE_CALLVIRT,                   "callvirt",         VarPop,             VarPush,     InlineMethod,       IObjModel,   1,  0xFF,    0x6F,    CALL)
    OPDEF(CEE_CPOBJ,                      "cpobj",            PopI+PopI,          Push0,       InlineType,         IObjModel,   1,  0xFF,    0x70,    NEXT)
    OPDEF(CEE_LDOBJ,                      "ldobj",            PopI,               Push1,       InlineType,         IObjModel,   1,  0xFF,    0x71,    NEXT)
    OPDEF(CEE_LDSTR,                      "ldstr",            Pop0,               PushRef,     InlineString,       IObjModel,   1,  0xFF,    0x72,    NEXT)
    OPDEF(CEE_NEWOBJ,                     "newobj",           VarPop,             PushRef,     InlineMethod,       IObjModel,   1,  0xFF,    0x73,    CALL)
    OPDEF(CEE_CASTCLASS,                  "castclass",        PopRef,             PushRef,     InlineType,         IObjModel,   1,  0xFF,    0x74,    NEXT)
    OPDEF(CEE_ISINST,                     "isinst",           PopRef,             PushI,       InlineType,         IObjModel,   1,  0xFF,    0x75,    NEXT)
    OPDEF(CEE_CONV_R_UN,                  "conv.r.un",        Pop1,               PushR8,      InlineNone,         IPrimitive,  1,  0xFF,    0x76,    NEXT)
    OPDEF(CEE_UNUSED58,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0x77,    NEXT)
    OPDEF(CEE_UNUSED1,                    "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0x78,    NEXT)
    OPDEF(CEE_UNBOX,                      "unbox",            PopRef,             PushI,       InlineType,         IPrimitive,  1,  0xFF,    0x79,    NEXT)
    OPDEF(CEE_THROW,                      "throw",            PopRef,             Push0,       InlineNone,         IObjModel,   1,  0xFF,    0x7A,    THROW)
    OPDEF(CEE_LDFLD,                      "ldfld",            PopRef,             Push1,       InlineField,        IObjModel,   1,  0xFF,    0x7B,    NEXT)
    OPDEF(CEE_LDFLDA,                     "ldflda",           PopRef,             PushI,       InlineField,        IObjModel,   1,  0xFF,    0x7C,    NEXT)
    OPDEF(CEE_STFLD,                      "stfld",            PopRef+Pop1,        Push0,       InlineField,        IObjModel,   1,  0xFF,    0x7D,    NEXT)
    OPDEF(CEE_LDSFLD,                     "ldsfld",           Pop0,               Push1,       InlineField,        IObjModel,   1,  0xFF,    0x7E,    NEXT)
    OPDEF(CEE_LDSFLDA,                    "ldsflda",          Pop0,               PushI,       InlineField,        IObjModel,   1,  0xFF,    0x7F,    NEXT)
    OPDEF(CEE_STSFLD,                     "stsfld",           Pop1,               Push0,       InlineField,        IObjModel,   1,  0xFF,    0x80,    NEXT)
    OPDEF(CEE_STOBJ,                      "stobj",            PopI+Pop1,          Push0,       InlineType,         IPrimitive,  1,  0xFF,    0x81,    NEXT)
    OPDEF(CEE_CONV_OVF_I1_UN,             "conv.ovf.i1.un",   Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0x82,    NEXT)
    OPDEF(CEE_CONV_OVF_I2_UN,             "conv.ovf.i2.un",   Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0x83,    NEXT)
    OPDEF(CEE_CONV_OVF_I4_UN,             "conv.ovf.i4.un",   Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0x84,    NEXT)
    OPDEF(CEE_CONV_OVF_I8_UN,             "conv.ovf.i8.un",   Pop1,               PushI8,      InlineNone,         IPrimitive,  1,  0xFF,    0x85,    NEXT)
    OPDEF(CEE_CONV_OVF_U1_UN,             "conv.ovf.u1.un",   Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0x86,    NEXT)
    OPDEF(CEE_CONV_OVF_U2_UN,             "conv.ovf.u2.un",   Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0x87,    NEXT)
    OPDEF(CEE_CONV_OVF_U4_UN,             "conv.ovf.u4.un",   Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0x88,    NEXT)
    OPDEF(CEE_CONV_OVF_U8_UN,             "conv.ovf.u8.un",   Pop1,               PushI8,      InlineNone,         IPrimitive,  1,  0xFF,    0x89,    NEXT)
    OPDEF(CEE_CONV_OVF_I_UN,              "conv.ovf.i.un",    Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0x8A,    NEXT)
    OPDEF(CEE_CONV_OVF_U_UN,              "conv.ovf.u.un",    Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0x8B,    NEXT)
    OPDEF(CEE_BOX,                        "box",              Pop1,               PushRef,     InlineType,         IPrimitive,  1,  0xFF,    0x8C,    NEXT)
    OPDEF(CEE_NEWARR,                     "newarr",           PopI,               PushRef,     InlineType,         IObjModel,   1,  0xFF,    0x8D,    NEXT)
    OPDEF(CEE_LDLEN,                      "ldlen",            PopRef,             PushI,       InlineNone,         IObjModel,   1,  0xFF,    0x8E,    NEXT)
    OPDEF(CEE_LDELEMA,                    "ldelema",          PopRef+PopI,        PushI,       InlineType,         IObjModel,   1,  0xFF,    0x8F,    NEXT)
    OPDEF(CEE_LDELEM_I1,                  "ldelem.i1",        PopRef+PopI,        PushI,       InlineNone,         IObjModel,   1,  0xFF,    0x90,    NEXT)
    OPDEF(CEE_LDELEM_U1,                  "ldelem.u1",        PopRef+PopI,        PushI,       InlineNone,         IObjModel,   1,  0xFF,    0x91,    NEXT)
    OPDEF(CEE_LDELEM_I2,                  "ldelem.i2",        PopRef+PopI,        PushI,       InlineNone,         IObjModel,   1,  0xFF,    0x92,    NEXT)
    OPDEF(CEE_LDELEM_U2,                  "ldelem.u2",        PopRef+PopI,        PushI,       InlineNone,         IObjModel,   1,  0xFF,    0x93,    NEXT)
    OPDEF(CEE_LDELEM_I4,                  "ldelem.i4",        PopRef+PopI,        PushI,       InlineNone,         IObjModel,   1,  0xFF,    0x94,    NEXT)
    OPDEF(CEE_LDELEM_U4,                  "ldelem.u4",        PopRef+PopI,        PushI,       InlineNone,         IObjModel,   1,  0xFF,    0x95,    NEXT)
    OPDEF(CEE_LDELEM_I8,                  "ldelem.i8",        PopRef+PopI,        PushI8,      InlineNone,         IObjModel,   1,  0xFF,    0x96,    NEXT)
    OPDEF(CEE_LDELEM_I,                   "ldelem.i",         PopRef+PopI,        PushI,       InlineNone,         IObjModel,   1,  0xFF,    0x97,    NEXT)
    OPDEF(CEE_LDELEM_R4,                  "ldelem.r4",        PopRef+PopI,        PushR4,      InlineNone,         IObjModel,   1,  0xFF,    0x98,    NEXT)
    OPDEF(CEE_LDELEM_R8,                  "ldelem.r8",        PopRef+PopI,        PushR8,      InlineNone,         IObjModel,   1,  0xFF,    0x99,    NEXT)
    OPDEF(CEE_LDELEM_REF,                 "ldelem.ref",       PopRef+PopI,        PushRef,     InlineNone,         IObjModel,   1,  0xFF,    0x9A,    NEXT)
    OPDEF(CEE_STELEM_I,                   "stelem.i",         PopRef+PopI+PopI,   Push0,       InlineNone,         IObjModel,   1,  0xFF,    0x9B,    NEXT)
    OPDEF(CEE_STELEM_I1,                  "stelem.i1",        PopRef+PopI+PopI,   Push0,       InlineNone,         IObjModel,   1,  0xFF,    0x9C,    NEXT)
    OPDEF(CEE_STELEM_I2,                  "stelem.i2",        PopRef+PopI+PopI,   Push0,       InlineNone,         IObjModel,   1,  0xFF,    0x9D,    NEXT)
    OPDEF(CEE_STELEM_I4,                  "stelem.i4",        PopRef+PopI+PopI,   Push0,       InlineNone,         IObjModel,   1,  0xFF,    0x9E,    NEXT)
    OPDEF(CEE_STELEM_I8,                  "stelem.i8",        PopRef+PopI+PopI8,  Push0,       InlineNone,         IObjModel,   1,  0xFF,    0x9F,    NEXT)
    OPDEF(CEE_STELEM_R4,                  "stelem.r4",        PopRef+PopI+PopR4,  Push0,       InlineNone,         IObjModel,   1,  0xFF,    0xA0,    NEXT)
    OPDEF(CEE_STELEM_R8,                  "stelem.r8",        PopRef+PopI+PopR8,  Push0,       InlineNone,         IObjModel,   1,  0xFF,    0xA1,    NEXT)
    OPDEF(CEE_STELEM_REF,                 "stelem.ref",       PopRef+PopI+PopRef, Push0,       InlineNone,         IObjModel,   1,  0xFF,    0xA2,    NEXT)
    OPDEF(CEE_LDELEM,                     "ldelem",           PopRef+PopI,        Push1,       InlineType,         IObjModel,   1,  0xFF,    0xA3,    NEXT)
    OPDEF(CEE_STELEM,                     "stelem",           PopRef+PopI+Pop1,   Push0,       InlineType,         IObjModel,   1,  0xFF,    0xA4,    NEXT)
    OPDEF(CEE_UNBOX_ANY,                  "unbox.any",        PopRef,             Push1,       InlineType,         IObjModel,   1,  0xFF,    0xA5,    NEXT)
    OPDEF(CEE_UNUSED5,                    "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xA6,    NEXT)
    OPDEF(CEE_UNUSED6,                    "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xA7,    NEXT)
    OPDEF(CEE_UNUSED7,                    "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xA8,    NEXT)
    OPDEF(CEE_UNUSED8,                    "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xA9,    NEXT)
    OPDEF(CEE_UNUSED9,                    "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xAA,    NEXT)
    OPDEF(CEE_UNUSED10,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xAB,    NEXT)
    OPDEF(CEE_UNUSED11,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xAC,    NEXT)
    OPDEF(CEE_UNUSED12,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xAD,    NEXT)
    OPDEF(CEE_UNUSED13,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xAE,    NEXT)
    OPDEF(CEE_UNUSED14,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xAF,    NEXT)
    OPDEF(CEE_UNUSED15,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xB0,    NEXT)
    OPDEF(CEE_UNUSED16,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xB1,    NEXT)
    OPDEF(CEE_UNUSED17,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xB2,    NEXT)
    OPDEF(CEE_CONV_OVF_I1,                "conv.ovf.i1",      Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0xB3,    NEXT)
    OPDEF(CEE_CONV_OVF_U1,                "conv.ovf.u1",      Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0xB4,    NEXT)
    OPDEF(CEE_CONV_OVF_I2,                "conv.ovf.i2",      Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0xB5,    NEXT)
    OPDEF(CEE_CONV_OVF_U2,                "conv.ovf.u2",      Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0xB6,    NEXT)
    OPDEF(CEE_CONV_OVF_I4,                "conv.ovf.i4",      Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0xB7,    NEXT)
    OPDEF(CEE_CONV_OVF_U4,                "conv.ovf.u4",      Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0xB8,    NEXT)
    OPDEF(CEE_CONV_OVF_I8,                "conv.ovf.i8",      Pop1,               PushI8,      InlineNone,         IPrimitive,  1,  0xFF,    0xB9,    NEXT)
    OPDEF(CEE_CONV_OVF_U8,                "conv.ovf.u8",      Pop1,               PushI8,      InlineNone,         IPrimitive,  1,  0xFF,    0xBA,    NEXT)
    OPDEF(CEE_UNUSED50,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xBB,    NEXT)
    OPDEF(CEE_UNUSED18,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xBC,    NEXT)
    OPDEF(CEE_UNUSED19,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xBD,    NEXT)
    OPDEF(CEE_UNUSED20,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xBE,    NEXT)
    OPDEF(CEE_UNUSED21,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xBF,    NEXT)
    OPDEF(CEE_UNUSED22,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xC0,    NEXT)
    OPDEF(CEE_UNUSED23,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xC1,    NEXT)
    OPDEF(CEE_REFANYVAL,                  "refanyval",        Pop1,               PushI,       InlineType,         IPrimitive,  1,  0xFF,    0xC2,    NEXT)
    OPDEF(CEE_CKFINITE,                   "ckfinite",         Pop1,               PushR8,      InlineNone,         IPrimitive,  1,  0xFF,    0xC3,    NEXT)
    OPDEF(CEE_UNUSED24,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xC4,    NEXT)
    OPDEF(CEE_UNUSED25,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xC5,    NEXT)
    OPDEF(CEE_MKREFANY,                   "mkrefany",         PopI,               Push1,       InlineType,         IPrimitive,  1,  0xFF,    0xC6,    NEXT)
    OPDEF(CEE_UNUSED59,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xC7,    NEXT)
    OPDEF(CEE_UNUSED60,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xC8,    NEXT)
    OPDEF(CEE_UNUSED61,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xC9,    NEXT)
    OPDEF(CEE_UNUSED62,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xCA,    NEXT)
    OPDEF(CEE_UNUSED63,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xCB,    NEXT)
    OPDEF(CEE_UNUSED64,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xCC,    NEXT)
    OPDEF(CEE_UNUSED65,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xCD,    NEXT)
    OPDEF(CEE_UNUSED66,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xCE,    NEXT)
    OPDEF(CEE_UNUSED67,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xCF,    NEXT)
    OPDEF(CEE_LDTOKEN,                    "ldtoken",          Pop0,               PushI,       InlineTok,          IPrimitive,  1,  0xFF,    0xD0,    NEXT)
    OPDEF(CEE_CONV_U2,                    "conv.u2",          Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0xD1,    NEXT)
    OPDEF(CEE_CONV_U1,                    "conv.u1",          Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0xD2,    NEXT)
    OPDEF(CEE_CONV_I,                     "conv.i",           Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0xD3,    NEXT)
    OPDEF(CEE_CONV_OVF_I,                 "conv.ovf.i",       Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0xD4,    NEXT)
    OPDEF(CEE_CONV_OVF_U,                 "conv.ovf.u",       Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0xD5,    NEXT)
    OPDEF(CEE_ADD_OVF,                    "add.ovf",          Pop1+Pop1,          Push1,       InlineNone,         IPrimitive,  1,  0xFF,    0xD6,    NEXT)
    OPDEF(CEE_ADD_OVF_UN,                 "add.ovf.un",       Pop1+Pop1,          Push1,       InlineNone,         IPrimitive,  1,  0xFF,    0xD7,    NEXT)
    OPDEF(CEE_MUL_OVF,                    "mul.ovf",          Pop1+Pop1,          Push1,       InlineNone,         IPrimitive,  1,  0xFF,    0xD8,    NEXT)
    OPDEF(CEE_MUL_OVF_UN,                 "mul.ovf.un",       Pop1+Pop1,          Push1,       InlineNone,         IPrimitive,  1,  0xFF,    0xD9,    NEXT)
    OPDEF(CEE_SUB_OVF,                    "sub.ovf",          Pop1+Pop1,          Push1,       InlineNone,         IPrimitive,  1,  0xFF,    0xDA,    NEXT)
    OPDEF(CEE_SUB_OVF_UN,                 "sub.ovf.un",       Pop1+Pop1,          Push1,       InlineNone,         IPrimitive,  1,  0xFF,    0xDB,    NEXT)
    OPDEF(CEE_ENDFINALLY,                 "endfinally",       Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xDC,    RETURN)
    OPDEF(CEE_LEAVE,                      "leave",            Pop0,               Push0,       InlineBrTarget,     IPrimitive,  1,  0xFF,    0xDD,    BRANCH)
    OPDEF(CEE_LEAVE_S,                    "leave.s",          Pop0,               Push0,       ShortInlineBrTarget,IPrimitive,  1,  0xFF,    0xDE,    BRANCH)
    OPDEF(CEE_STIND_I,                    "stind.i",          PopI+PopI,          Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xDF,    NEXT)
    OPDEF(CEE_CONV_U,                     "conv.u",           Pop1,               PushI,       InlineNone,         IPrimitive,  1,  0xFF,    0xE0,    NEXT)
    OPDEF(CEE_UNUSED26,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xE1,    NEXT)
    OPDEF(CEE_UNUSED27,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xE2,    NEXT)
    OPDEF(CEE_UNUSED28,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xE3,    NEXT)
    OPDEF(CEE_UNUSED29,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xE4,    NEXT)
    OPDEF(CEE_UNUSED30,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xE5,    NEXT)
    OPDEF(CEE_UNUSED31,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xE6,    NEXT)
    OPDEF(CEE_UNUSED32,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xE7,    NEXT)
    OPDEF(CEE_UNUSED33,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xE8,    NEXT)
    OPDEF(CEE_UNUSED34,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xE9,    NEXT)
    OPDEF(CEE_UNUSED35,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xEA,    NEXT)
    OPDEF(CEE_UNUSED36,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xEB,    NEXT)
    OPDEF(CEE_UNUSED37,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xEC,    NEXT)
    OPDEF(CEE_UNUSED38,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xED,    NEXT)
    OPDEF(CEE_UNUSED39,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xEE,    NEXT)
    OPDEF(CEE_UNUSED40,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xEF,    NEXT)
    OPDEF(CEE_UNUSED41,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xF0,    NEXT)
    OPDEF(CEE_UNUSED42,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xF1,    NEXT)
    OPDEF(CEE_UNUSED43,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xF2,    NEXT)
    OPDEF(CEE_UNUSED44,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xF3,    NEXT)
    OPDEF(CEE_UNUSED45,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xF4,    NEXT)
    OPDEF(CEE_UNUSED46,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xF5,    NEXT)
    OPDEF(CEE_UNUSED47,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xF6,    NEXT)
    OPDEF(CEE_UNUSED48,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  1,  0xFF,    0xF7,    NEXT)
    OPDEF(CEE_PREFIX7,                    "prefix7",          Pop0,               Push0,       InlineNone,         IInternal,   1,  0xFF,    0xF8,    META)
    OPDEF(CEE_PREFIX6,                    "prefix6",          Pop0,               Push0,       InlineNone,         IInternal,   1,  0xFF,    0xF9,    META)
    OPDEF(CEE_PREFIX5,                    "prefix5",          Pop0,               Push0,       InlineNone,         IInternal,   1,  0xFF,    0xFA,    META)
    OPDEF(CEE_PREFIX4,                    "prefix4",          Pop0,               Push0,       InlineNone,         IInternal,   1,  0xFF,    0xFB,    META)
    OPDEF(CEE_PREFIX3,                    "prefix3",          Pop0,               Push0,       InlineNone,         IInternal,   1,  0xFF,    0xFC,    META)
    OPDEF(CEE_PREFIX2,                    "prefix2",          Pop0,               Push0,       InlineNone,         IInternal,   1,  0xFF,    0xFD,    META)
    OPDEF(CEE_PREFIX1,                    "prefix1",          Pop0,               Push0,       InlineNone,         IInternal,   1,  0xFF,    0xFE,    META)
    OPDEF(CEE_PREFIXREF,                  "prefixref",        Pop0,               Push0,       InlineNone,         IInternal,   1,  0xFF,    0xFF,    META)

    OPDEF(CEE_ARGLIST,                    "arglist",          Pop0,               PushI,       InlineNone,         IPrimitive,  2,  0xFE,    0x00,    NEXT)
    OPDEF(CEE_CEQ,                        "ceq",              Pop1+Pop1,          PushI,       InlineNone,         IPrimitive,  2,  0xFE,    0x01,    NEXT)
    OPDEF(CEE_CGT,                        "cgt",              Pop1+Pop1,          PushI,       InlineNone,         IPrimitive,  2,  0xFE,    0x02,    NEXT)
    OPDEF(CEE_CGT_UN,                     "cgt.un",           Pop1+Pop1,          PushI,       InlineNone,         IPrimitive,  2,  0xFE,    0x03,    NEXT)
    OPDEF(CEE_CLT,                        "clt",              Pop1+Pop1,          PushI,       InlineNone,         IPrimitive,  2,  0xFE,    0x04,    NEXT)
    OPDEF(CEE_CLT_UN,                     "clt.un",           Pop1+Pop1,          PushI,       InlineNone,         IPrimitive,  2,  0xFE,    0x05,    NEXT)
    OPDEF(CEE_LDFTN,                      "ldftn",            Pop0,               PushI,       InlineMethod,       IPrimitive,  2,  0xFE,    0x06,    NEXT)
    OPDEF(CEE_LDVIRTFTN,                  "ldvirtftn",        PopRef,             PushI,       InlineMethod,       IPrimitive,  2,  0xFE,    0x07,    NEXT)
    OPDEF(CEE_UNUSED56,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  2,  0xFE,    0x08,    NEXT)
    OPDEF(CEE_LDARG,                      "ldarg",            Pop0,               Push1,       InlineVar,          IPrimitive,  2,  0xFE,    0x09,    NEXT)
    OPDEF(CEE_LDARGA,                     "ldarga",           Pop0,               PushI,       InlineVar,          IPrimitive,  2,  0xFE,    0x0A,    NEXT)
    OPDEF(CEE_STARG,                      "starg",            Pop1,               Push0,       InlineVar,          IPrimitive,  2,  0xFE,    0x0B,    NEXT)
    OPDEF(CEE_LDLOC,                      "ldloc",            Pop0,               Push1,       InlineVar,          IPrimitive,  2,  0xFE,    0x0C,    NEXT)
    OPDEF(CEE_LDLOCA,                     "ldloca",           Pop0,               PushI,       InlineVar,          IPrimitive,  2,  0xFE,    0x0D,    NEXT)
    OPDEF(CEE_STLOC,                      "stloc",            Pop1,               Push0,       InlineVar,          IPrimitive,  2,  0xFE,    0x0E,    NEXT)
    OPDEF(CEE_LOCALLOC,                   "localloc",         PopI,               PushI,       InlineNone,         IPrimitive,  2,  0xFE,    0x0F,    NEXT)
    OPDEF(CEE_UNUSED57,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  2,  0xFE,    0x10,    NEXT)
    OPDEF(CEE_ENDFILTER,                  "endfilter",        PopI,               Push0,       InlineNone,         IPrimitive,  2,  0xFE,    0x11,    RETURN)
    OPDEF(CEE_UNALIGNED,                  "unaligned.",       Pop0,               Push0,       ShortInlineI,       IPrefix,     2,  0xFE,    0x12,    META)
    OPDEF(CEE_VOLATILE,                   "volatile.",        Pop0,               Push0,       InlineNone,         IPrefix,     2,  0xFE,    0x13,    META)
    OPDEF(CEE_TAILCALL,                   "tail.",            Pop0,               Push0,       InlineNone,         IPrefix,     2,  0xFE,    0x14,    META)
    OPDEF(CEE_INITOBJ,                    "initobj",          PopI,               Push0,       InlineType,         IObjModel,   2,  0xFE,    0x15,    NEXT)
    OPDEF(CEE_CONSTRAINED,                "constrained.",     Pop0,               Push0,       InlineType,         IPrefix,     2,  0xFE,    0x16,    META)
    OPDEF(CEE_CPBLK,                      "cpblk",            PopI+PopI+PopI,     Push0,       InlineNone,         IPrimitive,  2,  0xFE,    0x17,    NEXT)
    OPDEF(CEE_INITBLK,                    "initblk",          PopI+PopI+PopI,     Push0,       InlineNone,         IPrimitive,  2,  0xFE,    0x18,    NEXT)
    OPDEF(CEE_UNUSED69,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  2,  0xFE,    0x19,    NEXT)
    OPDEF(CEE_RETHROW,                    "rethrow",          Pop0,               Push0,       InlineNone,         IObjModel,   2,  0xFE,    0x1A,    THROW)
    OPDEF(CEE_UNUSED51,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  2,  0xFE,    0x1B,    NEXT)
    OPDEF(CEE_SIZEOF,                     "sizeof",           Pop0,               PushI,       InlineType,         IPrimitive,  2,  0xFE,    0x1C,    NEXT)
    OPDEF(CEE_REFANYTYPE,                 "refanytype",       Pop1,               PushI,       InlineNone,         IPrimitive,  2,  0xFE,    0x1D,    NEXT)
    OPDEF(CEE_READONLY,                   "readonly.",        Pop0,               Push0,       InlineNone,         IPrefix,     2,  0xFE,    0x1E,    META)
    OPDEF(CEE_UNUSED53,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  2,  0xFE,    0x1F,    NEXT)
    OPDEF(CEE_UNUSED54,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  2,  0xFE,    0x20,    NEXT)
    OPDEF(CEE_UNUSED55,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  2,  0xFE,    0x21,    NEXT)
    OPDEF(CEE_UNUSED70,                   "unused",           Pop0,               Push0,       InlineNone,         IPrimitive,  2,  0xFE,    0x22,    NEXT)
    return "unknwn";
}

#undef OPDEF

static const BYTE s_OpCodeFlags[] =
        {
#define InlineNone           0
#define ShortInlineVar       1
#define InlineVar            2
#define ShortInlineI         1
#define InlineI              4
#define InlineI8             8
#define ShortInlineR         4
#define InlineR              8
#define ShortInlineBrTarget  1 | OPCODEFLAGS_BranchTarget
#define InlineBrTarget       4 | OPCODEFLAGS_BranchTarget
#define InlineMethod         4
#define InlineField          4
#define InlineType           4
#define InlineString         4
#define InlineSig            4
#define InlineRVA            4
#define InlineTok            4
#define InlineSwitch         0 | OPCODEFLAGS_Switch

#define OPDEF(c,s,pop,push,args,type,l,s1,s2,flow) args,
#include "opcode.def"
#undef OPDEF

#undef InlineNone
#undef ShortInlineVar
#undef InlineVar
#undef ShortInlineI
#undef InlineI
#undef InlineI8
#undef ShortInlineR
#undef InlineR
#undef ShortInlineBrTarget
#undef InlineBrTarget
#undef InlineMethod
#undef InlineField
#undef InlineType
#undef InlineString
#undef InlineSig
#undef InlineRVA
#undef InlineTok
#undef InlineSwitch
                0,                              // CEE_COUNT
                4 | OPCODEFLAGS_BranchTarget,   // CEE_SWITCH_ARG
        };

static int k_rgnStackPushes[] = {

#define OPDEF(c,s,pop,push,args,type,l,s1,s2,ctrl) \
	 push ,

#define Push0    0
#define Push1    1
#define PushI    1
#define PushI4   1
#define PushR4   1
#define PushI8   1
#define PushR8   1
#define PushRef  1
#define VarPush  1          // Test code doesn't call vararg fcns, so this should not be used

#include "opcode.def"

#undef Push0
#undef Push1
#undef PushI
#undef PushI4
#undef PushR4
#undef PushI8
#undef PushR8
#undef PushRef
#undef VarPush
#undef OPDEF
        0,  // CEE_COUNT
        0   // CEE_SWITCH_ARG
};

WCHAR *mainAssemblyName = nullptr;
int mainAssemblyNameLength = 0;
WCHAR *mainModuleName = nullptr;
int mainModuleNameLength = 0;
mdMethodDef mainToken = 0;
bool rewriteMainOnly = false;

ILRewriter::ILRewriter(
    ICorProfilerInfo * pICorProfilerInfo,
    ICorProfilerFunctionControl * pICorProfilerFunctionControl,
    ModuleID moduleID,
    mdToken tkMethod)
    : m_pICorProfilerInfo(pICorProfilerInfo), m_pICorProfilerFunctionControl(pICorProfilerFunctionControl),
      m_moduleId(moduleID), m_tkMethod(tkMethod), m_fGenerateTinyHeader(false),
      m_pEH(nullptr), m_pOffsetToInstr(nullptr), m_pOutputBuffer(nullptr), m_pIMethodMalloc(nullptr)
{
    m_IL.m_pNext = &m_IL;
    m_IL.m_pPrev = &m_IL;

    m_nInstrs = 0;
}

ILRewriter::~ILRewriter()
{
    ILInstr * p = m_IL.m_pNext;
    while (p != &m_IL)
    {
        ILInstr * t = p->m_pNext;
        delete p;
        p = t;
    }
    delete[] m_pEH;
    delete[] m_pOffsetToInstr;
    delete[] m_pOutputBuffer;

    if (m_pIMethodMalloc)
        m_pIMethodMalloc->Release();
}

HRESULT ILRewriter::Import()
{
    LPCBYTE pMethodBytes;

    IfFailRet(m_pICorProfilerInfo->GetILFunctionBody(
            m_moduleId, m_tkMethod, &pMethodBytes, nullptr));

    COR_ILMETHOD_DECODER decoder((COR_ILMETHOD*)pMethodBytes);

    // Import the header flags
    m_tkLocalVarSig = decoder.GetLocalVarSigTok();
    m_maxStack = decoder.GetMaxStack();
    m_flags = (decoder.GetFlags() & CorILMethod_InitLocals);

    m_CodeSize = decoder.GetCodeSize();

    IfFailRet(ImportIL(decoder.Code));

    IfFailRet(ImportEH(decoder.EH, decoder.EHCount()));

    return S_OK;
}

HRESULT ILRewriter::ImportIL(LPCBYTE pIL)
{
    m_pOffsetToInstr = new ILInstr*[m_CodeSize + 1];
    IfNullRet(m_pOffsetToInstr);

    ZeroMemory(m_pOffsetToInstr, m_CodeSize * sizeof(ILInstr*));

    // Set the sentinel instruction
    m_pOffsetToInstr[m_CodeSize] = &m_IL;
    m_IL.m_opcode = -1;

    bool fBranch = false;
    unsigned offset = 0;
    while (offset < m_CodeSize)
    {
        unsigned startOffset = offset;
        unsigned opcode = pIL[offset++];

        if (opcode == CEE_PREFIX1)
        {
            if (offset >= m_CodeSize)
            {
                assert(false);
                return COR_E_INVALIDPROGRAM;
            }
            opcode = 0x100 + pIL[offset++];
        }

        if ((CEE_PREFIX7 <= opcode) && (opcode <= CEE_PREFIX2))
        {
            // NOTE: CEE_PREFIX2-7 are currently not supported
            assert(false);
            return COR_E_INVALIDPROGRAM;
        }

        if (opcode >= CEE_COUNT)
        {
            assert(false);
            return COR_E_INVALIDPROGRAM;
        }

        BYTE flags = s_OpCodeFlags[opcode];

        int size = (flags & OPCODEFLAGS_SizeMask);
        if (offset + size > m_CodeSize)
        {
            assert(false);
            return COR_E_INVALIDPROGRAM;
        }

        ILInstr * pInstr = NewILInstr();
        IfNullRet(pInstr);

        pInstr->m_opcode = opcode;

        InsertBefore(&m_IL, pInstr);

        m_pOffsetToInstr[startOffset] = pInstr;

        switch (flags)
        {
            case 0:
                break;
            case 1:
                pInstr->m_Arg8 = *(UNALIGNED INT8 *)&(pIL[offset]);
                break;
            case 2:
                pInstr->m_Arg16 = *(UNALIGNED INT16 *)&(pIL[offset]);
                break;
            case 4:
                pInstr->m_Arg32 = *(UNALIGNED INT32 *)&(pIL[offset]);
                break;
            case 8:
                pInstr->m_Arg64 = *(UNALIGNED INT64 *)&(pIL[offset]);
                break;
            case 1 | OPCODEFLAGS_BranchTarget:
                pInstr->m_Arg32 = offset + 1 + *(UNALIGNED INT8 *)&(pIL[offset]);
                fBranch = true;
                break;
            case 4 | OPCODEFLAGS_BranchTarget:
                pInstr->m_Arg32 = offset + 4 + *(UNALIGNED INT32 *)&(pIL[offset]);
                fBranch = true;
                break;
            case 0 | OPCODEFLAGS_Switch:
            {
                if (offset + sizeof(INT32) > m_CodeSize)
                {
                    assert(false);
                    return COR_E_INVALIDPROGRAM;
                }

                unsigned nTargets = *(UNALIGNED INT32 *)&(pIL[offset]);
                pInstr->m_Arg32 = nTargets;
                offset += sizeof(INT32);

                unsigned base = offset + nTargets * sizeof(INT32);

                for (unsigned iTarget = 0; iTarget < nTargets; iTarget++)
                {
                    if (offset + sizeof(INT32) > m_CodeSize)
                    {
                        assert(false);
                        return COR_E_INVALIDPROGRAM;
                    }

                    pInstr = NewILInstr();
                    IfNullRet(pInstr);

                    pInstr->m_opcode = CEE_SWITCH_ARG;

                    pInstr->m_Arg32 = base + *(UNALIGNED INT32 *)&(pIL[offset]);
                    offset += sizeof(INT32);

                    InsertBefore(&m_IL, pInstr);
                }
                fBranch = true;
                break;
            }
            default:
                assert(false);
                break;
        }
        offset += size;
    }
    assert(offset == m_CodeSize);

    if (fBranch)
    {
        // Go over all control flow instructions and resolve the targets
        for (ILInstr * pInstr = m_IL.m_pNext; pInstr != &m_IL; pInstr = pInstr->m_pNext)
        {
            if (s_OpCodeFlags[pInstr->m_opcode] & OPCODEFLAGS_BranchTarget)
                pInstr->m_pTarget = GetInstrFromOffset(pInstr->m_Arg32);
        }
    }

    return S_OK;
}

HRESULT ILRewriter::ImportEH(const COR_ILMETHOD_SECT_EH* pILEH, unsigned nEH)
{
    assert(m_pEH == NULL);

    m_nEH = nEH;

    if (nEH == 0)
        return S_OK;

    IfNullRet(m_pEH = new EHClause[m_nEH]);
    for (unsigned iEH = 0; iEH < m_nEH; iEH++)
    {
        // If the EH clause is in tiny form, the call to pILEH->EHClause() below will
        // use this as a scratch buffer to expand the EH clause into its fat form.
        COR_ILMETHOD_SECT_EH_CLAUSE_FAT scratch;

        const COR_ILMETHOD_SECT_EH_CLAUSE_FAT* ehInfo;
        ehInfo = (COR_ILMETHOD_SECT_EH_CLAUSE_FAT*)pILEH->EHClause(iEH, &scratch);

        EHClause* clause = &(m_pEH[iEH]);
        clause->m_Flags = ehInfo->GetFlags();

        clause->m_pTryBegin = GetInstrFromOffset(ehInfo->GetTryOffset());
        clause->m_pTryEnd = GetInstrFromOffset(ehInfo->GetTryOffset() + ehInfo->GetTryLength());
        clause->m_pHandlerBegin = GetInstrFromOffset(ehInfo->GetHandlerOffset());
        clause->m_pHandlerEnd = GetInstrFromOffset(ehInfo->GetHandlerOffset() + ehInfo->GetHandlerLength())->m_pPrev;
        if ((clause->m_Flags & COR_ILEXCEPTION_CLAUSE_FILTER) == 0)
            clause->m_ClassToken = ehInfo->GetClassToken();
        else
            clause->m_pFilter = GetInstrFromOffset(ehInfo->GetFilterOffset());
    }

    return S_OK;
}

ILInstr* ILRewriter::NewILInstr()
{
    m_nInstrs++;
    return new ILInstr();
}

ILInstr* ILRewriter::GetInstrFromOffset(unsigned offset)
{
    ILInstr * pInstr = NULL;

    if (offset <= m_CodeSize)
        pInstr = m_pOffsetToInstr[offset];

    assert(pInstr != NULL);
    return pInstr;
}

void ILRewriter::PrintEhs()
{
    LOG(
        if (m_pEH != nullptr) {
            for (int i = 0; i < m_nEH; i++) {
                tout << "handler: " << m_pEH[i].m_pHandlerBegin << " " << m_pEH[i].m_pHandlerEnd << "\n";
                tout << "    try: " << m_pEH[i].m_pTryBegin << " " << m_pEH[i].m_pTryEnd << "\n";
                tout << "\n";
            }
        }
    );
}

void ILRewriter::InsertBefore(ILInstr * pWhere, ILInstr * pWhat)
{
    pWhat->m_pNext = pWhere;
    pWhat->m_pPrev = pWhere->m_pPrev;

    pWhat->m_pNext->m_pPrev = pWhat;
    pWhat->m_pPrev->m_pNext = pWhat;

    AdjustState(pWhat);
}

void ILRewriter::InsertAfter(ILInstr * pWhere, ILInstr * pWhat)
{
    pWhat->m_pNext = pWhere->m_pNext;
    pWhat->m_pPrev = pWhere;

    pWhat->m_pNext->m_pPrev = pWhat;
    pWhat->m_pPrev->m_pNext = pWhat;

    AdjustState(pWhat);
}

void ILRewriter::AdjustState(ILInstr * pNewInstr)
{
    m_maxStack += k_rgnStackPushes[pNewInstr->m_opcode];
}


ILInstr * ILRewriter::GetILList()
{
    return &m_IL;
}

/////////////////////////////////////////////////////////////////////////////////////////////////
//
// E X P O R T
//
////////////////////////////////////////////////////////////////////////////////////////////////


HRESULT ILRewriter::Export()
{
    // One instruction produces 2 + sizeof(native int) bytes in the worst case which can be 10 bytes for 64-bit.
    // For simplification we just use 10 here.
    unsigned maxSize = m_nInstrs * 10;

    m_pOutputBuffer = new BYTE[maxSize];
    IfNullRet(m_pOutputBuffer);

    again:
    BYTE * pIL = m_pOutputBuffer;

    bool fBranch = false;
    unsigned offset = 0;

    // Go over all instructions and produce code for them
    for (ILInstr * pInstr = m_IL.m_pNext; pInstr != &m_IL; pInstr = pInstr->m_pNext)
    {
        assert(offset < maxSize);
        pInstr->m_offset = offset;

        unsigned opcode = pInstr->m_opcode;
        if (opcode < CEE_COUNT)
        {
            // CEE_PREFIX1 refers not to instruction prefixes (like tail.), but to
            // the lead byte of multi-byte opcodes. For now, the only lead byte
            // supported is CEE_PREFIX1 = 0xFE.
            if (opcode >= 0x100)
                m_pOutputBuffer[offset++] = CEE_PREFIX1;

            // This appears to depend on an implicit conversion from
            // unsigned opcode down to BYTE, to deliberately lose data and have
            // opcode >= 0x100 wrap around to 0.
            m_pOutputBuffer[offset++] = (opcode & 0xFF);
        }

        BYTE flags = s_OpCodeFlags[pInstr->m_opcode];
        switch (flags)
        {
            case 0:
                break;
            case 1:
                *(UNALIGNED INT8 *)&(pIL[offset]) = pInstr->m_Arg8;
                break;
            case 2:
                *(UNALIGNED INT16 *)&(pIL[offset]) = pInstr->m_Arg16;
                break;
            case 4:
                *(UNALIGNED INT32 *)&(pIL[offset]) = pInstr->m_Arg32;
                break;
            case 8:
                *(UNALIGNED INT64 *)&(pIL[offset]) = pInstr->m_Arg64;
                break;
            case 1 | OPCODEFLAGS_BranchTarget:
                fBranch = true;
                break;
            case 4 | OPCODEFLAGS_BranchTarget:
                fBranch = true;
                break;
            case 0 | OPCODEFLAGS_Switch:
                *(UNALIGNED INT32 *)&(pIL[offset]) = pInstr->m_Arg32;
                offset += sizeof(INT32);
                break;
            default:
                assert(false);
                break;
        }
        offset += (flags & OPCODEFLAGS_SizeMask);
    }
    m_IL.m_offset = offset;

    if (fBranch)
    {
        bool fTryAgain = false;
        unsigned switchBase = 0;

        // Go over all control flow instructions and resolve the targets
        for (ILInstr * pInstr = m_IL.m_pNext; pInstr != &m_IL; pInstr = pInstr->m_pNext)
        {
            unsigned opcode = pInstr->m_opcode;

            if (pInstr->m_opcode == CEE_SWITCH)
            {
                switchBase = pInstr->m_offset + 1 + sizeof(INT32) * (pInstr->m_Arg32 + 1);
                continue;
            }
            if (opcode == CEE_SWITCH_ARG)
            {
                // Switch args are special
                *(UNALIGNED INT32 *)&(pIL[pInstr->m_offset]) = pInstr->m_pTarget->m_offset - switchBase;
                continue;
            }

            BYTE flags = s_OpCodeFlags[pInstr->m_opcode];

            if (flags & OPCODEFLAGS_BranchTarget)
            {
                int delta = pInstr->m_pTarget->m_offset - pInstr->m_pNext->m_offset;

                switch (flags)
                {
                    case 1 | OPCODEFLAGS_BranchTarget:
                        // Check if delta is too big to fit into an INT8.
                        //
                        // (see #pragma at top of file)
                        if ((INT8)delta != delta)
                        {
                            if (opcode == CEE_LEAVE_S)
                            {
                                pInstr->m_opcode = CEE_LEAVE;
                            }
                            else
                            {
                                assert(opcode >= CEE_BR_S && opcode <= CEE_BLT_UN_S);
                                pInstr->m_opcode = opcode - CEE_BR_S + CEE_BR;
                                assert(pInstr->m_opcode >= CEE_BR && pInstr->m_opcode <= CEE_BLT_UN);
                            }
                            fTryAgain = true;
                            continue;
                        }
                        *(UNALIGNED INT8 *)&(pIL[pInstr->m_pNext->m_offset - sizeof(INT8)]) = delta;
                        break;
                    case 4 | OPCODEFLAGS_BranchTarget:
                        *(UNALIGNED INT32 *)&(pIL[pInstr->m_pNext->m_offset - sizeof(INT32)]) = delta;
                        break;
                    default:
                        assert(false);
                        break;
                }
            }
        }

        // Do the whole thing again if we changed the size of some branch targets
        if (fTryAgain)
            goto again;
    }

    unsigned codeSize = offset;
    unsigned totalSize;
    LPBYTE pBody = NULL;
    if (m_fGenerateTinyHeader)
    {
        // Make sure we can fit in a tiny header
        if (codeSize >= 64)
            return E_FAIL;

        totalSize = sizeof(IMAGE_COR_ILMETHOD_TINY) + codeSize;
        pBody = AllocateILMemory(totalSize);
        IfNullRet(pBody);

        BYTE * pCurrent = pBody;

        // Here's the tiny header
        *pCurrent = (BYTE)(CorILMethod_TinyFormat | (codeSize << 2));
        pCurrent += sizeof(IMAGE_COR_ILMETHOD_TINY);

        // And the body
        CopyMemory(pCurrent, m_pOutputBuffer, codeSize);
    }
    else
    {
        // Use FAT header

        unsigned alignedCodeSize = (offset + 3) & ~3;

        totalSize = sizeof(IMAGE_COR_ILMETHOD_FAT) + alignedCodeSize +
                    (m_nEH ? (sizeof(IMAGE_COR_ILMETHOD_SECT_FAT) + sizeof(IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT) * m_nEH) : 0);

        pBody = AllocateILMemory(totalSize);
        IfNullRet(pBody);

        BYTE * pCurrent = pBody;

        IMAGE_COR_ILMETHOD_FAT *pHeader = (IMAGE_COR_ILMETHOD_FAT *)pCurrent;
        pHeader->Flags = m_flags | (m_nEH ? CorILMethod_MoreSects : 0) | CorILMethod_FatFormat;
        pHeader->Size = sizeof(IMAGE_COR_ILMETHOD_FAT) / sizeof(DWORD);
        pHeader->MaxStack = m_maxStack;
        pHeader->CodeSize = offset;
        pHeader->LocalVarSigTok = m_tkLocalVarSig;

        pCurrent = (BYTE*)(pHeader + 1);

        CopyMemory(pCurrent, m_pOutputBuffer, codeSize);
        pCurrent += alignedCodeSize;

        if (m_nEH != 0)
        {
            IMAGE_COR_ILMETHOD_SECT_FAT *pEH = (IMAGE_COR_ILMETHOD_SECT_FAT *)pCurrent;
            pEH->Kind = CorILMethod_Sect_EHTable | CorILMethod_Sect_FatFormat;
            pEH->DataSize = (unsigned)(sizeof(IMAGE_COR_ILMETHOD_SECT_FAT) + sizeof(IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT) * m_nEH);

            pCurrent = (BYTE*)(pEH + 1);

            for (unsigned iEH = 0; iEH < m_nEH; iEH++)
            {
                EHClause *pSrc = &(m_pEH[iEH]);
                IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT * pDst = (IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT *)pCurrent;

                pDst->Flags = pSrc->m_Flags;
                pDst->TryOffset = pSrc->m_pTryBegin->m_offset;
                pDst->TryLength = pSrc->m_pTryEnd->m_offset - pSrc->m_pTryBegin->m_offset;
                pDst->HandlerOffset = pSrc->m_pHandlerBegin->m_offset;
                pDst->HandlerLength = pSrc->m_pHandlerEnd->m_pNext->m_offset - pSrc->m_pHandlerBegin->m_offset;
                if ((pSrc->m_Flags & COR_ILEXCEPTION_CLAUSE_FILTER) == 0)
                    pDst->ClassToken = pSrc->m_ClassToken;
                else
                    pDst->FilterOffset = pSrc->m_pFilter->m_offset;

                pCurrent = (BYTE*)(pDst + 1);
            }
        }
    }

    IfFailRet(SetILFunctionBody(totalSize, pBody));
    DeallocateILMemory(pBody);

    return S_OK;
}

HRESULT ILRewriter::SetILFunctionBody(unsigned size, LPBYTE pBody)
{
    if (m_pICorProfilerFunctionControl != NULL)
    {
        // We're supplying IL for a rejit, so use the rejit mechanism
        IfFailRet(m_pICorProfilerFunctionControl->SetILFunctionBody(size, pBody));
    }
    else
    {
        // "classic-style" instrumentation on first JIT, so use old mechanism
        IfFailRet(m_pICorProfilerInfo->SetILFunctionBody(m_moduleId, m_tkMethod, pBody));
    }

    return S_OK;
}

LPBYTE ILRewriter::AllocateILMemory(unsigned size)
{
    if (m_pICorProfilerFunctionControl != NULL)
    {
        // We're supplying IL for a rejit, so we can just allocate from
        // the heap
        return new BYTE[size];
    }

    // Else, this is "classic-style" instrumentation on first JIT, and
    // need to use the CLR's IL allocator

    if (FAILED(m_pICorProfilerInfo->GetILFunctionBodyAllocator(m_moduleId, &m_pIMethodMalloc)))
        return NULL;

    return (LPBYTE)m_pIMethodMalloc->Alloc(size);
}

void ILRewriter::DeallocateILMemory(LPBYTE pBody)
{
    if (m_pICorProfilerFunctionControl == NULL)
    {
        // Old-style instrumentation does not provide a way to free up bytes
        return;
    }

    delete[] pBody;
}

HRESULT AddProbe(
    ILRewriter * pilr,
    UINT_PTR methodAddress,
    ULONG32 methodSignature,
    ILInstr *pInsertProbeBeforeThisInstr)
{
    ILInstr * pNewInstr = nullptr;

    constexpr auto CEE_LDC_I = sizeof(size_t) == 8 ? CEE_LDC_I8 : sizeof(size_t) == 4 ? CEE_LDC_I4 : throw std::logic_error("size_t must be defined as 8 or 4");

    pNewInstr = pilr->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I;
    pNewInstr->m_Arg64 = methodAddress;
    pilr->InsertBefore(pInsertProbeBeforeThisInstr, pNewInstr);

    pNewInstr = pilr->NewILInstr();
    pNewInstr->m_opcode = CEE_CALLI;
    pNewInstr->m_Arg32 = methodSignature;
    pilr->InsertBefore(pInsertProbeBeforeThisInstr, pNewInstr);

    return S_OK;
}

HRESULT AddProbeAfter(
        ILRewriter * pilr,
        UINT_PTR methodAddress,
        ULONG32 methodSignature,
        ILInstr *pInsertProbeAfterThisInstr,
        ILInstr *beforehandInstr[],
        int instrSize)
{
    ILInstr * pNewInstr = nullptr;

    constexpr auto CEE_LDC_I = sizeof(size_t) == 8 ? CEE_LDC_I8 : sizeof(size_t) == 4 ? CEE_LDC_I4 : throw std::logic_error("size_t must be defined as 8 or 4");

    pNewInstr = pilr->NewILInstr();
    pNewInstr->m_opcode = CEE_CALLI;
    pNewInstr->m_Arg32 = methodSignature;
    pilr->InsertAfter(pInsertProbeAfterThisInstr, pNewInstr);

    pNewInstr = pilr->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I;
    pNewInstr->m_Arg64 = methodAddress;
    pilr->InsertAfter(pInsertProbeAfterThisInstr, pNewInstr);

    return S_OK;
}

HRESULT AddEnterProbe(
    ILRewriter * pilr,
    UINT_PTR methodAddress,
    ULONG32 methodSignature,
    int methodId)
{
    ILInstr * pFirstOriginalInstr = pilr->GetILList()->m_pNext;

    auto pNewInstr = pilr->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I4;
    pNewInstr->m_Arg32 = (INT32)pFirstOriginalInstr->m_offset;
    pilr->InsertBefore(pFirstOriginalInstr, pNewInstr);

    ILInstr * offsetInstr;
    offsetInstr = pilr->NewILInstr();
    offsetInstr->m_opcode = CEE_LDC_I4;
    offsetInstr->m_Arg32 = (INT32)methodId;
    pilr->InsertBefore(pFirstOriginalInstr, offsetInstr);

    ILInstr * spontaneousInstr;
    spontaneousInstr = pilr->NewILInstr();
    spontaneousInstr->m_opcode = CEE_LDC_I4_0;
    pilr->InsertBefore(pFirstOriginalInstr, spontaneousInstr);

    return AddProbe(pilr, methodAddress, methodSignature, pFirstOriginalInstr);
}


HRESULT AddExitProbe(
        ILRewriter * pilr,
        mdMethodDef functionId,
        UINT_PTR methodAddress,
        ULONG32 methodSignature)
{
    HRESULT hr;
    BOOL fAtLeastOneProbeAdded = FALSE;
    BOOL isTailCall = FALSE;

    // Find all RETs, and insert a call to the exit probe before each one.
    for (ILInstr * pInstr = pilr->GetILList()->m_pNext; pInstr != pilr->GetILList(); pInstr = pInstr->m_pNext)
    {
        switch (pInstr->m_opcode)
        {
            case CEE_TAILCALL:
            {
                isTailCall = TRUE;
                break;
            }
            case CEE_RET:
            {
                // TODO: support instrumenting of tailcalls
                if (isTailCall) {
                    isTailCall = FALSE;
                    break;
                }
                // We want any branches or leaves that targeted the RET instruction to
                // actually target the epilog instructions we're adding. So turn the "RET"
                // into ["NOP", "RET"], and THEN add the epilog between the NOP & RET. That
                // ensures that any branches that went to the RET will now go to the NOP and
                // then execute our epilog.

                // NOTE: The NOP is not strictly required, but is a simplification of the implementation.
                // RET->NOP
                pInstr->m_opcode = CEE_NOP;

                // Add the new RET after
                ILInstr * pNewRet = pilr->NewILInstr();
                pNewRet->m_opcode = CEE_RET;
                pilr->InsertAfter(pInstr, pNewRet);

                auto pNewInstr = pilr->NewILInstr();
                pNewInstr->m_opcode = CEE_LDC_I4;
                pNewInstr->m_Arg32 = (INT32)pInstr->m_offset;
                pilr->InsertBefore(pNewRet, pNewInstr);

                ILInstr * offsetInstr;
                offsetInstr = pilr->NewILInstr();
                offsetInstr->m_opcode = CEE_LDC_I4;
                offsetInstr->m_Arg32 = (INT32)functionId;
                pilr->InsertBefore(pNewRet, offsetInstr);

                // Add now insert the epilog before the new RET
                hr = AddProbe(pilr, methodAddress, methodSignature, pNewRet);
                if (FAILED(hr))
                    return hr;
                fAtLeastOneProbeAdded = TRUE;

                // Advance pInstr after all this gunk so the for loop continues properly
                pInstr = pNewRet;
                break;
            }

            default:
                break;
        }
    }

    if (!fAtLeastOneProbeAdded)
        return E_FAIL;

    return S_OK;
}

void countOffsets(ILRewriter *pilr) {
    unsigned offset = 0;

    ILInstr *first = pilr->GetILList();

    // Go over all instructions and produce code for them
    for (ILInstr * pInstr = first->m_pNext; pInstr != first; pInstr = pInstr->m_pNext)
    {
        pInstr->m_offset = offset;

        unsigned opcode = pInstr->m_opcode;
        if (opcode < CEE_COUNT)
        {
            // CEE_PREFIX1 refers not to instruction prefixes (like tail.), but to
            // the lead byte of multi-byte opcodes. For now, the only lead byte
            // supported is CEE_PREFIX1 = 0xFE.
            if (opcode >= 0x100)
                offset++;

            // This appears to depend on an implicit conversion from
            // unsigned opcode down to BYTE, to deliberately lose data and have
            // opcode >= 0x100 wrap around to 0.
            offset++;
        }

        BYTE flags = s_OpCodeFlags[pInstr->m_opcode];
        switch (flags)
        {
            case 0:
            case 1:
            case 2:
            case 4:
            case 8:
            case 1 | OPCODEFLAGS_BranchTarget:
            case 4 | OPCODEFLAGS_BranchTarget:
                break;
            case 0 | OPCODEFLAGS_Switch:
                offset += sizeof(INT32);
                break;
            default:
                break;
        }
        offset += (flags & OPCODEFLAGS_SizeMask);
    }
}

// returns pointer to the new instruction
ILInstr *AddLDCInstrBefore(ILRewriter *pilr, ILInstr *pInstr, INT32 arg) {
    ILInstr *pNewInstr = pilr->NewILInstr();
    pNewInstr->m_opcode = CEE_LDC_I4;
    pNewInstr->m_Arg32 = arg;
    pilr->InsertBefore(pInstr, pNewInstr);

    return pNewInstr;
}

bool IsTailcallRet(ILInstr *pInstr) {
    return pInstr->m_opcode == CEE_RET && pInstr->m_pPrev->m_pPrev->m_opcode == CEE_TAILCALL;
}

bool IsPrefix(ILInstr *pInstr) {
    return pInstr->m_opcode == CEE_TAILCALL
        || pInstr->m_opcode == CEE_UNALIGNED
        || pInstr->m_opcode == CEE_VOLATILE
        || pInstr->m_opcode == CEE_READONLY
        || pInstr->m_opcode == CEE_CONSTRAINED
        ;
}

void PrintILInstructions(ILRewriter *pilr) {
    LOG(
        for (ILInstr* pInstr = pilr->GetILList()->m_pNext; pInstr != pilr->GetILList(); pInstr = pInstr->m_pNext) {
            tout << pInstr << "   " << pInstr->m_offset << " " << opcodetostr(pInstr->m_opcode);
            if (pInstr->m_opcode >= CEE_BR_S && CEE_BLT_UN >= pInstr->m_opcode)
                tout << "tg: " << pInstr->m_pTarget->m_offset;
            if (pInstr->m_opcode == 295)
                tout << "tg: " << pInstr->m_pTarget->m_offset;
            tout << "\n";
        }

        tout << "\n" << "exception handlers" << std::endl;

        pilr->PrintEhs();

        tout << std::endl;
    );
}

// advances the instruction pointer to the copied version of the original one
HRESULT AddCoverageProbeWithNop(
        ILRewriter *pilr,
        ILInstr *&pInstr,
        UINT_PTR methodAddress,
        ULONG32 methodSignature,
        int methodId)
{
    // adding the new instruction
    ILInstr * pNewInstr = pilr->NewILInstr();
    pNewInstr->m_opcode = pInstr->m_opcode;
    pNewInstr->m_Arg64 = pInstr->m_Arg64; // using the widest argument of the union to copy it
    pilr->InsertAfter(pInstr, pNewInstr);

    pInstr->m_opcode = CEE_NOP;

    AddLDCInstrBefore(pilr, pNewInstr, (INT32)pInstr->m_offset);

    AddLDCInstrBefore(pilr, pNewInstr, methodId);

    // adding the probe
    IfFailRet(AddProbe(pilr, methodAddress, methodSignature, pNewInstr));

    // changing exception handlers bounds if we were on the end of handler block
    if (pilr->m_pEH != nullptr) {
        for (int i = 0; i < pilr->m_nEH; i++) {
            if (pilr->m_pEH[i].m_pHandlerEnd == pInstr) {
                pilr->m_pEH[i].m_pHandlerEnd = pNewInstr;
            }
        }
    }

    pInstr = pNewInstr;
    return S_OK;
}

// Uses the general-purpose ILRewriter class to import original
// IL, rewrite it, and send the result to the CLR
HRESULT RewriteIL(
        ICorProfilerInfo * pICorProfilerInfo,
        ICorProfilerFunctionControl * pICorProfilerFunctionControl,
        ModuleID moduleID,
        mdMethodDef methodDef,
        int methodId,
        bool isMain)
{
    ILRewriter rewriter(pICorProfilerInfo, pICorProfilerFunctionControl, moduleID, methodDef);
    auto pilr = &rewriter;

    auto covProb = vsharp::getProbes();

    INT_PTR enterMethodAddress;
    ULONG32 enterMethodSignature;
    INT_PTR leaveMethodAddress;
    ULONG32 leaveMethodSignature;
    if (isMain) {
        enterMethodAddress = covProb->Track_EnterMain_Addr;
        enterMethodSignature = covProb->Track_EnterMain_Sig.getSig();
        leaveMethodAddress = covProb->Track_LeaveMain_Addr;
        leaveMethodSignature = covProb->Track_LeaveMain_Sig.getSig();
    }
    else {
        enterMethodAddress = covProb->Track_Enter_Addr;
        enterMethodSignature = covProb->Track_Enter_Sig.getSig();
        leaveMethodAddress = covProb->Track_Leave_Addr;
        leaveMethodSignature = covProb->Track_Leave_Sig.getSig();
    }

    IfFailRet(rewriter.Import());
    countOffsets(&rewriter);
    
    if (isMain) {
        LOG(tout << "original main method: ");
        PrintILInstructions(pilr);
    }

    BOOL isTailCall = FALSE;

    // keeping <target, branch> in case we need it for extra br insertion
    std::set<std::pair<ILInstr*, ILInstr*>> branchTargets;
    std::set<unsigned> coveredInstructions;

    // adding probes for coverage tracking
    for (ILInstr * pInstr = pilr->GetILList()->m_pNext; pInstr != pilr->GetILList(); pInstr = pInstr->m_pNext)
    {
        // branch coverage
        if ((CEE_BR_S <= pInstr->m_opcode && pInstr->m_opcode <= CEE_SWITCH)
            || pInstr->m_opcode == CEE_LEAVE || pInstr->m_opcode == CEE_LEAVE_S) {
            coveredInstructions.insert(pInstr->m_offset);
            AddCoverageProbeWithNop(pilr, pInstr, covProb->Branch_Addr, covProb->Branch_Sig.getSig(), methodId);

            // inserting all switch cases as possible target points
            if (pInstr->m_opcode == CEE_SWITCH) {
                ILInstr *curSwitchArg = pInstr->m_pNext;
                for (int i = 0; i < pInstr->m_Arg32; i++) {
                    assert(curSwitchArg->m_opcode == 295); // checking switch arg constant
                    branchTargets.insert({ curSwitchArg->m_pTarget, pInstr });
                    curSwitchArg = curSwitchArg->m_pNext;
                }
                // inserting first instruction after switch
                branchTargets.insert({ curSwitchArg, pInstr });
            }
            else {
                // inserting true and false branchings
                branchTargets.insert({ pInstr->m_pTarget, pInstr });
                branchTargets.insert({ pInstr->m_pNext, pInstr });
            }

            continue;
        }

        // ret & call coverage
        switch (pInstr->m_opcode)
        {
            case CEE_STSFLD:
            {
                ILInstr* instr;
                if (IsPrefix(pInstr->m_pPrev)) {
                    instr = pInstr->m_pPrev;
                }
                else {
                    instr = pInstr;
                }
                coveredInstructions.insert(instr->m_offset);
                AddCoverageProbeWithNop(pilr, instr, covProb->Track_Stsfld_Addr, covProb->Track_Stsfld_Sig.getSig(), methodId);

                // advancing original instr to avoid loops
                if (IsPrefix(pInstr->m_pPrev)) {
                    pInstr = instr->m_pNext;
                }
                else {
                    pInstr = instr;
                }
                break;
            }
            case CEE_TAILCALL:
            {
                isTailCall = TRUE;
                break;
            }
            case CEE_CALL:
            case CEE_CALLI:
            case CEE_CALLVIRT:
            case CEE_NEWOBJ:
            {
                INT_PTR trackCallAddr;
                ULONG32 trackCallSig;
                ILInstr *instr;
                bool isPrefixCall = IsPrefix(pInstr->m_pPrev);
                if (isPrefixCall) {
                    // instructions:
                    //
                    // prefix.
                    // call
                    //
                    // need to add the probe before the prefix
                    instr = pInstr->m_pPrev;
                }
                else {
                    instr = pInstr;
                }

                if (isTailCall) {
                    trackCallAddr = covProb->Track_Tailcall_Addr;
                    trackCallSig = covProb->Track_Tailcall_Sig.getSig();
                }
                else {
                    trackCallAddr = covProb->Track_Call_Addr;
                    trackCallSig = covProb->Track_Call_Sig.getSig();
                }

                coveredInstructions.insert(instr->m_offset);
                AddCoverageProbeWithNop(pilr, instr, trackCallAddr, trackCallSig, methodId);

                // moving the instruction pointer to the call to not break the loop
                if (isPrefixCall) {
                    pInstr = instr->m_pNext; 
                }
                else {
                    pInstr = instr;
                }

                // another basic block is generated after the call; unless it's tail., adding target after the call finalizes
                if (!isTailCall) {
                    branchTargets.insert({ pInstr->m_pNext, pInstr });
                }
                break;
            }
            case CEE_RET:
            {
                if (isTailCall) {
                    // instruction sequence is:
                    // tail.
                    // call
                    // ret <- returning flag to its default position here
                    isTailCall = FALSE;
                    break;
                }
                // We want any branches or leaves that targeted the RET instruction to
                // actually target the epilog instructions we're adding. So turn the "RET"
                // into ["NOP", "RET"], and THEN add the epilog between the NOP & RET. That
                // ensures that any branches that went to the RET will now go to the NOP and
                // then execute our epilog.

                // NOTE: The NOP is not strictly required, but is a simplification of the implementation.
                // RET->NOP
                coveredInstructions.insert(pInstr->m_offset);
                AddCoverageProbeWithNop(pilr, pInstr, leaveMethodAddress, leaveMethodSignature, methodId);
                break;
            }

            // handling exception blocks
            case CEE_ENDFINALLY:
            case CEE_ENDFILTER:
            // case CEE_THROW:
            // case CEE_RETHROW:
            {
                coveredInstructions.insert(pInstr->m_offset);
                AddCoverageProbeWithNop(pilr, pInstr, covProb->Track_Coverage_Addr, covProb->Track_Coverage_Sig.getSig(), methodId);
                break;
            }

            default:
                break;
        }
    }

    // adding probes for branch targets now as they can point anywhere in the code
    for (auto targetAndBranch : branchTargets) {
        ILInstr *target = targetAndBranch.first;
        ILInstr *branch = targetAndBranch.second;

        if (target == pilr->GetILList() || coveredInstructions.find(target->m_offset) != coveredInstructions.end())
            continue;
        coveredInstructions.insert(target->m_offset);

        if (!IsTailcallRet(target)) {
            AddCoverageProbeWithNop(pilr, target, covProb->Track_Coverage_Addr, covProb->Track_Coverage_Sig.getSig(), methodId);
            continue;
        }

        // target is a ret after a tailcall:
        //
        // tail.
        // call
        // ret
        //
        // cannot add instructions before the return; changing the target of the original branch and creating new ret

        // generated IL-code:
        //
        // 0: <normal control flow before the original branch>
        // 1: original branch with target to 3
        // 2: br with target to 5
        // 3: probe call
        // 4: ret
        // 5: <normal control flow after the original branch>

        // inserting new ret after the branch
        ILInstr *pNewRet = pilr->NewILInstr();
        pNewRet->m_opcode = CEE_RET;
        pilr->InsertAfter(branch, pNewRet);

        // remembering the original ret's offset
        ILInstr *probeStart = AddLDCInstrBefore(pilr, pNewRet, (INT32)target->m_offset);

        AddLDCInstrBefore(pilr, pNewRet, methodId);

        // adding leave probe as it's a normal return without tailcall
        IfFailRet(AddProbe(pilr, leaveMethodAddress, leaveMethodSignature, pNewRet));

        // rerouting the original branch target to our probe
        branch->m_pTarget = probeStart;

        if (branch->m_opcode == CEE_BR || branch->m_opcode == CEE_BR_S)
            continue; // the branch is unconditional so we will always go to the target; no need for the skipping branch

        // adding unconditional branch to skip the inserted instructions in case the branch fails
        ILInstr *skipBranch = pilr->NewILInstr();
        skipBranch->m_opcode = CEE_BR;
        skipBranch->m_pTarget = pNewRet->m_pNext;
        pilr->InsertAfter(branch, skipBranch);
    }

    IfFailRet(AddEnterProbe(&rewriter, enterMethodAddress, enterMethodSignature, methodId));

    if (isMain) {
        LOG(tout << "rewritten main method: ");
        PrintILInstructions(pilr);
    }

    IfFailRet(rewriter.Export());

    return S_OK;
}
