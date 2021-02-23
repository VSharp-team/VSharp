#ifndef INSTRUCTIONS_H_
#define INSTRUCTIONS_H_

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

static const char* s_OpCodeNames[] =
{
#define OPDEF(c,s,pop,push,args,type,l,s1,s2,flow) s,
#include "opcode.def"
#undef OPDEF
    "<count>",
    "<switch_arg>"
};

static const char s_OpCodeFlags[] =
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
};

static int k_rgnStackPops[] = {

#define OPDEF(c,s,pop,push,args,type,l,s1,s2,ctrl) \
	 pop ,

#define Pop0    0
#define Pop1    1
#define PopI    1
#define PopI4   1
#define PopR4   1
#define PopI8   1
#define PopR8   1
#define PopRef  1
#define VarPop  1          // Test code doesn't call vararg fcns, so this should not be used

#include "opcode.def"

#undef Pop0
#undef Pop1
#undef PopI
#undef PopI4
#undef PopR4
#undef PopI8
#undef PopR8
#undef PopRef
#undef VarPop
#undef OPDEF
};

#endif // INSTRUCTIONS_H_
