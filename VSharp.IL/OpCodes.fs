namespace VSharp

open System.Reflection
open System.Reflection.Emit

exception IncorrectCIL of string

type OpCodeValues =
    | Nop               = 0x0000s
    | Break             = 0x0001s
    | Ldarg_0           = 0x0002s
    | Ldarg_1           = 0x0003s
    | Ldarg_2           = 0x0004s
    | Ldarg_3           = 0x0005s
    | Ldloc_0           = 0x0006s
    | Ldloc_1           = 0x0007s
    | Ldloc_2           = 0x0008s
    | Ldloc_3           = 0x0009s
    | Stloc_0           = 0x000As
    | Stloc_1           = 0x000Bs
    | Stloc_2           = 0x000Cs
    | Stloc_3           = 0x000Ds
    | Ldarg_S           = 0x000Es
    | Ldarga_S          = 0x000Fs
    | Starg_S           = 0x0010s
    | Ldloc_S           = 0x0011s
    | Ldloca_S          = 0x0012s
    | Stloc_S           = 0x0013s
    | Ldnull            = 0x0014s
    | Ldc_I4_M1         = 0x0015s
    | Ldc_I4_0          = 0x0016s
    | Ldc_I4_1          = 0x0017s
    | Ldc_I4_2          = 0x0018s
    | Ldc_I4_3          = 0x0019s
    | Ldc_I4_4          = 0x001As
    | Ldc_I4_5          = 0x001Bs
    | Ldc_I4_6          = 0x001Cs
    | Ldc_I4_7          = 0x001Ds
    | Ldc_I4_8          = 0x001Es
    | Ldc_I4_S          = 0x001Fs
    | Ldc_I4            = 0x0020s
    | Ldc_I8            = 0x0021s
    | Ldc_R4            = 0x0022s
    | Ldc_R8            = 0x0023s
    | Dup               = 0x0025s
    | Pop               = 0x0026s
    | Jmp               = 0x0027s
    | Call              = 0x0028s
    | Calli             = 0x0029s
    | Ret               = 0x002As
    | Br_S              = 0x002Bs
    | Brfalse_S         = 0x002Cs
    | Brtrue_S          = 0x002Ds
    | Beq_S             = 0x002Es
    | Bge_S             = 0x002Fs
    | Bgt_S             = 0x0030s
    | Ble_S             = 0x0031s
    | Blt_S             = 0x0032s
    | Bne_Un_S          = 0x0033s
    | Bge_Un_S          = 0x0034s
    | Bgt_Un_S          = 0x0035s
    | Ble_Un_S          = 0x0036s
    | Blt_Un_S          = 0x0037s
    | Br                = 0x0038s
    | Brfalse           = 0x0039s
    | Brtrue            = 0x003As
    | Beq               = 0x003Bs
    | Bge               = 0x003Cs
    | Bgt               = 0x003Ds
    | Ble               = 0x003Es
    | Blt               = 0x003Fs
    | Bne_Un            = 0x0040s
    | Bge_Un            = 0x0041s
    | Bgt_Un            = 0x0042s
    | Ble_Un            = 0x0043s
    | Blt_Un            = 0x0044s
    | Switch            = 0x0045s
    | Ldind_I1          = 0x0046s
    | Ldind_U1          = 0x0047s
    | Ldind_I2          = 0x0048s
    | Ldind_U2          = 0x0049s
    | Ldind_I4          = 0x004As
    | Ldind_U4          = 0x004Bs
    | Ldind_I8          = 0x004Cs
    | Ldind_I           = 0x004Ds
    | Ldind_R4          = 0x004Es
    | Ldind_R8          = 0x004Fs
    | Ldind_Ref         = 0x0050s
    | Stind_Ref         = 0x0051s
    | Stind_I1          = 0x0052s
    | Stind_I2          = 0x0053s
    | Stind_I4          = 0x0054s
    | Stind_I8          = 0x0055s
    | Stind_R4          = 0x0056s
    | Stind_R8          = 0x0057s
    | Add               = 0x0058s
    | Sub               = 0x0059s
    | Mul               = 0x005As
    | Div               = 0x005Bs
    | Div_Un            = 0x005Cs
    | Rem               = 0x005Ds
    | Rem_Un            = 0x005Es
    | And               = 0x005Fs
    | Or                = 0x0060s
    | Xor               = 0x0061s
    | Shl               = 0x0062s
    | Shr               = 0x0063s
    | Shr_Un            = 0x0064s
    | Neg               = 0x0065s
    | Not               = 0x0066s
    | Conv_I1           = 0x0067s
    | Conv_I2           = 0x0068s
    | Conv_I4           = 0x0069s
    | Conv_I8           = 0x006As
    | Conv_R4           = 0x006Bs
    | Conv_R8           = 0x006Cs
    | Conv_U4           = 0x006Ds
    | Conv_U8           = 0x006Es
    | Callvirt          = 0x006Fs
    | Cpobj             = 0x0070s
    | Ldobj             = 0x0071s
    | Ldstr             = 0x0072s
    | Newobj            = 0x0073s
    | Castclass         = 0x0074s
    | Isinst            = 0x0075s
    | Conv_R_Un         = 0x0076s
    | Unbox             = 0x0079s
    | Throw             = 0x007As
    | Ldfld             = 0x007Bs
    | Ldflda            = 0x007Cs
    | Stfld             = 0x007Ds
    | Ldsfld            = 0x007Es
    | Ldsflda           = 0x007Fs
    | Stsfld            = 0x0080s
    | Stobj             = 0x0081s
    | Conv_Ovf_I1_Un    = 0x0082s
    | Conv_Ovf_I2_Un    = 0x0083s
    | Conv_Ovf_I4_Un    = 0x0084s
    | Conv_Ovf_I8_Un    = 0x0085s
    | Conv_Ovf_U1_Un    = 0x0086s
    | Conv_Ovf_U2_Un    = 0x0087s
    | Conv_Ovf_U4_Un    = 0x0088s
    | Conv_Ovf_U8_Un    = 0x0089s
    | Conv_Ovf_I_Un     = 0x008As
    | Conv_Ovf_U_Un     = 0x008Bs
    | Box               = 0x008Cs
    | Newarr            = 0x008Ds
    | Ldlen             = 0x008Es
    | Ldelema           = 0x008Fs
    | Ldelem_I1         = 0x0090s
    | Ldelem_U1         = 0x0091s
    | Ldelem_I2         = 0x0092s
    | Ldelem_U2         = 0x0093s
    | Ldelem_I4         = 0x0094s
    | Ldelem_U4         = 0x0095s
    | Ldelem_I8         = 0x0096s
    | Ldelem_I          = 0x0097s
    | Ldelem_R4         = 0x0098s
    | Ldelem_R8         = 0x0099s
    | Ldelem_Ref        = 0x009As
    | Stelem_I          = 0x009Bs
    | Stelem_I1         = 0x009Cs
    | Stelem_I2         = 0x009Ds
    | Stelem_I4         = 0x009Es
    | Stelem_I8         = 0x009Fs
    | Stelem_R4         = 0x00A0s
    | Stelem_R8         = 0x00A1s
    | Stelem_Ref        = 0x00A2s
    | Ldelem            = 0x00A3s
    | Stelem            = 0x00A4s
    | Unbox_Any         = 0x00A5s
    | Conv_Ovf_I1       = 0x00B3s
    | Conv_Ovf_U1       = 0x00B4s
    | Conv_Ovf_I2       = 0x00B5s
    | Conv_Ovf_U2       = 0x00B6s
    | Conv_Ovf_I4       = 0x00B7s
    | Conv_Ovf_U4       = 0x00B8s
    | Conv_Ovf_I8       = 0x00B9s
    | Conv_Ovf_U8       = 0x00BAs
    | Refanyval         = 0x00C2s
    | Ckfinite          = 0x00C3s
    | Mkrefany          = 0x00C6s
    | Ldtoken           = 0x00D0s
    | Conv_U2           = 0x00D1s
    | Conv_U1           = 0x00D2s
    | Conv_I            = 0x00D3s
    | Conv_Ovf_I        = 0x00D4s
    | Conv_Ovf_U        = 0x00D5s
    | Add_Ovf           = 0x00D6s
    | Add_Ovf_Un        = 0x00D7s
    | Mul_Ovf           = 0x00D8s
    | Mul_Ovf_Un        = 0x00D9s
    | Sub_Ovf           = 0x00DAs
    | Sub_Ovf_Un        = 0x00DBs
    | Endfinally        = 0x00DCs
    | Leave             = 0x00DDs
    | Leave_S           = 0x00DEs
    | Stind_I           = 0x00DFs
    | Conv_U            = 0x00E0s
    | Prefix7           = 0x00F8s
    | Prefix6           = 0x00F9s
    | Prefix5           = 0x00FAs
    | Prefix4           = 0x00FBs
    | Prefix3           = 0x00FCs
    | Prefix2           = 0x00FDs
    | Prefix1           = 0x00FEs
    | Prefixref         = 0x00FFs
    | Arglist           = 0xFE00s
    | Ceq               = 0xFE01s
    | Cgt               = 0xFE02s
    | Cgt_Un            = 0xFE03s
    | Clt               = 0xFE04s
    | Clt_Un            = 0xFE05s
    | Ldftn             = 0xFE06s
    | Ldvirtftn         = 0xFE07s
    | Ldarg             = 0xFE09s
    | Ldarga            = 0xFE0As
    | Starg             = 0xFE0Bs
    | Ldloc             = 0xFE0Cs
    | Ldloca            = 0xFE0Ds
    | Stloc             = 0xFE0Es
    | Localloc          = 0xFE0Fs
    | Endfilter         = 0xFE11s
    | Unaligned_        = 0xFE12s
    | Volatile_         = 0xFE13s
    | Tail_             = 0xFE14s
    | Initobj           = 0xFE15s
    | Constrained_      = 0xFE16s
    | Cpblk             = 0xFE17s
    | Initblk           = 0xFE18s
    | Rethrow           = 0xFE1As
    | Sizeof            = 0xFE1Cs
    | Refanytype        = 0xFE1Ds
    | Readonly_         = 0xFE1Es

type offset = int<byte_offset>
module Offset =
    let from (x : int) : offset = LanguagePrimitives.Int32WithMeasure x

module internal OpCodeOperations =

    let isSingleByteOpCodeValue (opCode : OpCode) = opCode.Size = 1
    let isSingleByteOpCode = (<>) OpCodes.Prefix1.Value

    let private equalSizeOpCodesCount = 0x100
    let singleByteOpCodes = Array.create equalSizeOpCodesCount OpCodes.Nop;
    let twoBytesOpCodes = Array.create equalSizeOpCodesCount OpCodes.Nop;

    do
        // Filling opcodes
        let (&&&) = Microsoft.FSharp.Core.Operators.(&&&)
        for field in typeof<OpCodes>.GetRuntimeFields() do
            match field.GetValue() with
            | :? OpCode as opCode ->
                let value = int opCode.Value
                if isSingleByteOpCodeValue opCode then singleByteOpCodes[value] <- opCode
                else twoBytesOpCodes[value &&& 0xFF] <- opCode
            | _ -> ()

    let getOpCode (ilBytes : byte[]) (offset : offset) =
        let offset = int offset
        let b1 = int16 ilBytes[offset]
        if isSingleByteOpCode b1 then singleByteOpCodes[int b1]
        elif offset + 1 >= ilBytes.Length then raise (IncorrectCIL("Prefix instruction FE without suffix!"))
        else twoBytesOpCodes[int ilBytes[offset + 1]]

    let writeOpCode (ilBytes : byte[]) (offset : offset) opCode =
        let offset = int offset
        if isSingleByteOpCodeValue opCode then
            ilBytes[offset] <- byte (opCode.Value &&& 0xFFs)
        else
            ilBytes[offset] <- byte OpCodes.Prefix1.Value
            ilBytes[offset + 1] <- byte (opCode.Value &&& 0xFFs)
