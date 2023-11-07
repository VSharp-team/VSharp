namespace VSharp

open System.Collections.Generic
open System.Reflection
open global.System
open System.Reflection.Emit
open VSharp
open System.Runtime.InteropServices

[<type: StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
type probes = {
    mutable ldarg_0 : uint64
    mutable ldarg_1 : uint64
    mutable ldarg_2 : uint64
    mutable ldarg_3 : uint64
    mutable ldarg_S : uint64
    mutable ldarg : uint64
    mutable ldarga : uint64

    mutable ldloc_0 : uint64
    mutable ldloc_1 : uint64
    mutable ldloc_2 : uint64
    mutable ldloc_3 : uint64
    mutable ldloc_S : uint64
    mutable ldloc : uint64
    mutable ldloca : uint64

    mutable starg_S : uint64
    mutable starg : uint64
    mutable stloc_0 : uint64
    mutable stloc_1 : uint64
    mutable stloc_2 : uint64
    mutable stloc_3 : uint64
    mutable stloc_S : uint64
    mutable stloc : uint64

    mutable ldc : uint64
    mutable dup : uint64
    mutable pop : uint64

    mutable brtrue : uint64
    mutable brfalse : uint64
    mutable switch : uint64

    mutable unOp : uint64
    mutable binOp : uint64
    mutable execBinOp_4 : uint64
    mutable execBinOp_8 : uint64
    mutable execBinOp_f4 : uint64
    mutable execBinOp_f8 : uint64
    mutable execBinOp_p : uint64
    mutable execBinOp_8_4 : uint64
    mutable execBinOp_4_p : uint64
    mutable execBinOp_p_4 : uint64
    mutable execBinOp_4_ovf : uint64
    mutable execBinOp_8_ovf : uint64
    mutable execBinOp_f4_ovf : uint64
    mutable execBinOp_f8_ovf : uint64
    mutable execBinOp_p_ovf : uint64
    mutable execBinOp_8_4_ovf : uint64
    mutable execBinOp_4_p_ovf : uint64
    mutable execBinOp_p_4_ovf : uint64

    mutable ldind : uint64
    mutable stind : uint64
    mutable execStind_I1 : uint64
    mutable execStind_I2 : uint64
    mutable execStind_I4 : uint64
    mutable execStind_I8 : uint64
    mutable execStind_R4 : uint64
    mutable execStind_R8 : uint64
    mutable execStind_ref : uint64

    mutable conv : uint64
    mutable conv_Ovf : uint64

    mutable newarr : uint64
    mutable localloc : uint64
    mutable ldobj : uint64
    mutable ldstr : uint64
    mutable ldtoken : uint64
    mutable stobj : uint64
    mutable initobj : uint64
    mutable ldlen : uint64

    mutable cpobj : uint64
    mutable execCpobj : uint64
    mutable cpblk : uint64
    mutable execCpblk : uint64
    mutable initblk : uint64
    mutable execInitblk : uint64

    mutable castclass : uint64
    mutable isinst : uint64

    mutable box : uint64
    mutable unbox : uint64
    mutable unboxAny : uint64

    mutable ldfld : uint64
    mutable ldflda : uint64
    mutable stfld_4 : uint64
    mutable stfld_8 : uint64
    mutable stfld_f4 : uint64
    mutable stfld_f8 : uint64
    mutable stfld_p : uint64
    mutable stfld_struct : uint64

    mutable ldsfld : uint64
    mutable ldsflda : uint64
    mutable stsfld : uint64

    mutable ldelema : uint64
    mutable ldelem : uint64
    mutable execLdelema : uint64
    mutable execLdelem : uint64

    mutable stelem : uint64
    mutable execStelem_I : uint64
    mutable execStelem_I1 : uint64
    mutable execStelem_I2 : uint64
    mutable execStelem_I4 : uint64
    mutable execStelem_I8 : uint64
    mutable execStelem_R4 : uint64
    mutable execStelem_R8 : uint64
    mutable execStelem_Ref : uint64
    mutable execStelem_Struct : uint64

    mutable ckfinite : uint64
    mutable sizeof : uint64
    mutable ldftn : uint64
    mutable ldvirtftn : uint64
    mutable arglist : uint64
    mutable mkrefany : uint64

    mutable enter : uint64
    mutable enterMain : uint64
    mutable leave : uint64
    mutable leaveMain_0 : uint64
    mutable leaveMain_4 : uint64
    mutable leaveMain_8 : uint64
    mutable leaveMain_f4 : uint64
    mutable leaveMain_f8 : uint64
    mutable leaveMain_p : uint64
    mutable finalizeCall : uint64
    mutable execCall : uint64
    mutable call : uint64
    mutable pushFrame : uint64
    mutable callVirt : uint64
    mutable newobj : uint64
    mutable calli : uint64
    mutable throw : uint64
    mutable rethrow : uint64

    mutable mem_p : uint64
    mutable mem_1_idx : uint64
    mutable mem_2_idx : uint64
    mutable mem_4_idx : uint64
    mutable mem_8_idx : uint64
    mutable mem_f4_idx : uint64
    mutable mem_f8_idx : uint64
    mutable mem_p_idx : uint64
    mutable mem2_4 : uint64
    mutable mem2_8 : uint64
    mutable mem2_f4 : uint64
    mutable mem2_f8 : uint64
//    mutable mem2_p : uint64
    mutable mem2_8_4 : uint64
//    mutable mem2_4_p : uint64
//    mutable mem2_p_1 : uint64
//    mutable mem2_p_2 : uint64
//    mutable mem2_p_4 : uint64
//    mutable mem2_p_8 : uint64
//    mutable mem2_p_f4 : uint64
//    mutable mem2_p_f8 : uint64
//    mutable mem3_p_p_p : uint64
//    mutable mem3_p_p_i1 : uint64
//    mutable mem3_p_p_i2 : uint64
//    mutable mem3_p_p_i4 : uint64
//    mutable mem3_p_p_i8 : uint64
//    mutable mem3_p_p_f4 : uint64
//    mutable mem3_p_p_f8 : uint64
//    mutable mem3_p_i1_p : uint64
    mutable unmem_1 : uint64
    mutable unmem_2 : uint64
    mutable unmem_4 : uint64
    mutable unmem_8 : uint64
    mutable unmem_f4 : uint64
    mutable unmem_f8 : uint64
    mutable unmem_p : uint64

    mutable dumpInstruction : uint64
}
with
    member private x.Probe2str =
        let map = Dictionary<uint64, string>()
        typeof<probes>.GetFields() |> Seq.iter (fun fld -> map.Add(fld.GetValue x |> unbox, fld.Name))
        map
    member x.AddressToString (address : int64) =
        let result = ref ""
        if x.Probe2str.TryGetValue(uint64 address, result) then "probe_" + result.Value
        else toString address

[<type: StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
type signatureTokens = {
    mutable void_sig : uint32
    mutable bool_sig : uint32
    mutable void_u1_sig : uint32
    mutable void_u4_sig : uint32
    mutable void_i_sig : uint32
    mutable bool_i_sig : uint32
    mutable bool_u2_sig : uint32
    mutable i1_i1_sig : uint32
    mutable i2_i1_sig : uint32
    mutable i4_i1_sig : uint32
    mutable i8_i1_sig : uint32
    mutable r4_i1_sig : uint32
    mutable r8_i1_sig : uint32
    mutable i_i1_sig : uint32
    mutable void_i_i1_sig : uint32
    mutable void_i_i2_sig : uint32
    mutable void_i_u2_sig : uint32
    mutable void_i_i4_sig : uint32
    mutable void_i_i8_sig : uint32
    mutable void_i_r4_sig : uint32
    mutable void_i_r8_sig : uint32
    mutable void_i_i_sig : uint32
    mutable void_i4_i4_sig : uint32
    mutable void_i4_i_sig : uint32
    mutable void_i8_i4_sig : uint32
    mutable void_i8_i8_sig : uint32
    mutable void_r4_r4_sig : uint32
    mutable void_r8_r8_sig : uint32
    mutable bool_i_i4_sig : uint32
    mutable bool_i_i_sig : uint32
    mutable void_i_i_i_sig : uint32
    mutable void_i_i_i1_sig : uint32
    mutable void_i_i_i2_sig : uint32
    mutable void_i_i_i4_sig : uint32
    mutable void_i_i_i8_sig : uint32
    mutable void_i_i_r4_sig : uint32
    mutable void_i_i_r8_sig : uint32
    mutable void_i_i1_i_sig : uint32
    mutable void_i1_i1_i1_sig : uint32
    mutable void_i2_i1_i1_sig : uint32
    mutable void_i4_i1_i1_sig : uint32
    mutable void_i8_i1_i1_sig : uint32
    mutable void_r4_i1_i1_sig : uint32
    mutable void_r8_i1_i1_sig : uint32
    mutable void_i_i1_i1_sig : uint32
    mutable void_token_u2_bool_u4_u4_sig : uint32
    mutable void_offset_sig : uint32
    mutable void_u1_offset_sig : uint32
    mutable void_u2_offset_sig : uint32
    mutable void_i4_offset_sig : uint32
    mutable void_i8_offset_sig : uint32
    mutable void_r4_offset_sig : uint32
    mutable void_r8_offset_sig : uint32
    mutable void_i_offset_sig : uint32
    mutable void_token_offset_sig : uint32
    mutable void_i_i1_offset_sig : uint32
    mutable void_i_i2_offset_sig : uint32
    mutable void_i_i4_offset_sig : uint32
    mutable void_i_i8_offset_sig : uint32
    mutable void_i_r4_offset_sig : uint32
    mutable void_i_r8_offset_sig : uint32
    mutable void_i_i_offset_sig : uint32
    mutable void_i_token_offset_sig : uint32
    mutable void_i_i4_i4_offset_sig : uint32
    mutable void_u2_i4_i4_offset_sig : uint32
    mutable void_u2_i4_i_offset_sig : uint32
    mutable void_u2_i8_i4_offset_sig : uint32
    mutable void_u2_i8_i8_offset_sig : uint32
    mutable void_u2_r4_r4_offset_sig : uint32
    mutable void_u2_r8_r8_offset_sig : uint32
    mutable void_u2_i_i_offset_sig : uint32
    mutable void_u2_i_i4_offset_sig : uint32
    mutable void_i_i_i_offset_sig : uint32
    mutable void_i_i_i1_offset_sig : uint32
    mutable void_i_i_i2_offset_sig : uint32
    mutable void_i_i_i4_offset_sig : uint32
    mutable void_i_i_i8_offset_sig : uint32
    mutable void_i_i_r4_offset_sig : uint32
    mutable void_i_i_r8_offset_sig : uint32
    mutable void_i_i1_i_offset_sig : uint32
    mutable void_token_u4_u4_u4_sig : uint32
    mutable void_token_i_i_offset_sig : uint32
    mutable void_token_i_i4_offset_sig : uint32
    mutable void_token_i_i8_offset_sig : uint32
    mutable void_token_i_r4_offset_sig : uint32
    mutable void_token_i_r8_offset_sig : uint32
    mutable void_token_token_bool_u2_offset_sig : uint32
}
with
    member private x.SigToken2str =
        let map = Dictionary<uint32, string>()
        typeof<signatureTokens>.GetFields() |> Seq.iter (fun fld ->
            let token : uint32 = fld.GetValue x |> unbox
            if not <| map.ContainsKey token then map.Add(token, fld.Name))
        map
    member x.TokenToString (token : int32) =
        let result = ref ""
        if x.SigToken2str.TryGetValue(uint32 token, result) then result.Value
        else "<UNKNOWN TOKEN!>"

[<type: StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
type rawMethodProperties = {
    mutable token : uint32
    mutable ilCodeSize : uint32
    mutable assemblyNameLength : uint32
    mutable moduleNameLength : uint32
    mutable maxStackSize : uint32
    mutable signatureTokensLength : uint32
}

[<type: StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
type instrumentedMethodProperties = {
    mutable ilCodeSize : uint32
    mutable maxStackSize : uint32
}

[<type: StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
type rawExceptionHandler = {
    mutable flags : int
    mutable tryOffset : uint32
    mutable tryLength : uint32
    mutable handlerOffset : uint32
    mutable handlerLength : uint32
    mutable matcher : uint32
}

type rawMethodBody = {
    properties : rawMethodProperties
    assembly : string
    moduleName : string
    tokens : signatureTokens
    il : byte array
    ehs : rawExceptionHandler array
}

type instrumentedMethodBody = {
    properties : instrumentedMethodProperties
    il : byte array
    ehs : rawExceptionHandler array
}


type evaluationStackCellType =
    | I1 = 1
    | I2 = 2
    | I4 = 3
    | I8 = 4
    | R4 = 5
    | R8 = 6
    | I = 7
    | Ref = 8
    | Struct = 9

type stackState = evaluationStackCellType stack

type opcode =
    | OpCode of OpCode
    | SwitchArg
    override x.ToString() =
        match x with
        | OpCode op -> op.Name
        | SwitchArg -> "<SwitchArg>"
    member x.StackBehaviourPush =
        match x with
        | OpCode op ->
            match op.StackBehaviourPush with
            | StackBehaviour.Push0 -> 0u
            | StackBehaviour.Push1
            | StackBehaviour.Pushi
            | StackBehaviour.Pushi8
            | StackBehaviour.Pushr4
            | StackBehaviour.Pushr8
            | StackBehaviour.Pushref -> 1u
            | StackBehaviour.Push1_push1 -> 2u
            | StackBehaviour.Varpush -> 1u
            | _ -> __unreachable__()
        | SwitchArg -> 0u
    member x.StackBehaviourPop =
        match x with
        | OpCode op ->
            match op.StackBehaviourPop with
            | StackBehaviour.Pop0 -> 0u
            | StackBehaviour.Pop1
            | StackBehaviour.Popi
            | StackBehaviour.Popref -> 1u
            | StackBehaviour.Pop1_pop1
            | StackBehaviour.Popi_pop1
            | StackBehaviour.Popi_popi
            | StackBehaviour.Popi_popi8
            | StackBehaviour.Popi_popr4
            | StackBehaviour.Popi_popr8
            | StackBehaviour.Popref_pop1
            | StackBehaviour.Popref_popi -> 2u
            | StackBehaviour.Popi_popi_popi
            | StackBehaviour.Popref_popi_popi
            | StackBehaviour.Popref_popi_popi8
            | StackBehaviour.Popref_popi_popr4
            | StackBehaviour.Popref_popi_popr8
            | StackBehaviour.Popref_popi_popref
            | StackBehaviour.Popref_popi_pop1 -> 3u
            | StackBehaviour.Varpop -> __unreachable__()
            | _ -> __unreachable__()
        | SwitchArg -> 0u

type ilInstrOperand =
    | NoArg
    | Target of ilInstr
    | Arg8 of byte
    | Arg16 of int16
    | Arg32 of int32
    | Arg64 of int64

and ilInstr = {
    mutable prev : ilInstr
    mutable next : ilInstr
    mutable opcode : opcode
    mutable offset : uint32
    mutable stackState : stackState option
    mutable arg : ilInstrOperand
}
with
    member x.Arg8 with get() =
        match x.arg with
        | Arg8 v -> v
        | _ -> internalfailf "Requesting 8-bit arg of instruction %O with arg %O" x.opcode x.arg
    member x.Arg16 with get() =
        match x.arg with
        | Arg16 v -> v
        | _ -> internalfailf "Requesting 16-bit arg of instruction %O with arg %O" x.opcode x.arg
    member x.Arg32 with get() =
        match x.arg with
        | Arg32 v -> v
        | _ -> internalfailf "Requesting 32-bit arg of instruction %O with arg %O" x.opcode x.arg
    member x.Arg64 with get() =
        match x.arg with
        | Arg64 v -> v
        | _ -> internalfailf "Requesting 64-bit arg of instruction %O with arg %O" x.opcode x.arg
    member x.Target with get() =
        match x.arg with
        | Target v -> v
        | _ -> internalfailf "Requesting target arg of instruction %O with arg %O" x.opcode x.arg
    override x.ToString() =
        x.opcode.ToString()

type ehClauseMatcher =
    | ClassToken of uint32
    | FilterEH of ilInstr

type ehClause = {
    flags : int
    tryBegin : ilInstr
    mutable tryEnd : ilInstr
    handlerBegin : ilInstr
    mutable handlerEnd : ilInstr
    matcher : ehClauseMatcher
}

module NumberCreator =
    let public extractInt32 (ilBytes : byte []) (pos : offset) =
        BitConverter.ToInt32(ilBytes, int pos)
    let public extractOffset (ilBytes : byte []) (pos : offset) : offset =
        BitConverter.ToInt32(ilBytes, int pos) |> Offset.from
    let public extractUnsignedInt32 (ilBytes : byte []) (pos : offset) =
        BitConverter.ToUInt32(ilBytes, int pos)
    let public extractUnsignedInt16 (ilBytes : byte []) (pos : offset) =
        BitConverter.ToUInt16(ilBytes, int pos)
    let public extractInt64 (ilBytes : byte []) (pos : offset) =
        BitConverter.ToInt64(ilBytes, int pos)
    let public extractInt8 (ilBytes : byte []) (pos : offset) =
        ilBytes[int pos] |> sbyte |> int
    let public extractUnsignedInt8 (ilBytes : byte []) (pos : offset) =
        ilBytes[int pos]
    let public extractFloat64 (ilBytes : byte []) (pos : offset) =
        BitConverter.ToDouble(ilBytes, int pos)
    let public extractFloat32 (ilBytes : byte []) (pos : offset) =
        BitConverter.ToSingle(ilBytes, int pos)

module EvaluationStackTyper =

    let fail() = internalfail "Stack typer validation failed!"

    let typeAbstraction =
        let result = Dictionary<int32 *int32, evaluationStackCellType>()
        result.Add((typeof<int8>.Module.MetadataToken, typeof<int8>.MetadataToken), evaluationStackCellType.I1)
        result.Add((typeof<uint8>.Module.MetadataToken, typeof<uint8>.MetadataToken), evaluationStackCellType.I1)
        result.Add((typeof<char>.Module.MetadataToken, typeof<char>.MetadataToken), evaluationStackCellType.I2)
        result.Add((typeof<bool>.Module.MetadataToken, typeof<bool>.MetadataToken), evaluationStackCellType.I1)
        result.Add((typeof<int16>.Module.MetadataToken, typeof<int16>.MetadataToken), evaluationStackCellType.I2)
        result.Add((typeof<uint16>.Module.MetadataToken, typeof<uint16>.MetadataToken), evaluationStackCellType.I2)
        result.Add((typeof<int32>.Module.MetadataToken, typeof<int32>.MetadataToken), evaluationStackCellType.I4)
        result.Add((typeof<uint32>.Module.MetadataToken, typeof<uint32>.MetadataToken), evaluationStackCellType.I4)
        result.Add((typeof<int64>.Module.MetadataToken, typeof<int64>.MetadataToken), evaluationStackCellType.I8)
        result.Add((typeof<uint64>.Module.MetadataToken, typeof<uint64>.MetadataToken), evaluationStackCellType.I8)
        result.Add((typeof<float32>.Module.MetadataToken, typeof<float32>.MetadataToken), evaluationStackCellType.R4)
        result.Add((typeof<double>.Module.MetadataToken, typeof<double>.MetadataToken), evaluationStackCellType.R8)
        result.Add((typeof<IntPtr>.Module.MetadataToken, typeof<IntPtr>.MetadataToken), evaluationStackCellType.I)
        result.Add((typeof<UIntPtr>.Module.MetadataToken, typeof<UIntPtr>.MetadataToken), evaluationStackCellType.I)
        result

    let abstractType (typ : Type) =
        if typ.IsValueType then
            let typ = if typ.IsEnum then EnumUtils.getEnumUnderlyingTypeChecked typ else typ
            let result = ref evaluationStackCellType.I1
            if typeAbstraction.TryGetValue((typ.Module.MetadataToken, typ.MetadataToken), result) then result.Value
            else evaluationStackCellType.Struct
        else evaluationStackCellType.Ref

    let push (s : stackState) = abstractType >> Stack.push s

    let take (s : stackState) count =
        if Stack.size s < count then fail()
        Seq.take count s |> Seq.rev |> List.ofSeq

    let isI4 = function
        | evaluationStackCellType.I1
        | evaluationStackCellType.I2
        | evaluationStackCellType.I4 -> true
        | _ -> false

    let isFloat = function
        | evaluationStackCellType.R4
        | evaluationStackCellType.R8 -> true
        | _ -> false

    let mergeAbstraction a1 a2 =
        if a1 = a2 then a1
        elif isI4 a1 && isI4 a2 then evaluationStackCellType.I4
        elif isFloat a1 && isFloat a2 then evaluationStackCellType.R8
        else fail()

    let mergeStackStates s1 s2 = List.map2 mergeAbstraction s1 s2

    let typeLdarg (m : MethodBase) (s : stackState) idx =
        let hasThis = m.CallingConvention.HasFlag(CallingConventions.HasThis)
        if hasThis && idx = 0 then
            Stack.push s evaluationStackCellType.Ref
        else
            let idx = if hasThis then idx - 1 else idx
            m.GetParameters().[idx].ParameterType |> push s

    let typeLdloc (m : MethodBase) (s : stackState) idx =
        m.GetMethodBody().LocalVariables.[idx].LocalType |> push s

    let typeBinop (s : stackState) =
        // See ECMA-335, sec. III.1.5
        let t1, s = Stack.pop s
        let t2, s = Stack.pop s
        let t1_is_I4 =
            match t1 with
            | evaluationStackCellType.I1
            | evaluationStackCellType.I2
            | evaluationStackCellType.I4 -> true
            | _ -> false
        let t2_is_I4 =
            match t2 with
            | evaluationStackCellType.I1
            | evaluationStackCellType.I2
            | evaluationStackCellType.I4 -> true
            | _ -> false
        let t1_is_F =
            match t1 with
            | evaluationStackCellType.R4
            | evaluationStackCellType.R8 -> true
            | _ -> false
        let t2_is_F =
            match t2 with
            | evaluationStackCellType.R4
            | evaluationStackCellType.R8 -> true
            | _ -> false
        if t1_is_I4 && t2_is_I4 then
            if t1 = t2 then t1 else evaluationStackCellType.I4
        elif t1 = evaluationStackCellType.I8 && t2 = evaluationStackCellType.I8 then evaluationStackCellType.I8
        elif t1_is_F && t2_is_F then
            if t1 = t2 then t1 else evaluationStackCellType.R8
        elif t1 = evaluationStackCellType.I || t1 = evaluationStackCellType.Ref then t1
        elif t2 = evaluationStackCellType.I || t2 = evaluationStackCellType.Ref then t2
        else fail()
        |> Stack.push s
    let typeShiftOp (s : stackState) =
        // TODO: implement fully #do
        // See ECMA-335, sec. III.1.5, table III.6
        let _, s = Stack.pop s
        let t2, s = Stack.pop s
        Stack.push s t2

//    let REMOVE_ME (m : Reflection.MethodBase) (instr : ilInstr) =
//        let instr, arg =
//            match instr.opcode with
//            | OpCode op ->
//                let arg =
//                    if op = OpCodes.Call || op = OpCodes.Callvirt || op = OpCodes.Newobj then
//                        match instr.arg with
//                        | Arg32 token ->
//                            let callee = Reflection.resolveMethod m token
//                            Reflection.methodToString callee
//                        | _ -> __unreachable__()
//                    else
//                        match instr.arg with
//                        | NoArg -> ""
//                        | Arg8 a -> a.ToString()
//                        | Arg16 a -> a.ToString()
//                        | Arg32 a -> a.ToString()
//                        | Target t ->
//                            match t.opcode with
//                            | OpCode op -> op.Name
//                            | SwitchArg _ -> "<SwitchArg>"
//                op.Name, arg
//            | SwitchArg -> "<SwitchArg>", ""
//        instr + " " + arg

    let typeInstruction (m : MethodBase) (instr : ilInstr) =
        let s =
            match instr.stackState with
            | Some s -> s
            | None -> fail()
//       let res =
        match instr.opcode with
        | OpCode op ->
//            Logger.trace "typer before: [%O] %O: %O" instr.offset (REMOVE_ME m instr) s.Length
            let opcodeValue = LanguagePrimitives.EnumOfValue op.Value
            match opcodeValue with
            | OpCodeValues.Ldarg_0 -> typeLdarg m s 0
            | OpCodeValues.Ldarg_1 -> typeLdarg m s 1
            | OpCodeValues.Ldarg_2 -> typeLdarg m s 2
            | OpCodeValues.Ldarg_3 -> typeLdarg m s 3
            | OpCodeValues.Ldarg_S -> instr.Arg8 |> int |> typeLdarg m s
            | OpCodeValues.Ldarg -> instr.Arg16 |> int |> typeLdarg m s
            | OpCodeValues.Ldloc_0 -> typeLdloc m s 0
            | OpCodeValues.Ldloc_1 -> typeLdloc m s 1
            | OpCodeValues.Ldloc_2 -> typeLdloc m s 2
            | OpCodeValues.Ldloc_3 -> typeLdloc m s 3
            | OpCodeValues.Ldloc_S -> instr.Arg8 |> int |> typeLdloc m s
            | OpCodeValues.Ldloc -> instr.Arg16 |> int |> typeLdloc m s

            | OpCodeValues.Ldarga_S
            | OpCodeValues.Ldloca_S
            | OpCodeValues.Ldarga
            | OpCodeValues.Ldloca -> Stack.push s evaluationStackCellType.I

            | OpCodeValues.Stloc_0
            | OpCodeValues.Stloc_1
            | OpCodeValues.Stloc_2
            | OpCodeValues.Stloc_3
            | OpCodeValues.Starg_S
            | OpCodeValues.Stloc_S
            | OpCodeValues.Starg
            | OpCodeValues.Stloc
            | OpCodeValues.Pop

            | OpCodeValues.Brfalse_S
            | OpCodeValues.Brtrue_S
            | OpCodeValues.Brfalse
            | OpCodeValues.Brtrue
            | OpCodeValues.Switch -> Stack.drop 1 s

            | OpCodeValues.Beq_S
            | OpCodeValues.Bge_S
            | OpCodeValues.Bgt_S
            | OpCodeValues.Ble_S
            | OpCodeValues.Blt_S
            | OpCodeValues.Bne_Un_S
            | OpCodeValues.Bge_Un_S
            | OpCodeValues.Bgt_Un_S
            | OpCodeValues.Ble_Un_S
            | OpCodeValues.Blt_Un_S
            | OpCodeValues.Beq
            | OpCodeValues.Bge
            | OpCodeValues.Bgt
            | OpCodeValues.Ble
            | OpCodeValues.Blt
            | OpCodeValues.Bne_Un
            | OpCodeValues.Bge_Un
            | OpCodeValues.Bgt_Un
            | OpCodeValues.Ble_Un
            | OpCodeValues.Blt_Un
            | OpCodeValues.Cpobj -> Stack.drop 2 s

            | OpCodeValues.Ldc_I4_M1
            | OpCodeValues.Ldc_I4_0
            | OpCodeValues.Ldc_I4_1
            | OpCodeValues.Ldc_I4_2
            | OpCodeValues.Ldc_I4_3
            | OpCodeValues.Ldc_I4_4
            | OpCodeValues.Ldc_I4_5
            | OpCodeValues.Ldc_I4_6
            | OpCodeValues.Ldc_I4_7
            | OpCodeValues.Ldc_I4_8
            | OpCodeValues.Ldc_I4_S
            | OpCodeValues.Ldc_I4 -> Stack.push s evaluationStackCellType.I4
            | OpCodeValues.Ldc_I8 -> Stack.push s evaluationStackCellType.I8
            | OpCodeValues.Ldc_R4 -> Stack.push s evaluationStackCellType.R4
            | OpCodeValues.Ldc_R8 -> Stack.push s evaluationStackCellType.R8
            | OpCodeValues.Ldnull -> Stack.push s evaluationStackCellType.Ref

            | OpCodeValues.Dup -> Stack.dup s

            | OpCodeValues.Ldind_I1
            | OpCodeValues.Ldind_U1 -> Stack.push (Stack.drop 1 s) evaluationStackCellType.I1
            | OpCodeValues.Ldind_I2
            | OpCodeValues.Ldind_U2 -> Stack.push (Stack.drop 1 s) evaluationStackCellType.I2
            | OpCodeValues.Ldind_I4
            | OpCodeValues.Ldind_U4 -> Stack.push (Stack.drop 1 s) evaluationStackCellType.I4
            | OpCodeValues.Ldind_I8 -> Stack.push (Stack.drop 1 s) evaluationStackCellType.I8
            | OpCodeValues.Ldind_I -> Stack.push (Stack.drop 1 s) evaluationStackCellType.I
            | OpCodeValues.Ldind_R4 -> Stack.push (Stack.drop 1 s) evaluationStackCellType.R4
            | OpCodeValues.Ldind_R8 -> Stack.push (Stack.drop 1 s) evaluationStackCellType.R8
            | OpCodeValues.Ldind_Ref -> Stack.push (Stack.drop 1 s) evaluationStackCellType.Ref
            | OpCodeValues.Stind_Ref
            | OpCodeValues.Stind_I1
            | OpCodeValues.Stind_I2
            | OpCodeValues.Stind_I4
            | OpCodeValues.Stind_I8
            | OpCodeValues.Stind_R4
            | OpCodeValues.Stind_R8
            | OpCodeValues.Stind_I -> Stack.drop 2 s

            | OpCodeValues.Add
            | OpCodeValues.Sub
            | OpCodeValues.Mul
            | OpCodeValues.Div
            | OpCodeValues.Div_Un
            | OpCodeValues.Rem
            | OpCodeValues.Rem_Un
            | OpCodeValues.And
            | OpCodeValues.Or
            | OpCodeValues.Xor
            | OpCodeValues.Add_Ovf
            | OpCodeValues.Add_Ovf_Un
            | OpCodeValues.Mul_Ovf
            | OpCodeValues.Mul_Ovf_Un
            | OpCodeValues.Sub_Ovf
            | OpCodeValues.Sub_Ovf_Un -> typeBinop s
            | OpCodeValues.Shl
            | OpCodeValues.Shr
            | OpCodeValues.Shr_Un -> typeShiftOp s

            | OpCodeValues.Ceq
            | OpCodeValues.Cgt
            | OpCodeValues.Cgt_Un
            | OpCodeValues.Clt
            | OpCodeValues.Clt_Un -> Stack.push (Stack.drop 2 s) evaluationStackCellType.I4

            | OpCodeValues.Conv_I1
            | OpCodeValues.Conv_U1
            | OpCodeValues.Conv_Ovf_I1
            | OpCodeValues.Conv_Ovf_U1
            | OpCodeValues.Conv_Ovf_I1_Un
            | OpCodeValues.Conv_Ovf_U1_Un -> Stack.push (Stack.drop 1 s) evaluationStackCellType.I1
            | OpCodeValues.Conv_I2
            | OpCodeValues.Conv_U2
            | OpCodeValues.Conv_Ovf_I2
            | OpCodeValues.Conv_Ovf_U2
            | OpCodeValues.Conv_Ovf_U2_Un
            | OpCodeValues.Conv_Ovf_I2_Un -> Stack.push (Stack.drop 1 s) evaluationStackCellType.I2
            | OpCodeValues.Conv_I4
            | OpCodeValues.Conv_U4
            | OpCodeValues.Conv_Ovf_I4
            | OpCodeValues.Conv_Ovf_U4
            | OpCodeValues.Conv_Ovf_I4_Un
            | OpCodeValues.Conv_Ovf_U4_Un -> Stack.push (Stack.drop 1 s) evaluationStackCellType.I4
            | OpCodeValues.Conv_I8
            | OpCodeValues.Conv_U8
            | OpCodeValues.Conv_Ovf_I8
            | OpCodeValues.Conv_Ovf_U8
            | OpCodeValues.Conv_Ovf_I8_Un
            | OpCodeValues.Conv_Ovf_U8_Un -> Stack.push (Stack.drop 1 s) evaluationStackCellType.I8
            | OpCodeValues.Conv_R4 -> Stack.push (Stack.drop 1 s) evaluationStackCellType.R4
            | OpCodeValues.Conv_R8
            | OpCodeValues.Conv_R_Un -> Stack.push (Stack.drop 1 s) evaluationStackCellType.R8
            | OpCodeValues.Conv_I
            | OpCodeValues.Conv_U
            | OpCodeValues.Conv_Ovf_I
            | OpCodeValues.Conv_Ovf_U
            | OpCodeValues.Conv_Ovf_I_Un
            | OpCodeValues.Conv_Ovf_U_Un -> Stack.push (Stack.drop 1 s) evaluationStackCellType.I

            | OpCodeValues.Ldobj -> Stack.push (Stack.drop 1 s) evaluationStackCellType.Struct
            | OpCodeValues.Ldstr -> Stack.push s evaluationStackCellType.Ref
            | OpCodeValues.Unbox -> Stack.push s evaluationStackCellType.I
            | OpCodeValues.Throw
            | OpCodeValues.Leave_S
            | OpCodeValues.Leave -> Stack.empty

            | OpCodeValues.Ldsfld ->
                let fieldInfo = Reflection.resolveField m instr.Arg32
                fieldInfo.FieldType |> push s
            | OpCodeValues.Ldfld ->
                let s = Stack.drop 1 s
                let fieldInfo = Reflection.resolveField m instr.Arg32
                fieldInfo.FieldType |> push s

            | OpCodeValues.Ldflda -> Stack.push (Stack.drop 1 s) evaluationStackCellType.I
            | OpCodeValues.Ldsflda -> Stack.push s evaluationStackCellType.I

            | OpCodeValues.Stfld -> Stack.drop 2 s
            | OpCodeValues.Stsfld -> Stack.drop 1 s
            | OpCodeValues.Stobj -> Stack.drop 2 s
            | OpCodeValues.Unbox_Any ->
                let s = Stack.drop 1 s
                Reflection.resolveType m instr.Arg32 |> push s
            | OpCodeValues.Box
            | OpCodeValues.Newarr -> Stack.push (Stack.drop 1 s) evaluationStackCellType.Ref
            | OpCodeValues.Ldlen -> Stack.push (Stack.drop 1 s) evaluationStackCellType.I

            | OpCodeValues.Ldelema -> Stack.push (Stack.drop 2 s) evaluationStackCellType.I
            | OpCodeValues.Ldelem_I1
            | OpCodeValues.Ldelem_U1 -> Stack.push (Stack.drop 2 s) evaluationStackCellType.I1
            | OpCodeValues.Ldelem_I2
            | OpCodeValues.Ldelem_U2 -> Stack.push (Stack.drop 2 s) evaluationStackCellType.I2
            | OpCodeValues.Ldelem_I4
            | OpCodeValues.Ldelem_U4 -> Stack.push (Stack.drop 2 s) evaluationStackCellType.I4
            | OpCodeValues.Ldelem_I8 -> Stack.push (Stack.drop 2 s) evaluationStackCellType.I8
            | OpCodeValues.Ldelem_I -> Stack.push (Stack.drop 2 s) evaluationStackCellType.I
            | OpCodeValues.Ldelem_R4 -> Stack.push (Stack.drop 2 s) evaluationStackCellType.R4
            | OpCodeValues.Ldelem_R8 -> Stack.push (Stack.drop 2 s) evaluationStackCellType.R8
            | OpCodeValues.Ldelem_Ref -> Stack.push (Stack.drop 2 s) evaluationStackCellType.Ref
            | OpCodeValues.Ldelem ->
                let s = Stack.drop 2 s
                Reflection.resolveType m instr.Arg32 |> push s

            | OpCodeValues.Stelem_I
            | OpCodeValues.Stelem_I1
            | OpCodeValues.Stelem_I2
            | OpCodeValues.Stelem_I4
            | OpCodeValues.Stelem_I8
            | OpCodeValues.Stelem_R4
            | OpCodeValues.Stelem_R8
            | OpCodeValues.Stelem_Ref
            | OpCodeValues.Stelem -> Stack.drop 3 s

            | OpCodeValues.Refanyval
            | OpCodeValues.Ldvirtftn -> Stack.push (Stack.drop 1 s) evaluationStackCellType.I
            | OpCodeValues.Mkrefany -> Stack.push (Stack.drop 1 s) evaluationStackCellType.Ref
            | OpCodeValues.Ldtoken
            | OpCodeValues.Arglist
            | OpCodeValues.Ldftn -> Stack.push s evaluationStackCellType.I
            | OpCodeValues.Localloc -> Stack.push (Stack.drop 1 s) evaluationStackCellType.I
            | OpCodeValues.Initobj -> Stack.drop 1 s
            | OpCodeValues.Cpblk -> Stack.drop 3 s
            | OpCodeValues.Initblk -> Stack.drop 3 s
            | OpCodeValues.Sizeof -> Stack.push s evaluationStackCellType.I
            | OpCodeValues.Refanytype -> Stack.push (Stack.drop 1 s) evaluationStackCellType.I4

            | OpCodeValues.Call
            | OpCodeValues.Callvirt
            | OpCodeValues.Newobj ->
                let callee = Reflection.resolveMethod m instr.Arg32
                let hasThis = callee.CallingConvention.HasFlag(CallingConventions.HasThis)
                let pops = callee.GetParameters().Length
                let pops =
                    if hasThis && opcodeValue <> OpCodeValues.Newobj then pops + 1
                    else pops
                let s = Stack.drop pops s
                let returnType = Reflection.getMethodReturnType callee
                if opcodeValue = OpCodeValues.Newobj then
                    Stack.push s evaluationStackCellType.Ref
                elif Reflection.hasNonVoidResult callee then
                    push s returnType
                else s
            | OpCodeValues.Calli ->
                // TODO: resolve and parse signature
                internalfail "typeInstruction: Calli is not implemented"
            | OpCodeValues.Ret ->
                let s = if Reflection.hasNonVoidResult m then Stack.drop 1 s else s
                if not (Stack.isEmpty s) then fail()
                s
            | _ -> s
        | SwitchArg -> s
//       Logger.trace "typer after: %O" res.Length
//       res

    let private createStackState (m : MethodBase) (startInstr : ilInstr) =
        let q = Queue<ilInstr>()
        q.Enqueue(startInstr)
        while q.Count > 0 do
            let instr = q.Dequeue()
            let s = typeInstruction m instr
            let next =
                match instr.opcode with
                | OpCode op ->
                    let opcodeValue = LanguagePrimitives.EnumOfValue op.Value
                    match opcodeValue with
                    | OpCodeValues.Ret
                    | OpCodeValues.Throw
                    | OpCodeValues.Rethrow -> []
                    | OpCodeValues.Br_S
                    | OpCodeValues.Br
                    | OpCodeValues.Leave
                    | OpCodeValues.Leave_S -> [instr.Target]
                    | _ ->
                        match instr.arg with
                        | Target tgt ->
                            [instr.next; tgt]
                        | _ -> [instr.next]
                | SwitchArg -> [instr.next; instr.Target]
            next |> Seq.iter (fun nxt ->
                match nxt.stackState with
                | None ->
                    nxt.stackState <- Some s
                    q.Enqueue nxt
                | Some s' ->
                    nxt.stackState <- Some (mergeStackStates s s'))

    let createBodyStackState (m : MethodBase) (startInstr : ilInstr) =
        assert(startInstr.stackState = None)
        startInstr.stackState <- Some Stack.empty
        createStackState m startInstr

    let createEHStackState (m : MethodBase) (flags : int) (startInstr : ilInstr) =
        let catchFlags = LanguagePrimitives.EnumToValue ExceptionHandlingClauseOptions.Clause
        let filterFlags = LanguagePrimitives.EnumToValue ExceptionHandlingClauseOptions.Filter
        // TODO: finially! #do
        match flags with
        | _ when flags = catchFlags ->
            // Pushing exception ref for catch clause
            startInstr.stackState <- Some [evaluationStackCellType.Ref]
        | _ when flags = filterFlags ->
            // Pushing exception ref for filter clause
            startInstr.stackState <- Some [evaluationStackCellType.Ref]
        | _ -> startInstr.stackState <- Some Stack.empty
        createStackState m startInstr

[<AllowNullLiteral>]
type ILRewriter(body : rawMethodBody, m : MethodBase) =
    let code = body.il
    let codeSize = Array.length code
    let mutable instrCount = 0u
    let mutable maxStackSize = body.properties.maxStackSize
    let offsetToInstr : ilInstr array = Array.zeroCreate (codeSize + 1)
    let mutable ehs : ehClause array = Array.empty
    let il : ilInstr = Reflection.createObject typeof<ilInstr> :?> ilInstr

    let invalidProgram reason =
        Logger.error "Invalid program: %s" reason
        raise <| IncorrectCIL reason

    let adjustState (instr : ilInstr) =
        maxStackSize <- maxStackSize + instr.opcode.StackBehaviourPush

    new (body : rawMethodBody) =
        // If this line throws exception, we should improve resolving assemblies by names.
        // Probably we should track assemblies from the directory of executed assembly
        let m = Reflection.resolveMethodBase body.assembly body.moduleName (int body.properties.token)
        ILRewriter(body, m)

    static member PrintILInstr (tokens : signatureTokens option) (probes : probes option) (m : MethodBase) (instr : ilInstr) =
        let opcode, arg =
            match instr.opcode with
            | OpCode op ->
                let arg =
                    if op = OpCodes.Call || op = OpCodes.Callvirt || op = OpCodes.Newobj then
                        match instr.arg with
                        | Arg32 token ->
                            let callee = Reflection.resolveMethod m token
                            Reflection.methodToString callee
                        | _ -> __unreachable__()
                    elif op = OpCodes.Calli then
                        match tokens with
                        | Some tokens -> tokens.TokenToString instr.Arg32
                        | None -> instr.Arg32.ToString()
                    else
                        match instr.arg with
                        | NoArg -> ""
                        | Arg8 a -> a.ToString()
                        | Arg16 a -> a.ToString()
                        | Arg32 a -> a.ToString()
                        | Arg64 a ->
                            match probes with
                            | Some probes -> probes.AddressToString a
                            | None -> a.ToString()
                        | Target t ->
                            match t.opcode with
                            | OpCode op -> sprintf "(%x) %s" t.offset op.Name
                            | SwitchArg _ -> "<SwitchArg>"
                op.Name, arg
            | SwitchArg -> "<SwitchArg>", ""
        sprintf "[%x] %s %s" instr.offset opcode arg

    member x.ILInstrToString (probes : probes) (instr : ilInstr) =
        ILRewriter.PrintILInstr (Some body.tokens) (Some probes) m instr

    member x.InstrEq instr1 instr2 =
        Microsoft.FSharp.Core.LanguagePrimitives.PhysicalEquality instr1 instr2

    member x.IsEnd instr =
        x.InstrEq instr il

    member private x.TraverseProgram action =
        let mutable instr = il.next
        while not <| x.IsEnd instr do
            action instr
            instr <- instr.next

    member x.PrintInstructions heading probes =
        Logger.trace "============== %s: =============" (heading + " (" + ehs.Length.ToString() + " handlers)")
        x.TraverseProgram (x.ILInstrToString probes >> Logger.trace "%s")

    member x.NewInstr opcode =
        instrCount <- instrCount + 1u
        {prev = il; next = il; opcode = opcode; offset = 0u; stackState = None; arg = Arg8 0uy}

    member x.NewInstr opcode =
        instrCount <- instrCount + 1u
        {prev = il; next = il; opcode = OpCode opcode; offset = 0u; stackState = None; arg = Arg8 0uy}

    member x.CopyInstruction instr =
        instrCount <- instrCount + 1u
        {prev = instr.prev; next = instr.next; opcode = instr.opcode; offset = instr.offset; stackState = instr.stackState; arg = instr.arg}

    member x.Instructions with get() =
        let result = Dictionary<offset, ilInstr>()
        x.TraverseProgram (fun instr ->
            result.Add(Offset.from (int instr.offset), instr))
        result

    member x.InstrFromOffset offset =
        if offset > codeSize then
            invalidProgram (sprintf "Too large offset %d requested!" offset)
        offsetToInstr[offset]

    member x.InsertBefore(where : ilInstr, what : ilInstr) =
        what.next <- where
        what.prev <- where.prev
        what.next.prev <- what
        what.prev.next <- what
        adjustState what

    member x.InsertAfter(where : ilInstr, what : ilInstr) =
        what.next <- where.next
        what.prev <- where
        what.next.prev <- what
        what.prev.next <- what
        for eh in ehs do
            if x.InstrEq eh.tryEnd where then
                eh.tryEnd <- what
            if x.InstrEq eh.handlerEnd where then
                eh.handlerEnd <- what
        adjustState what

    member x.Method = m
    member x.InstructionsCount with get() = instrCount
    member x.InitialMaxStackSize with get() = body.properties.maxStackSize
    member x.MaxStackSize with get() = maxStackSize

    member x.IsLastEHInstr (instr : ilInstr) =
        Array.exists (fun eh -> x.InstrEq eh.handlerEnd instr) ehs
    member x.ILList = il

    member private x.ReplaceBranchAlias (instr : ilInstr) (op : OpCode) (brop : OpCode) =
        let newInstr = x.CopyInstruction instr
        x.InsertAfter(instr, newInstr)
        instr.opcode <- OpCode op
        instr.arg <- NoArg
        match instr.stackState with
        | Some (_ :: _ :: tl)  -> newInstr.stackState <- Some(evaluationStackCellType.I4 :: tl)
        | _ -> __unreachable__()
        newInstr.opcode <- OpCode brop

    member private x.IsFloatBinOp (instr : ilInstr) =
        match instr.stackState with
        | Some (x :: y :: _) ->
            match x with
            | evaluationStackCellType.R4 -> assert(y = evaluationStackCellType.R4); true
            | evaluationStackCellType.R8 -> assert(y = evaluationStackCellType.R8); true
            | _ -> false
        | _ -> __unreachable__()

    member private x.ImportIL() =
        // Set the sentinel instruction
        il.next <- il
        il.prev <- il
        offsetToInstr.[codeSize] <- il

        // TODO: unify code with Instruction.fs (parseInstruction)
        let mutable branch = false
        let mutable offset = 0<offsets>
        let codeSize : offset = Offset.from codeSize
        while offset < codeSize do
            let startOffset = offset
            let op = OpCodeOperations.getOpCode code offset
            offset <- offset + Offset.from op.Size

            let size =
                match op.OperandType with
                | OperandType.InlineNone
                | OperandType.InlineSwitch -> 0<offsets>
                | OperandType.ShortInlineVar
                | OperandType.ShortInlineI
                | OperandType.ShortInlineBrTarget -> 1<offsets>
                | OperandType.InlineVar -> 2<offsets>
                | OperandType.InlineI
                | OperandType.InlineMethod
                | OperandType.InlineType
                | OperandType.InlineString
                | OperandType.InlineSig
                | OperandType.InlineTok
                | OperandType.ShortInlineR
                | OperandType.InlineField
                | OperandType.InlineBrTarget -> 4<offsets>
                | OperandType.InlineI8
                | OperandType.InlineR -> 8<offsets>
                | _ -> __unreachable__()

            if offset + size > codeSize then invalidProgram "IL stream unexpectedly ended!"

            let instr = x.NewInstr (OpCode op)
            instr.offset <- uint32 startOffset
            offsetToInstr.[int startOffset] <- instr
            x.InsertBefore(il, instr)
            offsetToInstr.[int startOffset] <- instr
            match op.OperandType with
            | OperandType.InlineNone -> instr.arg <- NoArg
            | OperandType.ShortInlineVar
            | OperandType.ShortInlineI ->
                instr.arg <- Arg8 code.[int offset]
            | OperandType.InlineVar ->
                instr.arg <- Arg16 <| BitConverter.ToInt16(code, int offset)
            | OperandType.InlineI
            | OperandType.InlineMethod
            | OperandType.InlineType
            | OperandType.InlineString
            | OperandType.InlineSig
            | OperandType.InlineTok
            | OperandType.ShortInlineR
            | OperandType.InlineField ->
                instr.arg <- Arg32 <| NumberCreator.extractInt32 code offset
            | OperandType.InlineI8
            | OperandType.InlineR ->
                instr.arg <- Arg64 <| BitConverter.ToInt64(code, int offset)
            | OperandType.ShortInlineBrTarget ->
                instr.arg <- offset + 1<offsets> + (code.[int offset] |> sbyte |> int |> Offset.from) |> int |> Arg32
                branch <- true;
            | OperandType.InlineBrTarget ->
                instr.arg <- offset + 4<offsets> + NumberCreator.extractOffset code offset |> int |> Arg32
                branch <- true;
            | OperandType.InlineSwitch ->
                let sizeOfInt = Offset.from sizeof<int>
                if offset + sizeOfInt > codeSize then
                    invalidProgram "IL stream unexpectedly ended!"
                let targetsCount = NumberCreator.extractInt32 code offset
                instr.arg <- Arg32 targetsCount
                offset <- offset + sizeOfInt
                let baseOffset = offset + targetsCount * sizeOfInt
                for i in 1 .. targetsCount do
                    if offset + sizeOfInt > codeSize then
                        invalidProgram "IL stream unexpectedly ended!"
                    let instr = x.NewInstr SwitchArg
                    instr.arg <- baseOffset + NumberCreator.extractOffset code offset |> int |> Arg32
                    offset <- offset + sizeOfInt
                    x.InsertBefore(il, instr)
                branch <- true
            | _ -> invalidProgram "Unexpected operand type!"

            offset <- offset + size

        assert(offset = codeSize)
        if branch then
            x.TraverseProgram (fun instr ->
                let isJump =
                    match instr.opcode with
                    | OpCode opcode ->
                        match opcode.OperandType with
                        | OperandType.ShortInlineBrTarget
                        | OperandType.InlineBrTarget -> true
                        | _ -> false
                    | SwitchArg -> true
                if isJump then
                    match instr.arg with
                    | Arg32 offset ->
                        instr.arg <- Target <| x.InstrFromOffset offset
                    | _ -> invalidProgram "Wrong operand of branching instruction!")

        // TODO: improve 'EvaluationStackTyper'
        // EvaluationStackTyper.createBodyStackState m il.next

    member private x.RecalculateOffsets() =
        let mutable branch = false
        let mutable tryAgain = true
        let mutable offset = 0
        while tryAgain do
            offset <- 0
            x.TraverseProgram (fun instr ->
                instr.offset <- uint32 offset

                match instr.opcode with
                | OpCode op ->
                    offset <- offset + op.Size
                    match instr.arg with
                    | NoArg -> ()
                    | Arg8 _ -> offset <- offset + sizeof<int8>
                    | Arg16 _ -> offset <- offset + sizeof<int16>
                    | Arg32 _ -> offset <- offset + sizeof<int32>
                    | Arg64 _ -> offset <- offset + sizeof<int64>
                    | Target _ ->
                        match op.OperandType with
                        | OperandType.ShortInlineBrTarget -> offset <- offset + 1
                        | OperandType.InlineBrTarget -> offset <- offset + 4
                        | _ -> __unreachable__()
                        branch <- true
                | SwitchArg ->
                    branch <- true
                    offset <- offset + sizeof<int32>)

            tryAgain <- false
            if branch then
                x.TraverseProgram (fun instr ->
                    match instr.opcode, instr.arg with
                    | OpCode op, Target tgt when op.OperandType = OperandType.ShortInlineBrTarget ->
                        let delta = int tgt.offset - int instr.next.offset
                        if delta < int SByte.MinValue || delta > int SByte.MaxValue then
                            if op = OpCodes.Leave_S then instr.opcode <- OpCode OpCodes.Leave
                            else
                                assert(op.Value >= OpCodes.Br_S.Value && op.Value <= OpCodes.Blt_Un_S.Value)
                                let op = OpCodeOperations.singleByteOpCodes.[op.Value - OpCodes.Br_S.Value + OpCodes.Br.Value |> int];
                                assert(op.Value >= OpCodes.Br.Value && op.Value <= OpCodes.Blt_Un.Value)
                                instr.opcode <- OpCode op
                            tryAgain <- true
                    | _ -> ())
        uint32 offset

    member private x.ImportEH() =
        let parseEH (raw : rawExceptionHandler) = {
            flags = raw.flags
            tryBegin = x.InstrFromOffset <| int raw.tryOffset
            tryEnd = (x.InstrFromOffset <| int (raw.tryOffset + raw.tryLength)).prev
            handlerBegin =
                let start = x.InstrFromOffset <| int raw.handlerOffset
                // TODO: improve 'EvaluationStackTyper'
                // EvaluationStackTyper.createEHStackState m raw.flags start
                start
            handlerEnd = (x.InstrFromOffset <| int (raw.handlerOffset + raw.handlerLength)).prev
            matcher = if raw.flags &&& 0x0001 = 0 then ClassToken raw.matcher else FilterEH (x.InstrFromOffset <| int raw.matcher)
        }
        ehs <- Array.map parseEH body.ehs

    member x.Import() =
        x.ImportIL()
        x.ImportEH() // TODO: replaceBranchAlias in eh! #do
        x.RecalculateOffsets() |> ignore
        maxStackSize <- body.properties.maxStackSize

    member x.Export() = // TODO: refactor export #do
        // One instruction produces 2 + sizeof(native int) bytes in the worst case which can be 10 bytes for 64-bit.
        // For simplification we just use 10 here.
        let maxSize = int instrCount * 10
        let outputIL = Array.zeroCreate maxSize

        let mutable branch = false
        let mutable tryAgain = true
        while tryAgain do
            let mutable offset = 0
            x.TraverseProgram (fun instr ->
                assert(offset < maxSize)
                instr.offset <- uint32 offset

                match instr.opcode with
                | OpCode op ->
                    if uint16 op.Value >= 0x100us then
                        outputIL.[offset] <- byte OpCodes.Prefix1.Value
                        offset <- offset + 1
                    outputIL.[offset] <- byte (op.Value &&& 0xFFs)
                    offset <- offset + 1
                    match instr.arg with
                    | NoArg -> ()
                    | Arg8 arg ->
                        outputIL.[offset] <- arg
                        offset <- offset + sizeof<int8>
                    | Arg16 arg ->
                        let success = BitConverter.TryWriteBytes(Span(outputIL, offset, sizeof<int16>), arg)
                        assert success
                        offset <- offset + sizeof<int16>
                    | Arg32 arg ->
                        let success = BitConverter.TryWriteBytes(Span(outputIL, offset, sizeof<int32>), arg)
                        assert success
                        offset <- offset + sizeof<int32>
                    | Arg64 arg ->
                        let success = BitConverter.TryWriteBytes(Span(outputIL, offset, sizeof<int64>), arg)
                        assert success
                        offset <- offset + sizeof<int64>
                    | Target _ ->
                        match op.OperandType with
                        | OperandType.ShortInlineBrTarget -> offset <- offset + 1
                        | OperandType.InlineBrTarget -> offset <- offset + 4
                        | _ -> __unreachable__()
                        branch <- true
                | SwitchArg ->
                    branch <- true
                    offset <- offset + sizeof<int32>)

            il.offset <- uint32 offset

            tryAgain <- false
            if branch then
                let mutable switchBase = 0
                x.TraverseProgram (fun instr ->
                    match instr.opcode with
                    | OpCode op when op = OpCodes.Switch ->
                        match instr.arg with
                        | Arg32 arg -> switchBase <- int instr.offset + 1 + sizeof<int32> * (arg + 1)
                        | _ -> __unreachable__()
                    | SwitchArg ->
                        match instr.arg with
                        | Target tgt ->
                            let success = BitConverter.TryWriteBytes(Span(outputIL, int instr.offset, sizeof<int>), int tgt.offset - switchBase)
                            assert success
                        | _ -> __unreachable__()
                    | OpCode op ->
                        match instr.arg with
                        | Target tgt ->
                            let delta = int tgt.offset - int instr.next.offset
                            match op.OperandType with
                            | OperandType.ShortInlineBrTarget ->
                                if delta < int SByte.MinValue || delta > int SByte.MaxValue then
                                    if op = OpCodes.Leave_S then instr.opcode <- OpCode OpCodes.Leave
                                    else
                                        assert(op.Value >= OpCodes.Br_S.Value && op.Value <= OpCodes.Blt_Un_S.Value)
                                        let op = OpCodeOperations.singleByteOpCodes.[op.Value - OpCodes.Br_S.Value + OpCodes.Br.Value |> int];
                                        assert(op.Value >= OpCodes.Br.Value && op.Value <= OpCodes.Blt_Un.Value)
                                        instr.opcode <- OpCode op
                                    tryAgain <- true
                                else outputIL.[int instr.next.offset - sizeof<int8>] <- byte (int8 delta)
                            | OperandType.InlineBrTarget ->
                                let success = BitConverter.TryWriteBytes(Span(outputIL, int instr.next.offset - sizeof<int32>, sizeof<int32>), delta)
                                assert success
                            | _ -> __unreachable__()
                        | _ -> ())

        let methodProps = {ilCodeSize = uint32 il.offset; maxStackSize = maxStackSize}
        let encodeEH (eh : ehClause) = {
            flags = eh.flags
            tryOffset = eh.tryBegin.offset
            tryLength = eh.tryEnd.next.offset - eh.tryBegin.offset
            handlerOffset = eh.handlerBegin.offset
            handlerLength = eh.handlerEnd.next.offset - eh.handlerBegin.offset
            matcher =
                match eh.matcher with
                | ClassToken tok -> tok
                | FilterEH instr -> instr.offset
        }
        let ehs = Array.map encodeEH ehs
        {properties = methodProps; il = Array.truncate (int methodProps.ilCodeSize) outputIL; ehs = ehs}
