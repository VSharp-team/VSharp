namespace VSharp

open System.Collections.Generic
open System.Reflection
open System.Runtime.InteropServices
open global.System
open System.Reflection.Emit
open VSharp

type ehcType =
    | Filter of offset
    | Catch of Type
    | Finally
    | Fault

type public exceptionHandlingClause = {
    tryOffset : offset
    tryLength : offset
    handlerOffset : offset
    handlerLength : offset
    ehcType : ehcType
}
    with
    static member Create (eh : ExceptionHandlingClause) =
        let flags = eh.Flags
        let ehcType =
            if flags = ExceptionHandlingClauseOptions.Filter then Filter (Offset.from eh.FilterOffset)
            elif flags = ExceptionHandlingClauseOptions.Finally then Finally
            elif flags = ExceptionHandlingClauseOptions.Fault then Fault
            else Catch eh.CatchType
        {
            tryOffset = Offset.from eh.TryOffset
            tryLength = Offset.from eh.TryLength
            handlerOffset = Offset.from eh.HandlerOffset
            handlerLength = Offset.from eh.HandlerLength
            ehcType = ehcType
        }

    static member CreateArray (methodBody : MethodBody) =
        let ehs = methodBody.ExceptionHandlingClauses
        let length = ehs.Count
        let result = Array.zeroCreate length
        for i = 0 to length - 1 do
            result[i] <- exceptionHandlingClause.Create ehs[i]
        result

type rawMethodBody = {
    il : byte array
    ehs : exceptionHandlingClause array
}
    with
    static member Empty with get() =
        { il = Array.empty; ehs = Array.empty }

    static member Create (m : MethodBase) =
        let methodBody = m.GetMethodBody()
        if methodBody = null then rawMethodBody.Empty
        else { il = methodBody.GetILAsByteArray(); ehs = exceptionHandlingClause.CreateArray methodBody }

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

    with
    member x.Size (operandType : OperandType) =
        match x with
        | NoArg -> 0
        | Target _ ->
            match operandType with
            | OperandType.ShortInlineBrTarget -> sizeof<int8>
            | OperandType.InlineBrTarget -> sizeof<int32>
            | _ -> __unreachable__()
        | Arg8 _ -> sizeof<int8>
        | Arg16 _ -> sizeof<int16>
        | Arg32 _ -> sizeof<int32>
        | Arg64 _ -> sizeof<int64>

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
        | _ -> internalfail $"Requesting 8-bit arg of instruction {x.opcode} with arg {x.arg}"
    member x.Arg16 with get() =
        match x.arg with
        | Arg16 v -> v
        | _ -> internalfail $"Requesting 16-bit arg of instruction {x.opcode} with arg {x.arg}"
    member x.Arg32 with get() =
        match x.arg with
        | Arg32 v -> v
        | _ -> internalfail $"Requesting 32-bit arg of instruction {x.opcode} with arg {x.arg}"
    member x.Arg64 with get() =
        match x.arg with
        | Arg64 v -> v
        | _ -> internalfail $"Requesting 64-bit arg of instruction {x.opcode} with arg {x.arg}"
    member x.Target with get() =
        match x.arg with
        | Target v -> v
        | _ -> internalfail $"Requesting target arg of instruction {x.opcode} with arg {x.arg}"
    member x.SizeWithArg with get() =
        match x.opcode with
        | OpCode op -> op.Size + x.arg.Size op.OperandType
        | SwitchArg -> sizeof<int32>
    member x.EndOffset with get() =
        x.offset + uint x.SizeWithArg
    member x.IsJump with get() =
        match x.opcode with
        | OpCode opcode ->
            match opcode.OperandType with
            | OperandType.ShortInlineBrTarget
            | OperandType.InlineBrTarget -> true
            | _ -> false
        | SwitchArg -> true

    override x.ToString() =
        x.opcode.ToString()

type private rewriterEhcType =
    | FilterEH of ilInstr
    | CatchEH of Type
    | FinallyEH
    | FaultEH
    with
    member x.Export() =
        match x with
        | FilterEH instr -> int instr.offset |> Offset.from |> Filter
        | CatchEH t -> Catch t
        | FinallyEH -> Finally
        | FaultEH -> Fault

type private ehClause = {
    tryBegin : ilInstr
    mutable tryEnd : ilInstr
    handlerBegin : ilInstr
    mutable handlerEnd : ilInstr
    ehcType : rewriterEhcType
}
    with
    member x.Export() =
        let tryOffset = int x.tryBegin.offset
        let handlerOffset = int x.handlerBegin.offset
        {
            tryOffset = Offset.from tryOffset
            tryLength = Offset.from (int x.tryEnd.EndOffset - tryOffset)
            handlerOffset = Offset.from handlerOffset
            handlerLength = Offset.from (int x.handlerEnd.EndOffset - handlerOffset)
            ehcType = x.ehcType.Export()
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

module internal EvaluationStackTyper =

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
        m.GetMethodBody().LocalVariables[idx].LocalType |> push s

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

type internal analysisEvent =
    | Calli
    | CallVirt of MethodBase
    | Ldsfld of fieldId
    | Stsfld of fieldId

module internal ILRewriter =

    type private analyseResult =
        {
            method : MethodBase
            events : HashSet<analysisEvent>
            dependencies : MethodBase list
        }
        with
        static member Empty(m : MethodBase) = { method = m; events = HashSet<analysisEvent>(); dependencies = List.empty }
        member x.IsComplete with get() = List.isEmpty x.dependencies
        member x.Union (other : analyseResult) =
            x.events.UnionWith other.events
            let newDependencies =
                x.dependencies @ other.dependencies
                |> List.filter (fun d -> d <> x.method && d <> other.method)
                |> List.distinct
            { x with dependencies = newDependencies }

    let printILInstr (m : MethodBase) (instr : ilInstr) =
        let opcode, arg =
            match instr.opcode with
            | OpCode op ->
                let arg =
                    if op = OpCodes.Call || op = OpCodes.Callvirt || op = OpCodes.Newobj then
                        Reflection.resolveMethod m instr.Arg32 |> Reflection.methodToString
                    elif op = OpCodes.Calli then instr.Arg32.ToString()
                    else
                        match instr.arg with
                        | NoArg -> ""
                        | Arg8 a -> a.ToString()
                        | Arg16 a -> a.ToString()
                        | Arg32 a -> a.ToString()
                        | Arg64 a -> a.ToString()
                        | Target t ->
                            match t.opcode with
                            | OpCode op -> $"(%x{t.offset}) {op.Name}"
                            | SwitchArg -> "<SwitchArg>"
                op.Name, arg
            | SwitchArg -> "<SwitchArg>", ""
        $"[%x{instr.offset}] {opcode} {arg}"

    type private ILRewriter internal (body : rawMethodBody, m : MethodBase) =
        let code = body.il
        let codeSize = Array.length code
        let mutable instrCount = 0u
        let offsetToInstr : ilInstr array = Array.zeroCreate (codeSize + 1)
        let mutable ehs : ehClause array = Array.empty
        let il : ilInstr = Reflection.createObject typeof<ilInstr> :?> ilInstr
        let mutable codeRewritten = false

        let invalidProgram reason =
            Logger.error $"Invalid program: {reason}"
            raise <| IncorrectCIL reason

        new (method : MethodBase) =
            let body = rawMethodBody.Create method
            ILRewriter(body, method)

        member x.InstrEq instr1 instr2 =
            Microsoft.FSharp.Core.LanguagePrimitives.PhysicalEquality instr1 instr2

        member x.IsEnd instr =
            x.InstrEq instr il

        member private x.InstructionsSeq =
            seq {
                let mutable instr = il.next
                while not <| x.IsEnd instr do
                    yield instr
                    instr <- instr.next
            }

        member x.PrintInstructions heading =
            Logger.trace "============== %s: =============" (heading + " (" + ehs.Length.ToString() + " handlers)")
            for instr in x.InstructionsSeq do
                printILInstr m instr |> Logger.trace "%s"

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
            for instr in x.InstructionsSeq do
                result.Add(Offset.from (int instr.offset), instr)
            result

        member x.InstrFromOffset offset =
            if offset > codeSize then
                invalidProgram $"Too large offset %x{offset} requested!"
            offsetToInstr[offset]

        member x.InsertBefore(where : ilInstr, what : ilInstr) =
            what.next <- where
            what.prev <- where.prev
            what.next.prev <- what
            what.prev.next <- what

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

        member x.Method = m
        member x.InstructionsCount with get() = instrCount

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
            offsetToInstr[codeSize] <- il

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
                offsetToInstr[int startOffset] <- instr
                x.InsertBefore(il, instr)
                offsetToInstr[int startOffset] <- instr
                match op.OperandType with
                | OperandType.InlineNone -> instr.arg <- NoArg
                | OperandType.ShortInlineVar
                | OperandType.ShortInlineI ->
                    instr.arg <- Arg8 code[int offset]
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
                    let delta = code[int offset] |> sbyte |> int |> Offset.from
                    instr.arg <- offset + (Offset.from sizeof<int8>) + delta |> int |> Arg32
                    branch <- true;
                | OperandType.InlineBrTarget ->
                    let delta = NumberCreator.extractOffset code offset
                    instr.arg <- offset + (Offset.from sizeof<int32>) + delta |> int |> Arg32
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
                for instr in x.InstructionsSeq do
                    if instr.IsJump then
                        match instr.arg with
                        | Arg32 offset ->
                            instr.arg <- Target <| x.InstrFromOffset offset
                        | _ -> invalidProgram "Wrong operand of branching instruction!"

            // TODO: improve 'EvaluationStackTyper'
            // EvaluationStackTyper.createBodyStackState m il.next

        member private x.ImportEH() =
            let parseEH (eh : exceptionHandlingClause) = {
                tryBegin = x.InstrFromOffset <| int eh.tryOffset
                tryEnd = (x.InstrFromOffset <| int (eh.tryOffset + eh.tryLength)).prev
                handlerBegin =
                    let start = x.InstrFromOffset <| int eh.handlerOffset
                    // TODO: improve 'EvaluationStackTyper'
                    // EvaluationStackTyper.createEHStackState m raw.flags start
                    start
                handlerEnd = (x.InstrFromOffset <| int (eh.handlerOffset + eh.handlerLength)).prev
                ehcType =
                    match eh.ehcType with
                    | Filter offset -> FilterEH (x.InstrFromOffset (int offset))
                    | Catch t -> CatchEH t
                    | Finally -> FinallyEH
                    | Fault -> FaultEH
            }
            ehs <- Array.map parseEH body.ehs

        member x.Import() =
            x.ImportIL()
            x.ImportEH()
            x.RecalculateOffsets() |> ignore

        member private x.RecalculateOffsets([<Out>] hasBranch : bool byref) =
            let mutable offset = 0
            hasBranch <- false
            // Recalculating offsets
            for instr in x.InstructionsSeq do
                instr.offset <- uint32 offset
                offset <- offset + instr.SizeWithArg
                hasBranch <- hasBranch || instr.IsJump
            offset

        member private x.NormalizeIL() =
            let mutable hasBranch = false
            let mutable tryAgain = true
            let mutable size = 0
            while tryAgain do
                size <- x.RecalculateOffsets(&hasBranch)
                tryAgain <- false
                if hasBranch then
                    // Changing short jumps to long, if needed
                    for instr in x.InstructionsSeq do
                        match instr.opcode, instr.arg with
                        | OpCode op, Target tgt when op.OperandType = OperandType.ShortInlineBrTarget ->
                            let delta = int tgt.offset - int instr.EndOffset
                            if delta < int SByte.MinValue || delta > int SByte.MaxValue then
                                if op = OpCodes.Leave_S then instr.opcode <- OpCode OpCodes.Leave
                                else
                                    assert(op.Value >= OpCodes.Br_S.Value && op.Value <= OpCodes.Blt_Un_S.Value)
                                    let index = op.Value - OpCodes.Br_S.Value + OpCodes.Br.Value |> int
                                    let op = OpCodeOperations.singleByteOpCodes[index];
                                    assert(op.Value >= OpCodes.Br.Value && op.Value <= OpCodes.Blt_Un.Value)
                                    instr.opcode <- OpCode op
                                tryAgain <- true
                        | _ -> ()
            uint32 size

        member private x.ToBytes() =
            // Normalization before exporting to IL bytes
            let ilCodeSize = x.NormalizeIL() |> int

            // One instruction produces 2 + sizeof(native int) bytes in the worst case which can be 10 bytes for 64-bit.
            // For simplification we just use 10 here.
            let maxSize = int instrCount * 10
            let outputIL = Array.zeroCreate maxSize

            let mutable switchBase = -1
            for instr in x.InstructionsSeq do
                let instrOffset = int instr.offset
                let mutable offset = instrOffset
                assert(offset < maxSize)
                match instr.opcode with
                | OpCode op ->
                    if uint16 op.Value >= 0x100us then
                        outputIL[offset] <- byte OpCodes.Prefix1.Value
                        offset <- offset + 1
                    outputIL[offset] <- byte (op.Value &&& 0xFFs)
                    offset <- offset + 1
                    match instr.arg with
                    | NoArg -> ()
                    | Arg8 arg -> outputIL[offset] <- arg
                    | Arg16 arg ->
                        let success = BitConverter.TryWriteBytes(Span(outputIL, offset, sizeof<int16>), arg)
                        assert success
                    | Arg32 arg ->
                        if op = OpCodes.Switch then
                            switchBase <- offset + sizeof<int32> * (arg + 1)
                        let success = BitConverter.TryWriteBytes(Span(outputIL, offset, sizeof<int32>), arg)
                        assert success
                    | Arg64 arg ->
                        let success = BitConverter.TryWriteBytes(Span(outputIL, offset, sizeof<int64>), arg)
                        assert success
                    | Target tgt ->
                        let delta = int tgt.offset - int instr.EndOffset
                        match op.OperandType with
                        | OperandType.ShortInlineBrTarget ->
                            assert(delta >= int SByte.MinValue && delta <= int SByte.MaxValue)
                            outputIL[offset] <- byte (int8 delta)
                        | OperandType.InlineBrTarget ->
                            let bytes = Span(outputIL, offset, sizeof<int32>)
                            let success = BitConverter.TryWriteBytes(bytes, delta)
                            assert success
                        | _ -> __unreachable__()
                | SwitchArg ->
                    match instr.arg with
                    | Target tgt ->
                        assert(switchBase >= 0)
                        let bytes = Span(outputIL, instrOffset, sizeof<int>)
                        let offset = int tgt.offset - switchBase
                        let success = BitConverter.TryWriteBytes(bytes, offset)
                        assert success
                    | _ -> __unreachable__()

            let ehs = ehs |> Array.map (fun eh -> eh.Export())
            { il = Array.truncate ilCodeSize outputIL; ehs = ehs }

        member x.Export() =
            if codeRewritten then x.ToBytes()
            else body

        member private x.ParseCallee (instr : ilInstr) =
            Reflection.resolveMethod m instr.Arg32

        member x.AnalyseMethod() =
            let events = HashSet<analysisEvent>()
            let dependencies = HashSet<MethodBase>()
            let mutable instr = il.next
            while not <| x.IsEnd instr do
                match instr.opcode with
                | OpCode opcode ->
                    if opcode = OpCodes.Call || opcode = OpCodes.Newobj then
                        let method = x.ParseCallee instr
                        dependencies.Add method |> ignore
                    elif opcode = OpCodes.Calli then
                        events.Add Calli |> ignore
                    elif opcode = OpCodes.Callvirt then
                        let method = x.ParseCallee instr
                        events.Add (CallVirt method) |> ignore
                    elif opcode = OpCodes.Ldsfld || opcode = OpCodes.Ldsflda then
                        let field = Reflection.resolveField m instr.Arg32 |> Reflection.wrapField
                        events.Add (Ldsfld field) |> ignore
                    elif opcode = OpCodes.Stsfld then
                        let field = Reflection.resolveField m instr.Arg32 |> Reflection.wrapField
                        events.Add (Stsfld field) |> ignore
                | _ -> ()
                instr <- instr.next
            { method = m; events = events; dependencies = Seq.toList dependencies }

    let private rewriterCache = Dictionary<MethodBase, ILRewriter>()
    let private eventsCache = Dictionary<MethodBase, analyseResult>()

    let private createRewriterFromBody (rawMethodBody : rawMethodBody) (m : MethodBase) =
        let exists, ilRewriter = rewriterCache.TryGetValue m
        if exists then ilRewriter
        else
            let ilRewriter = ILRewriter(rawMethodBody, m)
            ilRewriter.Import()
            rewriterCache.Add(m, ilRewriter)
            ilRewriter

    let private createRewriterFromMethod (m : MethodBase) =
        let exists, ilRewriter = rewriterCache.TryGetValue m
        if exists then ilRewriter
        else
            let ilRewriter = ILRewriter(m)
            ilRewriter.Import()
            rewriterCache.Add(m, ilRewriter)
            ilRewriter

    let rewriteIL (body : rawMethodBody) (m : MethodBase) =
        let ilRewriter = createRewriterFromBody body m
        ilRewriter.Export()

    let instructionsOfMethod (body : rawMethodBody) (m : MethodBase) =
        let ilRewriter = createRewriterFromBody body m
        ilRewriter.Instructions

    let private checkEvents (events : HashSet<analysisEvent>) (failPredicate : analysisEvent -> bool) =
        let mutable success = true
        for event in events do
            if success then
                success <- failPredicate event |> not
        success

    type private checkResult =
        {
            success : bool
            analyseResult : analyseResult
        }
        with
        static member Empty(m : MethodBase) = { success = true; analyseResult = analyseResult.Empty m }
        member x.Union (other : checkResult) =
            {
                success = x.success && other.success
                analyseResult = x.analyseResult.Union other.analyseResult
            }

    let rec private checkCallee (checkedMethods : HashSet<MethodBase>) failPredicate skipPredicate m k =
        // Recursive methods handling
        if checkedMethods.Add m then
            let result =
                let exists, result = eventsCache.TryGetValue m
                if exists then result
                else
                    let rewriter = createRewriterFromMethod m
                    rewriter.AnalyseMethod()
            let success = checkEvents result.events failPredicate
            if success then
                let checkResult = { success = true; analyseResult = result }
                if result.IsComplete then k checkResult
                else
                    let methods = result.dependencies
                    checkDependencies checkedMethods failPredicate skipPredicate checkResult methods (fun checkResult ->
                    eventsCache[m] <- checkResult.analyseResult
                    k checkResult)
            else k { success = false; analyseResult = result }
        else checkResult.Empty m |> k

    and private checkDependencies checkedMethods failPredicate skipPredicate resultAcc dependencies k =
        assert resultAcc.success
        let analyseDependencies (resultAcc : checkResult) method (k : checkResult -> 'a) =
            if resultAcc.success && not (skipPredicate method) then
                checkCallee checkedMethods failPredicate skipPredicate method (fun calleeResult ->
                resultAcc.Union calleeResult |> k)
            else k resultAcc
        Cps.List.foldlk analyseDependencies resultAcc dependencies k

    let analyseMethod rawMethodBody m failPredicate skipPredicate =
        let result =
            let exists, result = eventsCache.TryGetValue m
            if exists then result
            else
                let rewriter = createRewriterFromBody rawMethodBody m
                rewriter.AnalyseMethod()
        let success = checkEvents result.events failPredicate
        if success then
            if result.IsComplete then true
            else
                let checkedMethods = HashSet<MethodBase>()
                checkedMethods.Add m |> ignore
                let mutable checkResult = { success = true; analyseResult = result }
                for method in result.dependencies do
                    if checkResult.success && not (skipPredicate method) then
                        let calleeResult = checkCallee checkedMethods failPredicate skipPredicate method id
                        checkResult <- checkResult.Union calleeResult
                eventsCache[m] <- checkResult.analyseResult
                checkResult.success
        else false
