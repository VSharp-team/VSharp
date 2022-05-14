namespace VSharp.Concolic

open VSharp
open System.Reflection
open System.Reflection.Emit
open System.Collections.Generic
open VSharp.Concolic
open VSharp.Interpreter.IL

type Instrumenter(communicator : Communicator, entryPoint : MethodBase, probes : probes) =
    // TODO: should we consider executed assembly build options here?
    let ldc_i : opcode = (if System.Environment.Is64BitOperatingSystem then OpCodes.Ldc_I8 else OpCodes.Ldc_I4) |> OpCode
    let mutable currentStaticFieldID = 0
    let staticFieldIDs = Dictionary<int, FieldInfo>()
    let registerStaticFieldID (fieldInfo : FieldInfo) =
        if staticFieldIDs.ContainsValue(fieldInfo) |> not then
            currentStaticFieldID <- currentStaticFieldID + 1
            staticFieldIDs.Add(currentStaticFieldID, fieldInfo)
            currentStaticFieldID
        else
            let kvp = staticFieldIDs |> Seq.find (fun kvp -> kvp.Value = fieldInfo)
            kvp.Key
    let hasComplexSize (t : System.Type) =
        (t.IsValueType && not t.IsPrimitive) || t.IsGenericParameter

    static member private instrumentedFunctions = HashSet<MethodBase>()
    [<DefaultValue>] val mutable tokens : signatureTokens
    [<DefaultValue>] val mutable rewriter : ILRewriter
    [<DefaultValue>] val mutable m : MethodBase

    member x.StaticFieldByID id = staticFieldIDs[id]

    member private x.MkCalli(instr : ilInstr byref, signature : uint32) =
        instr <- x.rewriter.NewInstr OpCodes.Calli
        instr.arg <- Arg32 (int32 signature)

    member private x.PrependInstr(opcode, arg, beforeInstr : ilInstr byref) =
        let mutable newInstr = x.rewriter.CopyInstruction(beforeInstr)
        x.rewriter.InsertAfter(beforeInstr, newInstr)
        swap &newInstr &beforeInstr
        newInstr.opcode <- OpCode opcode
        newInstr.arg <- arg
        newInstr

    member private x.PrependNop(beforeInstr : ilInstr byref) =
        x.PrependInstr(OpCodes.Nop, NoArg, &beforeInstr)

    member private x.PrependBranch(opcode, beforeInstr : ilInstr byref) =
        x.PrependInstr(opcode, NoArg (*In chain of prepends, the address of instruction constantly changes. Deferring it.*), &beforeInstr)

    member private x.AppendInstr (opcode : OpCode) arg (afterInstr : ilInstr) =
        let dupInstr = x.rewriter.NewInstr opcode
        dupInstr.arg <- arg
        x.rewriter.InsertAfter(afterInstr, dupInstr)

    member private x.AppendNop (afterInstr : ilInstr) =
        x.AppendInstr OpCodes.Nop NoArg afterInstr
        afterInstr.next

    member private x.PrependDup(beforeInstr : ilInstr byref) = x.PrependInstr(OpCodes.Dup, NoArg, &beforeInstr)
    member private x.AppendDup afterInstr = x.AppendInstr OpCodes.Dup NoArg afterInstr

    member private x.PrependProbe(methodAddress : uint64, args : (OpCode * ilInstrOperand) list, signature, beforeInstr : ilInstr byref) =
        let result = beforeInstr
        let mutable newInstr = x.rewriter.CopyInstruction(beforeInstr)
        x.rewriter.InsertAfter(beforeInstr, newInstr)
        swap &newInstr &beforeInstr

        match args with
        | (opcode, arg)::tail ->
            newInstr.opcode <- OpCode opcode
            newInstr.arg <- arg
            for opcode, arg in tail do
                let newInstr = x.rewriter.NewInstr opcode
                newInstr.arg <- arg
                x.rewriter.InsertBefore(beforeInstr, newInstr)

            newInstr <- x.rewriter.NewInstr ldc_i
            newInstr.arg <- Arg64 (int64 methodAddress)
            x.rewriter.InsertBefore(beforeInstr, newInstr)
        | [] ->
            newInstr.opcode <- ldc_i
            newInstr.arg <- Arg64 (int64 methodAddress)

        x.MkCalli(&newInstr, signature)
        x.rewriter.InsertBefore(beforeInstr, newInstr)
        result

    member private x.PrependProbeWithOffset(methodAddress : uint64, args : (OpCode * ilInstrOperand) list, signature, beforeInstr : ilInstr byref) =
        x.PrependProbe(methodAddress, List.append args [(OpCodes.Ldc_I4, beforeInstr.offset |> int32 |> Arg32)], signature, &beforeInstr)

    member private x.AppendProbe(methodAddress : uint64, args : (OpCode * ilInstrOperand) list, signature, afterInstr : ilInstr) =
        let mutable calliInstr = afterInstr
        x.MkCalli(&calliInstr, signature)
        x.rewriter.InsertAfter(afterInstr, calliInstr)

        let newInstr = x.rewriter.NewInstr ldc_i
        newInstr.arg <- Arg64 (int64 methodAddress)
        x.rewriter.InsertAfter(afterInstr, newInstr)

        for opcode, arg in List.rev args do
            let newInstr = x.rewriter.NewInstr opcode
            newInstr.arg <- arg
            x.rewriter.InsertAfter(afterInstr, newInstr)

        calliInstr

    // NOTE: offset is sent from client to SILI
    member private x.AppendProbeWithOffset(methodAddress : uint64, args : (OpCode * ilInstrOperand) list, offset, signature, afterInstr : ilInstr) =
        x.AppendProbe(methodAddress, List.append args [(OpCodes.Ldc_I4, offset |> int32 |> Arg32)], signature, afterInstr)

    member private x.PlaceEnterProbe (firstInstr : ilInstr byref) =
        let isSpontaneous = if Reflection.isExternalMethod x.m || Reflection.isStaticConstructor x.m then 1 else 0
        let locals, localsCount =
            match x.m.GetMethodBody() with
            | null -> null, 0
            | mb ->
                let locals = mb.LocalVariables
                locals, locals.Count
        let parameters = x.m.GetParameters()
        let hasThis = Reflection.hasThis x.m
        let argsCount = parameters.Length
        let totalArgsCount = if hasThis then argsCount + 1 else argsCount
        if x.m.MethodHandle = entryPoint.MethodHandle then
            let args = [(OpCodes.Ldc_I4, Arg32 x.m.MetadataToken)
                        (OpCodes.Ldc_I4, Arg32 (Coverage.moduleToken x.m.Module))
                        (OpCodes.Ldc_I4, Arg32 totalArgsCount)
//                        (OpCodes.Ldc_I4, Arg32 1) // Arguments of entry point are concrete
                        (OpCodes.Ldc_I4, Arg32 0) // Arguments of entry point are symbolic
                        (OpCodes.Ldc_I4, x.rewriter.MaxStackSize |> int32 |> Arg32)
                        (OpCodes.Ldc_I4, Arg32 localsCount)]
            x.PrependProbe(probes.enterMain, args, x.tokens.void_token_u4_u2_bool_u4_u4_sig, &firstInstr) |> ignore
        else
            let args = [(OpCodes.Ldc_I4, Arg32 x.m.MetadataToken)
                        (OpCodes.Ldc_I4, Arg32 (Coverage.moduleToken x.m.Module))
                        (OpCodes.Ldc_I4, x.rewriter.MaxStackSize |> int32 |> Arg32)
                        (OpCodes.Ldc_I4, Arg32 totalArgsCount)
                        (OpCodes.Ldc_I4, Arg32 localsCount)
                        (OpCodes.Ldc_I4, Arg32 isSpontaneous)]
            x.PrependProbe(probes.enter, args, x.tokens.void_token_u4_u4_u4_u4_i1_sig, &firstInstr) |> ignore
        for i = 0 to argsCount - 1 do
            let argType = parameters.[i].ParameterType
            if hasComplexSize argType then
                let idx = if hasThis then i + 1 else i
                let sizeInstr = x.TypeSizeInstr argType (fun () -> x.AcceptArgTypeToken argType idx)
                x.PrependProbe(probes.setArgSize, [(OpCodes.Ldc_I4, Arg32 idx); sizeInstr], x.tokens.void_i1_size_sig, &firstInstr) |> ignore
        for i = 0 to localsCount - 1 do
            let locType = locals.[i].LocalType
            if hasComplexSize locType then
                let sizeInstr = x.TypeSizeInstr locType (fun () -> x.AcceptLocVarTypeToken locType i)
                x.PrependProbe(probes.setLocSize, [(OpCodes.Ldc_I4, Arg32 i); sizeInstr], x.tokens.void_i1_size_sig, &firstInstr) |> ignore

        // If x.m is a constructor of structure, then we should register structure address (it becomes explicit only here)
        if x.m.IsConstructor && x.m.DeclaringType.IsValueType then
            x.PrependProbe(probes.enterStructCtor, [(OpCodes.Ldarg_0, NoArg); (OpCodes.Conv_I, NoArg)], x.tokens.void_i_sig, &firstInstr) |> ignore

    member private x.PrependMem_p(idx, instr : ilInstr byref) =
        x.PrependInstr(OpCodes.Conv_I, NoArg, &instr) |> ignore
        x.PrependProbe(probes.mem_p, [(OpCodes.Ldc_I4, Arg32 idx); (OpCodes.Ldc_I4, Arg32 (int instr.offset))], x.tokens.void_i_i1_offset_sig, &instr) |> ignore

    member private x.PrependMem_i1(idx, instr : ilInstr byref) =
        x.PrependProbe(probes.mem_1, [(OpCodes.Ldc_I4, Arg32 idx); (OpCodes.Ldc_I4, Arg32 (int instr.offset))], x.tokens.void_i1_i1_offset_sig, &instr) |> ignore

    member private x.PrependMem_i2(idx, instr : ilInstr byref) =
        x.PrependProbe(probes.mem_2, [(OpCodes.Ldc_I4, Arg32 idx); (OpCodes.Ldc_I4, Arg32 (int instr.offset))], x.tokens.void_i2_i1_offset_sig, &instr) |> ignore

    member private x.PrependMem_i4(idx, instr : ilInstr byref) =
        x.PrependProbe(probes.mem_4, [(OpCodes.Ldc_I4, Arg32 idx); (OpCodes.Ldc_I4, Arg32 (int instr.offset))], x.tokens.void_i4_i1_offset_sig, &instr) |> ignore

    member private x.PrependMem_i8(idx, instr : ilInstr byref) =
        x.PrependProbe(probes.mem_8, [(OpCodes.Ldc_I4, Arg32 idx); (OpCodes.Ldc_I4, Arg32 (int instr.offset))], x.tokens.void_i8_i1_offset_sig, &instr) |> ignore

    member private x.PrependMem_f4(idx, instr : ilInstr byref) =
        x.PrependProbe(probes.mem_f4, [(OpCodes.Ldc_I4, Arg32 idx); (OpCodes.Ldc_I4, Arg32 (int instr.offset))], x.tokens.void_r4_i1_offset_sig, &instr) |> ignore

    member private x.PrependMem_f8(idx, instr : ilInstr byref) =
        x.PrependProbe(probes.mem_f8, [(OpCodes.Ldc_I4, Arg32 idx); (OpCodes.Ldc_I4, Arg32 (int instr.offset))], x.tokens.void_r8_i1_offset_sig, &instr) |> ignore

    member private x.PrependMem2_p (instr : ilInstr byref) =
        x.PrependMem_p(1, &instr)
        x.PrependMem_p(0, &instr)

    member private x.PrependMem2_p_1 (instr : ilInstr byref) =
        x.PrependMem_i1(1, &instr)
        x.PrependMem_p(0, &instr)

    member private x.PrependMem2_p_2 (instr : ilInstr byref) =
        x.PrependMem_i2(1, &instr)
        x.PrependMem_p(0, &instr)

    member private x.PrependMem2_p_4 (instr : ilInstr byref) =
        x.PrependMem_i4(1, &instr)
        x.PrependMem_p(0, &instr)

    member private x.PrependMem2_p_8 (instr : ilInstr byref) =
        x.PrependMem_i8(1, &instr)
        x.PrependMem_p(0, &instr)

    member private x.PrependMem2_p_f4 (instr : ilInstr byref) =
        x.PrependMem_f4(1, &instr)
        x.PrependMem_p(0, &instr)

    member private x.PrependMem2_p_f8 (instr : ilInstr byref) =
        x.PrependMem_f8(1, &instr)
        x.PrependMem_p(0, &instr)

    member private x.PrependMem2_4_p (instr : ilInstr byref) =
        x.PrependMem_p(1, &instr)
        x.PrependMem_i4(0, &instr)

    member private x.PrependMem3_p (instr : ilInstr byref) =
        x.PrependMem_p(2, &instr)
        x.PrependMem_p(1, &instr)
        x.PrependMem_p(0, &instr)

    member private x.PrependMem3_p_i1_p (instr : ilInstr byref) =
        x.PrependMem_p(2, &instr)
        x.PrependMem_i1(1, &instr)
        x.PrependMem_p(0, &instr)

    member private x.PrependMem3_p_p_i1 (instr : ilInstr byref) =
        x.PrependMem_i1(2, &instr)
        x.PrependMem_p(1, &instr)
        x.PrependMem_p(0, &instr)

    member private x.PrependMem3_p_p_i2 (instr : ilInstr byref) =
        x.PrependMem_i2(2, &instr)
        x.PrependMem_p(1, &instr)
        x.PrependMem_p(0, &instr)

    member private x.PrependPopOpmem (instr : ilInstr byref) =
        x.PrependProbe(probes.popOpmem, [(OpCodes.Ldc_I4, Arg32 (int instr.offset))], x.tokens.void_offset_sig, &instr)

    member private x.AppendMem_p(idx, offset, instr : ilInstr) =
        x.AppendProbe(probes.mem_p, [(OpCodes.Ldc_I4, Arg32 idx); (OpCodes.Ldc_I4, Arg32 offset)], x.tokens.void_i_i1_offset_sig, instr) |> ignore
        x.AppendInstr OpCodes.Conv_I NoArg instr

    member private x.AppendMem_i1(idx, offset, instr : ilInstr) =
        x.AppendProbe(probes.mem_1, [(OpCodes.Ldc_I4, Arg32 idx); (OpCodes.Ldc_I4, Arg32 offset)], x.tokens.void_i1_i1_offset_sig, instr) |> ignore

    member private x.AppendMem_i2(idx, offset, instr : ilInstr) =
        x.AppendProbe(probes.mem_2, [(OpCodes.Ldc_I4, Arg32 idx); (OpCodes.Ldc_I4, Arg32 offset)], x.tokens.void_i2_i1_offset_sig, instr) |> ignore

    member private x.AppendMem_i4(idx, offset, instr : ilInstr) =
        x.AppendProbe(probes.mem_4, [(OpCodes.Ldc_I4, Arg32 idx); (OpCodes.Ldc_I4, Arg32 offset)], x.tokens.void_i4_i1_offset_sig, instr) |> ignore

    member private x.AppendMem_i8(idx, offset, instr : ilInstr) =
        x.AppendProbe(probes.mem_8, [(OpCodes.Ldc_I4, Arg32 idx); (OpCodes.Ldc_I4, Arg32 offset)], x.tokens.void_i8_i1_offset_sig, instr) |> ignore

    member private x.AppendMem_f4(idx, offset, instr : ilInstr) =
        x.AppendProbe(probes.mem_f4, [(OpCodes.Ldc_I4, Arg32 idx); (OpCodes.Ldc_I4, Arg32 offset)], x.tokens.void_r4_i1_offset_sig, instr) |> ignore

    member private x.AppendMem_f8(idx, offset, instr : ilInstr) =
        x.AppendProbe(probes.mem_f8, [(OpCodes.Ldc_I4, Arg32 idx); (OpCodes.Ldc_I4, Arg32 offset)], x.tokens.void_r8_i1_offset_sig, instr) |> ignore

    member private x.PrependValidLeaveMain(instr : ilInstr byref) =
        match instr.stackState with
        | _ when Reflection.hasNonVoidResult x.m |> not ->
            x.PrependProbeWithOffset(probes.leaveMain_0, [], x.tokens.void_offset_sig, &instr) |> ignore
        | UnOp evaluationStackCellType.I1
        | UnOp evaluationStackCellType.I2
        | UnOp evaluationStackCellType.I4 ->
            // TODO: 2Misha: Why mem value and then dup and pass it to probe?
            x.PrependDup &instr |> ignore
            x.PrependMem_i4(0, &instr)
            x.PrependDup &instr |> ignore
            x.PrependProbeWithOffset(probes.leaveMain_4, [], x.tokens.void_i4_offset_sig, &instr) |> ignore
        | UnOp evaluationStackCellType.I8 ->
            x.PrependDup &instr |> ignore
            x.PrependMem_i8(0, &instr)
            x.PrependDup &instr |> ignore
            x.PrependProbeWithOffset(probes.leaveMain_8, [], x.tokens.void_i8_offset_sig, &instr) |> ignore
        | UnOp evaluationStackCellType.R4 ->
            x.PrependDup &instr |> ignore
            x.PrependMem_f4(0, &instr)
            x.PrependDup &instr |> ignore
            x.PrependProbeWithOffset(probes.leaveMain_f4, [], x.tokens.void_r4_offset_sig, &instr) |> ignore
        | UnOp evaluationStackCellType.R8 ->
            x.PrependDup &instr |> ignore
            x.PrependMem_f8(0, &instr)
            x.PrependDup &instr |> ignore
            x.PrependProbeWithOffset(probes.leaveMain_f8, [], x.tokens.void_r8_offset_sig, &instr) |> ignore
        | UnOp evaluationStackCellType.I ->
            x.PrependDup &instr |> ignore
            x.PrependMem_p(0, &instr)
            x.PrependDup &instr |> ignore
            x.PrependProbeWithOffset(probes.leaveMain_p, [], x.tokens.void_i_offset_sig, &instr) |> ignore
        | UnOp evaluationStackCellType.Ref ->
            x.PrependDup &instr |> ignore
            x.PrependMem_p(0, &instr)
            x.PrependDup &instr |> ignore
            x.PrependInstr(OpCodes.Conv_I, NoArg, &instr) |> ignore
            x.PrependProbeWithOffset(probes.leaveMain_p, [], x.tokens.void_i_offset_sig, &instr) |> ignore
        | UnOp evaluationStackCellType.Struct
        | UnOp evaluationStackCellType.RefLikeStruct ->
            x.PrependDup &instr |> ignore
            let returnType = Reflection.getMethodReturnType x.m
            let returnTypeToken = x.AcceptReturnTypeToken returnType
            x.PrependInstr(OpCodes.Box, Arg32 returnTypeToken, &instr) |> ignore
            x.PrependDup &instr |> ignore
            x.PrependMem_p(0, &instr)
            x.PrependInstr(OpCodes.Conv_I, NoArg, &instr) |> ignore
            x.PrependProbeWithOffset(probes.leaveMain_p, [], x.tokens.void_i_offset_sig, &instr) |> ignore
        | _ -> internalfailf "PrependValidLeaveMain: unexpected stack state! %O" instr.stackState

    member private x.PlaceLeaveProbe(instr : ilInstr byref) =
        if x.m.MethodHandle = entryPoint.MethodHandle then
            x.PrependValidLeaveMain(&instr)
        else
            let returnsSomething = Reflection.hasNonVoidResult x.m
            let args = [(OpCodes.Ldc_I4, (if returnsSomething then 1 else 0) |> Arg32)]
            x.PrependProbeWithOffset(probes.leave, args, x.tokens.void_u1_offset_sig, &instr) |> ignore

    member x.MethodName with get() = x.m.Name

    member private x.PrependLdcDefault(t : System.Type, instr : ilInstr byref) =
        match t with
        | _ when not t.IsValueType -> x.PrependInstr(OpCodes.Ldnull, NoArg, &instr)
        | _ when t = typeof<bool> -> x.PrependInstr(OpCodes.Ldc_I4_0, NoArg, &instr)
        | _ when t = typeof<int8> -> x.PrependInstr(OpCodes.Ldc_I4_0, NoArg, &instr)
        | _ when t = typeof<uint8> -> x.PrependInstr(OpCodes.Ldc_I4_0,NoArg, &instr)
        | _ when t = typeof<int16> -> x.PrependInstr(OpCodes.Ldc_I4_0, NoArg, &instr)
        | _ when t = typeof<uint16> -> x.PrependInstr(OpCodes.Ldc_I4_0, NoArg, &instr)
        | _ when t = typeof<int> -> x.PrependInstr(OpCodes.Ldc_I4_0, NoArg, &instr)
        | _ when t = typeof<uint> -> x.PrependInstr(OpCodes.Ldc_I4_0, NoArg, &instr)
        | _ when t = typeof<int64> -> x.PrependInstr(OpCodes.Ldc_I8, (Arg64 0L), &instr)
        | _ when t = typeof<uint64> -> x.PrependInstr(OpCodes.Ldc_I8, (Arg64 0L), &instr)
        | _ when t = typeof<single> -> x.PrependInstr(OpCodes.Ldc_R4, (Arg32 0), &instr)
        | _ when t = typeof<double> -> x.PrependInstr(OpCodes.Ldc_R8, (Arg64 0L), &instr)
        | _ -> __unreachable__()

    member private x.SizeOfIndirection = function
        | OpCodeValues.Ldind_I1
        | OpCodeValues.Ldind_U1
        | OpCodeValues.Stind_I1 -> 1
        | OpCodeValues.Ldind_I2
        | OpCodeValues.Ldind_U2
        | OpCodeValues.Stind_I2 -> 2
        | OpCodeValues.Ldind_I4
        | OpCodeValues.Ldind_U4
        | OpCodeValues.Ldind_R4
        | OpCodeValues.Stind_I4
        | OpCodeValues.Stind_R4 -> 4
        | OpCodeValues.Ldind_I8
        | OpCodeValues.Ldind_R8
        | OpCodeValues.Stind_I8
        | OpCodeValues.Stind_R8 -> 8
        | OpCodeValues.Ldind_I
        | OpCodeValues.Ldind_Ref
        | OpCodeValues.Stind_I
        | OpCodeValues.Stind_Ref -> System.IntPtr.Size
        | _ -> __unreachable__()

    member private x.PrependMemUnmemForType(t : evaluationStackCellType, idx, instr : ilInstr byref) =
        match t with
        | evaluationStackCellType.I1 ->
            x.PrependMem_i1(idx, &instr)
            probes.unmem_1, x.tokens.i1_i1_sig
        | evaluationStackCellType.I2 ->
            x.PrependMem_i2(idx, &instr)
            probes.unmem_2, x.tokens.i2_i1_sig
        | evaluationStackCellType.I4 ->
            x.PrependMem_i4(idx, &instr)
            probes.unmem_4, x.tokens.i4_i1_sig
        | evaluationStackCellType.I8 ->
            x.PrependMem_i8(idx, &instr)
            probes.unmem_8, x.tokens.i8_i1_sig
        | evaluationStackCellType.R4 ->
            x.PrependMem_f4(idx, &instr)
            probes.unmem_f4, x.tokens.r4_i1_sig
        | evaluationStackCellType.R8 ->
            x.PrependMem_f8(idx, &instr)
            probes.unmem_f8, x.tokens.r8_i1_sig
        | evaluationStackCellType.I ->
            x.PrependMem_p(idx, &instr)
            probes.unmem_p, x.tokens.i_i1_sig
        | evaluationStackCellType.Ref ->
            x.PrependMem_p(idx, &instr)
            probes.unmem_p, x.tokens.i_i1_sig
        | evaluationStackCellType.Struct
            // TODO: support struct
//            x.PrependInstr(OpCodes.Box, NoArg, &instr)
//            x.PrependMem_p(idx, order, &instr)
//            probes.unmem_p, x.tokens.i_i1_sig
        | evaluationStackCellType.RefLikeStruct ->
            __notImplemented__()
            // TODO: support struct
//            x.PrependInstr(OpCodes.Box, NoArg, &instr)
//            x.PrependMem_p(idx, order, &instr)
//            probes.unmem_p, x.tokens.i_i1_sig
        | _ -> __unreachable__()

    member private x.PrependMemForType(t : evaluationStackCellType, idx, instr : ilInstr byref) =
        match t with
        | evaluationStackCellType.I1 -> x.PrependMem_i1(idx, &instr)
        | evaluationStackCellType.I2 -> x.PrependMem_i2(idx, &instr)
        | evaluationStackCellType.I4 -> x.PrependMem_i4(idx, &instr)
        | evaluationStackCellType.I8 -> x.PrependMem_i8(idx, &instr)
        | evaluationStackCellType.R4 -> x.PrependMem_f4(idx, &instr)
        | evaluationStackCellType.R8 -> x.PrependMem_f8(idx, &instr)
        | evaluationStackCellType.I -> x.PrependMem_p(idx, &instr)
        | evaluationStackCellType.Ref -> x.PrependMem_p(idx, &instr)
        | evaluationStackCellType.Struct
            // TODO: support struct
//            x.PrependInstr(OpCodes.Box, NoArg, &instr)
//            x.PrependMem_p(idx, order, &instr)
//            probes.unmem_p, x.tokens.i_i1_sig
        | evaluationStackCellType.RefLikeStruct ->
            __notImplemented__()
            // TODO: support struct
//            x.PrependInstr(OpCodes.Box, NoArg, &instr)
//            x.PrependMem_p(idx, order, &instr)
//            probes.unmem_p, x.tokens.i_i1_sig
        | _ -> __unreachable__()

    member private x.AppendMemForType(t : evaluationStackCellType, idx, offset, instr : ilInstr) =
        match t with
        | evaluationStackCellType.I1 -> x.AppendMem_i1(idx, offset, instr)
        | evaluationStackCellType.I2 -> x.AppendMem_i2(idx, offset, instr)
        | evaluationStackCellType.I4 -> x.AppendMem_i4(idx, offset, instr)
        | evaluationStackCellType.I8 -> x.AppendMem_i8(idx, offset, instr)
        | evaluationStackCellType.R4 -> x.AppendMem_f4(idx, offset, instr)
        | evaluationStackCellType.R8 -> x.AppendMem_f8(idx, offset, instr)
        | evaluationStackCellType.I -> x.AppendMem_p(idx, offset, instr)
        | evaluationStackCellType.Ref -> x.AppendMem_p(idx, offset, instr)
        | evaluationStackCellType.Struct
        | evaluationStackCellType.RefLikeStruct ->
            __notImplemented__()
            // TODO: support struct
        | _ -> __unreachable__()

    member private x.AcceptTypeToken (t : System.Type) accept =
        if t.Module = x.m.Module && t.IsTypeDefinition && not (t.IsGenericType || t.IsGenericTypeDefinition) then
            t.MetadataToken
        else accept()

    member private x.AcceptFieldTypeToken (f : FieldInfo) =
        if f.Module = x.m.Module then
            x.AcceptTypeToken f.DeclaringType (fun () -> x.AcceptFieldDefTypeToken f.MetadataToken)
        else
            x.AcceptFieldRefTypeToken f.MetadataToken

    member private x.AcceptFieldRefTypeToken (memberRef : int) =
        communicator.ParseFieldRefTypeToken memberRef |> int

    member private x.AcceptFieldDefTypeToken (fieldDef : int) =
        communicator.ParseFieldDefTypeToken fieldDef |> int

    member private x.AcceptArgTypeToken (t : System.Type) idx =
        x.AcceptTypeToken t (fun () ->
            let methodDef = x.m.MetadataToken
            communicator.ParseArgTypeToken methodDef idx |> int)

    member private x.AcceptLocVarTypeToken (t : System.Type) idx =
        x.AcceptTypeToken t (fun () -> communicator.ParseLocalTypeToken idx |> int)

    member private x.AcceptReturnTypeToken (t : System.Type) =
        x.AcceptTypeToken t (communicator.ParseReturnTypeToken >> int)

    member private x.AcceptDeclaringTypeToken (m : MethodBase) (methodToken : int) =
        x.AcceptTypeToken m.DeclaringType (fun () -> communicator.ParseDeclaringTypeToken methodToken |> int)

    member private x.TypeSizeInstr (t : System.Type) getToken =
        if t.IsByRef || t.IsArray || (t.IsConstructedGenericType && t.GetGenericTypeDefinition().FullName = "System.ByReference`1") then
            OpCodes.Ldc_I4, Arg32 System.IntPtr.Size
        elif TypeUtils.isGround t then
            let size = TypeUtils.internalSizeOf t
            OpCodes.Ldc_I4, Arg32 (int size)
        else
            let typeToken = getToken()
            OpCodes.Sizeof, Arg32 typeToken

    member x.PlaceProbes() =
        let instructions = x.rewriter.CopyInstructions()
        let cfg = CFG.findCfg x.m
        let basicBlocks = cfg.sortedOffsets
        let mutable currentBasicBlockIndex = 0
        assert(not <| Array.isEmpty instructions)
        let mutable atLeastOneReturnFound = false
        let mutable hasPrefix = false
        let mutable prefixCell = instructions.[0]
        let mutable prefix : ilInstr byref = &prefixCell
        x.PlaceEnterProbe(&instructions.[0])
        for i in 0 .. instructions.Length - 1 do
            let instr = &instructions.[i]
            if not hasPrefix then prefix <- instr
            match instr.opcode with
            | OpCode op ->
                let prependTarget = if hasPrefix then &prefix else &instr
                let dumpedInfo = x.rewriter.ILInstrToString probes instr
                let idx = communicator.SendStringAndReadItsIndex dumpedInfo
                x.PrependProbe(probes.dumpInstruction, [OpCodes.Ldc_I4, idx |> int |> Arg32], x.tokens.void_u4_sig, &prependTarget) |> ignore
                if not hasPrefix && uint32 basicBlocks[currentBasicBlockIndex] = instr.offset then
                    x.PrependProbeWithOffset(probes.trackCoverage, [], x.tokens.void_offset_sig, &instr) |> ignore
                    currentBasicBlockIndex <- currentBasicBlockIndex + 1

                let opcodeValue = LanguagePrimitives.EnumOfValue op.Value
                match opcodeValue with
                // Prefixes
                | OpCodeValues.Unaligned_
                | OpCodeValues.Volatile_
                | OpCodeValues.Tail_
                | OpCodeValues.Constrained_
                | OpCodeValues.Readonly_  ->
                    hasPrefix <- true

                // Concrete instructions
                | OpCodeValues.Ldarga_S ->
                    let index = int instr.Arg8
                    let argType = x.m |> Reflection.getMethodArgumentType index
                    if hasComplexSize argType then
                        x.AppendProbe(probes.ldarga_struct, [(OpCodes.Ldc_I4, Arg32 index)], x.tokens.void_i_u2_sig, instr) |> ignore
                    else
                        let size = TypeUtils.internalSizeOf argType |> int
                        x.AppendProbe(probes.ldarga_primitive, [(OpCodes.Ldc_I4, Arg32 index); (OpCodes.Ldc_I4, Arg32 size)], x.tokens.void_i_u2_size_sig, instr) |> ignore
                    x.AppendDup instr
                | OpCodeValues.Ldloca_S ->
                    let index = int instr.Arg8
                    let locType = x.m.GetMethodBody().LocalVariables.[index].LocalType
                    if hasComplexSize locType then
                        x.AppendProbe(probes.ldloca_struct, [(OpCodes.Ldc_I4, Arg32 index)], x.tokens.void_i_u2_sig, instr) |> ignore
                    else
                        let size = TypeUtils.internalSizeOf locType |> int
                        x.AppendProbe(probes.ldloca_primitive, [(OpCodes.Ldc_I4, Arg32 index); (OpCodes.Ldc_I4, Arg32 size)], x.tokens.void_i_u2_size_sig, instr) |> ignore
                    x.AppendDup instr
                | OpCodeValues.Ldarga ->
                    let index = int instr.Arg16
                    let argType = x.m |> Reflection.getMethodArgumentType index
                    if hasComplexSize argType then
                        x.AppendProbe(probes.ldarga_struct, [(OpCodes.Ldc_I4, Arg32 index)], x.tokens.void_i_u2_sig, instr) |> ignore
                    else
                        let size = TypeUtils.internalSizeOf argType |> int
                        x.AppendProbe(probes.ldarga_primitive, [(OpCodes.Ldc_I4, Arg32 index); (OpCodes.Ldc_I4, Arg32 size)], x.tokens.void_i_u2_size_sig, instr) |> ignore
                    x.AppendDup instr
                | OpCodeValues.Ldloca ->
                    let index = int instr.Arg16
                    let locType = x.m.GetMethodBody().LocalVariables.[index].LocalType
                    if hasComplexSize locType then
                        x.AppendProbe(probes.ldloca_struct, [(OpCodes.Ldc_I4, Arg32 index)], x.tokens.void_i_u2_sig, instr) |> ignore
                    else
                        let size = TypeUtils.internalSizeOf locType |> int
                        x.AppendProbe(probes.ldloca_primitive, [(OpCodes.Ldc_I4, Arg32 index); (OpCodes.Ldc_I4, Arg32 size)], x.tokens.void_i_u2_size_sig, instr) |> ignore
                    x.AppendDup instr
                | OpCodeValues.Ldnull
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
                | OpCodeValues.Ldc_I4
                | OpCodeValues.Ldc_I8
                | OpCodeValues.Ldc_R4
                | OpCodeValues.Ldc_R8 -> x.AppendProbe(probes.ldc, [], x.tokens.void_sig, instr) |> ignore
                | OpCodeValues.Pop -> x.AppendProbe(probes.pop, [], x.tokens.void_sig, instr) |> ignore
                | OpCodeValues.Ldtoken -> x.AppendProbe(probes.ldtoken, [], x.tokens.void_sig, instr) |> ignore
                | OpCodeValues.Arglist -> x.AppendProbe(probes.arglist, [], x.tokens.void_sig, instr) |> ignore
                | OpCodeValues.Ldftn -> x.AppendProbe(probes.ldftn, [], x.tokens.void_sig, instr) |> ignore
                | OpCodeValues.Sizeof -> x.AppendProbe(probes.sizeof, [], x.tokens.void_sig, instr) |> ignore

                // Branchings
                | OpCodeValues.Brfalse_S
                | OpCodeValues.Brfalse -> x.PrependProbeWithOffset(probes.brfalse, [], x.tokens.void_offset_sig, &prependTarget) |> ignore
                | OpCodeValues.Brtrue_S
                | OpCodeValues.Brtrue -> x.PrependProbeWithOffset(probes.brtrue, [], x.tokens.void_offset_sig, &prependTarget) |> ignore
                | OpCodeValues.Switch -> x.PrependProbeWithOffset(probes.switch, [], x.tokens.void_offset_sig, &prependTarget) |> ignore

                // Symbolic stack instructions
                | OpCodeValues.Ldarg_0 -> x.AppendProbeWithOffset(probes.ldarg_0, [], prependTarget.offset, x.tokens.void_offset_sig, instr) |> ignore
                | OpCodeValues.Ldarg_1 -> x.AppendProbeWithOffset(probes.ldarg_1, [], prependTarget.offset, x.tokens.void_offset_sig, instr) |> ignore
                | OpCodeValues.Ldarg_2 -> x.AppendProbeWithOffset(probes.ldarg_2, [], prependTarget.offset, x.tokens.void_offset_sig, instr) |> ignore
                | OpCodeValues.Ldarg_3 -> x.AppendProbeWithOffset(probes.ldarg_3, [], prependTarget.offset, x.tokens.void_offset_sig, instr) |> ignore
                | OpCodeValues.Ldloc_0 -> x.AppendProbeWithOffset(probes.ldloc_0, [], prependTarget.offset, x.tokens.void_offset_sig, instr) |> ignore
                | OpCodeValues.Ldloc_1 -> x.AppendProbeWithOffset(probes.ldloc_1, [], prependTarget.offset, x.tokens.void_offset_sig, instr) |> ignore
                | OpCodeValues.Ldloc_2 -> x.AppendProbeWithOffset(probes.ldloc_2, [], prependTarget.offset, x.tokens.void_offset_sig, instr) |> ignore
                | OpCodeValues.Ldloc_3 -> x.AppendProbeWithOffset(probes.ldloc_3, [], prependTarget.offset, x.tokens.void_offset_sig, instr) |> ignore
                | OpCodeValues.Stloc_0 -> x.AppendProbeWithOffset(probes.stloc_0, [], prependTarget.offset, x.tokens.void_offset_sig, instr) |> ignore
                | OpCodeValues.Stloc_1 -> x.AppendProbeWithOffset(probes.stloc_1, [], prependTarget.offset, x.tokens.void_offset_sig, instr) |> ignore
                | OpCodeValues.Stloc_2 -> x.AppendProbeWithOffset(probes.stloc_2, [], prependTarget.offset, x.tokens.void_offset_sig, instr) |> ignore
                | OpCodeValues.Stloc_3 -> x.AppendProbeWithOffset(probes.stloc_3, [], prependTarget.offset, x.tokens.void_offset_sig, instr) |> ignore
                | OpCodeValues.Ldarg_S -> x.AppendProbeWithOffset(probes.ldarg_S, [(OpCodes.Ldc_I4, instr.Arg8 |> int |> Arg32)], prependTarget.offset, x.tokens.void_u1_offset_sig, instr) |> ignore
                | OpCodeValues.Starg_S -> x.AppendProbeWithOffset(probes.starg_S, [(OpCodes.Ldc_I4, instr.Arg8 |> int |> Arg32)], prependTarget.offset, x.tokens.void_u1_offset_sig, instr) |> ignore
                | OpCodeValues.Ldloc_S -> x.AppendProbeWithOffset(probes.ldloc_S, [(OpCodes.Ldc_I4, instr.Arg8 |> int |> Arg32)], prependTarget.offset, x.tokens.void_u1_offset_sig, instr) |> ignore
                | OpCodeValues.Stloc_S -> x.AppendProbeWithOffset(probes.stloc_S, [(OpCodes.Ldc_I4, instr.Arg8 |> int |> Arg32)], prependTarget.offset, x.tokens.void_u1_offset_sig, instr) |> ignore
                | OpCodeValues.Ldarg -> x.AppendProbeWithOffset(probes.ldarg, [(OpCodes.Ldc_I4, instr.Arg16 |> int |> Arg32)], prependTarget.offset, x.tokens.void_u2_offset_sig, instr) |> ignore
                | OpCodeValues.Starg -> x.AppendProbeWithOffset(probes.starg, [(OpCodes.Ldc_I4, instr.Arg16 |> int |> Arg32)], prependTarget.offset, x.tokens.void_u2_offset_sig, instr) |> ignore
                | OpCodeValues.Ldloc -> x.AppendProbeWithOffset(probes.ldloc, [(OpCodes.Ldc_I4, instr.Arg16 |> int |> Arg32)], prependTarget.offset, x.tokens.void_u2_offset_sig, instr) |> ignore
                | OpCodeValues.Stloc -> x.AppendProbeWithOffset(probes.stloc, [(OpCodes.Ldc_I4, instr.Arg16 |> int |> Arg32)], prependTarget.offset, x.tokens.void_u2_offset_sig, instr) |> ignore
                | OpCodeValues.Dup -> x.AppendProbeWithOffset(probes.dup, [], prependTarget.offset, x.tokens.void_offset_sig, instr) |> ignore

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
                | OpCodeValues.Shl
                | OpCodeValues.Shr
                | OpCodeValues.Shr_Un
                | OpCodeValues.Add_Ovf
                | OpCodeValues.Add_Ovf_Un
                | OpCodeValues.Mul_Ovf
                | OpCodeValues.Mul_Ovf_Un
                | OpCodeValues.Sub_Ovf
                | OpCodeValues.Sub_Ovf_Un
                | OpCodeValues.Ceq
                | OpCodeValues.Cgt
                | OpCodeValues.Cgt_Un
                | OpCodeValues.Clt
                | OpCodeValues.Clt_Un ->
                    // calli track_binop
                    // branch_true A
                    // calli mem2
                    // ldc op
                    // calli unmem 0
                    // calli unmem 1
                    // calli exec
                    // calli unmem 0
                    // calli unmem 1
                    // A: binop

                    let isUnchecked =
                        match opcodeValue with
                        | OpCodeValues.Add_Ovf
                        | OpCodeValues.Add_Ovf_Un
                        | OpCodeValues.Mul_Ovf
                        | OpCodeValues.Mul_Ovf_Un
                        | OpCodeValues.Sub_Ovf
                        | OpCodeValues.Sub_Ovf_Un -> false
                        | _ -> true

                    // Track
                    x.PrependProbe(probes.binOp, [], x.tokens.bool_sig, &prependTarget) |> ignore
                    let br = x.PrependBranch(OpCodes.Brtrue_S, &prependTarget)

                    // Mem and get exec with unmem
                    let execProbe, execSig, unmem1Probe, unmem1Sig, unmem2Probe, unmem2Sig =
                        match instr.stackState with // TODO: unify getting stackState #do
                        | BinOp(evaluationStackCellType.I4, evaluationStackCellType.I4)
                        | BinOp(evaluationStackCellType.I1, evaluationStackCellType.I1)
                        | BinOp(evaluationStackCellType.I1, evaluationStackCellType.I2)
                        | BinOp(evaluationStackCellType.I1, evaluationStackCellType.I4)
                        | BinOp(evaluationStackCellType.I2, evaluationStackCellType.I1)
                        | BinOp(evaluationStackCellType.I2, evaluationStackCellType.I2)
                        | BinOp(evaluationStackCellType.I2, evaluationStackCellType.I4)
                        | BinOp(evaluationStackCellType.I4, evaluationStackCellType.I1)
                        | BinOp(evaluationStackCellType.I4, evaluationStackCellType.I2) ->
                            x.PrependProbeWithOffset(probes.mem2_4, [], x.tokens.void_i4_i4_offset_sig, &prependTarget) |> ignore
                            (if isUnchecked then probes.execBinOp_4 else probes.execBinOp_4_ovf), x.tokens.void_u2_i4_i4_offset_sig,
                                probes.unmem_4, x.tokens.i4_i1_sig, probes.unmem_4, x.tokens.i4_i1_sig
                        | BinOp(evaluationStackCellType.I4, evaluationStackCellType.I8) ->
                            x.PrependProbeWithOffset(probes.mem2_8_4, [], x.tokens.void_i8_i4_offset_sig, &prependTarget) |> ignore
                            (if isUnchecked then probes.execBinOp_8_4 else probes.execBinOp_8_4_ovf), x.tokens.void_u2_i8_i4_offset_sig,
                                probes.unmem_8, x.tokens.i8_i1_sig, probes.unmem_4, x.tokens.i4_i1_sig
                        | BinOp(evaluationStackCellType.I8, evaluationStackCellType.I8) ->
                            x.PrependProbeWithOffset(probes.mem2_8, [], x.tokens.void_i8_i8_offset_sig, &prependTarget) |> ignore
                            (if isUnchecked then probes.execBinOp_8 else probes.execBinOp_8_ovf), x.tokens.void_u2_i8_i8_offset_sig,
                                probes.unmem_8, x.tokens.i8_i1_sig, probes.unmem_8, x.tokens.i8_i1_sig
                        | BinOp(evaluationStackCellType.R4, evaluationStackCellType.R4) ->
                            x.PrependProbeWithOffset(probes.mem2_f4, [], x.tokens.void_r4_r4_offset_sig, &prependTarget) |> ignore
                            (if isUnchecked then probes.execBinOp_f4 else probes.execBinOp_f4_ovf), x.tokens.void_u2_r4_r4_offset_sig,
                                probes.unmem_f4, x.tokens.r4_i1_sig, probes.unmem_f4, x.tokens.r4_i1_sig
                        | BinOp(evaluationStackCellType.R8, evaluationStackCellType.R8) ->
                            x.PrependProbeWithOffset(probes.mem2_f8, [], x.tokens.void_r8_r8_offset_sig, &prependTarget) |> ignore
                            (if isUnchecked then probes.execBinOp_f8 else probes.execBinOp_f8_ovf), x.tokens.void_u2_r8_r8_offset_sig,
                                probes.unmem_f8, x.tokens.r8_i1_sig, probes.unmem_f8, x.tokens.r8_i1_sig
                        | BinOp(evaluationStackCellType.I, evaluationStackCellType.I)
                        | BinOp(evaluationStackCellType.I, evaluationStackCellType.Ref)
                        | BinOp(evaluationStackCellType.Ref, evaluationStackCellType.I)
                        | BinOp(evaluationStackCellType.Ref, evaluationStackCellType.Ref) ->
                            x.PrependMem2_p &prependTarget
                            (if isUnchecked then probes.execBinOp_p else probes.execBinOp_p_ovf), x.tokens.void_u2_i_i_offset_sig,
                                probes.unmem_p, x.tokens.i_i1_sig, probes.unmem_p, x.tokens.i_i1_sig
                        | BinOp(evaluationStackCellType.I1, evaluationStackCellType.I)
                        | BinOp(evaluationStackCellType.I2, evaluationStackCellType.I)
                        | BinOp(evaluationStackCellType.I4, evaluationStackCellType.I)
                        | BinOp(evaluationStackCellType.I1, evaluationStackCellType.Ref)
                        | BinOp(evaluationStackCellType.I2, evaluationStackCellType.Ref)
                        | BinOp(evaluationStackCellType.I4, evaluationStackCellType.Ref) ->
                            x.PrependMem2_p_4 &prependTarget
                            (if isUnchecked then probes.execBinOp_p_4 else probes.execBinOp_p_4_ovf), x.tokens.void_u2_i_i4_offset_sig,
                                probes.unmem_p, x.tokens.i_i1_sig, probes.unmem_4, x.tokens.i4_i1_sig
                        | BinOp(evaluationStackCellType.I, evaluationStackCellType.I1)
                        | BinOp(evaluationStackCellType.I, evaluationStackCellType.I2)
                        | BinOp(evaluationStackCellType.I, evaluationStackCellType.I4)
                        | BinOp(evaluationStackCellType.Ref, evaluationStackCellType.I1)
                        | BinOp(evaluationStackCellType.Ref, evaluationStackCellType.I2)
                        | BinOp(evaluationStackCellType.Ref, evaluationStackCellType.I4) ->
                            x.PrependMem2_4_p &prependTarget
                            (if isUnchecked then probes.execBinOp_4_p else probes.execBinOp_4_p_ovf), x.tokens.void_u2_i4_i_offset_sig,
                                probes.unmem_4, x.tokens.i4_i1_sig, probes.unmem_p, x.tokens.i_i1_sig
                        | BinOp(x, y) -> internalfailf "Unexpected binop ([%O]%O) evaluation stack types: %O, %O" i opcodeValue x y
                        | stack -> internalfailf "Unexpected binop (%O) evaluation stack types! stack: %O" opcodeValue stack

                    x.PrependInstr(OpCodes.Ldc_I4, op.Value |> int |> Arg32 , &prependTarget) |> ignore
                    x.PrependProbe(unmem1Probe, [(OpCodes.Ldc_I4, Arg32 0)], unmem1Sig, &prependTarget) |> ignore
                    x.PrependProbe(unmem2Probe, [(OpCodes.Ldc_I4, Arg32 1)], unmem2Sig, &prependTarget) |> ignore
                    x.PrependProbeWithOffset(execProbe, [], execSig, &prependTarget) |> ignore
                    x.PrependProbe(unmem1Probe, [(OpCodes.Ldc_I4, Arg32 0)], unmem1Sig, &prependTarget) |> ignore
                    x.PrependProbe(unmem2Probe, [(OpCodes.Ldc_I4, Arg32 1)], unmem2Sig, &prependTarget) |> ignore
                    x.PrependPopOpmem &prependTarget |> ignore
                    br.arg <- Target prependTarget

                | OpCodeValues.Neg
                | OpCodeValues.Not ->
                    match instr.opcode with
                    | OpCode op -> x.AppendProbeWithOffset(probes.unOp, [(OpCodes.Ldc_I4, op.Value |> int |> Arg32)], prependTarget.offset, x.tokens.void_u2_offset_sig, instr) |> ignore
                    | _ -> __unreachable__()

                | OpCodeValues.Conv_I1
                | OpCodeValues.Conv_I2
                | OpCodeValues.Conv_I4
                | OpCodeValues.Conv_I8
                | OpCodeValues.Conv_R4
                | OpCodeValues.Conv_R8
                | OpCodeValues.Conv_U4
                | OpCodeValues.Conv_U8
                | OpCodeValues.Conv_R_Un
                | OpCodeValues.Conv_U2
                | OpCodeValues.Conv_U1
                | OpCodeValues.Conv_I
                | OpCodeValues.Conv_U ->
                    x.AppendProbeWithOffset(probes.conv, [], prependTarget.offset, x.tokens.void_offset_sig, instr) |> ignore
                | OpCodeValues.Conv_Ovf_I1_Un
                | OpCodeValues.Conv_Ovf_I2_Un
                | OpCodeValues.Conv_Ovf_I4_Un
                | OpCodeValues.Conv_Ovf_I8_Un
                | OpCodeValues.Conv_Ovf_U1_Un
                | OpCodeValues.Conv_Ovf_U2_Un
                | OpCodeValues.Conv_Ovf_U4_Un
                | OpCodeValues.Conv_Ovf_U8_Un
                | OpCodeValues.Conv_Ovf_I_Un
                | OpCodeValues.Conv_Ovf_U_Un
                | OpCodeValues.Conv_Ovf_I1
                | OpCodeValues.Conv_Ovf_U1
                | OpCodeValues.Conv_Ovf_I2
                | OpCodeValues.Conv_Ovf_U2
                | OpCodeValues.Conv_Ovf_I4
                | OpCodeValues.Conv_Ovf_U4
                | OpCodeValues.Conv_Ovf_I8
                | OpCodeValues.Conv_Ovf_U8
                | OpCodeValues.Conv_Ovf_I
                | OpCodeValues.Conv_Ovf_U ->
                    x.AppendProbeWithOffset(probes.conv, [], prependTarget.offset, x.tokens.void_offset_sig, instr) |> ignore

                | OpCodeValues.Ldind_I1
                | OpCodeValues.Ldind_U1
                | OpCodeValues.Ldind_I2
                | OpCodeValues.Ldind_U2
                | OpCodeValues.Ldind_I4
                | OpCodeValues.Ldind_U4
                | OpCodeValues.Ldind_I8
                | OpCodeValues.Ldind_I
                | OpCodeValues.Ldind_R4
                | OpCodeValues.Ldind_R8
                | OpCodeValues.Ldind_Ref ->
                    // dup
                    // calli track_ldind
                    // ldind
                    x.PrependDup &prependTarget |> ignore
                    x.PrependInstr(OpCodes.Ldc_I4, Arg32 (x.SizeOfIndirection opcodeValue), &prependTarget) |> ignore
                    x.PrependProbeWithOffset(probes.ldind, [], x.tokens.void_i_i4_offset_sig, &prependTarget) |> ignore

                | OpCodeValues.Stind_Ref
                | OpCodeValues.Stind_I1
                | OpCodeValues.Stind_I2
                | OpCodeValues.Stind_I4
                | OpCodeValues.Stind_I8
                | OpCodeValues.Stind_R4
                | OpCodeValues.Stind_R8
                | OpCodeValues.Stind_I ->
                    // calli mem2
                    // calli unmem 0
                    // ldc sizeOfPtr
                    // calli track_stind
                    // branch_true A
                    // calli unmem 0
                    // calli unmem 1
                    // calli exec
                    // A: calli unmem 0
                    // calli unmem 1
                    // stind

                    let execProbe, execSig, unmem2Probe, unmem2Sig =
                        match opcodeValue with
                        | OpCodeValues.Stind_I ->
                            match instr.stackState with
                            | BinOp(evaluationStackCellType.I, evaluationStackCellType.I)
                            | BinOp(evaluationStackCellType.I, evaluationStackCellType.Ref) -> ()
                            | _ -> internalfail "Stack validation failed"
                            x.PrependMem2_p &prependTarget
                            probes.execStind_ref, x.tokens.void_i_i_offset_sig, probes.unmem_p, x.tokens.i_i1_sig
                        | OpCodeValues.Stind_Ref ->
                            match instr.stackState with
                            | BinOp(evaluationStackCellType.Ref, evaluationStackCellType.I)
                            | BinOp(evaluationStackCellType.Ref, evaluationStackCellType.Ref) -> ()
                            | _ -> internalfail "Stack validation failed"
                            x.PrependMem2_p &prependTarget
                            probes.execStind_ref, x.tokens.void_i_i_offset_sig, probes.unmem_p, x.tokens.i_i1_sig
                        | OpCodeValues.Stind_I1 ->
                            match instr.stackState with
                            | BinOp(evaluationStackCellType.I1, evaluationStackCellType.I)
                            | BinOp(evaluationStackCellType.I1, evaluationStackCellType.Ref) -> ()
                            | BinOp(evaluationStackCellType.I2, evaluationStackCellType.I)
                            | BinOp(evaluationStackCellType.I2, evaluationStackCellType.Ref) -> ()
                            | BinOp(evaluationStackCellType.I4, evaluationStackCellType.I)
                            | BinOp(evaluationStackCellType.I4, evaluationStackCellType.Ref) -> ()
                            | _ -> internalfail "Stack validation failed"
                            x.PrependMem2_p_1 &prependTarget
                            probes.execStind_I1, x.tokens.void_i_i1_offset_sig, probes.unmem_1, x.tokens.i1_i1_sig
                        | OpCodeValues.Stind_I2 ->
                            match instr.stackState with
                            | BinOp(evaluationStackCellType.I2, evaluationStackCellType.I)
                            | BinOp(evaluationStackCellType.I2, evaluationStackCellType.Ref) -> ()
                            | BinOp(evaluationStackCellType.I4, evaluationStackCellType.I)
                            | BinOp(evaluationStackCellType.I4, evaluationStackCellType.Ref) -> ()
                            | _ -> internalfail "Stack validation failed"
                            x.PrependMem2_p_2 &prependTarget
                            probes.execStind_I2, x.tokens.void_i_i2_offset_sig, probes.unmem_2, x.tokens.i2_i1_sig
                        | OpCodeValues.Stind_I4 ->
                            match instr.stackState with
                            | BinOp(evaluationStackCellType.I4, evaluationStackCellType.I)
                            | BinOp(evaluationStackCellType.I4, evaluationStackCellType.Ref) -> ()
                            | _ -> internalfail "Stack validation failed"
                            x.PrependMem2_p_4 &prependTarget
                            probes.execStind_I4, x.tokens.void_i_i4_offset_sig, probes.unmem_4, x.tokens.i4_i1_sig
                        | OpCodeValues.Stind_I8 ->
                            match instr.stackState with
                            | BinOp(evaluationStackCellType.I8, evaluationStackCellType.I)
                            | BinOp(evaluationStackCellType.I8, evaluationStackCellType.Ref) -> ()
                            | _ -> internalfail "Stack validation failed"
                            x.PrependMem2_p_8 &prependTarget
                            probes.execStind_I8, x.tokens.void_i_i8_offset_sig, probes.unmem_8, x.tokens.i8_i1_sig
                        | OpCodeValues.Stind_R4 ->
                            match instr.stackState with
                            | BinOp(evaluationStackCellType.R4, evaluationStackCellType.I)
                            | BinOp(evaluationStackCellType.R4, evaluationStackCellType.Ref) -> ()
                            | _ -> internalfail "Stack validation failed"
                            x.PrependMem2_p_f4 &prependTarget
                            probes.execStind_R4, x.tokens.void_i_r4_offset_sig, probes.unmem_f4, x.tokens.r4_i1_sig
                        | OpCodeValues.Stind_R8 ->
                            match instr.stackState with
                            | BinOp(evaluationStackCellType.R8, evaluationStackCellType.I)
                            | BinOp(evaluationStackCellType.R8, evaluationStackCellType.Ref) -> ()
                            | _ -> internalfail "Stack validation failed"
                            x.PrependMem2_p_f8 &prependTarget
                            probes.execStind_R8, x.tokens.void_i_r8_offset_sig, probes.unmem_f8, x.tokens.r8_i1_sig
                        | _ -> __unreachable__()

                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependInstr(OpCodes.Ldc_I4, Arg32 (x.SizeOfIndirection opcodeValue), &prependTarget) |> ignore
                    x.PrependProbe(probes.stind, [], x.tokens.bool_i_i4_sig, &prependTarget) |> ignore
                    let br_true = x.PrependBranch(OpCodes.Brtrue_S, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(unmem2Probe, [(OpCodes.Ldc_I4, Arg32 1)], unmem2Sig, &prependTarget) |> ignore
                    x.PrependProbeWithOffset(execProbe, [], execSig, &prependTarget) |> ignore
                    let unmem_p = x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    br_true.arg <- Target unmem_p
                    x.PrependProbe(unmem2Probe, [(OpCodes.Ldc_I4, Arg32 1)], unmem2Sig, &prependTarget) |> ignore
                    x.PrependPopOpmem &prependTarget |> ignore

                | OpCodeValues.Mkrefany -> x.AppendProbe(probes.mkrefany, [], x.tokens.void_sig, instr) |> ignore
                | OpCodeValues.Newarr ->
                    x.AppendProbeWithOffset(probes.newarr, [], prependTarget.offset, x.tokens.void_i_token_offset_sig, instr) |> ignore
                    x.AppendInstr OpCodes.Ldc_I4 instr.arg instr
                    x.AppendInstr OpCodes.Conv_I NoArg instr
                    x.AppendDup instr
                | OpCodeValues.Localloc ->
                    x.AppendProbeWithOffset(probes.newarr, [], prependTarget.offset, x.tokens.void_i_offset_sig, instr) |> ignore
                    x.AppendDup instr
                | OpCodeValues.Cpobj ->
                    // calli mem2
                    // calli unmem 0
                    // calli unmem 1
                    // calli unmem 0
                    // calli unmem 1
                    // calli track_cpobj
                    // branch_true A
                    // ldc token
                    // calli unmem 0
                    // calli unmem 1
                    // calli exec
                    // A: cpobj
                    x.PrependMem2_p &prependTarget
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.cpobj, [], x.tokens.bool_i_i_sig, &prependTarget) |> ignore
                    let br = x.PrependBranch(OpCodes.Brtrue_S, &prependTarget)
                    x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbeWithOffset(probes.execCpobj, [], x.tokens.void_token_i_i_offset_sig, &prependTarget) |> ignore
                    br.arg <- Target prependTarget
                    x.PrependPopOpmem &prependTarget |> ignore
                | OpCodeValues.Ldobj ->
                    x.PrependDup &prependTarget |> ignore
                    x.PrependProbeWithOffset(probes.ldobj, [], x.tokens.void_i_offset_sig, &prependTarget) |> ignore
                | OpCodeValues.Ldstr ->
                    x.AppendProbe(probes.ldstr, [], x.tokens.void_i_sig, instr) |> ignore
                    x.AppendInstr OpCodes.Conv_I NoArg instr
                    x.AppendInstr OpCodes.Dup NoArg instr
                | OpCodeValues.Castclass ->
                    x.PrependDup &prependTarget |> ignore
                    x.PrependInstr(OpCodes.Conv_I, NoArg, &prependTarget) |> ignore
                    x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget) |> ignore
                    x.PrependProbeWithOffset(probes.castclass, [], x.tokens.void_i_token_offset_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.disableInstrumentation, [], x.tokens.void_sig, &prependTarget) |> ignore
                    x.AppendProbe(probes.enableInstrumentation, [], x.tokens.void_sig, instr) |> ignore
                | OpCodeValues.Isinst ->
                    x.PrependDup &prependTarget |> ignore
                    x.PrependInstr(OpCodes.Conv_I, NoArg, &prependTarget) |> ignore
                    x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget) |> ignore
                    x.PrependProbeWithOffset(probes.isinst, [], x.tokens.void_i_token_offset_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.disableInstrumentation, [], x.tokens.void_sig, &prependTarget) |> ignore
                    x.AppendProbe(probes.enableInstrumentation, [], x.tokens.void_sig, instr) |> ignore
                | OpCodeValues.Unbox ->
                     x.PrependDup &prependTarget |> ignore
                     x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget) |> ignore
                     x.PrependProbeWithOffset(probes.unbox, [], x.tokens.void_i_token_offset_sig, &prependTarget) |> ignore
                     x.PrependProbe(probes.disableInstrumentation, [], x.tokens.void_sig, &prependTarget) |> ignore
                     x.AppendProbe(probes.enableInstrumentation, [], x.tokens.void_sig, instr) |> ignore
                | OpCodeValues.Unbox_Any ->
                     x.PrependDup &prependTarget |> ignore
                     x.PrependInstr(OpCodes.Conv_I, NoArg, &prependTarget) |> ignore
                     x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget) |> ignore
                     x.PrependProbeWithOffset(probes.unboxAny, [], x.tokens.void_i_token_offset_sig, &prependTarget) |> ignore
                     x.PrependProbe(probes.disableInstrumentation, [], x.tokens.void_sig, &prependTarget) |> ignore
                     x.AppendProbe(probes.enableInstrumentation, [], x.tokens.void_sig, instr) |> ignore
                | OpCodeValues.Ldfld ->
                    let isStruct =
                        match instr.stackState with
                        | UnOp evaluationStackCellType.Struct
                        | UnOp evaluationStackCellType.RefLikeStruct -> true
                        | _ -> false
                    let fieldInfo = Reflection.resolveField x.m instr.Arg32
                    // NOTE: getting field offset in runtime
                    if isStruct then
                        // TODO: here we potentially could calculate offsets inside open generic type!
                        //       Should do this at runtime, rather than at instrumentation step
                        let fieldOffset = CSharpUtils.LayoutUtils.GetFieldOffset fieldInfo
                        x.PrependInstr(OpCodes.Ldc_I4, Arg32 fieldOffset, &prependTarget) |> ignore
                    else
                        x.PrependDup(&prependTarget) |> ignore
                        x.PrependInstr(OpCodes.Conv_I, NoArg, &prependTarget) |> ignore
                        x.PrependDup(&prependTarget) |> ignore
                        x.PrependInstr(OpCodes.Ldflda, instr.arg, &prependTarget) |> ignore
                        x.PrependInstr(OpCodes.Conv_I, NoArg, &prependTarget) |> ignore
                    let fieldSizeOpcode, fieldSizeArg = x.TypeSizeInstr fieldInfo.FieldType (fun () -> x.AcceptFieldTypeToken fieldInfo)
                    x.PrependInstr(fieldSizeOpcode, fieldSizeArg, &prependTarget) |> ignore
                    if isStruct then
                        x.PrependProbeWithOffset(probes.ldfld_struct, [], x.tokens.void_i4_i4_offset_sig, &prependTarget) |> ignore
                    else
                        x.PrependProbeWithOffset(probes.ldfld, [], x.tokens.void_i_i_i4_offset_sig, &prependTarget) |> ignore
                | OpCodeValues.Ldflda ->
                    x.PrependDup &prependTarget |> ignore
                    x.PrependInstr(OpCodes.Conv_I, NoArg, &prependTarget) |> ignore
                    x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget) |> ignore
                    x.PrependProbeWithOffset(probes.ldflda, [], x.tokens.void_i_token_offset_sig, &prependTarget) |> ignore
                | OpCodeValues.Stfld ->
                    // box [if struct]
                    // calli mem2
                    // ldc token
                    // calli unmem 0
                    // calli unmem 1
                    // calli track_stfld
                    // calli unmem 0
                    // calli unmem 1
                    // unbox [if struct]
                    // stfld

                    let isStruct, isRefLikeStruct =
                        match instr.stackState with
                        | UnOp evaluationStackCellType.Struct -> true, false
                        | UnOp evaluationStackCellType.RefLikeStruct -> false, true
                        | _ -> false, false
                    let fieldInfo = Reflection.resolveField x.m instr.Arg32
                    let fieldToken = lazy(x.AcceptFieldTypeToken fieldInfo)
                    let fieldSizeOpcode, fieldSizeArg as sizeInstr = x.TypeSizeInstr fieldInfo.FieldType fieldToken.Force

                    if isRefLikeStruct then
                        match instr.stackState with
                        | Some (_ :: (_, src) :: _) ->
                            let offset = prependTarget.offset
                            src |> List.iter (fun srcInstr ->
                                let srcInstr = instructions |> Array.find (fun i -> i.offset = srcInstr.offset)
                                let args = [OpCodes.Dup, NoArg; OpCodes.Conv_I, NoArg]
                                x.AppendProbeWithOffset(probes.mem_refLikeStruct, args, offset, x.tokens.void_i_offset_sig, srcInstr) |> ignore)
                        | _ -> __unreachable__()
                        // TODO: here we potentially could calculate offsets inside open generic type!
                        //       Should do this at runtime, rather than at instrumentation step
                        // TODO: refactor all cases and use ldflda #Dima
                        let fieldOffset = CSharpUtils.LayoutUtils.GetFieldOffset fieldInfo
                        let args = [(OpCodes.Ldc_I4, Arg32 fieldOffset); sizeInstr]
                        x.PrependProbeWithOffset(probes.stfld_refLikeStruct, args, x.tokens.void_i4_i4_offset_sig, &prependTarget) |> ignore
                    else
                        if isStruct then
                            let typeToken = fieldToken.Value
                            x.PrependInstr(OpCodes.Box, Arg32 typeToken, &prependTarget) |> ignore
                        let probe, signature, unmem2Probe, unmem2Sig =
                            match instr.stackState with
                            | BinOp(evaluationStackCellType.I1, evaluationStackCellType.I)
                            | BinOp(evaluationStackCellType.I2, evaluationStackCellType.I)
                            | BinOp(evaluationStackCellType.I4, evaluationStackCellType.I)
                            | BinOp(evaluationStackCellType.I1, evaluationStackCellType.Ref)
                            | BinOp(evaluationStackCellType.I2, evaluationStackCellType.Ref)
                            | BinOp(evaluationStackCellType.I4, evaluationStackCellType.Ref) ->
                                x.PrependMem2_p_4 &prependTarget
                                probes.stfld_4, x.tokens.void_i_i_i4_offset_sig, probes.unmem_4, x.tokens.i4_i1_sig
                            | BinOp(evaluationStackCellType.I8, evaluationStackCellType.I)
                            | BinOp(evaluationStackCellType.I8, evaluationStackCellType.Ref) ->
                                x.PrependMem2_p_8 &prependTarget
                                probes.stfld_8, x.tokens.void_i_i_i8_offset_sig, probes.unmem_8, x.tokens.i8_i1_sig
                            | BinOp(evaluationStackCellType.R4, evaluationStackCellType.I)
                            | BinOp(evaluationStackCellType.R4, evaluationStackCellType.Ref) ->
                                x.PrependMem2_p_f4 &prependTarget
                                probes.stfld_f4, x.tokens.void_i_i_r4_offset_sig, probes.unmem_f4, x.tokens.r4_i1_sig
                            | BinOp(evaluationStackCellType.R8, evaluationStackCellType.I)
                            | BinOp(evaluationStackCellType.R8, evaluationStackCellType.Ref) ->
                                x.PrependMem2_p_f8 &prependTarget
                                probes.stfld_f8, x.tokens.void_i_i_r8_offset_sig, probes.unmem_f8, x.tokens.r8_i1_sig
                            | BinOp(evaluationStackCellType.I, evaluationStackCellType.I)
                            | BinOp(evaluationStackCellType.I, evaluationStackCellType.Ref)
                            | BinOp(evaluationStackCellType.Ref, evaluationStackCellType.I)
                            | BinOp(evaluationStackCellType.Ref, evaluationStackCellType.Ref) ->
                                x.PrependMem2_p &prependTarget
                                probes.stfld_p, x.tokens.void_i_i_i_offset_sig, probes.unmem_p, x.tokens.i_i1_sig
                            | BinOp(evaluationStackCellType.Struct, evaluationStackCellType.I)
                            | BinOp(evaluationStackCellType.Struct, evaluationStackCellType.Ref) ->
                                x.PrependMem2_p &prependTarget
                                probes.stfld_struct, x.tokens.void_i_i4_i_i_offset_sig, probes.unmem_p, x.tokens.i_i1_sig
                            | _ -> __unreachable__()

                        x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                        x.PrependInstr(OpCodes.Ldflda, instr.arg, &prependTarget) |> ignore
                        if isStruct then
                            x.PrependInstr(fieldSizeOpcode, fieldSizeArg, &prependTarget) |> ignore
                        x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                        x.PrependProbe(unmem2Probe, [(OpCodes.Ldc_I4, Arg32 1)], unmem2Sig, &prependTarget) |> ignore
                        x.PrependProbeWithOffset(probe, [], signature, &prependTarget) |> ignore
                        x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                        x.PrependProbe(unmem2Probe, [(OpCodes.Ldc_I4, Arg32 1)], unmem2Sig, &prependTarget) |> ignore
                        if isStruct then
                            let typeToken = fieldToken.Value
                            x.PrependInstr(OpCodes.Unbox_Any, Arg32 typeToken, &prependTarget) |> ignore
                    x.PrependPopOpmem &prependTarget |> ignore
                | OpCodeValues.Ldsfld ->
                    x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget) |> ignore
                    x.PrependProbeWithOffset(probes.ldsfld, [], x.tokens.void_token_offset_sig, &prependTarget) |> ignore
                | OpCodeValues.Ldsflda ->
                    let fieldInfo = Reflection.resolveField x.m instr.Arg32
                    let fieldSizeOpcode, fieldSizeArg = x.TypeSizeInstr fieldInfo.FieldType (fun () -> x.AcceptFieldTypeToken fieldInfo)
                    let id = registerStaticFieldID fieldInfo |> int16
                    x.AppendProbe(probes.ldsflda, [], x.tokens.void_i_i4_i2_sig, instr) |> ignore
                    x.AppendInstr OpCodes.Ldc_I4 (Arg16 id) instr
                    x.AppendInstr fieldSizeOpcode fieldSizeArg instr
                    x.AppendInstr OpCodes.Conv_I NoArg instr
                    x.AppendDup instr
                | OpCodeValues.Stsfld ->
                    x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget) |> ignore
                    x.PrependProbeWithOffset(probes.stsfld, [], x.tokens.void_token_offset_sig, &prependTarget) |> ignore
                | OpCodeValues.Stobj ->
                    // TODO: implement via append dup to sources of operands
                    __notImplemented__() // ?????????????????
                | OpCodeValues.Box ->
                    x.AppendProbeWithOffset(probes.box, [], prependTarget.offset, x.tokens.void_i_offset_sig, instr) |> ignore
                    x.AppendInstr OpCodes.Conv_I NoArg instr
                    x.AppendDup instr
                | OpCodeValues.Ldlen ->
                    x.PrependDup &prependTarget |> ignore
                    x.PrependInstr(OpCodes.Conv_I, NoArg, &prependTarget) |> ignore
                    x.PrependProbeWithOffset(probes.ldlen, [], x.tokens.void_i_offset_sig, &prependTarget) |> ignore
                | OpCodeValues.Ldelema
                | OpCodeValues.Ldelem_I1
                | OpCodeValues.Ldelem_U1
                | OpCodeValues.Ldelem_I2
                | OpCodeValues.Ldelem_U2
                | OpCodeValues.Ldelem_I4
                | OpCodeValues.Ldelem_U4
                | OpCodeValues.Ldelem_I8
                | OpCodeValues.Ldelem_I
                | OpCodeValues.Ldelem_R4
                | OpCodeValues.Ldelem_R8
                | OpCodeValues.Ldelem_Ref
                | OpCodeValues.Ldelem ->
                    let track = if opcodeValue = OpCodeValues.Ldelema then probes.ldelema else probes.ldelem
                    let exec = if opcodeValue = OpCodeValues.Ldelema then probes.execLdelema else probes.execLdelem
                    // calli mem2
                    // calli unmem 0
                    // calli unmem 1
                    // calli unmem 0
                    // calli unmem 1
                    // ldc size of elem
                    // calli track_ldelem(a)
                    // branch_true A
                    // calli unmem 0
                    // calli unmem 1
                    // calli exec
                    // A: ldelem(a)

                    let elemSizeOpcode, elemSizeArg =
                        match opcodeValue with
                        | OpCodeValues.Ldelem_I
                        | OpCodeValues.Ldelem_Ref -> OpCodes.Ldc_I4, Arg32 sizeof<System.IntPtr>
                        | OpCodeValues.Ldelem_I1 -> OpCodes.Ldc_I4, Arg32 sizeof<int8>
                        | OpCodeValues.Ldelem_I2 -> OpCodes.Ldc_I4, Arg32 sizeof<int16>
                        | OpCodeValues.Ldelem_I4 -> OpCodes.Ldc_I4, Arg32 sizeof<int32>
                        | OpCodeValues.Ldelem_I8 -> OpCodes.Ldc_I4, Arg32 sizeof<int64>
                        | OpCodeValues.Ldelem_R4 -> OpCodes.Ldc_I4, Arg32 sizeof<float>
                        | OpCodeValues.Ldelem_R8 -> OpCodes.Ldc_I4, Arg32 sizeof<double>
                        | OpCodeValues.Ldelem
                        | OpCodeValues.Ldelema -> OpCodes.Sizeof, instr.arg
                        | _ -> __unreachable__()

                    x.PrependMem2_p &prependTarget
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependInstr(elemSizeOpcode, elemSizeArg, &prependTarget) |> ignore
                    x.PrependProbe(track, [], x.tokens.bool_i_i_i4_sig, &prependTarget) |> ignore
                    let br = x.PrependBranch(OpCodes.Brtrue_S, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbeWithOffset(exec, [], x.tokens.void_i_i_offset_sig, &prependTarget) |> ignore
                    br.arg <- Target prependTarget

                | OpCodeValues.Stelem_I
                | OpCodeValues.Stelem_I1
                | OpCodeValues.Stelem_I2
                | OpCodeValues.Stelem_I4
                | OpCodeValues.Stelem_I8
                | OpCodeValues.Stelem_R4
                | OpCodeValues.Stelem_R8
                | OpCodeValues.Stelem_Ref
                | OpCodeValues.Stelem ->
                    // TODO: remove unmem before exec, take it from storage!
                    // box [if struct]
                    // calli mem3
                    // calli unmem 0
                    // calli unmem 1
                    // ldc size of elem
                    // calli track_stelem
                    // brtrue A
                    // calli unmem 0
                    // calli unmem 1
                    // calli unmem 2
                    // calli exec
                    // A: calli unmem 0
                    // calli unmem 1
                    // calli unmem 2
                    // unbox [if struct]
                    // stelem

                    let isStruct =
                        match instr.stackState with
                        | UnOp evaluationStackCellType.Struct ->
                            assert(op = OpCodes.Stelem)
                            true
                        | UnOp evaluationStackCellType.RefLikeStruct ->
                            // Ref-like structs can't be stored into arrays
                            __unreachable__()
                        | _ -> false
                    let typeTokenArg = instr.arg

                    if isStruct then
                        x.PrependInstr(OpCodes.Box, typeTokenArg, &prependTarget) |> ignore

                    let execProbe, execSig, unmem3Probe, unmem3Sig, elemSizeOpcode, elemSizeArg =
                        match opcodeValue, instr.stackState with
                        | OpCodeValues.Stelem_I, _
                        | OpCodeValues.Stelem, TernOp(evaluationStackCellType.I, _, evaluationStackCellType.Ref) ->
                            x.PrependMem_p(2, &prependTarget)
                            probes.execStelem_Ref, x.tokens.void_i_i_i_offset_sig, probes.unmem_p, x.tokens.i_i1_sig, OpCodes.Ldc_I4, Arg32 sizeof<System.IntPtr>
                        | OpCodeValues.Stelem_Ref, _
                        | OpCodeValues.Stelem, TernOp(evaluationStackCellType.Ref, _, evaluationStackCellType.Ref) ->
                            x.PrependMem_p(2, &prependTarget)
                            probes.execStelem_Ref, x.tokens.void_i_i_i_offset_sig, probes.unmem_p, x.tokens.i_i1_sig, OpCodes.Ldc_I4, Arg32 sizeof<System.IntPtr>
                        | OpCodeValues.Stelem_I1, _
                        | OpCodeValues.Stelem, TernOp(evaluationStackCellType.I1, _, evaluationStackCellType.Ref) ->
                            x.PrependMem_i1(2, &prependTarget)
                            probes.execStelem_I1, x.tokens.void_i_i_i1_offset_sig, probes.unmem_1, x.tokens.i1_i1_sig, OpCodes.Ldc_I4, Arg32 sizeof<int8>
                        | OpCodeValues.Stelem_I2, _
                        | OpCodeValues.Stelem, TernOp(evaluationStackCellType.I2, _, evaluationStackCellType.Ref) ->
                            x.PrependMem_i2(2, &prependTarget)
                            probes.execStelem_I2, x.tokens.void_i_i_i2_offset_sig, probes.unmem_2, x.tokens.i2_i1_sig, OpCodes.Ldc_I4, Arg32 sizeof<int16>
                        | OpCodeValues.Stelem_I4, _
                        | OpCodeValues.Stelem, TernOp(evaluationStackCellType.I4, _, evaluationStackCellType.Ref) ->
                            x.PrependMem_i4(2, &prependTarget)
                            probes.execStelem_I4, x.tokens.void_i_i_i4_offset_sig, probes.unmem_4, x.tokens.i4_i1_sig, OpCodes.Ldc_I4, Arg32 sizeof<int32>
                        | OpCodeValues.Stelem_I8, _
                        | OpCodeValues.Stelem, TernOp(evaluationStackCellType.I8, _, evaluationStackCellType.Ref) ->
                            x.PrependMem_i8(2, &prependTarget)
                            probes.execStelem_I8, x.tokens.void_i_i_i8_offset_sig, probes.unmem_8, x.tokens.i8_i1_sig, OpCodes.Ldc_I4, Arg32 sizeof<int64>
                        | OpCodeValues.Stelem_R4, _
                        | OpCodeValues.Stelem, TernOp(evaluationStackCellType.R4, _, evaluationStackCellType.Ref) ->
                            x.PrependMem_f4(2, &prependTarget)
                            probes.execStelem_R4, x.tokens.void_i_i_r4_offset_sig, probes.unmem_f4, x.tokens.r4_i1_sig, OpCodes.Ldc_I4, Arg32 sizeof<float>
                        | OpCodeValues.Stelem_R8, _
                        | OpCodeValues.Stelem, TernOp(evaluationStackCellType.R8, _, evaluationStackCellType.Ref) ->
                            x.PrependMem_f8(2, &prependTarget)
                            probes.execStelem_R8, x.tokens.void_i_i_r8_offset_sig, probes.unmem_f8, x.tokens.r8_i1_sig, OpCodes.Ldc_I4, Arg32 sizeof<double>
                        | OpCodeValues.Stelem, TernOp(evaluationStackCellType.Struct, _, evaluationStackCellType.Ref)
                        | OpCodeValues.Stelem, TernOp(evaluationStackCellType.RefLikeStruct, _, evaluationStackCellType.Ref) ->
                            x.PrependMem_p(2, &prependTarget)
                            probes.execStelem_Struct, x.tokens.void_i_i_i_offset_sig, probes.unmem_p, x.tokens.i_i1_sig, OpCodes.Sizeof, instr.arg
                        | _ -> __unreachable__()

                    x.PrependMem_p(1, &prependTarget)
                    x.PrependMem_p(0, &prependTarget)

                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependInstr(elemSizeOpcode, elemSizeArg, &prependTarget) |> ignore
                    x.PrependProbe(probes.stelem, [], x.tokens.bool_i_i_i4_sig, &prependTarget) |> ignore
                    let brtrue = x.PrependBranch(OpCodes.Brtrue_S, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(unmem3Probe, [(OpCodes.Ldc_I4, Arg32 2)], unmem3Sig, &prependTarget) |> ignore
                    x.PrependProbeWithOffset(execProbe, [], execSig, &prependTarget) |> ignore
                    let tgt = x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(unmem3Probe, [(OpCodes.Ldc_I4, Arg32 2)], unmem3Sig, &prependTarget) |> ignore
                    if isStruct then
                        x.PrependInstr(OpCodes.Unbox_Any, typeTokenArg, &prependTarget) |> ignore
                    brtrue.arg <- Target tgt
                    x.PrependPopOpmem &prependTarget |> ignore

                | OpCodeValues.Ckfinite ->  x.AppendProbe(probes.ckfinite, [], x.tokens.void_sig, instr) |> ignore
                | OpCodeValues.Ldvirtftn ->
                     x.PrependDup &prependTarget |> ignore
                     x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget) |> ignore
                     x.PrependProbeWithOffset(probes.ldvirtftn, [], x.tokens.void_i_token_offset_sig, &prependTarget) |> ignore
                | OpCodeValues.Initobj ->
                     x.PrependDup &prependTarget |> ignore
                     x.PrependProbe(probes.initobj, [], x.tokens.void_i_sig, &prependTarget) |> ignore
                | OpCodeValues.Cpblk ->
                    // calli mem3
                    // calli unmem 0
                    // calli unmem 1
                    // calli unmem 2
                    // calli unmem 0
                    // calli unmem 1
                    // calli track_cpblk
                    // branch_true A
                    // calli unmem 0
                    // calli unmem 1
                    // calli unmem 2
                    // calli exec
                    // A: cpblk
                    x.PrependMem3_p &prependTarget
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 2)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.cpblk, [], x.tokens.bool_i_i_sig, &prependTarget) |> ignore
                    let br = x.PrependBranch(OpCodes.Brtrue_S, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 2)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbeWithOffset(probes.execCpblk, [], x.tokens.void_i_i_i_offset_sig, &prependTarget) |> ignore
                    br.arg <- Target prependTarget
                    x.PrependPopOpmem &prependTarget |> ignore
                | OpCodeValues.Initblk ->
                    // calli mem3
                    // calli unmem 0
                    // calli unmem 1
                    // calli unmem 2
                    // calli unmem 0
                    // calli track_initblk
                    // branch_true A
                    // calli unmem 0
                    // calli unmem 1
                    // calli unmem 2
                    // calli exec
                    // A: initblk
                    x.PrependMem3_p_i1_p &prependTarget
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_1, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i1_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 2)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.initblk, [], x.tokens.bool_i_sig, &prependTarget) |> ignore
                    let br = x.PrependBranch(OpCodes.Brtrue_S, &prependTarget)
                    x.PrependProbe(probes.unmem_1, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i1_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_1, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i1_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 2)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbeWithOffset(probes.execInitblk, [], x.tokens.void_i_i1_i_offset_sig, &prependTarget) |> ignore
                    br.arg <- Target prependTarget
                    x.PrependPopOpmem &prependTarget |> ignore

                | OpCodeValues.Rethrow ->
                    atLeastOneReturnFound <- true
                    x.PrependProbeWithOffset(probes.rethrow, [], x.tokens.void_offset_sig, &prependTarget) |> ignore

                | OpCodeValues.Call
                | OpCodeValues.Callvirt
                | OpCodeValues.Newobj ->
                    // ldc argsCount
                    // calli track_call
                    // if (callee is modeled internal call || callee is not executable) { mem args }
                    // elif (callee has this) { mem this }
                    // if (callee is executable) {
                    //     brtrue A
                    //     calli exec
                    //     if (callee is modeled internal call) { br B }
                    //     A: if (callee is modeled internal call) { calli pushInternalCallResult }
                    //     B: if (NOT callee is modeled internal call) {
                    //           if (call opcode is newobj of value type) { calli pushTemporaryAllocatedStruct }
                    //           calli pushFrame
                    //     }
                    //     if (callee is modeled internal call) { calli disableInstrumentation }
                    //     call callee
                    //     if (callee is modeled internal call) { calli enableInstrumentation }
                    //     calli finalizeCall
                    //     if call opcode is newobj {
                    //         dup
                    //         conv.i
                    //         calli newobj_probe
                    //     }
                    // } else {
                    //    if callee is modeled internal call {
                    //        pop args
                    //        ldc default return value
                    //        mem default value
                    //        calli exec
                    //        unmem default value
                    //    } else {
                    //        call detour
                    //    }
                    // }

                    match instr.arg with
                    | Arg32 token ->
                        // TODO: if method is F# internal call, instr.arg <- token of static ctor of type #do
                        // TODO: if method is C# internal call, instr.arg <- token of C# implementation #do
                        let callee = Reflection.resolveMethod x.m token
                        let isNewObj = opcodeValue = OpCodeValues.Newobj

                        if isNewObj && not callee.DeclaringType.IsValueType then
                            x.AppendProbe(probes.newobj, [], x.tokens.void_i_sig, instr) |> ignore
                            x.AppendInstr OpCodes.Conv_I NoArg instr
                            x.AppendDup instr
                        let returnValues = if Reflection.hasNonVoidResult callee then 1 else 0
                        let finalize = x.AppendProbe(probes.finalizeCall, [(OpCodes.Ldc_I4, Arg32 returnValues)], x.tokens.void_u1_sig, instr)

                        let parameters = callee.GetParameters()
                        let hasThis = Reflection.hasThis callee && not isNewObj
                        let argsCount = parameters.Length
                        let argsCount = if hasThis then argsCount + 1 else argsCount
                        let isModeledInternalCall = InstructionsSet.isFSharpInternalCall callee // TODO: add other cases
                        let isExecutableMethod = Loader.isExecutable callee
                        x.PrependProbe(probes.call, [(OpCodes.Ldc_I4, Arg32 argsCount)], x.tokens.bool_u2_sig, &prependTarget) |> ignore

                        if isModeledInternalCall || not isExecutableMethod then
                            match instr.stackState with
                            | Some list ->
                                let types = List.take argsCount list |> Array.ofList
                                let opmemOffset = int prependTarget.offset
                                for i = 0 to argsCount - 1 do
                                    let t, src = types.[i]
                                    src |> List.iter (fun srcInstr ->
                                        let srcInstr = instructions |> Array.find (fun i -> i.offset = srcInstr.offset)
                                        x.AppendMemForType(t, argsCount - i - 1, opmemOffset, srcInstr)
                                        x.AppendInstr OpCodes.Dup NoArg srcInstr)
                            | None -> __unreachable__()
                            elif hasThis then
                                match instr.stackState with
                                | Some list ->
                                    let opmemOffset = int prependTarget.offset
                                    let t, src = List.item (argsCount - 1) list
                                    src |> List.iter (fun srcInstr ->
                                        let srcInstr = instructions |> Array.find (fun i -> i.offset = srcInstr.offset)
                                        x.AppendMemForType(t, 0, opmemOffset, srcInstr)
                                        x.AppendInstr OpCodes.Dup NoArg srcInstr)
                                | None -> __unreachable__()

                        if isExecutableMethod then
                            let br_push = x.PrependBranch(OpCodes.Brtrue_S, &prependTarget)
                            let br_call =
                                if isModeledInternalCall then
                                    x.PrependProbeWithOffset(probes.execInternalCall, [(OpCodes.Ldc_I4, Arg32 argsCount)], x.tokens.void_i4_offset_sig, &prependTarget) |> ignore
                                    x.PrependBranch(OpCodes.Br, &prependTarget) |> Some
                                elif hasThis then
                                    x.PrependProbeWithOffset(probes.execThisCall, [(OpCodes.Ldc_I4, Arg32 argsCount)], x.tokens.void_i4_offset_sig, &prependTarget) |> ignore
                                    None
                                else
                                    x.PrependProbeWithOffset(probes.execCall, [(OpCodes.Ldc_I4, Arg32 argsCount)], x.tokens.void_i4_offset_sig, &prependTarget) |> ignore
                                    None

                            let pushStart = x.PrependNop &prependTarget
                            br_push.arg <- Target pushStart
                            if isModeledInternalCall && (returnValues > 0 || isNewObj) then
                                x.PrependProbe(probes.pushInternalCallResult, [], x.tokens.void_sig, &prependTarget) |> ignore

                            let callStart = x.PrependNop &prependTarget
                            br_call |> Option.iter (fun instr -> instr.arg <- Target callStart)
                            if not isModeledInternalCall then
                                if isNewObj && callee.DeclaringType.IsValueType then
                                    let allocatedType = callee.DeclaringType
                                    let sizeInstr = x.TypeSizeInstr allocatedType (fun () -> x.AcceptDeclaringTypeToken callee token)
                                    x.PrependProbeWithOffset(probes.pushTemporaryAllocatedStruct, [sizeInstr], x.tokens.void_size_offset_sig, &prependTarget) |> ignore

                                let expectedToken = if opcodeValue = OpCodeValues.Callvirt then 0 else callee.MetadataToken
                                let pushFrameArgs = [(OpCodes.Ldc_I4, Arg32 token)
                                                     (OpCodes.Ldc_I4, Arg32 expectedToken)
                                                     (OpCodes.Ldc_I4, Arg32 (if isNewObj then 1 else 0))
                                                     (OpCodes.Ldc_I4, Arg32 argsCount)]
                                x.PrependProbeWithOffset(probes.pushFrame, pushFrameArgs, x.tokens.void_token_token_bool_u2_offset_sig, &prependTarget) |> ignore
                            if isModeledInternalCall && argsCount > 0 || hasThis then
                                x.PrependPopOpmem &prependTarget |> ignore

                            if isModeledInternalCall then
                                x.PrependProbe(probes.disableInstrumentation, [], x.tokens.void_sig, &prependTarget) |> ignore
                                x.AppendProbe(probes.enableInstrumentation, [], x.tokens.void_sig, instr) |> ignore
                        else
                            // TODO: see listing above
                            __notImplemented__()
//                            if isModeledInternalCall then
//                                for i = 1 to argsCount do
//                                    x.PrependInstr(OpCodes.Pop, NoArg, &prependTarget)
//                                // TODO: if internal call mod el raised exception, raise it in concolic
//                                if retType <> typeof<System.Void> then
//                                    x.PrependLdcDefault(retType, &instr)
//                                    let probe, token = x.PrependMemUnmemForType(EvaluationStackTyper.abstractType retType, argsCount, &prependTarget)
//                                    x.PrependProbeWithOffset(probes.execInternalCall, [(OpCodes.Ldc_I4, Arg32 argsCount)], x.tokens.void_i4_offset_sig, &prependTarget) |> ignore
//                                    x.PrependProbe(probe, [(OpCodes.Ldc_I4, Arg32 argsCount)], token, &prependTarget) |> ignore
//                                else
//                                    x.PrependProbeWithOffset(probes.execInternalCall, [(OpCodes.Ldc_I4, Arg32 argsCount)], x.tokens.void_i4_offset_sig, &prependTarget) |> ignore
//                                if argsCount > 0 then
//                                    x.PrependPopOpmem &prependTarget |> ignore
                        finalize.offset <- instr.offset
                        instructions.[i] <- finalize
                    | _ -> __unreachable__()
                | OpCodeValues.Calli -> __notImplemented__()

                | OpCodeValues.Ret ->
                    assert (not hasPrefix)
                    atLeastOneReturnFound <- true
                    x.PlaceLeaveProbe &instr
                | OpCodeValues.Throw ->
                    x.PrependProbeWithOffset(probes.throw, [], x.tokens.void_offset_sig, &prependTarget) |> ignore
                    atLeastOneReturnFound <- true

                // Ignored instructions
                | OpCodeValues.Nop
                | OpCodeValues.Break
                | OpCodeValues.Jmp
                | OpCodeValues.Refanyval
                | OpCodeValues.Refanytype
                | OpCodeValues.Endfinally
                | OpCodeValues.Br_S
                | OpCodeValues.Br
                | OpCodeValues.Leave
                | OpCodeValues.Leave_S
                | OpCodeValues.Endfilter -> ()
                | _ -> __unreachable__()

                if hasPrefix && op.OpCodeType <> OpCodeType.Prefix then
                    hasPrefix <- false
                instructions.[i] <- instr
            | SwitchArg -> ()
        assert atLeastOneReturnFound

    member x.Skip (body : rawMethodBody) =
        { properties = {ilCodeSize = body.properties.ilCodeSize; maxStackSize = body.properties.maxStackSize}; il = body.il; ehs = body.ehs}

    member private x.NeedToSkip = Array.empty

    member x.Instrument(body : rawMethodBody) =
        assert(x.rewriter = null)
        x.tokens <- body.tokens
        x.rewriter <- ILRewriter(body)
        x.m <- x.rewriter.Method
        let t = x.m.DeclaringType
        if t = typeof<System.InvalidProgramException> || t = typeof<System.TypeLoadException> then
            internalfailf "Incorrect instrumentation: exception %O is thrown!" t
        let shouldInstrument = Array.contains (Reflection.methodToString x.m) x.NeedToSkip |> not
        let result =
            if shouldInstrument && Instrumenter.instrumentedFunctions.Add x.m then
                Logger.trace "Instrumenting %s (token = %X)" (Reflection.methodToString x.m) body.properties.token
                x.rewriter.Import()
                x.rewriter.PrintInstructions "before instrumentation" probes
                x.PlaceProbes()
                x.rewriter.PrintInstructions "after instrumentation" probes
                let result = x.rewriter.Export()
                result
            else
                Logger.trace "Duplicate JITting of %s" x.MethodName
                x.Skip body

        x.rewriter <- null
        result
