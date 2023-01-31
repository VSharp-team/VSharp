namespace VSharp.Concolic

open VSharp
open System.Reflection
open System.Reflection.Emit
open System.Collections.Generic
open VSharp.Interpreter.IL

type Instrumenter(communicator : Communicator, entryPoint : MethodBase, probes : probes) =
    // TODO: should we consider executed assembly build options here?
    let ldc_i : opcode = (if System.Environment.Is64BitOperatingSystem then OpCodes.Ldc_I8 else OpCodes.Ldc_I4) |> VSharp.OpCode
    static member private instrumentedFunctions = HashSet<MethodBase>()
    [<DefaultValue>] val mutable tokens : signatureTokens
    [<DefaultValue>] val mutable rewriter : ILRewriter
    [<DefaultValue>] val mutable m : MethodBase

    member private x.MkCalli(instr : ilInstr byref, signature : uint32) =
        instr <- x.rewriter.NewInstr OpCodes.Calli
        instr.arg <- Arg32 (int32 signature)

    member private x.PrependInstr(opcode, arg, beforeInstr : ilInstr byref) =
        let mutable newInstr = x.rewriter.CopyInstruction(beforeInstr)
        x.rewriter.InsertAfter(beforeInstr, newInstr)
        swap &newInstr &beforeInstr
        newInstr.opcode <- VSharp.OpCode opcode
        newInstr.arg <- arg

    member private x.PrependNop(beforeInstr : ilInstr byref) =
        let mutable newInstr = x.rewriter.CopyInstruction(beforeInstr)
        x.rewriter.InsertAfter(beforeInstr, newInstr)
        swap &newInstr &beforeInstr
        newInstr.opcode <- VSharp.OpCode OpCodes.Nop
        newInstr.arg <- NoArg
        newInstr

    member private x.PrependBranch(opcode, beforeInstr : ilInstr byref) =
        let mutable newInstr = x.rewriter.CopyInstruction(beforeInstr)
        x.rewriter.InsertAfter(beforeInstr, newInstr)
        swap &newInstr &beforeInstr
        newInstr.opcode <- VSharp.OpCode opcode
        newInstr.arg <- NoArg // In chain of prepends, the address of instruction constantly changes. Deferring it.
        newInstr

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
            newInstr.opcode <- opcode |> VSharp.OpCode
            newInstr.arg <- arg
            for (opcode, arg) in tail do
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
        x.PrependProbe(methodAddress, List.append args [(OpCodes.Ldc_I4, beforeInstr.offset |> int32 |> Arg32)], signature, &beforeInstr) // TODO: offset may be wrong?! #do

    member private x.AppendProbe(methodAddress : uint64, args : (OpCode * ilInstrOperand) list, signature, afterInstr : ilInstr) =
        let mutable newInstr = afterInstr
        x.MkCalli(&newInstr, signature)
        x.rewriter.InsertAfter(afterInstr, newInstr)

        let newInstr = x.rewriter.NewInstr ldc_i
        newInstr.arg <- Arg64 (int64 methodAddress)
        x.rewriter.InsertAfter(afterInstr, newInstr)

        for (opcode, arg) in List.rev args do
            let newInstr = x.rewriter.NewInstr opcode
            newInstr.arg <- arg
            x.rewriter.InsertAfter(afterInstr, newInstr)

    // NOTE: offset is needed for sending concrete information from concolic to SILI
    member private x.AppendProbeWithOffset(methodAddress : uint64, args : (OpCode * ilInstrOperand) list, signature, afterInstr : ilInstr) =
        x.AppendProbe(methodAddress, List.append args [(OpCodes.Ldc_I4, afterInstr.offset |> int32 |> Arg32)], signature, afterInstr)

    member private x.AppendProbeWithOffsetMemUnmem(methodAddress : uint64, args : (OpCode * ilInstrOperand) list, signature, prependTarget : ilInstr byref, afterInstr : ilInstr) =
        // NOTE: for interaction with SILI mem, unmem are needed
        x.AppendProbe(methodAddress, List.append args [(OpCodes.Ldc_I4, afterInstr.offset |> int32 |> Arg32)], signature, afterInstr)
        let t = x.TypeOfFirstStackElement afterInstr
        let probe, token = x.PrependMemUnmemForType(t, 0, 0, &prependTarget)
        x.PrependProbe(probe, [(OpCodes.Ldc_I4, Arg32 0)], token, &prependTarget) |> ignore

    member private x.PlaceEnterProbe (firstInstr : ilInstr byref) =
        let localsCount =
            match x.m.GetMethodBody() with
            | null -> 0
            | mb -> mb.LocalVariables.Count
        let argsCount = x.m.GetParameters().Length
        let argsCount = if Reflection.hasThis x.m then argsCount + 1 else argsCount
        if x.m = entryPoint then
            let args = [(OpCodes.Ldc_I4, Arg32 x.m.MetadataToken)
                        (OpCodes.Ldc_I4, Arg32 argsCount)
//                        (OpCodes.Ldc_I4, Arg32 1) // Arguments of entry point are concrete
                        (OpCodes.Ldc_I4, Arg32 0) // Arguments of entry point are symbolic
                        (OpCodes.Ldc_I4, x.rewriter.MaxStackSize |> int32 |> Arg32)
                        (OpCodes.Ldc_I4, Arg32 localsCount)]
            x.PrependProbe(probes.enterMain, args, x.tokens.void_token_u2_bool_u4_u4_sig, &firstInstr)
        else
            let args = [(OpCodes.Ldc_I4, Arg32 x.m.MetadataToken)
                        (OpCodes.Ldc_I4, x.rewriter.MaxStackSize |> int32 |> Arg32)
                        (OpCodes.Ldc_I4, Arg32 argsCount)
                        (OpCodes.Ldc_I4, Arg32 localsCount)]
            x.PrependProbe(probes.enter, args, x.tokens.void_token_u4_u4_u4_sig, &firstInstr)

    member private x.PrependMem_p(idx, order, instr : ilInstr byref) =
        x.PrependInstr(OpCodes.Conv_I, NoArg, &instr)
        x.PrependProbe(probes.mem_p_idx, [(OpCodes.Ldc_I4, Arg32 idx); (OpCodes.Ldc_I4, Arg32 order)], x.tokens.void_i_i1_i1_sig, &instr) |> ignore

    member private x.PrependMem_i1(idx, order, instr : ilInstr byref) =
        x.PrependProbe(probes.mem_1_idx, [(OpCodes.Ldc_I4, Arg32 idx); (OpCodes.Ldc_I4, Arg32 order)], x.tokens.void_i1_i1_i1_sig, &instr) |> ignore

    member private x.PrependMem_i2(idx, order, instr : ilInstr byref) =
        x.PrependProbe(probes.mem_2_idx, [(OpCodes.Ldc_I4, Arg32 idx); (OpCodes.Ldc_I4, Arg32 order)], x.tokens.void_i2_i1_i1_sig, &instr) |> ignore

    member private x.PrependMem_i4(idx, order, instr : ilInstr byref) =
        x.PrependProbe(probes.mem_4_idx, [(OpCodes.Ldc_I4, Arg32 idx); (OpCodes.Ldc_I4, Arg32 order)], x.tokens.void_i4_i1_i1_sig, &instr) |> ignore

    member private x.PrependMem_i8(idx, order, instr : ilInstr byref) =
        x.PrependProbe(probes.mem_8_idx, [(OpCodes.Ldc_I4, Arg32 idx); (OpCodes.Ldc_I4, Arg32 order)], x.tokens.void_i8_i1_i1_sig, &instr) |> ignore

    member private x.PrependMem_f4(idx, order, instr : ilInstr byref) =
        x.PrependProbe(probes.mem_f4_idx, [(OpCodes.Ldc_I4, Arg32 idx); (OpCodes.Ldc_I4, Arg32 order)], x.tokens.void_r4_i1_i1_sig, &instr) |> ignore

    member private x.PrependMem_f8(idx, order, instr : ilInstr byref) =
        x.PrependProbe(probes.mem_f8_idx, [(OpCodes.Ldc_I4, Arg32 idx); (OpCodes.Ldc_I4, Arg32 order)], x.tokens.void_r8_i1_i1_sig, &instr) |> ignore

    member private x.PrependMem2_p (instr : ilInstr byref) =
        x.PrependMem_p(1, 0, &instr)
        x.PrependMem_p(0, 1, &instr)

    member private x.PrependMem2_p_1 (instr : ilInstr byref) =
        x.PrependMem_i1(1, 0, &instr)
        x.PrependMem_p(0, 1, &instr)

    member private x.PrependMem2_p_2 (instr : ilInstr byref) =
        x.PrependMem_i2(1, 0, &instr)
        x.PrependMem_p(0, 1, &instr)

    member private x.PrependMem2_p_4 (instr : ilInstr byref) =
        x.PrependMem_i4(1, 0, &instr)
        x.PrependMem_p(0, 1, &instr)

    member private x.PrependMem2_p_8 (instr : ilInstr byref) =
        x.PrependMem_i8(1, 0, &instr)
        x.PrependMem_p(0, 1, &instr)

    member private x.PrependMem2_p_f4 (instr : ilInstr byref) =
        x.PrependMem_f4(1, 0, &instr)
        x.PrependMem_p(0, 1, &instr)

    member private x.PrependMem2_p_f8 (instr : ilInstr byref) =
        x.PrependMem_f8(1, 0, &instr)
        x.PrependMem_p(0, 1, &instr)

    member private x.PrependMem2_4_p (instr : ilInstr byref) =
        x.PrependMem_p(1, 0, &instr)
        x.PrependMem_i4(0, 1, &instr)

    member private x.PrependMem3_p (instr : ilInstr byref) =
        x.PrependMem_p(2, 0, &instr)
        x.PrependMem_p(1, 1, &instr)
        x.PrependMem_p(0, 2, &instr)

    member private x.PrependMem3_p_i1_p (instr : ilInstr byref) =
        x.PrependMem_p(2, 0, &instr)
        x.PrependMem_i1(1, 1, &instr)
        x.PrependMem_p(0, 2, &instr)

    member private x.PrependMem3_p_p_i1 (instr : ilInstr byref) =
        x.PrependMem_i1(2, 0, &instr)
        x.PrependMem_p(1, 1, &instr)
        x.PrependMem_p(0, 2, &instr)

    member private x.PrependMem3_p_p_i2 (instr : ilInstr byref) =
        x.PrependMem_i2(2, 0, &instr)
        x.PrependMem_p(1, 1, &instr)
        x.PrependMem_p(0, 2, &instr)

    member private x.PrependValidLeaveMain(instr : ilInstr byref) =
        match instr.stackState with
        | _ when Reflection.hasNonVoidResult x.m |> not ->
            x.PrependProbeWithOffset(probes.leaveMain_0, [], x.tokens.void_offset_sig, &instr) |> ignore
        | Some (evaluationStackCellType.I1 :: _)
        | Some (evaluationStackCellType.I2 :: _)
        | Some (evaluationStackCellType.I4 :: _) ->
            x.PrependMem_i4(0, 0, &instr)
            x.PrependProbe(probes.unmem_4, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i4_i1_sig, &instr) |> ignore
            x.PrependProbeWithOffset(probes.leaveMain_4, [], x.tokens.void_i4_offset_sig, &instr) |> ignore
            x.PrependProbe(probes.unmem_4, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i4_i1_sig, &instr) |> ignore
        | Some (evaluationStackCellType.I8 :: _) ->
            x.PrependMem_i8(0, 0, &instr)
            x.PrependProbe(probes.unmem_8, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i8_i1_sig, &instr) |> ignore
            x.PrependProbeWithOffset(probes.leaveMain_8, [], x.tokens.void_i8_offset_sig, &instr) |> ignore
            x.PrependProbe(probes.unmem_8, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i8_i1_sig, &instr) |> ignore
        | Some (evaluationStackCellType.R4 :: _) ->
            x.PrependMem_f4(0, 0, &instr)
            x.PrependProbe(probes.unmem_f4, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.r4_i1_sig, &instr) |> ignore
            x.PrependProbeWithOffset(probes.leaveMain_f4, [], x.tokens.void_r4_offset_sig, &instr) |> ignore
            x.PrependProbe(probes.unmem_f4, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.r4_i1_sig, &instr) |> ignore
        | Some (evaluationStackCellType.R8 :: _) ->
            x.PrependMem_f8(0, 0, &instr)
            x.PrependProbe(probes.unmem_f8, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.r8_i1_sig, &instr) |> ignore
            x.PrependProbeWithOffset(probes.leaveMain_f8, [], x.tokens.void_r8_offset_sig, &instr) |> ignore
            x.PrependProbe(probes.unmem_f8, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.r8_i1_sig, &instr) |> ignore
        | Some (evaluationStackCellType.I :: _) ->
            x.PrependMem_p(0, 0, &instr)
            x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &instr) |> ignore
            x.PrependProbeWithOffset(probes.leaveMain_p, [], x.tokens.void_i_offset_sig, &instr) |> ignore
            x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &instr) |> ignore
        | Some (evaluationStackCellType.Ref :: _) ->
            x.PrependMem_p(0, 0, &instr)
            x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &instr) |> ignore
            x.PrependProbeWithOffset(probes.leaveMain_p, [], x.tokens.void_i_offset_sig, &instr) |> ignore
            x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &instr) |> ignore
        | Some (evaluationStackCellType.Struct :: _) ->
            x.PrependMem_p(0, 0, &instr)
            x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &instr) |> ignore
            x.PrependProbeWithOffset(probes.leaveMain_p, [], x.tokens.void_i_offset_sig, &instr) |> ignore
            x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &instr) |> ignore
        | _ -> internalfailf "PrependValidLeaveMain: unexpected stack state! %O" instr.stackState

    member private x.PlaceLeaveProbe(instr : ilInstr byref) =
        if x.m = entryPoint then
            x.PrependValidLeaveMain(&instr)
        else
            let returnsSomething = Reflection.hasNonVoidResult x.m
            let args = [(OpCodes.Ldc_I4, (if returnsSomething then 1 else 0) |> Arg32)]
            x.PrependProbeWithOffset(probes.leave, args, x.tokens.void_u1_offset_sig, &instr) |> ignore

    member x.MethodName with get() = x.m.Name

    member private x.PrependLdcDefault(t : System.Type, instr : ilInstr byref) =
        match t with
        | _ when not t.IsValueType -> x.PrependInstr(OpCodes.Ldnull, NoArg, &instr)
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
        | _ when t = typeof<bool> -> x.PrependInstr(OpCodes.Ldc_I4_0, NoArg, &instr)
        | _ -> __unreachable__()

    member private x.TypeOfFirstStackElement (instr : ilInstr) =
        match instr.stackState with
        | Some(typ :: _) -> typ
        | stack -> internalfailf "TypeOfFirstStackElement: unable to take element type, stack = %O" stack

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

    member private x.PrependMemUnmemForType(t : evaluationStackCellType, idx, order, instr : ilInstr byref) =
        match t with
        | evaluationStackCellType.I1 ->
            x.PrependMem_i1(idx, order, &instr)
            probes.unmem_1, x.tokens.i1_i1_sig
        | evaluationStackCellType.I2 ->
            x.PrependMem_i2(idx, order, &instr)
            probes.unmem_2, x.tokens.i2_i1_sig
        | evaluationStackCellType.I4 ->
            x.PrependMem_i4(idx, order, &instr)
            probes.unmem_4, x.tokens.i4_i1_sig
        | evaluationStackCellType.I8 ->
            x.PrependMem_i8(idx, order, &instr)
            probes.unmem_8, x.tokens.i8_i1_sig
        | evaluationStackCellType.R4 ->
            x.PrependMem_f4(idx, order, &instr)
            probes.unmem_f4, x.tokens.r4_i1_sig
        | evaluationStackCellType.R8 ->
            x.PrependMem_f8(idx, order, &instr)
            probes.unmem_f8, x.tokens.r8_i1_sig
        | evaluationStackCellType.I ->
            x.PrependMem_p(idx, order, &instr)
            probes.unmem_p, x.tokens.i_i1_sig
        | evaluationStackCellType.Ref ->
            x.PrependMem_p(idx, order, &instr)
            probes.unmem_p, x.tokens.i_i1_sig
        | evaluationStackCellType.Struct ->
            // TODO: support struct
//            x.PrependInstr(OpCodes.Box, NoArg, &instr)
            x.PrependMem_p(idx, order, &instr)
            probes.unmem_p, x.tokens.i_i1_sig
        | _ -> __unreachable__()

    member x.PlaceProbes() =
        let instructions = x.rewriter.CopyInstructions()
        assert(not <| Array.isEmpty instructions)
        let mutable atLeastOneReturnFound = false
        let mutable hasPrefix = false
        let mutable prefix : ilInstr byref = &instructions.[0]
        x.PlaceEnterProbe(&instructions.[0]) |> ignore
        for i in 0 .. instructions.Length - 1 do
            let instr = &instructions.[i]
            if not hasPrefix then prefix <- instr
            match instr.opcode with
            | OpCode op ->
                let prependTarget = if hasPrefix then &prefix else &instr
                let dumpedInfo = x.rewriter.ILInstrToString probes instr
                let idx = communicator.SendStringAndReadItsIndex dumpedInfo
                x.PrependProbe(probes.dumpInstruction, [OpCodes.Ldc_I4, idx |> int |> Arg32], x.tokens.void_u4_sig, &prependTarget) |> ignore
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
                    x.AppendProbe(probes.ldarga, [(OpCodes.Ldc_I4, instr.Arg8 |> int |> Arg32)], x.tokens.void_i_u2_sig, instr)
                    x.AppendDup instr
                | OpCodeValues.Ldloca_S ->
                    x.AppendProbe(probes.ldloca, [(OpCodes.Ldc_I4, instr.Arg8 |> int |> Arg32)], x.tokens.void_i_u2_sig, instr)
                    x.AppendDup instr
                | OpCodeValues.Ldarga ->
                    x.AppendProbe(probes.ldarga, [(OpCodes.Ldc_I4, instr.Arg16 |> int |> Arg32)], x.tokens.void_i_u2_sig, instr)
                    x.AppendDup instr
                | OpCodeValues.Ldloca ->
                    x.AppendProbe(probes.ldloca, [(OpCodes.Ldc_I4, instr.Arg16 |> int |> Arg32)], x.tokens.void_i_u2_sig, instr)
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
                | OpCodeValues.Ldc_R8 -> x.AppendProbe(probes.ldc, [], x.tokens.void_sig, instr)
                | OpCodeValues.Pop -> x.AppendProbe(probes.pop, [], x.tokens.void_sig, instr)
                | OpCodeValues.Ldtoken -> x.AppendProbe(probes.ldtoken, [], x.tokens.void_sig, instr)
                | OpCodeValues.Arglist -> x.AppendProbe(probes.arglist, [], x.tokens.void_sig, instr)
                | OpCodeValues.Ldftn -> x.AppendProbe(probes.ldftn, [], x.tokens.void_sig, instr)
                | OpCodeValues.Sizeof -> x.AppendProbe(probes.sizeof, [], x.tokens.void_sig, instr)

                // Branchings
                | OpCodeValues.Brfalse_S
                | OpCodeValues.Brfalse -> x.PrependProbeWithOffset(probes.brfalse, [], x.tokens.void_offset_sig, &prependTarget) |> ignore
                | OpCodeValues.Brtrue_S
                | OpCodeValues.Brtrue -> x.PrependProbeWithOffset(probes.brtrue, [], x.tokens.void_offset_sig, &prependTarget) |> ignore
                | OpCodeValues.Switch -> x.PrependProbeWithOffset(probes.switch, [], x.tokens.void_offset_sig, &prependTarget) |> ignore

                // Symbolic stack instructions
                | OpCodeValues.Ldarg_0 -> x.AppendProbeWithOffset(probes.ldarg_0, [], x.tokens.void_offset_sig, instr)
                | OpCodeValues.Ldarg_1 -> x.AppendProbeWithOffset(probes.ldarg_1, [], x.tokens.void_offset_sig, instr)
                | OpCodeValues.Ldarg_2 -> x.AppendProbeWithOffset(probes.ldarg_2, [], x.tokens.void_offset_sig, instr)
                | OpCodeValues.Ldarg_3 -> x.AppendProbeWithOffset(probes.ldarg_3, [], x.tokens.void_offset_sig, instr)
                | OpCodeValues.Ldloc_0 -> x.AppendProbeWithOffset(probes.ldloc_0, [], x.tokens.void_offset_sig, instr)
                | OpCodeValues.Ldloc_1 -> x.AppendProbeWithOffset(probes.ldloc_1, [], x.tokens.void_offset_sig, instr)
                | OpCodeValues.Ldloc_2 -> x.AppendProbeWithOffset(probes.ldloc_2, [], x.tokens.void_offset_sig, instr)
                | OpCodeValues.Ldloc_3 -> x.AppendProbeWithOffset(probes.ldloc_3, [], x.tokens.void_offset_sig, instr)
                | OpCodeValues.Stloc_0 -> x.AppendProbeWithOffsetMemUnmem(probes.stloc_0, [], x.tokens.void_offset_sig, &prependTarget, instr)
                | OpCodeValues.Stloc_1 -> x.AppendProbeWithOffsetMemUnmem(probes.stloc_1, [], x.tokens.void_offset_sig, &prependTarget, instr)
                | OpCodeValues.Stloc_2 -> x.AppendProbeWithOffsetMemUnmem(probes.stloc_2, [], x.tokens.void_offset_sig, &prependTarget, instr)
                | OpCodeValues.Stloc_3 -> x.AppendProbeWithOffsetMemUnmem(probes.stloc_3, [], x.tokens.void_offset_sig, &prependTarget, instr)
                | OpCodeValues.Ldarg_S -> x.AppendProbeWithOffset(probes.ldarg_S, [(OpCodes.Ldc_I4, instr.Arg8 |> int |> Arg32)], x.tokens.void_u1_offset_sig, instr)
                | OpCodeValues.Starg_S -> x.AppendProbeWithOffsetMemUnmem(probes.starg_S, [(OpCodes.Ldc_I4, instr.Arg8 |> int |> Arg32)], x.tokens.void_u1_offset_sig, &prependTarget, instr)
                | OpCodeValues.Ldloc_S -> x.AppendProbeWithOffset(probes.ldloc_S, [(OpCodes.Ldc_I4, instr.Arg8 |> int |> Arg32)], x.tokens.void_u1_offset_sig, instr)
                | OpCodeValues.Stloc_S -> x.AppendProbeWithOffsetMemUnmem(probes.stloc_S, [(OpCodes.Ldc_I4, instr.Arg8 |> int |> Arg32)], x.tokens.void_u1_offset_sig, &prependTarget, instr)
                | OpCodeValues.Ldarg -> x.AppendProbeWithOffset(probes.ldarg, [(OpCodes.Ldc_I4, instr.Arg16 |> int |> Arg32)], x.tokens.void_u2_offset_sig, instr)
                | OpCodeValues.Starg -> x.AppendProbeWithOffsetMemUnmem(probes.starg, [(OpCodes.Ldc_I4, instr.Arg16 |> int |> Arg32)], x.tokens.void_u2_offset_sig, &prependTarget, instr)
                | OpCodeValues.Ldloc -> x.AppendProbeWithOffset(probes.ldloc, [(OpCodes.Ldc_I4, instr.Arg16 |> int |> Arg32)], x.tokens.void_u2_offset_sig, instr)
                | OpCodeValues.Stloc -> x.AppendProbeWithOffsetMemUnmem(probes.stloc, [(OpCodes.Ldc_I4, instr.Arg16 |> int |> Arg32)], x.tokens.void_u2_offset_sig, &prependTarget, instr)
                | OpCodeValues.Dup -> x.AppendProbeWithOffsetMemUnmem(probes.dup, [], x.tokens.void_offset_sig, &prependTarget, instr)

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
                        | Some (evaluationStackCellType.I4 :: evaluationStackCellType.I4 :: _)
                        | Some (evaluationStackCellType.I1 :: evaluationStackCellType.I1 :: _)
                        | Some (evaluationStackCellType.I1 :: evaluationStackCellType.I2 :: _)
                        | Some (evaluationStackCellType.I1 :: evaluationStackCellType.I4 :: _)
                        | Some (evaluationStackCellType.I2 :: evaluationStackCellType.I1 :: _)
                        | Some (evaluationStackCellType.I2 :: evaluationStackCellType.I2 :: _)
                        | Some (evaluationStackCellType.I2 :: evaluationStackCellType.I4 :: _)
                        | Some (evaluationStackCellType.I4 :: evaluationStackCellType.I1 :: _)
                        | Some (evaluationStackCellType.I4 :: evaluationStackCellType.I2 :: _) ->
                            x.PrependProbe(probes.mem2_4, [], x.tokens.void_i4_i4_sig, &prependTarget) |> ignore
                            (if isUnchecked then probes.execBinOp_4 else probes.execBinOp_4_ovf), x.tokens.void_u2_i4_i4_offset_sig,
                                probes.unmem_4, x.tokens.i4_i1_sig, probes.unmem_4, x.tokens.i4_i1_sig
                        | Some (evaluationStackCellType.I4 :: evaluationStackCellType.I8 :: _) ->
                            x.PrependProbe(probes.mem2_8_4, [], x.tokens.void_i8_i4_sig, &prependTarget) |> ignore
                            (if isUnchecked then probes.execBinOp_8_4 else probes.execBinOp_8_4_ovf), x.tokens.void_u2_i8_i4_offset_sig,
                                probes.unmem_8, x.tokens.i8_i1_sig, probes.unmem_4, x.tokens.i4_i1_sig
                        | Some (evaluationStackCellType.I8 :: evaluationStackCellType.I8 :: _) ->
                            x.PrependProbe(probes.mem2_8, [], x.tokens.void_i8_i8_sig, &prependTarget) |> ignore
                            (if isUnchecked then probes.execBinOp_8 else probes.execBinOp_8_ovf), x.tokens.void_u2_i8_i8_offset_sig,
                                probes.unmem_8, x.tokens.i8_i1_sig, probes.unmem_8, x.tokens.i8_i1_sig
                        | Some (evaluationStackCellType.R4 :: evaluationStackCellType.R4 :: _) ->
                            x.PrependProbe(probes.mem2_f4, [], x.tokens.void_r4_r4_sig, &prependTarget) |> ignore
                            (if isUnchecked then probes.execBinOp_f4 else probes.execBinOp_f4_ovf), x.tokens.void_u2_r4_r4_offset_sig,
                                probes.unmem_f4, x.tokens.r4_i1_sig, probes.unmem_f4, x.tokens.r4_i1_sig
                        | Some (evaluationStackCellType.R8 :: evaluationStackCellType.R8 :: _) ->
                            x.PrependProbe(probes.mem2_f8, [], x.tokens.void_r8_r8_sig, &prependTarget) |> ignore
                            (if isUnchecked then probes.execBinOp_f8 else probes.execBinOp_f8_ovf), x.tokens.void_u2_r8_r8_offset_sig,
                                probes.unmem_f8, x.tokens.r8_i1_sig, probes.unmem_f8, x.tokens.r8_i1_sig
                        | Some (evaluationStackCellType.I :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.I :: evaluationStackCellType.Ref :: _)
                        | Some (evaluationStackCellType.Ref :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.Ref :: evaluationStackCellType.Ref :: _) ->
                            x.PrependMem2_p &prependTarget
                            (if isUnchecked then probes.execBinOp_p else probes.execBinOp_p_ovf), x.tokens.void_u2_i_i_offset_sig,
                                probes.unmem_p, x.tokens.i_i1_sig, probes.unmem_p, x.tokens.i_i1_sig
                        | Some (evaluationStackCellType.I1 :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.I2 :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.I4 :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.I1 :: evaluationStackCellType.Ref :: _)
                        | Some (evaluationStackCellType.I2 :: evaluationStackCellType.Ref :: _)
                        | Some (evaluationStackCellType.I4 :: evaluationStackCellType.Ref :: _) ->
                            x.PrependMem2_p_4 &prependTarget
                            (if isUnchecked then probes.execBinOp_p_4 else probes.execBinOp_p_4_ovf), x.tokens.void_u2_i_i4_offset_sig,
                                probes.unmem_p, x.tokens.i_i1_sig, probes.unmem_4, x.tokens.i4_i1_sig
                        | Some (evaluationStackCellType.I :: evaluationStackCellType.I1 :: _)
                        | Some (evaluationStackCellType.I :: evaluationStackCellType.I2 :: _)
                        | Some (evaluationStackCellType.I :: evaluationStackCellType.I4 :: _)
                        | Some (evaluationStackCellType.Ref :: evaluationStackCellType.I1 :: _)
                        | Some (evaluationStackCellType.Ref :: evaluationStackCellType.I2 :: _)
                        | Some (evaluationStackCellType.Ref :: evaluationStackCellType.I4 :: _) ->
                            x.PrependMem2_4_p &prependTarget
                            (if isUnchecked then probes.execBinOp_4_p else probes.execBinOp_4_p_ovf), x.tokens.void_u2_i4_i_offset_sig,
                                probes.unmem_4, x.tokens.i4_i1_sig, probes.unmem_p, x.tokens.i_i1_sig
                        | Some (x :: y :: _) -> internalfailf "Unexpected binop ([%O]%O) evaluation stack types: %O, %O" i opcodeValue x y
                        | stack -> internalfailf "Unexpected binop (%O) evaluation stack types! stack: %O" opcodeValue stack

                    x.PrependInstr(OpCodes.Ldc_I4, op.Value |> int |> Arg32 , &prependTarget)
                    x.PrependProbe(unmem1Probe, [(OpCodes.Ldc_I4, Arg32 0)], unmem1Sig, &prependTarget) |> ignore
                    x.PrependProbe(unmem2Probe, [(OpCodes.Ldc_I4, Arg32 1)], unmem2Sig, &prependTarget) |> ignore
                    x.PrependProbeWithOffset(execProbe, [], execSig, &prependTarget) |> ignore
                    x.PrependProbe(unmem1Probe, [(OpCodes.Ldc_I4, Arg32 0)], unmem1Sig, &prependTarget) |> ignore
                    x.PrependProbe(unmem2Probe, [(OpCodes.Ldc_I4, Arg32 1)], unmem2Sig, &prependTarget) |> ignore
                    br.arg <- Target prependTarget

                | OpCodeValues.Neg
                | OpCodeValues.Not ->
                    match instr.opcode with
                    | OpCode op -> x.AppendProbeWithOffset(probes.unOp, [(OpCodes.Ldc_I4, op.Value |> int |> Arg32)], x.tokens.void_u2_offset_sig, instr)
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
                    x.AppendProbeWithOffsetMemUnmem(probes.conv, [], x.tokens.void_offset_sig, &prependTarget, instr)
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
                    x.AppendProbeWithOffsetMemUnmem(probes.conv, [], x.tokens.void_offset_sig, &prependTarget, instr)

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
                    x.PrependDup &prependTarget
                    x.PrependProbeWithOffset(probes.ldind, [], x.tokens.void_i_offset_sig, &prependTarget) |> ignore

                | OpCodeValues.Stind_Ref
                | OpCodeValues.Stind_I1
                | OpCodeValues.Stind_I2
                | OpCodeValues.Stind_I4
                | OpCodeValues.Stind_I8
                | OpCodeValues.Stind_R4
                | OpCodeValues.Stind_R8
                | OpCodeValues.Stind_I ->
                    // TODO: need to execute concrete stind? #do
                    // calli mem2
                    // calli unmem 0
                    // calli track_stind
                    // branch_true A
                    // calli unmem 0
                    // calli unmem 1
                    // calli exec
                    // br B
                    // A: calli unmem 0
                    // calli unmem 1
                    // stind
                    // B:

                    let execProbe, execSig, unmem2Probe, unmem2Sig =
                        match opcodeValue with
                        | OpCodeValues.Stind_I ->
                            match instr.stackState with
                            | Some (evaluationStackCellType.I :: evaluationStackCellType.I :: _)
                            | Some (evaluationStackCellType.I :: evaluationStackCellType.Ref :: _) -> ()
                            | _ -> internalfail "Stack validation failed"
                            x.PrependMem2_p &prependTarget
                            probes.execStind_ref, x.tokens.void_i_i_offset_sig, probes.unmem_p, x.tokens.i_i1_sig
                        | OpCodeValues.Stind_Ref ->
                            match instr.stackState with
                            | Some (evaluationStackCellType.Ref :: evaluationStackCellType.I :: _)
                            | Some (evaluationStackCellType.Ref :: evaluationStackCellType.Ref :: _) -> ()
                            | _ -> internalfail "Stack validation failed"
                            x.PrependMem2_p &prependTarget
                            probes.execStind_ref, x.tokens.void_i_i_offset_sig, probes.unmem_p, x.tokens.i_i1_sig
                        | OpCodeValues.Stind_I1 ->
                            match instr.stackState with
                            | Some (evaluationStackCellType.I1 :: evaluationStackCellType.I :: _)
                            | Some (evaluationStackCellType.I1 :: evaluationStackCellType.Ref :: _) -> ()
                            | Some (evaluationStackCellType.I2 :: evaluationStackCellType.I :: _)
                            | Some (evaluationStackCellType.I2 :: evaluationStackCellType.Ref :: _) -> ()
                            | Some (evaluationStackCellType.I4 :: evaluationStackCellType.I :: _)
                            | Some (evaluationStackCellType.I4 :: evaluationStackCellType.Ref :: _) -> ()
                            | _ -> internalfail "Stack validation failed"
                            x.PrependMem2_p_1 &prependTarget
                            probes.execStind_I1, x.tokens.void_i_i1_offset_sig, probes.unmem_1, x.tokens.i1_i1_sig
                        | OpCodeValues.Stind_I2 ->
                            match instr.stackState with
                            | Some (evaluationStackCellType.I2 :: evaluationStackCellType.I :: _)
                            | Some (evaluationStackCellType.I2 :: evaluationStackCellType.Ref :: _) -> ()
                            | Some (evaluationStackCellType.I4 :: evaluationStackCellType.I :: _)
                            | Some (evaluationStackCellType.I4 :: evaluationStackCellType.Ref :: _) -> ()
                            | _ -> internalfail "Stack validation failed"
                            x.PrependMem2_p_2 &prependTarget
                            probes.execStind_I2, x.tokens.void_i_i2_offset_sig, probes.unmem_2, x.tokens.i2_i1_sig
                        | OpCodeValues.Stind_I4 ->
                            match instr.stackState with
                            | Some (evaluationStackCellType.I4 :: evaluationStackCellType.I :: _)
                            | Some (evaluationStackCellType.I4 :: evaluationStackCellType.Ref :: _) -> ()
                            | _ -> internalfail "Stack validation failed"
                            x.PrependMem2_p_4 &prependTarget
                            probes.execStind_I4, x.tokens.void_i_i4_offset_sig, probes.unmem_4, x.tokens.i4_i1_sig
                        | OpCodeValues.Stind_I8 ->
                            match instr.stackState with
                            | Some (evaluationStackCellType.I8 :: evaluationStackCellType.I :: _)
                            | Some (evaluationStackCellType.I8 :: evaluationStackCellType.Ref :: _) -> ()
                            | _ -> internalfail "Stack validation failed"
                            x.PrependMem2_p_8 &prependTarget
                            probes.execStind_I8, x.tokens.void_i_i8_offset_sig, probes.unmem_8, x.tokens.i8_i1_sig
                        | OpCodeValues.Stind_R4 ->
                            match instr.stackState with
                            | Some (evaluationStackCellType.R4 :: evaluationStackCellType.I :: _)
                            | Some (evaluationStackCellType.R4 :: evaluationStackCellType.Ref :: _) -> ()
                            | _ -> internalfail "Stack validation failed"
                            x.PrependMem2_p_f4 &prependTarget
                            probes.execStind_R4, x.tokens.void_i_r4_offset_sig, probes.unmem_f4, x.tokens.r4_i1_sig
                        | OpCodeValues.Stind_R8 ->
                            match instr.stackState with
                            | Some (evaluationStackCellType.R8 :: evaluationStackCellType.I :: _)
                            | Some (evaluationStackCellType.R8 :: evaluationStackCellType.Ref :: _) -> ()
                            | _ -> internalfail "Stack validation failed"
                            x.PrependMem2_p_f8 &prependTarget
                            probes.execStind_R8, x.tokens.void_i_r8_offset_sig, probes.unmem_f8, x.tokens.r8_i1_sig
                        | _ -> __unreachable__()

//                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
//                    x.PrependProbe(unmem2Probe, [(OpCodes.Ldc_I4, Arg32 1)], unmem2Sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependInstr(OpCodes.Ldc_I4, Arg32 (x.SizeOfIndirection opcodeValue), &prependTarget)
                    x.PrependProbe(probes.stind, [], x.tokens.bool_i_i4_sig, &prependTarget) |> ignore
                    let br_true = x.PrependBranch(OpCodes.Brtrue_S, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(unmem2Probe, [(OpCodes.Ldc_I4, Arg32 1)], unmem2Sig, &prependTarget) |> ignore
                    x.PrependProbeWithOffset(execProbe, [], execSig, &prependTarget) |> ignore
                    let br = x.PrependBranch(OpCodes.Br, &prependTarget)
                    let unmem_p = x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    br_true.arg <- Target unmem_p
                    x.PrependProbe(unmem2Probe, [(OpCodes.Ldc_I4, Arg32 1)], unmem2Sig, &prependTarget) |> ignore
                    br.arg <- Target instr.next // TODO: need NOP before? #do

                | OpCodeValues.Mkrefany -> x.AppendProbe(probes.mkrefany, [], x.tokens.void_sig, instr)
                | OpCodeValues.Newarr ->
                     x.AppendProbeWithOffset(probes.newarr, [], x.tokens.void_i_token_offset_sig, instr)
                     x.AppendInstr OpCodes.Ldc_I4 instr.arg instr
                     x.AppendInstr OpCodes.Conv_I NoArg instr
                     x.AppendDup instr
                | OpCodeValues.Localloc ->
                     x.AppendProbeWithOffset(probes.newarr, [], x.tokens.void_i_offset_sig, instr)
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
                    x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbeWithOffset(probes.execCpobj, [], x.tokens.void_token_i_i_offset_sig, &prependTarget) |> ignore
                    br.arg <- Target prependTarget
                | OpCodeValues.Ldobj ->
                     x.PrependDup &prependTarget
                     x.PrependProbeWithOffset(probes.ldobj, [], x.tokens.void_i_offset_sig, &prependTarget) |> ignore
                | OpCodeValues.Ldstr ->
                     x.AppendProbe(probes.ldstr, [], x.tokens.void_i_sig, instr)
                     x.AppendInstr OpCodes.Conv_I NoArg instr
                     x.AppendInstr OpCodes.Dup NoArg instr
                | OpCodeValues.Castclass ->
                     x.PrependDup &prependTarget
                     x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget)
                     x.PrependProbeWithOffset(probes.castclass, [], x.tokens.void_i_token_offset_sig, &prependTarget) |> ignore
                | OpCodeValues.Isinst ->
                     x.PrependDup &prependTarget
                     x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget)
                     x.PrependProbeWithOffset(probes.isinst, [], x.tokens.void_i_token_offset_sig, &prependTarget) |> ignore
                | OpCodeValues.Unbox ->
                     x.PrependDup &prependTarget
                     x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget)
                     x.PrependProbeWithOffset(probes.unbox, [], x.tokens.void_i_token_offset_sig, &prependTarget) |> ignore
                | OpCodeValues.Unbox_Any ->
                     x.PrependDup &prependTarget
                     x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget)
                     x.PrependProbeWithOffset(probes.unboxAny, [], x.tokens.void_i_token_offset_sig, &prependTarget) |> ignore
                | OpCodeValues.Ldfld ->
                     x.PrependMem_p(0, 0, &prependTarget)
                     x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &instr) |> ignore
                     let fieldInfo = Reflection.resolveField x.m instr.Arg32
                     let fieldOffset = CSharpUtils.LayoutUtils.GetFieldOffset fieldInfo
                     x.PrependInstr(OpCodes.Ldc_I4, Arg32 fieldOffset, &prependTarget)
                     let fieldSize = TypeUtils.internalSizeOf fieldInfo.FieldType
                     x.PrependInstr(OpCodes.Ldc_I4, Arg32 fieldSize, &prependTarget)
                     x.PrependProbeWithOffset(probes.ldfld, [], x.tokens.void_i_i4_i4_offset_sig, &prependTarget) |> ignore
                     x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &instr) |> ignore
                | OpCodeValues.Ldflda ->
                     x.PrependDup &prependTarget
                     x.PrependInstr(OpCodes.Conv_I, NoArg, &prependTarget)
                     x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget)
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

                    let isStruct =
                        match instr.stackState with
                        | Some (evaluationStackCellType.Struct :: _) -> true
                        | _ -> false
                    let typeTokenArg = instr.arg

                    if isStruct then
                        x.PrependInstr(OpCodes.Box, typeTokenArg, &prependTarget)

                    let probe, signature, unmem2Probe, unmem2Sig =
                        match instr.stackState with
                        | Some (evaluationStackCellType.I1 :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.I2 :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.I4 :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.I1 :: evaluationStackCellType.Ref :: _)
                        | Some (evaluationStackCellType.I2 :: evaluationStackCellType.Ref :: _)
                        | Some (evaluationStackCellType.I4 :: evaluationStackCellType.Ref :: _) ->
                            x.PrependMem2_p_4 &prependTarget
                            probes.stfld_4, x.tokens.void_token_i_i4_offset_sig, probes.unmem_4, x.tokens.i4_i1_sig
                        | Some (evaluationStackCellType.I8 :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.I8 :: evaluationStackCellType.Ref :: _) ->
                            x.PrependMem2_p_8 &prependTarget
                            probes.stfld_8, x.tokens.void_token_i_i8_offset_sig, probes.unmem_8, x.tokens.i8_i1_sig
                        | Some (evaluationStackCellType.R4 :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.R4 :: evaluationStackCellType.Ref :: _) ->
                            x.PrependMem2_p_f4 &prependTarget
                            probes.stfld_f4, x.tokens.void_token_i_r4_offset_sig, probes.unmem_f4, x.tokens.r4_i1_sig
                        | Some (evaluationStackCellType.R8 :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.R8 :: evaluationStackCellType.Ref :: _) ->
                            x.PrependMem2_p_f8 &prependTarget
                            probes.stfld_f8, x.tokens.void_token_i_r8_offset_sig, probes.unmem_f8, x.tokens.r8_i1_sig
                        | Some (evaluationStackCellType.I :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.I :: evaluationStackCellType.Ref :: _)
                        | Some (evaluationStackCellType.Ref :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.Ref :: evaluationStackCellType.Ref :: _) ->
                            x.PrependMem2_p &prependTarget
                            probes.stfld_p, x.tokens.void_token_i_i_offset_sig, probes.unmem_p, x.tokens.i_i1_sig
                        | Some (evaluationStackCellType.Struct :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.Struct :: evaluationStackCellType.Ref :: _) ->
                            x.PrependMem2_p &prependTarget
                            probes.stfld_struct, x.tokens.void_token_i_i_offset_sig, probes.unmem_p, x.tokens.i_i1_sig
                        | _ -> __unreachable__()

                    x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
//                    x.PrependInstr(OpCodes.Conv_I, NoArg, &prependTarget)
                    x.PrependProbe(unmem2Probe, [(OpCodes.Ldc_I4, Arg32 1)], unmem2Sig, &prependTarget) |> ignore
                    x.PrependProbeWithOffset(probe, [], signature, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
//                    let field = Reflection.resolveField x.m instr.Arg32
//                    x.PrependInstr(OpCodes.Mkrefany, Arg32 field.FieldType.MetadataToken, &prependTarget)
                    x.PrependProbe(unmem2Probe, [(OpCodes.Ldc_I4, Arg32 1)], unmem2Sig, &prependTarget) |> ignore
                    if isStruct then
                        x.PrependInstr(OpCodes.Unbox_Any, typeTokenArg, &prependTarget)
                | OpCodeValues.Ldsfld ->
                    x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget)
                    x.PrependProbeWithOffset(probes.ldsfld, [], x.tokens.void_token_offset_sig, &prependTarget) |> ignore
                | OpCodeValues.Ldsflda ->
                    x.PrependDup &prependTarget
                    x.PrependProbe(probes.ldsflda, [], x.tokens.void_sig, &prependTarget) |> ignore
                | OpCodeValues.Stsfld ->
                    x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget)
                    x.PrependProbeWithOffset(probes.stsfld, [], x.tokens.void_token_offset_sig, &prependTarget) |> ignore
                | OpCodeValues.Stobj -> __notImplemented__() // ?????????????????
                | OpCodeValues.Box ->
                    x.AppendProbeWithOffset(probes.box, [], x.tokens.void_i_offset_sig, instr)
                    x.AppendDup instr
                | OpCodeValues.Ldlen ->
                    x.PrependInstr(OpCodes.Conv_I, NoArg, &prependTarget)
                    x.PrependProbe(probes.mem_p, [], x.tokens.void_i_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbeWithOffset(probes.ldlen, [], x.tokens.void_i_offset_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
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
                    // calli track_ldelem(a)
                    // branch_true A
                    // calli unmem 0
                    // calli unmem 1
                    // calli exec
                    // A: ldelem(a)
                    x.PrependMem2_p &prependTarget
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(track, [], x.tokens.bool_i_i_sig, &prependTarget) |> ignore
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
                    // TODO: need to execute concrete stelem? #do
                    // box [if struct]
                    // calli mem3
                    // calli unmem 0
                    // calli unmem 1
                    // calli track_stelem
                    // brtrue A
                    // calli unmem 0
                    // calli unmem 1
                    // calli unmem 2
                    // calli exec
                    // br B
                    // A: calli unmem 0
                    // calli unmem 1
                    // calli unmem 2
                    // unbox [if struct]
                    // stelem
                    // B:

                    let isStruct =
                        match instr.stackState with
                        | Some (evaluationStackCellType.Struct :: _) ->
                            assert(op = OpCodes.Stelem)
                            true
                        | _ -> false
                    let typeTokenArg = instr.arg

                    if isStruct then
                        x.PrependInstr(OpCodes.Box, typeTokenArg, &prependTarget)

                    let execProbe, execSig, unmem3Probe, unmem3Sig =
                        match opcodeValue, instr.stackState with
                        | OpCodeValues.Stelem_I, _
                        | OpCodeValues.Stelem, Some (evaluationStackCellType.I :: _ :: evaluationStackCellType.Ref :: _) ->
                            x.PrependMem_p(2, 0, &prependTarget)
                            probes.execStelem_Ref, x.tokens.void_i_i_i_offset_sig, probes.unmem_p, x.tokens.i_i1_sig
                        | OpCodeValues.Stelem_Ref, _
                        | OpCodeValues.Stelem, Some (evaluationStackCellType.Ref :: _ :: evaluationStackCellType.Ref :: _) ->
                            x.PrependMem_p(2, 0, &prependTarget)
                            probes.execStind_ref, x.tokens.void_i_i_i_offset_sig, probes.unmem_p, x.tokens.i_i1_sig
                        | OpCodeValues.Stelem_I1, _
                        | OpCodeValues.Stelem, Some (evaluationStackCellType.I1 :: _ :: evaluationStackCellType.Ref :: _) ->
                            x.PrependMem_i1(2, 0, &prependTarget)
                            probes.execStelem_I1, x.tokens.void_i_i_i1_offset_sig, probes.unmem_1, x.tokens.i1_i1_sig
                        | OpCodeValues.Stelem_I2, _
                        | OpCodeValues.Stelem, Some (evaluationStackCellType.I2 :: _ :: evaluationStackCellType.Ref :: _) ->
                            x.PrependMem_i2(2, 0, &prependTarget)
                            probes.execStelem_I2, x.tokens.void_i_i_i2_offset_sig, probes.unmem_2, x.tokens.i2_i1_sig
                        | OpCodeValues.Stelem_I4, _
                        | OpCodeValues.Stelem, Some (evaluationStackCellType.I4 :: _ :: evaluationStackCellType.Ref :: _) ->
                            x.PrependMem_i4(2, 0, &prependTarget)
                            probes.execStelem_I4, x.tokens.void_i_i_i4_offset_sig, probes.unmem_4, x.tokens.i4_i1_sig
                        | OpCodeValues.Stelem_I8, _
                        | OpCodeValues.Stelem, Some (evaluationStackCellType.I8 :: _ :: evaluationStackCellType.Ref :: _) ->
                            x.PrependMem_i8(2, 0, &prependTarget)
                            probes.execStelem_I8, x.tokens.void_i_i_i8_offset_sig, probes.unmem_8, x.tokens.i8_i1_sig
                        | OpCodeValues.Stelem_R4, _
                        | OpCodeValues.Stelem, Some (evaluationStackCellType.R4 :: _ :: evaluationStackCellType.Ref :: _) ->
                            x.PrependMem_f4(2, 0, &prependTarget)
                            probes.execStelem_R4, x.tokens.void_i_i_r4_offset_sig, probes.unmem_f4, x.tokens.r4_i1_sig
                        | OpCodeValues.Stelem_R8, _
                        | OpCodeValues.Stelem, Some (evaluationStackCellType.R8 :: _ :: evaluationStackCellType.Ref :: _) ->
                            x.PrependMem_f8(2, 0, &prependTarget)
                            probes.execStelem_R8, x.tokens.void_i_i_r8_offset_sig, probes.unmem_f8, x.tokens.r8_i1_sig
                        | OpCodeValues.Stelem, Some (evaluationStackCellType.Struct :: _ :: evaluationStackCellType.Ref :: _) ->
                            x.PrependMem_p(2, 0, &prependTarget)
                            probes.execStelem_Struct, x.tokens.void_i_i_i_offset_sig, probes.unmem_p, x.tokens.i_i1_sig
                        | _ -> __unreachable__()

                    x.PrependMem_p(1, 1, &prependTarget)
                    x.PrependMem_p(0, 2, &prependTarget)

                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.stelem, [], x.tokens.bool_i_i_sig, &prependTarget) |> ignore
                    let brtrue = x.PrependBranch(OpCodes.Brtrue_S, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(unmem3Probe, [(OpCodes.Ldc_I4, Arg32 2)], unmem3Sig, &prependTarget) |> ignore
                    x.PrependProbeWithOffset(execProbe, [], execSig, &prependTarget) |> ignore
                    let br = x.PrependBranch(OpCodes.Br, &prependTarget)
                    let tgt = x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget) |> ignore
                    x.PrependProbe(unmem3Probe, [(OpCodes.Ldc_I4, Arg32 2)], unmem3Sig, &prependTarget) |> ignore
                    if isStruct then
                        x.PrependInstr(OpCodes.Unbox_Any, typeTokenArg, &prependTarget)
                    brtrue.arg <- Target tgt
                    x.AppendInstr OpCodes.Nop NoArg instr
                    br.arg <- Target instr.next

                | OpCodeValues.Ckfinite ->  x.AppendProbe(probes.ckfinite, [], x.tokens.void_sig, instr)
                | OpCodeValues.Ldvirtftn ->
                     x.PrependDup &prependTarget
                     x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget)
                     x.PrependProbeWithOffset(probes.ldvirtftn, [], x.tokens.void_i_token_offset_sig, &prependTarget) |> ignore
                | OpCodeValues.Initobj ->
                     x.PrependDup &prependTarget
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

                | OpCodeValues.Rethrow ->
                    atLeastOneReturnFound <- true
                    x.PrependProbeWithOffset(probes.rethrow, [], x.tokens.void_offset_sig, &prependTarget) |> ignore

                | OpCodeValues.Call
                | OpCodeValues.Callvirt
                | OpCodeValues.Newobj ->
                    // mem args
                    // ldc argsCount
                    // calli track_call
                    // branch_true A
                    // if callee is internal call {
                    //    ldc default return value
                    //    mem default value
                    //    calli exec
                    //    unmem default value
                    // } else {
                    //    calli exec
                    // }
                    // branch B
                    // if callee is internal call {
                    //    A: nop
                    // } else {
                    //    A, B: nop
                    // }
                    // unmem args
                    // calli pushFrame
                    // call callee
                    // calli finalizeCall
                    // if callee is insteranl call {
                    //    B: nop
                    // } else {
                    //    nop
                    // }
                    // if call opcode is newobj {
                    //    dup
                    //    conv.i
                    //    calli newobj_probe
                    // }

                    match instr.arg with
                    | Arg32 token ->
                        // TODO: if method is F# internal call, instr.arg <- token of static ctor of type #do
                        // TODO: if method is C# internal call, instr.arg <- token of C# implementation #do
                        let callee = Reflection.resolveMethod x.m token
                        let hasThis = callee.CallingConvention.HasFlag(CallingConventions.HasThis)
                        let argsCount = callee.GetParameters().Length
                        let argsCount = if hasThis && opcodeValue <> OpCodeValues.Newobj then argsCount + 1 else argsCount
                        let unmems = List<uint64 * uint32>()
                        match instr.stackState with
                        | Some list ->
                            let types = List.take argsCount list |> Array.ofList
                            for i = 0 to argsCount - 1 do
                                let t = types.[i]
                                unmems.Add(x.PrependMemUnmemForType(t, argsCount - i - 1, i, &prependTarget))
                        | None -> internalfail "unexpected stack state"
                        x.PrependProbe(probes.call, [(OpCodes.Ldc_I4, Arg32 argsCount)], x.tokens.bool_u2_sig, &prependTarget) |> ignore
                        let br_true = x.PrependBranch(OpCodes.Brtrue_S, &prependTarget)
                        let calleeMethod = Application.getMethod callee
                        if calleeMethod.IsInternalCall then
                            let retType = Reflection.getMethodReturnType callee
                            x.PrependLdcDefault(retType, &instr)
                            let probe, token = x.PrependMemUnmemForType(EvaluationStackTyper.abstractType retType, argsCount, argsCount, &prependTarget)
                            x.PrependProbeWithOffset(probes.execCall, [(OpCodes.Ldc_I4, Arg32 argsCount)], x.tokens.void_i4_offset_sig, &prependTarget) |> ignore
                            x.PrependProbe(probe, [(OpCodes.Ldc_I4, Arg32 argsCount)], token, &prependTarget) |> ignore
                        else x.PrependProbeWithOffset(probes.execCall, [(OpCodes.Ldc_I4, Arg32 argsCount)], x.tokens.void_i4_offset_sig, &prependTarget) |> ignore
                        let br = x.PrependBranch(OpCodes.Br, &prependTarget)

                        let callStart = x.PrependNop(&prependTarget)
                        br_true.arg <- Target callStart
                        for i = argsCount - 1 downto 0 do
                            let probe, token = unmems.[i]
                            x.PrependProbe(probe, [(OpCodes.Ldc_I4, Arg32 (argsCount - 1 - i))], token, &prependTarget) |> ignore
                        let expectedToken = if opcodeValue = OpCodeValues.Callvirt then 0 else callee.MetadataToken
                        let args = [(OpCodes.Ldc_I4, Arg32 token)
                                    (OpCodes.Ldc_I4, Arg32 expectedToken)
                                    (OpCodes.Ldc_I4, Arg32 (if opcodeValue = OpCodeValues.Newobj then 1 else 0))
                                    (OpCodes.Ldc_I4, Arg32 argsCount)]
                        x.PrependProbeWithOffset(probes.pushFrame, args, x.tokens.void_token_token_bool_u2_offset_sig, &prependTarget) |> ignore

                        if opcodeValue = OpCodeValues.Newobj then
                            x.AppendProbe(probes.newobj, [], x.tokens.void_i_sig, instr)
                            x.AppendInstr OpCodes.Conv_I NoArg instr
                            x.AppendDup(instr)
                        let returnValues = if Reflection.hasNonVoidResult callee then 1 else 0
                        let nop = x.AppendNop instr
                        x.AppendProbe(probes.finalizeCall, [(OpCodes.Ldc_I4, Arg32 returnValues)], x.tokens.void_u1_sig, instr)

                        if calleeMethod.IsInternalCall then br.arg <- Target nop
                        else br.arg <- Target callStart
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
            | SwitchArg -> ()
        assert(atLeastOneReturnFound)

    member x.Skip (body : rawMethodBody) =
        { properties = {ilCodeSize = body.properties.ilCodeSize; maxStackSize = body.properties.maxStackSize}; il = body.il; ehs = body.ehs}

    member x.Instrument(body : rawMethodBody) =
        assert(x.rewriter = null)
        x.tokens <- body.tokens
        // TODO: call Application.getMethod and take ILRewriter there!
        x.rewriter <- ILRewriter(body)
        x.m <- x.rewriter.Method
        let result =
            if Instrumenter.instrumentedFunctions.Add x.m then
                Logger.trace "Instrumenting %s (token = %u)" (Reflection.methodToString x.m) body.properties.token
                try
                    x.rewriter.Import()
                    x.rewriter.PrintInstructions "before instrumentation" probes
                    x.PlaceProbes()
                    x.rewriter.PrintInstructions "after instrumentation" probes
                    let result = x.rewriter.Export()
                    result
                with e ->
                    Logger.error "Instrumentation failed: in method %O got exception %O" x.m e
                    x.Skip body
            else
                Logger.trace "Duplicate JITting of %s" x.MethodName
                x.Skip body

        x.rewriter <- null
        result
