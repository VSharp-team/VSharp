namespace VSharp.Concolic

open VSharp
open System.Reflection
open System.Reflection.Emit
open System.Collections.Generic
open VSharp.Interpreter.IL

type Instrumenter(communicator : Communicator, probes : probes) =
    // TODO: should we consider executed assembly build options here?
    let ldc_i : opcode = (if System.Environment.Is64BitOperatingSystem then OpCodes.Ldc_I8 else OpCodes.Ldc_I4) |> VSharp.Concolic.OpCode
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
        newInstr.opcode <- VSharp.Concolic.OpCode opcode
        newInstr.arg <- arg

    member private x.PrependBranch(opcode, beforeInstr : ilInstr byref) =
        let mutable newInstr = x.rewriter.CopyInstruction(beforeInstr)
        x.rewriter.InsertAfter(beforeInstr, newInstr)
        swap &newInstr &beforeInstr
        newInstr.opcode <- VSharp.Concolic.OpCode opcode
        newInstr.arg <- NoArg // In chain of prepends, the address of instruction constantly changes. Deferring it.
        newInstr

    member private x.AppendInstr (opcode : OpCode) arg (afterInstr : ilInstr) =
        let dupInstr = x.rewriter.NewInstr opcode
        dupInstr.arg <- arg
        x.rewriter.InsertAfter(afterInstr, dupInstr)

    member private x.PrependDup(beforeInstr : ilInstr byref) = x.PrependInstr(OpCodes.Dup, NoArg, &beforeInstr)
    member private x.AppendDup afterInstr = x.AppendInstr OpCodes.Dup NoArg afterInstr
//
//    member private x.PrependProbe(methodAddress : uint64, signature, beforeInstr : ilInstr byref) =
//        let mutable newInstr = x.rewriter.CopyInstruction(beforeInstr)
//        x.rewriter.InsertAfter(beforeInstr, newInstr)
//        swap &newInstr &beforeInstr
//
//        newInstr.opcode <- ldc_i
//        newInstr.arg <- Arg64 (int64 methodAddress)
//
//        x.MkCalli(&newInstr, signature)
//        x.rewriter.InsertBefore(beforeInstr, newInstr)

    member private x.PrependProbe(methodAddress : uint64, args : (OpCode * ilInstrOperand) list, signature, beforeInstr : ilInstr byref) =
        let mutable newInstr = x.rewriter.CopyInstruction(beforeInstr)
        x.rewriter.InsertAfter(beforeInstr, newInstr)
        swap &newInstr &beforeInstr

        match args with
        | (opcode, arg)::tail ->
            newInstr.opcode <- opcode |> VSharp.Concolic.OpCode
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

    member private x.PlaceEnterProbe (firstInstr : ilInstr byref) =
        let args = [(OpCodes.Ldc_I4, Arg32 x.m.MetadataToken); (OpCodes.Ldc_I4, x.rewriter.MaxStackSize |> int32 |> Arg32)]
        x.PrependProbe(probes.enter, args, x.tokens.void_token_u4_sig, &firstInstr)

    member private x.PlaceLeaveProbe(instr : ilInstr byref) =
        let returnsSomething = Reflection.hasNonVoidResult x.m
        let args = [(OpCodes.Ldc_I4, (if returnsSomething then 1 else 0) |> Arg32)]
        x.PrependProbe(probes.leave, args, x.tokens.void_u1_sig, &instr)

    member x.MethodName with get() = x.m.Name

    member x.PlaceProbes() =
        let instructions = x.rewriter.CopyInstructions()
        assert(not <| Array.isEmpty instructions)
        let mutable atLeastOneReturnFound = false
        let mutable hasPrefix = false
        let mutable prefix : ilInstr byref = &instructions.[0]
        x.PlaceEnterProbe(&instructions.[0]) //x.MethodName
        for i in 0 .. instructions.Length - 1 do
            let instr = &instructions.[i]
            if not hasPrefix then prefix <- instr
            match instr.opcode with
            | OpCode op ->
                let prependTarget = if hasPrefix then &prefix else &instr
//                let dumpedInfo = x.rewriter.ILInstrToString probes instr
//                let idx = communicator.SendStringAndReadItsIndex dumpedInfo
//                x.PrependProbe(probes.dumpInstruction, [OpCodes.Ldc_I4, idx |> int |> Arg32], x.tokens.void_u4_sig, &prependTarget);
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
                | OpCodeValues.Ldarga_S -> x.AppendProbe(probes.ldarga, [(OpCodes.Ldc_I4, instr.Arg8 |> int |> Arg32)], x.tokens.void_u1_sig, instr)
                | OpCodeValues.Ldloca_S -> x.AppendProbe(probes.ldloca, [(OpCodes.Ldc_I4, instr.Arg8 |> int |> Arg32)], x.tokens.void_u1_sig, instr)
                | OpCodeValues.Ldarga -> x.AppendProbe(probes.ldarga, [(OpCodes.Ldc_I4, instr.Arg16 |> int |> Arg32)], x.tokens.void_u2_sig, instr)
                | OpCodeValues.Ldloca -> x.AppendProbe(probes.ldloca, [(OpCodes.Ldc_I4, instr.Arg16 |> int |> Arg32)], x.tokens.void_u2_sig, instr)
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
                | OpCodeValues.Brfalse -> x.PrependProbe(probes.brfalse, [], x.tokens.void_sig, &prependTarget)
                | OpCodeValues.Brtrue_S
                | OpCodeValues.Brtrue -> x.PrependProbe(probes.brtrue, [], x.tokens.void_sig, &prependTarget)
                | OpCodeValues.Switch -> x.PrependProbe(probes.switch, [], x.tokens.void_sig, &prependTarget)

                // Symbolic stack instructions
                | OpCodeValues.Ldarg_0 -> x.AppendProbe(probes.ldarg_0, [], x.tokens.void_sig, instr)
                | OpCodeValues.Ldarg_1 -> x.AppendProbe(probes.ldarg_1, [], x.tokens.void_sig, instr)
                | OpCodeValues.Ldarg_2 -> x.AppendProbe(probes.ldarg_2, [], x.tokens.void_sig, instr)
                | OpCodeValues.Ldarg_3 -> x.AppendProbe(probes.ldarg_3, [], x.tokens.void_sig, instr)
                | OpCodeValues.Ldloc_0 -> x.AppendProbe(probes.ldloc_0, [], x.tokens.void_sig, instr)
                | OpCodeValues.Ldloc_1 -> x.AppendProbe(probes.ldloc_1, [], x.tokens.void_sig, instr)
                | OpCodeValues.Ldloc_2 -> x.AppendProbe(probes.ldloc_2, [], x.tokens.void_sig, instr)
                | OpCodeValues.Ldloc_3 -> x.AppendProbe(probes.ldloc_3, [], x.tokens.void_sig, instr)
                | OpCodeValues.Stloc_0 -> x.AppendProbe(probes.stloc_0, [], x.tokens.void_sig, instr)
                | OpCodeValues.Stloc_1 -> x.AppendProbe(probes.stloc_1, [], x.tokens.void_sig, instr)
                | OpCodeValues.Stloc_2 -> x.AppendProbe(probes.stloc_2, [], x.tokens.void_sig, instr)
                | OpCodeValues.Stloc_3 -> x.AppendProbe(probes.stloc_3, [], x.tokens.void_sig, instr)
                | OpCodeValues.Ldarg_S -> x.AppendProbe(probes.ldarg_S, [(OpCodes.Ldc_I4, instr.Arg8 |> int |> Arg32)], x.tokens.void_u1_sig, instr)
                | OpCodeValues.Starg_S -> x.AppendProbe(probes.starg_S, [(OpCodes.Ldc_I4, instr.Arg8 |> int |> Arg32)], x.tokens.void_u1_sig, instr)
                | OpCodeValues.Ldloc_S -> x.AppendProbe(probes.ldloc_S, [(OpCodes.Ldc_I4, instr.Arg8 |> int |> Arg32)], x.tokens.void_u1_sig, instr)
                | OpCodeValues.Stloc_S -> x.AppendProbe(probes.stloc_S, [(OpCodes.Ldc_I4, instr.Arg8 |> int |> Arg32)], x.tokens.void_u1_sig, instr)
                | OpCodeValues.Ldarg -> x.AppendProbe(probes.ldarg, [(OpCodes.Ldc_I4, instr.Arg16 |> int |> Arg32)], x.tokens.void_u2_sig, instr)
                | OpCodeValues.Starg -> x.AppendProbe(probes.starg, [(OpCodes.Ldc_I4, instr.Arg16 |> int |> Arg32)], x.tokens.void_u2_sig, instr)
                | OpCodeValues.Ldloc -> x.AppendProbe(probes.ldloc, [(OpCodes.Ldc_I4, instr.Arg16 |> int |> Arg32)], x.tokens.void_u2_sig, instr)
                | OpCodeValues.Stloc -> x.AppendProbe(probes.stloc, [(OpCodes.Ldc_I4, instr.Arg16 |> int |> Arg32)], x.tokens.void_u2_sig, instr)
                | OpCodeValues.Dup -> x.AppendProbe(probes.dup, [], x.tokens.void_sig, instr)

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
                    let isUnchecked =
                        match opcodeValue with
                        | OpCodeValues.Add_Ovf
                        | OpCodeValues.Add_Ovf_Un
                        | OpCodeValues.Mul_Ovf
                        | OpCodeValues.Mul_Ovf_Un
                        | OpCodeValues.Sub_Ovf
                        | OpCodeValues.Sub_Ovf_Un -> false
                        | _ -> true
                    let execProbe, execSig, memProbe, memSig, unmem1Probe, unmem1Sig, unmem2Probe, unmem2Sig =
                        match instr.stackState with
                        | Some (evaluationStackCellType.I4 :: evaluationStackCellType.I4 :: _)                        
                        | Some (evaluationStackCellType.I1 :: evaluationStackCellType.I1 :: _)
                        | Some (evaluationStackCellType.I1 :: evaluationStackCellType.I2 :: _)
                        | Some (evaluationStackCellType.I1 :: evaluationStackCellType.I4 :: _)
                        | Some (evaluationStackCellType.I2 :: evaluationStackCellType.I1 :: _)
                        | Some (evaluationStackCellType.I2 :: evaluationStackCellType.I2 :: _)
                        | Some (evaluationStackCellType.I2 :: evaluationStackCellType.I4 :: _)
                        | Some (evaluationStackCellType.I4 :: evaluationStackCellType.I1 :: _)
                        | Some (evaluationStackCellType.I4 :: evaluationStackCellType.I2 :: _) ->
                            (if isUnchecked then probes.execBinOp_4 else probes.execBinOp_4_ovf), x.tokens.void_u2_i4_i4_sig,
                                probes.mem2_4, x.tokens.void_i4_i4_sig, probes.unmem_4, x.tokens.i4_i1_sig, probes.unmem_4, x.tokens.i4_i1_sig
                        | Some (evaluationStackCellType.I8 :: evaluationStackCellType.I8 :: _) ->
                            (if isUnchecked then probes.execBinOp_8 else probes.execBinOp_8_ovf), x.tokens.void_u2_i8_i8_sig,
                                probes.mem2_8, x.tokens.void_i8_i8_sig, probes.unmem_8, x.tokens.i8_i1_sig, probes.unmem_8, x.tokens.i8_i1_sig
                        | Some (evaluationStackCellType.R4 :: evaluationStackCellType.R4 :: _) ->
                            (if isUnchecked then probes.execBinOp_f4 else probes.execBinOp_f4_ovf), x.tokens.void_u2_r4_r4_sig,
                                probes.mem2_f4, x.tokens.void_r4_r4_sig, probes.unmem_f4, x.tokens.r4_i1_sig, probes.unmem_f4, x.tokens.r4_i1_sig
                        | Some (evaluationStackCellType.R8 :: evaluationStackCellType.R8 :: _) ->
                            (if isUnchecked then probes.execBinOp_f8 else probes.execBinOp_f8_ovf), x.tokens.void_u2_r8_r8_sig,
                                probes.mem2_f8, x.tokens.void_r8_r8_sig, probes.unmem_f8, x.tokens.r8_i1_sig, probes.unmem_f8, x.tokens.r8_i1_sig
                        | Some (evaluationStackCellType.I :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.I :: evaluationStackCellType.Ref :: _)
                        | Some (evaluationStackCellType.Ref :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.Ref :: evaluationStackCellType.Ref :: _) ->
                            (if isUnchecked then probes.execBinOp_p else probes.execBinOp_p_ovf), x.tokens.void_u2_i_i_sig,
                                probes.mem2_p, x.tokens.void_i_i_sig, probes.unmem_p, x.tokens.i_i1_sig, probes.unmem_p, x.tokens.i_i1_sig
                        | Some (evaluationStackCellType.I1 :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.I2 :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.I4 :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.I1 :: evaluationStackCellType.Ref :: _)
                        | Some (evaluationStackCellType.I2 :: evaluationStackCellType.Ref :: _)
                        | Some (evaluationStackCellType.I4 :: evaluationStackCellType.Ref :: _) ->
                            (if isUnchecked then probes.execBinOp_p_4 else probes.execBinOp_p_4_ovf), x.tokens.void_u2_i_i4_sig,
                                probes.mem2_p_4, x.tokens.void_i_i4_sig, probes.unmem_p, x.tokens.i_i1_sig, probes.unmem_4, x.tokens.i4_i1_sig
                        | Some (evaluationStackCellType.I :: evaluationStackCellType.I1 :: _)
                        | Some (evaluationStackCellType.I :: evaluationStackCellType.I2 :: _)
                        | Some (evaluationStackCellType.I :: evaluationStackCellType.I4 :: _)
                        | Some (evaluationStackCellType.Ref :: evaluationStackCellType.I1 :: _)
                        | Some (evaluationStackCellType.Ref :: evaluationStackCellType.I2 :: _)
                        | Some (evaluationStackCellType.Ref :: evaluationStackCellType.I4 :: _) ->
                            (if isUnchecked then probes.execBinOp_4_p else probes.execBinOp_4_p_ovf), x.tokens.void_u2_i4_i_sig,
                                probes.mem2_4_p, x.tokens.void_i4_i_sig, probes.unmem_4, x.tokens.i4_i1_sig, probes.unmem_p, x.tokens.i_i1_sig
                        | _ -> internalfail "Unexpected binop evaluation stack types!"

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
                    x.PrependProbe(probes.binOp, [], x.tokens.bool_sig, &prependTarget)
                    let br = x.PrependBranch(OpCodes.Brtrue_S, &prependTarget)
                    x.PrependProbe(memProbe, [], memSig, &prependTarget)
                    x.PrependInstr(OpCodes.Ldc_I4, op.Value |> int |> Arg32 , &prependTarget)
                    x.PrependProbe(unmem1Probe, [(OpCodes.Ldc_I4, Arg32 0)], unmem1Sig, &prependTarget)
                    x.PrependProbe(unmem2Probe, [(OpCodes.Ldc_I4, Arg32 1)], unmem2Sig, &prependTarget)
                    x.PrependProbe(execProbe, [], execSig, &prependTarget)
                    x.PrependProbe(unmem1Probe, [(OpCodes.Ldc_I4, Arg32 0)], unmem1Sig, &prependTarget)
                    x.PrependProbe(unmem2Probe, [(OpCodes.Ldc_I4, Arg32 1)], unmem2Sig, &prependTarget)
                    br.arg <- Target instr

                | OpCodeValues.Neg
                | OpCodeValues.Not ->
                    match instr.opcode with
                    | OpCode op -> x.AppendProbe(probes.unOp, [(OpCodes.Ldc_I4, op.Value |> int |> Arg32)], x.tokens.void_u2_sig, instr)
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
                | OpCodeValues.Conv_U -> x.AppendProbe(probes.conv, [], x.tokens.void_sig, instr)
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
                | OpCodeValues.Conv_Ovf_U -> x.AppendProbe(probes.conv_Ovf, [], x.tokens.void_sig, instr)

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
                    x.PrependProbe(probes.ldind, [], x.tokens.void_i_sig, &prependTarget)

                | OpCodeValues.Stind_Ref
                | OpCodeValues.Stind_I1
                | OpCodeValues.Stind_I2
                | OpCodeValues.Stind_I4
                | OpCodeValues.Stind_I8
                | OpCodeValues.Stind_R4
                | OpCodeValues.Stind_R8
                | OpCodeValues.Stind_I ->
                    let execProbe, execSig, memProbe, memSig, unmem2Probe, unmem2Sig =
                        match opcodeValue with
                        | OpCodeValues.Stind_I ->
                            match instr.stackState with
                            | Some (evaluationStackCellType.I :: evaluationStackCellType.I :: _) -> ()
                            | _ -> internalfail "Stack validation failed"
                            probes.execStind_ref, x.tokens.void_i_i_sig, probes.mem2_p, x.tokens.void_i_i_sig, probes.unmem_p, x.tokens.i_i1_sig
                        | OpCodeValues.Stind_Ref ->
                            match instr.stackState with
                            | Some (evaluationStackCellType.Ref :: evaluationStackCellType.Ref :: _)
                            | Some (evaluationStackCellType.Ref :: evaluationStackCellType.I :: _) -> ()
                            | _ -> internalfail "Stack validation failed"
                            probes.execStind_ref, x.tokens.void_i_i_sig, probes.mem2_p, x.tokens.void_i_i_sig, probes.unmem_p, x.tokens.i_i1_sig
                        | OpCodeValues.Stind_I1 ->
                            match instr.stackState with
                            | Some (evaluationStackCellType.I1 :: evaluationStackCellType.I :: _) -> ()
                            | _ -> internalfail "Stack validation failed"
                            probes.execStind_I1, x.tokens.void_i_i1_sig, probes.mem2_p_1, x.tokens.void_i_i1_sig, probes.unmem_1, x.tokens.i1_i1_sig
                        | OpCodeValues.Stind_I2 ->
                            match instr.stackState with
                            | Some (evaluationStackCellType.I2 :: evaluationStackCellType.I :: _) -> ()
                            | _ -> internalfail "Stack validation failed"
                            probes.execStind_I2, x.tokens.void_i_i2_sig, probes.mem2_p_2, x.tokens.void_i_i2_sig, probes.unmem_2, x.tokens.i2_i1_sig
                        | OpCodeValues.Stind_I4 ->
                            match instr.stackState with
                            | Some (evaluationStackCellType.I4 :: evaluationStackCellType.I :: _) -> ()
                            | _ -> internalfail "Stack validation failed"
                            probes.execStind_I4, x.tokens.void_i_i4_sig, probes.mem2_p_4, x.tokens.void_i_i4_sig, probes.unmem_4, x.tokens.i4_i1_sig
                        | OpCodeValues.Stind_I8 ->
                            match instr.stackState with
                            | Some (evaluationStackCellType.I8 :: evaluationStackCellType.I :: _) -> ()
                            | _ -> internalfail "Stack validation failed"
                            probes.execStind_I8, x.tokens.void_i_i8_sig, probes.mem2_p_8, x.tokens.void_i_i8_sig, probes.unmem_8, x.tokens.i8_i1_sig
                        | OpCodeValues.Stind_R4 ->
                            match instr.stackState with
                            | Some (evaluationStackCellType.R4 :: evaluationStackCellType.I :: _) -> ()
                            | _ -> internalfail "Stack validation failed"
                            probes.execStind_R4, x.tokens.void_i_r4_sig, probes.mem2_p_f4, x.tokens.void_i_r4_sig, probes.unmem_f4, x.tokens.r4_i1_sig
                        | OpCodeValues.Stind_R8 ->
                            match instr.stackState with
                            | Some (evaluationStackCellType.R8 :: evaluationStackCellType.I :: _) -> ()
                            | _ -> internalfail "Stack validation failed"
                            probes.execStind_R8, x.tokens.void_i_r8_sig, probes.mem2_p_f8, x.tokens.void_i_r8_sig, probes.unmem_f8, x.tokens.r8_i1_sig
                        | _ -> __unreachable__()

                    // calli mem2
                    // calli unmem 0
                    // calli unmem 1
                    // calli unmem 0
                    // calli track_stind
                    // branch_true A
                    // calli unmem 0
                    // calli unmem 1
                    // calli exec
                    // A: stind
                    x.PrependProbe(memProbe, [], memSig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(unmem2Probe, [(OpCodes.Ldc_I4, Arg32 1)], unmem2Sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.stind, [], x.tokens.bool_i_sig, &prependTarget)
                    let br = x.PrependBranch(OpCodes.Brtrue_S, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(unmem2Probe, [(OpCodes.Ldc_I4, Arg32 1)], unmem2Sig, &prependTarget)
                    x.PrependProbe(execProbe, [], execSig, &prependTarget)
                    br.arg <- Target instr

                | OpCodeValues.Mkrefany -> x.AppendProbe(probes.mkrefany, [], x.tokens.void_sig, instr)
                | OpCodeValues.Newarr ->
                     x.AppendProbe(probes.newarr, [], x.tokens.void_i_token_sig, instr)
                     x.AppendInstr OpCodes.Ldc_I4 instr.arg instr
                     x.AppendDup instr
                | OpCodeValues.Localloc ->
                     x.AppendProbe(probes.newarr, [], x.tokens.void_i_sig, instr)
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
                    x.PrependProbe(probes.mem2_p, [], x.tokens.void_i_i_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.cpobj, [], x.tokens.bool_i_i_sig, &prependTarget)
                    let br = x.PrependBranch(OpCodes.Brtrue_S, &prependTarget)
                    x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.execCpobj, [], x.tokens.void_token_i_i_sig, &prependTarget)
                    br.arg <- Target instr
                | OpCodeValues.Ldobj ->
                     x.PrependDup &prependTarget
                     x.PrependProbe(probes.ldobj, [], x.tokens.void_i_sig, &prependTarget)
                | OpCodeValues.Ldstr ->
                     x.AppendProbe(probes.ldstr, [], x.tokens.void_i_sig, instr)
                     x.AppendDup instr
                | OpCodeValues.Castclass ->
                     x.PrependDup &prependTarget
                     x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget)
                     x.PrependProbe(probes.castclass, [], x.tokens.void_i_token_sig, &prependTarget)
                | OpCodeValues.Isinst ->
                     x.PrependDup &prependTarget
                     x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget)
                     x.PrependProbe(probes.isinst, [], x.tokens.void_i_token_sig, &prependTarget)
                | OpCodeValues.Unbox ->
                     x.PrependDup &prependTarget
                     x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget)
                     x.PrependProbe(probes.unbox, [], x.tokens.void_i_token_sig, &prependTarget)
                | OpCodeValues.Unbox_Any ->
                     x.PrependDup &prependTarget
                     x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget)
                     x.PrependProbe(probes.unboxAny, [], x.tokens.void_i_token_sig, &prependTarget)
                | OpCodeValues.Ldfld ->
                     x.PrependDup &prependTarget
                     x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget)
                     x.PrependProbe(probes.ldfld, [], x.tokens.void_i_token_sig, &prependTarget)
                | OpCodeValues.Ldflda ->
                     x.PrependDup &prependTarget
                     x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget)
                     x.PrependProbe(probes.ldflda, [], x.tokens.void_i_token_sig, &prependTarget)
                | OpCodeValues.Stfld ->
                    let probe, signature, memProbe, memSig, unmem2Probe, unmem2Sig =
                        match instr.stackState with
                        | Some (evaluationStackCellType.I1 :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.I2 :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.I4 :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.I1 :: evaluationStackCellType.Ref :: _)
                        | Some (evaluationStackCellType.I2 :: evaluationStackCellType.Ref :: _)
                        | Some (evaluationStackCellType.I4 :: evaluationStackCellType.Ref :: _) ->
                            probes.stfld_4, x.tokens.void_token_i_i4_sig, probes.mem2_p_4, x.tokens.void_i_i4_sig, probes.unmem_4, x.tokens.i4_i1_sig
                        | Some (evaluationStackCellType.I8 :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.I8 :: evaluationStackCellType.Ref :: _) ->
                            probes.stfld_8, x.tokens.void_token_i_i8_sig, probes.mem2_p_8, x.tokens.void_i_i8_sig, probes.unmem_8, x.tokens.i8_i1_sig
                        | Some (evaluationStackCellType.R4 :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.R4 :: evaluationStackCellType.Ref :: _) ->
                            probes.stfld_f4, x.tokens.void_token_i_r4_sig, probes.mem2_p_f4, x.tokens.void_i_r4_sig, probes.unmem_f4, x.tokens.r4_i1_sig
                        | Some (evaluationStackCellType.R8 :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.R8 :: evaluationStackCellType.Ref :: _) ->
                            probes.stfld_f8, x.tokens.void_token_i_r8_sig, probes.mem2_p_8, x.tokens.void_i_r8_sig, probes.unmem_f8, x.tokens.r8_i1_sig
                        | Some (evaluationStackCellType.I :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.I :: evaluationStackCellType.Ref :: _)
                        | Some (evaluationStackCellType.Ref :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.Ref :: evaluationStackCellType.Ref :: _) ->
                            probes.stfld_p, x.tokens.void_token_i_i_sig, probes.mem2_p, x.tokens.void_i_i_sig, probes.unmem_p, x.tokens.i_i1_sig
                        | Some (evaluationStackCellType.Struct :: evaluationStackCellType.I :: _)
                        | Some (evaluationStackCellType.Struct :: evaluationStackCellType.Ref :: _) ->
                            __notImplemented__()
                        | _ -> __unreachable__()
                    // calli mem2
                    // ldc token
                    // calli unmem 0
                    // calli unmem 1
                    // calli track_stfld
                    // calli unmem 0
                    // calli unmem 1
                    // stfld
                    x.PrependProbe(memProbe, [], memSig, &prependTarget)
                    x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(unmem2Probe, [(OpCodes.Ldc_I4, Arg32 1)], unmem2Sig, &prependTarget)
                    x.PrependProbe(probe, [], signature, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(unmem2Probe, [(OpCodes.Ldc_I4, Arg32 1)], unmem2Sig, &prependTarget)
                | OpCodeValues.Ldsfld ->
                     x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget)
                     x.PrependProbe(probes.ldsfld, [], x.tokens.void_token_sig, &prependTarget)
                | OpCodeValues.Ldsflda -> x.PrependProbe(probes.ldsflda, [], x.tokens.void_sig, &prependTarget)
                | OpCodeValues.Stsfld ->
                     x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget)
                     x.PrependProbe(probes.stsfld, [], x.tokens.void_token_sig, &prependTarget)
                | OpCodeValues.Stobj -> __notImplemented__() // ?????????????????
                | OpCodeValues.Box ->
                     x.AppendProbe(probes.box, [], x.tokens.void_i_sig, instr)
                     x.AppendDup instr
                | OpCodeValues.Ldlen ->
                     x.PrependDup &prependTarget
                     x.PrependProbe(probes.ldlen, [], x.tokens.void_i_sig, &prependTarget)

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
                    x.PrependProbe(probes.mem2_p, [], x.tokens.void_i_i_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(track, [], x.tokens.bool_i_i_sig, &prependTarget)
                    let br = x.PrependBranch(OpCodes.Brtrue_S, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(exec, [], x.tokens.void_i_i_sig, &prependTarget)
                    br.arg <- Target instr

                | OpCodeValues.Stelem_I
                | OpCodeValues.Stelem_I1
                | OpCodeValues.Stelem_I2
                | OpCodeValues.Stelem_I4
                | OpCodeValues.Stelem_I8
                | OpCodeValues.Stelem_R4
                | OpCodeValues.Stelem_R8
                | OpCodeValues.Stelem_Ref
                | OpCodeValues.Stelem ->
                    let execProbe, execSig, memProbe, memSig, unmem3Probe, unmem3Sig =
                        match opcodeValue, instr.stackState with
                        | OpCodeValues.Stelem_I, _
                        | OpCodeValues.Stelem, Some (evaluationStackCellType.I :: _ :: evaluationStackCellType.Ref :: _) ->
                            probes.execStelem_Ref, x.tokens.void_i_i_i_sig, probes.mem3_p_p_p, x.tokens.void_i_i_i_sig, probes.unmem_p, x.tokens.i_i1_sig
                        | OpCodeValues.Stelem_Ref, _
                        | OpCodeValues.Stelem, Some (evaluationStackCellType.Ref :: _ :: evaluationStackCellType.Ref :: _) ->
                            probes.execStind_ref, x.tokens.void_i_i_sig, probes.mem2_p, x.tokens.void_i_i_sig, probes.unmem_p, x.tokens.i_i1_sig
                        | OpCodeValues.Stelem_I1, _
                        | OpCodeValues.Stelem, Some (evaluationStackCellType.I1 :: _ :: evaluationStackCellType.Ref :: _) ->
                            probes.execStelem_I1, x.tokens.void_i_i_i1_sig, probes.mem3_p_p_i1, x.tokens.void_i_i_i1_sig, probes.unmem_1, x.tokens.i1_i1_sig
                        | OpCodeValues.Stelem_I2, _
                        | OpCodeValues.Stelem, Some (evaluationStackCellType.I2 :: _ :: evaluationStackCellType.Ref :: _) ->
                            probes.execStelem_I2, x.tokens.void_i_i_i2_sig, probes.mem3_p_p_i2, x.tokens.void_i_i_i2_sig, probes.unmem_2, x.tokens.i2_i1_sig
                        | OpCodeValues.Stelem_I4, _
                        | OpCodeValues.Stelem, Some (evaluationStackCellType.I4 :: _ :: evaluationStackCellType.Ref :: _) ->
                            probes.execStelem_I4, x.tokens.void_i_i_i4_sig, probes.mem3_p_p_i4, x.tokens.void_i_i_i4_sig, probes.unmem_4, x.tokens.i4_i1_sig
                        | OpCodeValues.Stelem_I8, _
                        | OpCodeValues.Stelem, Some (evaluationStackCellType.I8 :: _ :: evaluationStackCellType.Ref :: _) ->
                            probes.execStelem_I8, x.tokens.void_i_i_i8_sig, probes.mem3_p_p_i8, x.tokens.void_i_i_i8_sig, probes.unmem_8, x.tokens.i8_i1_sig
                        | OpCodeValues.Stelem_R4, _
                        | OpCodeValues.Stelem, Some (evaluationStackCellType.R4 :: _ :: evaluationStackCellType.Ref :: _) ->
                            probes.execStelem_R4, x.tokens.void_i_i_r4_sig, probes.mem3_p_p_f4, x.tokens.void_i_i_r4_sig, probes.unmem_f4, x.tokens.r4_i1_sig
                        | OpCodeValues.Stelem_R8, _
                        | OpCodeValues.Stelem, Some (evaluationStackCellType.R8 :: _ :: evaluationStackCellType.Ref :: _) ->
                            probes.execStelem_R8, x.tokens.void_i_i_r8_sig, probes.mem3_p_p_f8, x.tokens.void_i_i_r8_sig, probes.unmem_f8, x.tokens.r8_i1_sig
                        | _ -> __unreachable__()
                    // calli mem3
                    // calli unmem 0
                    // calli unmem 1
                    // calli unmem 2
                    // calli unmem 0
                    // calli unmem 1
                    // calli track_stelem
                    // branch_true A
                    // calli unmem 0
                    // calli unmem 1
                    // calli unmem 2
                    // calli exec
                    // A: stelem
                    x.PrependProbe(memProbe, [], memSig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(unmem3Probe, [(OpCodes.Ldc_I4, Arg32 2)], unmem3Sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.stelem, [], x.tokens.bool_i_i_sig, &prependTarget)
                    let br = x.PrependBranch(OpCodes.Brtrue_S, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(unmem3Probe, [(OpCodes.Ldc_I4, Arg32 2)], unmem3Sig, &prependTarget)
                    x.PrependProbe(execProbe, [], execSig, &prependTarget)
                    br.arg <- Target instr

                | OpCodeValues.Ckfinite ->  x.AppendProbe(probes.ckfinite, [], x.tokens.void_sig, instr)
                | OpCodeValues.Ldvirtftn ->
                     x.PrependDup &prependTarget
                     x.PrependInstr(OpCodes.Ldc_I4, instr.arg, &prependTarget)
                     x.PrependProbe(probes.ldvirtftn, [], x.tokens.void_i_token_sig, &prependTarget)
                | OpCodeValues.Initobj ->
                     x.PrependDup &prependTarget
                     x.PrependProbe(probes.initobj, [], x.tokens.void_i_sig, &prependTarget)
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
                    x.PrependProbe(probes.mem3_p_p_p, [], x.tokens.void_i_i_i_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 2)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.cpblk, [], x.tokens.bool_i_i_sig, &prependTarget)
                    let br = x.PrependBranch(OpCodes.Brtrue_S, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 2)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.execCpblk, [], x.tokens.void_i_i_i_sig, &prependTarget)
                    br.arg <- Target instr
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
                    x.PrependProbe(probes.mem3_p_i1_p, [], x.tokens.void_i_i1_i_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_1, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i1_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 2)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.initblk, [], x.tokens.bool_i_sig, &prependTarget)
                    let br = x.PrependBranch(OpCodes.Brtrue_S, &prependTarget)
                    x.PrependProbe(probes.unmem_1, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i1_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 0)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_1, [(OpCodes.Ldc_I4, Arg32 1)], x.tokens.i1_i1_sig, &prependTarget)
                    x.PrependProbe(probes.unmem_p, [(OpCodes.Ldc_I4, Arg32 2)], x.tokens.i_i1_sig, &prependTarget)
                    x.PrependProbe(probes.execInitblk, [], x.tokens.void_i_i1_i_sig, &prependTarget)
                    br.arg <- Target instr

                | OpCodeValues.Rethrow ->
                    atLeastOneReturnFound <- true
                    x.PrependProbe(probes.rethrow, [], x.tokens.void_sig, &prependTarget)

                | OpCodeValues.Call
                | OpCodeValues.Callvirt
                | OpCodeValues.Newobj ->
                    match instr.arg with
                    | Arg32 token ->
                        let callee = Reflection.resolveMethod x.m token
                        let hasThis = callee.CallingConvention.HasFlag(CallingConventions.HasThis)
                        let argsCount = callee.GetParameters().Length
                        let returnsSomething = Reflection.hasNonVoidResult callee
                        let count =
                            if hasThis && opcodeValue <> OpCodeValues.Newobj then argsCount + 1
                            else argsCount
                        let expectedToken = if opcodeValue = OpCodeValues.Callvirt then 0 else callee.MetadataToken 
                        let args = [(OpCodes.Ldc_I4, Arg32 expectedToken); (OpCodes.Ldc_I4, Arg32 count)]
                        x.PrependProbe(probes.call, args, x.tokens.void_token_u2_sig, &prependTarget)
                        let returnValues = if returnsSomething then 1 else 0
                        x.AppendProbe(probes.finalizeCall, [(OpCodes.Ldc_I4, Arg32 returnValues)], x.tokens.void_u1_sig, instr);

                        if opcodeValue = OpCodeValues.Newobj then
                            x.AppendProbe(probes.newobj, [], x.tokens.void_i_sig, instr)
                            x.AppendDup(instr)
                    | _ -> __unreachable__()
                | OpCodeValues.Calli -> __notImplemented__()

                | OpCodeValues.Ret ->
                    assert (not hasPrefix)
                    atLeastOneReturnFound <- true
                    x.PlaceLeaveProbe &instr
                | OpCodeValues.Throw ->
                    x.PrependProbe(probes.throw, [], x.tokens.void_sig, &prependTarget)
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
        x.rewriter <- ILRewriter(body)
        x.m <- x.rewriter.Method
        let result =
            if Instrumenter.instrumentedFunctions.Add x.m then
                Logger.trace "Instrumenting %s (token = %u)" (Reflection.methodToString x.m) body.properties.token
                x.rewriter.Import()
                x.rewriter.PrintInstructions "before instrumentation" probes
//                Logger.trace "Placing probes..."
                x.PlaceProbes()
//                Logger.trace "Done placing probes!"
                x.rewriter.PrintInstructions "after instrumentation" probes
//                Logger.trace "Exporting..."
                let result = x.rewriter.Export()
//                Logger.trace "Exported!"
                result
            else
                Logger.trace "Duplicate JITting of %s" x.MethodName
                x.Skip body

        x.rewriter <- null
        result
