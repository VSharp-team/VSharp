namespace VSharp.Concolic

open VSharp
open System.Reflection
open System.Reflection.Emit
open System.Collections.Generic
open VSharp.Interpreter.IL

type Instrumenter(probes : probes) =
    // TODO: should we consider executed assembly build options here?
    let ldc_i : opcode = (if System.Environment.Is64BitOperatingSystem then OpCodes.Ldc_I8 else OpCodes.Ldc_I4) |> VSharp.Concolic.OpCode
    static member private instrumentedFunctions = HashSet<MethodBase>()
    [<DefaultValue>] val mutable tokens : signatureTokens
    [<DefaultValue>] val mutable rewriter : ILRewriter
    [<DefaultValue>] val mutable m : MethodBase

    member private x.MkCalli(instr : ilInstr byref, signature : uint32) =
//        instr := x.rewriter.NewInstr OpCodes.Calli
        instr <- x.rewriter.NewInstr OpCodes.Calli
        instr.arg <- Arg32 (int32 signature)

    member private x.PrependProbe_void(methodAddress : uint64, beforeInstr : ilInstr byref) =
        let mutable newInstr = x.rewriter.CopyInstruction(beforeInstr)
        x.rewriter.InsertAfter(beforeInstr, newInstr)
        swap &newInstr &beforeInstr

        newInstr.opcode <- ldc_i
        newInstr.arg <- Arg64 (int64 methodAddress)

        x.MkCalli(&newInstr, x.tokens.void_sig)
        x.rewriter.InsertBefore(beforeInstr, newInstr)

    member private x.PrependProbe_void_with_args(methodAddress : uint64, args : (opcode * ilInstrOperand) list, signature, beforeInstr : ilInstr byref) =
        let mutable newInstr = x.rewriter.CopyInstruction(beforeInstr)
        x.rewriter.InsertAfter(beforeInstr, newInstr)
        swap &newInstr &beforeInstr

        match args with
        | (opcode, arg)::tail ->
            newInstr.opcode <- opcode
            newInstr.arg <- arg
            for (opcode, arg) in tail do
                let newInstr = x.rewriter.NewInstr opcode
                newInstr.arg <- arg
                x.rewriter.InsertBefore(beforeInstr, newInstr)
        | [] -> __unreachable__()

        newInstr <- x.rewriter.NewInstr ldc_i
        newInstr.arg <- Arg64 (int64 methodAddress)
        x.rewriter.InsertBefore(beforeInstr, newInstr)

        x.MkCalli(&newInstr, signature)
        x.rewriter.InsertBefore(beforeInstr, newInstr)

    member private x.PrependProbe_void_int32(methodAddress : uint64, arg : int32, beforeInstr : ilInstr byref) =
        let opcode = OpCodes.Ldc_I4 |> VSharp.Concolic.OpCode // TODO: previously was ldc_i, but it seems to be wrong
//        let opcode = ldc_i
        let arg = Arg32 arg
        x.PrependProbe_void_with_args(methodAddress, [(opcode, arg)], x.tokens.void_int32_sig, &beforeInstr)

    member private x.PrependProbe_void_uint32(methodAddress : uint64, arg : uint32, beforeInstr : ilInstr byref) =
        let opcode = OpCodes.Ldc_I4 |> VSharp.Concolic.OpCode // TODO: previously was ldc_i, but it seems to be wrong
//        let opcode = ldc_i
        let arg = Arg32 (int32 arg)
        x.PrependProbe_void_with_args(methodAddress, [(opcode, arg)], x.tokens.void_uint32_sig, &beforeInstr)

    member private x.PrependProbe_void_uint64(methodAddress : uint64, arg : uint64, beforeInstr : ilInstr byref) =
        let opcode = OpCodes.Ldc_I8 |> VSharp.Concolic.OpCode // TODO: previously was ldc_i, but it seems to be wrong
//        let opcode = ldc_i
        let arg = Arg64 (int64 arg)
        x.PrependProbe_void_with_args(methodAddress, [(opcode, arg)], x.tokens.void_uint64_sig, &beforeInstr)

    member private x.PrependProbe_void_bool_uint32(methodAddress : uint64, arg1 : bool, arg2 : uint32, beforeInstr : ilInstr byref) =
        let opcode1 = (if arg1 then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0) |> VSharp.Concolic.OpCode
        let opcode2 = OpCodes.Ldc_I4 |> VSharp.Concolic.OpCode // TODO: previously was ldc_i, but it seems to be wrong
//        let opcode2 = ldc_i
        let arg1 = NoArg
        let arg2 = Arg32 (int32 arg2)
        x.PrependProbe_void_with_args(methodAddress, [(opcode1, arg1); (opcode2, arg2)], x.tokens.void_bool_uint32_sig, &beforeInstr)

    member private x.PrependProbe_void_uint32_uint32(methodAddress : uint64, arg1 : uint32, arg2 : uint32, beforeInstr : ilInstr byref) =
        let opcode1 = OpCodes.Ldc_I4 |> VSharp.Concolic.OpCode // TODO: previously was ldc_i, but it seems to be wrong
        let opcode2 = OpCodes.Ldc_I4 |> VSharp.Concolic.OpCode // TODO: previously was ldc_i, but it seems to be wrong
//        let opcode1 = ldc_i
//        let opcode2 = ldc_i
        let arg1 = Arg32 (int32 arg1)
        let arg2 = Arg32 (int32 arg2)
        x.PrependProbe_void_with_args(methodAddress, [(opcode1, arg1); (opcode2, arg2)], x.tokens.void_uint32_uint32_sig, &beforeInstr)

    member private x.PrependProbe_void_uint32_uint32_uint64(methodAddress : uint64, arg1 : uint32, arg2 : uint32, arg3 : uint64, beforeInstr : ilInstr byref) =
        let opcode1 = OpCodes.Ldc_I4 |> VSharp.Concolic.OpCode // TODO: previously was ldc_i, but it seems to be wrong
        let opcode2 = OpCodes.Ldc_I4 |> VSharp.Concolic.OpCode // TODO: previously was ldc_i, but it seems to be wrong
        let opcode3 = OpCodes.Ldc_I8 |> VSharp.Concolic.OpCode // TODO: previously was ldc_i, but it seems to be wrong
//        let opcode1 = ldc_i
//        let opcode2 = ldc_i
//        let opcode3 = ldc_i
        let arg1 = Arg32 (int32 arg1)
        let arg2 = Arg32 (int32 arg2)
        let arg3 = Arg64 (int64 arg3)
        x.PrependProbe_void_with_args(methodAddress, [(opcode1, arg1); (opcode2, arg2); (opcode3, arg3)], x.tokens.void_uint32_uint32_uint64_sig, &beforeInstr)

    member private x.AppendProbe_void_with_args(methodAddress : uint64, args : (opcode * ilInstrOperand) list, signature, afterInstr : ilInstr) =
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

    member private x.AppendProbe_void(methodAddress : uint64, afterInstr : ilInstr) =
        x.AppendProbe_void_with_args(methodAddress, [], x.tokens.void_sig, afterInstr)

    member private x.AppendProbe_void_int32(methodAddress : uint64, arg : int32, afterInstr : ilInstr) =
        let opcode = OpCodes.Ldc_I4 |> VSharp.Concolic.OpCode // TODO: previously was ldc_i, but it seems to be wrong
//        let opcode = ldc_i
        let arg = Arg32 arg
        x.AppendProbe_void_with_args(methodAddress, [(opcode, arg)], x.tokens.void_int32_sig, afterInstr)

    member private x.AppendProbe_void_uint32(methodAddress : uint64, arg : uint32, afterInstr : ilInstr) =
        let opcode = OpCodes.Ldc_I4 |> VSharp.Concolic.OpCode // TODO: previously was ldc_i, but it seems to be wrong
//        let opcode = ldc_i
        let arg = Arg32 (int32 arg)
        x.AppendProbe_void_with_args(methodAddress, [(opcode, arg)], x.tokens.void_uint32_sig, afterInstr)

    member private x.PlaceEnterProbe (firstInstr : ilInstr byref) =
        x.PrependProbe_void_uint32_uint32(probes.enter, uint32 x.m.MetadataToken, x.rewriter.MaxStackSize, &firstInstr)

    member private x.PlaceLeaveProbe(instr : ilInstr byref) =
        let returnsSomething = Reflection.getMethodReturnType x.m <> typeof<System.Void>
        x.PrependProbe_void_uint32(probes.leave, (if returnsSomething then 1u else 0u), &instr)

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
                //let dumpedInfo = ...
                //prependProbe_void_ulong(reinterpret_cast<ULONGLONG>(DumpInstructionAddress), (unsigned long)dumpedInfo, prefix ? *prefix : pInstr);
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
                | OpCodeValues.Ldarga_S
                | OpCodeValues.Ldloca_S
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
                | OpCodeValues.Ldc_R8
                | OpCodeValues.Pop
                | OpCodeValues.Ldtoken
                | OpCodeValues.Arglist
                | OpCodeValues.Ldftn
                | OpCodeValues.Ldarga
                | OpCodeValues.Ldloca
                | OpCodeValues.Sizeof ->
                    let pushes = instr.opcode.StackBehaviourPush
                    let pops = instr.opcode.StackBehaviourPop
                    let probe =
                        if   pushes = 1u && pops = 0u then probes.push1Concrete
                        elif pushes = 0u && pops = 1u then probes.pop1
                        else __unreachable__()
                    x.AppendProbe_void(probe, instr)

                | OpCodeValues.Call
                | OpCodeValues.Callvirt
                | OpCodeValues.Newobj ->
                    match instr.arg with
                    | Arg32 token ->
                        let callee = Reflection.resolveMethod x.m token
                        let hasThis = callee.CallingConvention.HasFlag(CallingConventions.HasThis)
                        let argsCount = callee.GetParameters().Length
                        let returnsSomething = Reflection.getMethodReturnType callee <> typeof<System.Void>
                        let count =
                            if hasThis && opcodeValue <> OpCodeValues.Newobj then argsCount + 1
                            else argsCount
                        let prependTarget = if hasPrefix then &prefix else &instr
                        x.PrependProbe_void_uint32_uint32(probes.call, (if opcodeValue = OpCodeValues.Callvirt then 0u else uint32 token), uint32 count, &prependTarget);
                        x.AppendProbe_void_uint32(probes.finalizeCall, (if returnsSomething then 1u else 0u), instr);

                        if opcodeValue = OpCodeValues.Newobj then
                            x.AppendProbe_void(probes.push1Concrete, instr)
                    | _ -> __unreachable__()
                | OpCodeValues.Calli -> __notImplemented__()

                | OpCodeValues.Ret ->
                    assert (not hasPrefix)
                    atLeastOneReturnFound <- true
                    x.PlaceLeaveProbe &instr
                | OpCodeValues.Throw ->
                    atLeastOneReturnFound <- true

                // Branchings
                | OpCodeValues.Br_S
                | OpCodeValues.Brfalse_S
                | OpCodeValues.Brtrue_S
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
                | OpCodeValues.Br
                | OpCodeValues.Brfalse
                | OpCodeValues.Brtrue
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
                | OpCodeValues.Switch ->
                    let pops = instr.opcode.StackBehaviourPop
                    let prependTarget = if hasPrefix then &prefix else &instr
                    x.PrependProbe_void_int32(probes.pop, int32 pops, &prependTarget);

                // Symbolic stack instructions
                | OpCodeValues.Ldarg_0
                | OpCodeValues.Ldarg_1
                | OpCodeValues.Ldarg_2
                | OpCodeValues.Ldarg_3
                | OpCodeValues.Ldloc_0
                | OpCodeValues.Ldloc_1
                | OpCodeValues.Ldloc_2
                | OpCodeValues.Ldloc_3
                | OpCodeValues.Stloc_0
                | OpCodeValues.Stloc_1
                | OpCodeValues.Stloc_2
                | OpCodeValues.Stloc_3
                | OpCodeValues.Ldarg_S
                | OpCodeValues.Starg_S
                | OpCodeValues.Ldloc_S
                | OpCodeValues.Stloc_S
                | OpCodeValues.Dup
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
                | OpCodeValues.Neg
                | OpCodeValues.Not
                | OpCodeValues.Conv_I1
                | OpCodeValues.Conv_I2
                | OpCodeValues.Conv_I4
                | OpCodeValues.Conv_I8
                | OpCodeValues.Conv_R4
                | OpCodeValues.Conv_R8
                | OpCodeValues.Conv_U4
                | OpCodeValues.Conv_U8
                | OpCodeValues.Conv_R_Un
                | OpCodeValues.Refanyval
                | OpCodeValues.Mkrefany
                | OpCodeValues.Conv_U2
                | OpCodeValues.Conv_U1
                | OpCodeValues.Conv_I
                | OpCodeValues.Conv_U
                | OpCodeValues.Ceq
                | OpCodeValues.Cgt
                | OpCodeValues.Cgt_Un
                | OpCodeValues.Clt
                | OpCodeValues.Clt_Un
                | OpCodeValues.Ldarg
                | OpCodeValues.Starg
                | OpCodeValues.Ldloc
                | OpCodeValues.Stloc
                | OpCodeValues.Localloc
                | OpCodeValues.Refanytype

                // Rest instructions
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
                | OpCodeValues.Ldind_Ref
                | OpCodeValues.Stind_Ref
                | OpCodeValues.Stind_I1
                | OpCodeValues.Stind_I2
                | OpCodeValues.Stind_I4
                | OpCodeValues.Stind_I8
                | OpCodeValues.Stind_R4
                | OpCodeValues.Stind_R8
                | OpCodeValues.Cpobj
                | OpCodeValues.Ldobj
                | OpCodeValues.Ldstr
                | OpCodeValues.Castclass
                | OpCodeValues.Isinst
                | OpCodeValues.Unbox
                | OpCodeValues.Ldfld
                | OpCodeValues.Ldflda
                | OpCodeValues.Stfld
                | OpCodeValues.Ldsfld
                | OpCodeValues.Ldsflda
                | OpCodeValues.Stsfld
                | OpCodeValues.Stobj
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
                | OpCodeValues.Box
                | OpCodeValues.Newarr
                | OpCodeValues.Ldlen
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
                | OpCodeValues.Stelem_I
                | OpCodeValues.Stelem_I1
                | OpCodeValues.Stelem_I2
                | OpCodeValues.Stelem_I4
                | OpCodeValues.Stelem_I8
                | OpCodeValues.Stelem_R4
                | OpCodeValues.Stelem_R8
                | OpCodeValues.Stelem_Ref
                | OpCodeValues.Ldelem
                | OpCodeValues.Stelem
                | OpCodeValues.Unbox_Any
                | OpCodeValues.Conv_Ovf_I1
                | OpCodeValues.Conv_Ovf_U1
                | OpCodeValues.Conv_Ovf_I2
                | OpCodeValues.Conv_Ovf_U2
                | OpCodeValues.Conv_Ovf_I4
                | OpCodeValues.Conv_Ovf_U4
                | OpCodeValues.Conv_Ovf_I8
                | OpCodeValues.Conv_Ovf_U8
                | OpCodeValues.Ckfinite
                | OpCodeValues.Conv_Ovf_I
                | OpCodeValues.Conv_Ovf_U
                | OpCodeValues.Add_Ovf
                | OpCodeValues.Add_Ovf_Un
                | OpCodeValues.Mul_Ovf
                | OpCodeValues.Mul_Ovf_Un
                | OpCodeValues.Sub_Ovf
                | OpCodeValues.Sub_Ovf_Un
                | OpCodeValues.Stind_I
                | OpCodeValues.Ldvirtftn
                | OpCodeValues.Initobj
                | OpCodeValues.Cpblk
                | OpCodeValues.Initblk
                | OpCodeValues.Rethrow ->
                    // TODO: this clause should be removed!
                    let pushes = instr.opcode.StackBehaviourPush
                    let pops = instr.opcode.StackBehaviourPop
                    if not <| x.rewriter.IsLastEHInstr instr && not <| x.rewriter.IsEnd instr.next then
                        if pushes > 0u then
                            x.AppendProbe_void_int32(probes.push, int32 pushes, instr);
                        if pops > 0u then
                            x.AppendProbe_void_int32(probes.pop, int32 pops, instr);
                // Ignored instructions
                | OpCodeValues.Nop
                | OpCodeValues.Break
                | OpCodeValues.Jmp
                | OpCodeValues.Endfinally
                | OpCodeValues.Leave
                | OpCodeValues.Leave_S
                | OpCodeValues.Endfilter -> ()
                | _ -> __unreachable__()

                if hasPrefix && op.OpCodeType <> OpCodeType.Prefix then
                    hasPrefix <- false
            | SwitchArg -> ()
        assert(atLeastOneReturnFound)

    member x.Instrument(body : rawMethodBody) =
        assert(x.rewriter = null)
        x.tokens <- body.tokens
        x.rewriter <- ILRewriter(body)
        x.m <- x.rewriter.Method
        let result =
            if Instrumenter.instrumentedFunctions.Add x.m then
                Logger.trace "Instrumenting %s (token = %u)" (Reflection.methodToString x.m) body.properties.token
                x.rewriter.Import()
//                x.rewriter.PrintInstructions "before instrumentation" probes
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
                { properties = {ilCodeSize = body.properties.ilCodeSize; maxStackSize = body.properties.maxStackSize}; il = body.il; ehs = body.ehs}

        x.rewriter <- null
        result
