namespace VSharp.Concolic

open System
open System.Reflection.Emit
open VSharp
open VSharp.Interpreter.IL

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
    mutable arg : ilInstrOperand
}

type ehClauseMatcher =
    | ClassToken of uint32
    | Filter of ilInstr

type ehClause = {
    flags : int
    tryBegin : ilInstr
    tryEnd : ilInstr
    handlerBegin : ilInstr
    mutable handlerEnd : ilInstr
    matcher : ehClauseMatcher
}

[<AllowNullLiteral>]
type ILRewriter(body : rawMethodBody) =
    // If this line throws exception, we should improve resolving assemblies by names. Probably we should track assemblies from the directory of executed assembly
    let m = Reflection.resolveMethodBase body.assembly body.moduleName (int body.properties.token)
    let code = body.il
    let codeSize = Array.length code
    let mutable instrCount = 0u
    let mutable maxStackSize = body.properties.maxStackSize
    let offsetToInstr : ilInstr array = Array.zeroCreate (codeSize + 1)
    let mutable ehs : ehClause array = Array.empty
    let il : ilInstr = System.Runtime.Serialization.FormatterServices.GetUninitializedObject typeof<ilInstr> :?> ilInstr
//    = { prev = ref il; next = ref il; opcode = OpCodes.Nop; offset = 0u; arg = Arg8 0y}

    let invalidProgram reason =
        Logger.error "Invalid program: %s" reason
        raise <| IncorrectCIL reason

    let adjustState (instr : ilInstr) =
        maxStackSize <- maxStackSize + instr.opcode.StackBehaviourPush

    member x.ILInstrToString (probes : probes) (instr : ilInstr) =
        let instr, arg =
            match instr.opcode with
            | OpCode op ->
                let arg =
                    if op = OpCodes.Call || op = OpCodes.Callvirt || op = OpCodes.Newobj then
                        match instr.arg with
                        | Arg32 token ->
                            let callee = Reflection.resolveMethod m token
                            Reflection.methodToString callee
                        | _ -> __unreachable__()
                    elif op = OpCodes.Calli then ""
                    else
                        match instr.arg with
                        | NoArg -> ""
                        | Arg8 a -> a.ToString()
                        | Arg16 a -> a.ToString()
                        | Arg32 a -> a.ToString()
                        | Arg64 a -> probes.AddressToString a
                        | Target t ->
                            match t.opcode with
                            | OpCode op -> op.Name
                            | SwitchArg _ -> "<SwitchArg>"
                op.Name, arg
            | SwitchArg -> "<SwitchArg>", ""
        instr + " " + arg

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
        {prev = il; next = il; opcode = opcode; offset = 0u; arg = Arg8 0uy}

    member x.NewInstr opcode =
        instrCount <- instrCount + 1u
        {prev = il; next = il; opcode = OpCode opcode; offset = 0u; arg = Arg8 0uy}

    member x.CopyInstruction instr =
        instrCount <- instrCount + 1u
        {prev = instr.prev; next = instr.next; opcode = instr.opcode; offset = instr.offset; arg = instr.arg}

    member x.CopyInstructions() =
        let result = Array.zeroCreate<ilInstr> <| int instrCount
        let mutable index = 0
        x.TraverseProgram (fun instr ->
            result.[index] <- instr
            index <- index + 1)
        result

    member x.InstrFromOffset offset =
        if offset > codeSize then
            invalidProgram (sprintf "Too large offset %d requested!" offset)
        offsetToInstr.[offset]

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
            if x.InstrEq eh.handlerEnd where then
                eh.handlerEnd <- what
        adjustState what

    member x.Method = m
    member x.InstructionsCount with get() = instrCount
    member x.MaxStackSize with get() = maxStackSize

    member x.IsLastEHInstr (instr : ilInstr) =
        Array.exists (fun eh -> x.InstrEq eh.handlerEnd instr) ehs
    member x.ILList = il

    member private x.ImportIL() =
        // Set the sentinel instruction
        il.next <- il
        il.prev <- il
        offsetToInstr.[codeSize] <- il

        // TODO: unify code with Instruction.fs (parseInstruction)
        let mutable branch = false
        let mutable offset = 0
        while offset < codeSize do
            let startOffset = offset
            let opcode = int16 code.[offset]
            offset <- offset + 1

            let op =
                if opcode = OpCodes.Prefix1.Value then
                    if offset >= codeSize then invalidProgram "IL stream unexpectedly ended!"
                    offset <- offset + 1
                    Instruction.twoBytesOpCodes.[int code.[offset - 1]]
                else
                    Instruction.singleByteOpCodes.[int opcode]

            let size =
                match op.OperandType with
                | OperandType.InlineNone
                | OperandType.InlineSwitch -> 0
                | OperandType.ShortInlineVar
                | OperandType.ShortInlineI
                | OperandType.ShortInlineBrTarget -> 1
                | OperandType.InlineVar -> 2
                | OperandType.InlineI
                | OperandType.InlineMethod
                | OperandType.InlineType
                | OperandType.InlineString
                | OperandType.InlineSig
                | OperandType.InlineTok
                | OperandType.ShortInlineR
                | OperandType.InlineField
                | OperandType.InlineBrTarget -> 4
                | OperandType.InlineI8
                | OperandType.InlineR -> 8
                | _ -> __unreachable__()

            if offset + size > codeSize then invalidProgram "IL stream unexpectedly ended!"

            let instr = x.NewInstr (OpCode op)
            instr.offset <- uint32 startOffset
            offsetToInstr.[startOffset] <- instr
            x.InsertBefore(il, instr)
            offsetToInstr.[startOffset] <- instr
            match op.OperandType with
            | OperandType.InlineNone -> instr.arg <- NoArg
            | OperandType.ShortInlineVar
            | OperandType.ShortInlineI ->
                instr.arg <- Arg8 code.[offset]
            | OperandType.InlineVar ->
                instr.arg <- Arg16 <| BitConverter.ToInt16(code, offset)
            | OperandType.InlineI
            | OperandType.InlineMethod
            | OperandType.InlineType
            | OperandType.InlineString
            | OperandType.InlineSig
            | OperandType.InlineTok
            | OperandType.ShortInlineR
            | OperandType.InlineField ->
                instr.arg <- Arg32 <| BitConverter.ToInt32(code, offset)
            | OperandType.InlineI8
            | OperandType.InlineR ->
                instr.arg <- Arg64 <| BitConverter.ToInt64(code, offset)
            | OperandType.ShortInlineBrTarget ->
                instr.arg <- Arg32 <| offset + 1 + int (sbyte code.[offset])
                branch <- true;
            | OperandType.InlineBrTarget ->
                instr.arg <- Arg32 <| offset + 4 + BitConverter.ToInt32(code, offset)
                branch <- true;
            | OperandType.InlineSwitch ->
                if offset + sizeof<int> > codeSize then
                    invalidProgram "IL stream unexpectedly ended!"
                let targetsCount = BitConverter.ToInt32(code, offset)
                instr.arg <- Arg32 targetsCount
                offset <- offset + sizeof<int>
                let baseOffset = offset + targetsCount * sizeof<int>
                for i in 1 .. targetsCount do
                    if offset + sizeof<int> > codeSize then
                        invalidProgram "IL stream unexpectedly ended!"
                    let instr = x.NewInstr SwitchArg
                    instr.arg <- Arg32 <| baseOffset + BitConverter.ToInt32(code, offset)
                    offset <- offset + sizeof<int>
                    x.InsertBefore(il, instr)
                branch <- true
            | _ -> invalidProgram "Unexpected operand type!"

            offset <- offset + size
//            Logger.trace "Imported %O (offsets %d .. %d)" instr.opcode startOffset offset

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

    member private x.ImportEH() =
        let parseEH (raw : rawExceptionHandler) = {
            flags = raw.flags
            tryBegin = x.InstrFromOffset <| int raw.tryOffset
            tryEnd = x.InstrFromOffset <| int (raw.tryOffset + raw.tryLength)
            handlerBegin = x.InstrFromOffset <| int raw.handlerOffset
            handlerEnd = (x.InstrFromOffset <| int (raw.handlerOffset + raw.handlerLength)).prev
            matcher = if raw.flags &&& 0x0001 = 0 then ClassToken raw.matcher else Filter (x.InstrFromOffset <| int raw.matcher)
        }
        ehs <- Array.map parseEH body.ehs

    member x.Import() =
        x.ImportIL()
        x.ImportEH()
        maxStackSize <- body.properties.maxStackSize

    member x.Export() =
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
                        assert(success)
                        offset <- offset + sizeof<int16>
                    | Arg32 arg ->
                        let success = BitConverter.TryWriteBytes(Span(outputIL, offset, sizeof<int32>), arg)
                        assert(success)
                        offset <- offset + sizeof<int32>
                    | Arg64 arg ->
                        let success = BitConverter.TryWriteBytes(Span(outputIL, offset, sizeof<int64>), arg)
                        assert(success)
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
//                Logger.trace "Exported %O (offsets %d .. %d)" instr.opcode instr.offset offset)

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
                            assert(success)
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
                                        let op = Instruction.singleByteOpCodes.[op.Value - OpCodes.Br_S.Value + OpCodes.Br.Value |> int];
                                        assert(op.Value >= OpCodes.Br.Value && op.Value <= OpCodes.Blt_Un.Value)
                                        instr.opcode <- OpCode op
                                    tryAgain <- true
                                else outputIL.[int instr.next.offset - sizeof<int8>] <- byte (int8 delta)
                            | OperandType.InlineBrTarget ->
                                let success = BitConverter.TryWriteBytes(Span(outputIL, int instr.next.offset - sizeof<int32>, sizeof<int32>), delta)
                                assert(success)
                            | _ -> __unreachable__()
                        | _ -> ())

        let methodProps = {ilCodeSize = uint32 il.offset; maxStackSize = maxStackSize}
        let encodeEH (eh : ehClause) = {
            flags = eh.flags
            tryOffset = eh.tryBegin.offset
            tryLength = eh.tryEnd.offset - eh.tryBegin.offset
            handlerOffset = eh.handlerBegin.offset
            handlerLength = eh.handlerEnd.next.offset - eh.handlerBegin.offset
            matcher =
                match eh.matcher with
                | ClassToken tok -> tok
                | Filter instr -> instr.offset
        }
        let ehs = Array.map encodeEH ehs
        {properties = methodProps; il = Array.truncate (int methodProps.ilCodeSize) outputIL; ehs = ehs}
//        let result = {properties = methodProps; il = Array.truncate (int methodProps.ilCodeSize) outputIL; ehs = ehs}
//        if result.properties.ilCodeSize <> body.properties.ilCodeSize || result.properties.maxStackSize <> body.properties.maxStackSize then
//            raise <| Exception(sprintf "Imported and exported properties mismatch (%d, %d) vs (%d, %d)!" body.properties.ilCodeSize body.properties.maxStackSize result.properties.ilCodeSize result.properties.maxStackSize)
//        if result.ehs <> body.ehs then
//            raise <| Exception("Imported and exported handlers mismatch!")
//        if result.il.Length <> body.il.Length then
//            raise <| Exception(sprintf "Imported and exported IL lengths mismatch (%d and %d)!" body.il.Length result.il.Length)
//        else
//            for i in 0 .. result.il.Length - 1 do
//                if body.il.[i] <> result.il.[i] then
//                    Logger.trace "Mismatch in byte %d of imported and exported ILs (%d and %d)!" i body.il.[i] result.il.[i]
//        result
//        {properties = methodProps; il = outputIL; ehs = ehs}
