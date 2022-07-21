namespace VSharp

open System
open VSharp
open System.Reflection
open System.Reflection.Emit

type term = VSharp.Core.term
type state = VSharp.Core.state

type ipTransition =
    | FallThrough of offset
    | Return
    | UnconditionalBranch of offset
    | ConditionalBranch of offset * offset list
    // TODO: use this thing? #do
    | ExceptionMechanism

module TokenResolver =
    let private extractToken = NumberCreator.extractInt32

    let resolveFieldFromMetadata methodBase ilBytes = extractToken ilBytes >> Reflection.resolveField methodBase
    let resolveTypeFromMetadata methodBase ilBytes = extractToken ilBytes >> Reflection.resolveType methodBase
    let resolveMethodFromMetadata methodBase ilBytes = extractToken ilBytes >> Reflection.resolveMethod methodBase
    let resolveTokenFromMetadata methodBase ilBytes = extractToken ilBytes >> Reflection.resolveToken methodBase

module Instruction =

    let private operandType2operandSize = [| 4<offsets>; 4<offsets>; 4<offsets>; 8<offsets>; 4<offsets>
                                             0<offsets>; -1<offsets>; 8<offsets>; 4<offsets>; 4<offsets>
                                             4<offsets>; 4<offsets>; 4<offsets>; 4<offsets>; 2<offsets>
                                             1<offsets>; 1<offsets>; 4<offsets>; 1<offsets>|]

    let private jumpTargetsForNext (opCode : OpCode) _ (pos : offset) =
        let nextInstruction = pos + Offset.from opCode.Size + operandType2operandSize.[int opCode.OperandType]
        FallThrough nextInstruction

    let private jumpTargetsForBranch (opCode : OpCode) ilBytes (pos : offset) =
        let opcodeSize = Offset.from opCode.Size
        let offset =
            match opCode.OperandType with
            | OperandType.InlineBrTarget -> NumberCreator.extractInt32 ilBytes (pos + opcodeSize)
            | _ -> NumberCreator.extractInt8 ilBytes (pos + opcodeSize)

        let nextInstruction = pos + Offset.from opCode.Size + operandType2operandSize.[int opCode.OperandType]
        if offset = 0 && opCode <> OpCodes.Leave && opCode <> OpCodes.Leave_S
        then UnconditionalBranch nextInstruction
        else UnconditionalBranch <| Offset.from offset + nextInstruction

    let private inlineBrTarget extract (opCode : OpCode) ilBytes (pos : offset) =
        let opcodeSize = Offset.from opCode.Size
        let offset = extract ilBytes (pos + opcodeSize)
        let nextInstruction = pos + opcodeSize + operandType2operandSize.[int opCode.OperandType]
        ConditionalBranch(nextInstruction, [nextInstruction + offset])

    let private inlineSwitch (opCode : OpCode) ilBytes (pos : offset) =
        let opcodeSize = Offset.from opCode.Size
        let n = NumberCreator.extractUnsignedInt32 ilBytes (pos + opcodeSize) |> int
        let nextInstruction = pos + opcodeSize + 4<offsets> * n + 4<offsets>
        let nextOffsets =
            List.init n (fun x -> nextInstruction + Offset.from (NumberCreator.extractInt32 ilBytes (pos + opcodeSize + 4<offsets> * (x + 1))))
        ConditionalBranch(nextInstruction, nextOffsets)

    let private jumpTargetsForReturn _ _ _ = Return
    let private jumpTargetsForThrow _ _ _ = ExceptionMechanism

    let findNextInstructionOffsetAndEdges (opCode : OpCode) =
        match opCode.FlowControl with
        | FlowControl.Next
        | FlowControl.Call
        | FlowControl.Break
        | FlowControl.Meta -> jumpTargetsForNext
        | FlowControl.Branch -> jumpTargetsForBranch
        | FlowControl.Cond_Branch ->
            match opCode.OperandType with
            | OperandType.InlineBrTarget -> inlineBrTarget NumberCreator.extractOffset
            | OperandType.ShortInlineBrTarget -> inlineBrTarget (fun x y -> NumberCreator.extractInt8 x y |> Offset.from)
            | OperandType.InlineSwitch -> inlineSwitch
            | _ -> __notImplemented__()
        | FlowControl.Return -> jumpTargetsForReturn
        | FlowControl.Throw -> jumpTargetsForThrow
        | _ -> __notImplemented__()
        <| opCode

    let isLeaveOpCode (opCode : OpCode) = opCode = OpCodes.Leave || opCode = OpCodes.Leave_S

    let private isCallOpCode (opCode : OpCode) =
        opCode = OpCodes.Call
        || opCode = OpCodes.Calli
        || opCode = OpCodes.Callvirt
        || opCode = OpCodes.Tailcall
    let private isNewObjOpCode (opCode : OpCode) =
        opCode = OpCodes.Newobj
    let isDemandingCallOpCode (opCode : OpCode) =
        isCallOpCode opCode || isNewObjOpCode opCode
    let isFinallyClause (ehc : VSharp.ExceptionHandlingClause) =
        match ehc.ehcType with Finally -> true | _ -> false
    let isFilterClause (ehc : VSharp.ExceptionHandlingClause) =
        match ehc.ehcType with ehcType.Filter _ -> true | _ -> false
    let isCatchClause (ehc : VSharp.ExceptionHandlingClause) =
        match ehc.ehcType with Catch _ -> true | _ -> false

    let shouldExecuteFinallyClause (src : offset) (dst : offset) (ehc : VSharp.ExceptionHandlingClause) =
//        let srcOffset, dstOffset = src.Offset(), dst.Offset()
        let isInside offset = ehc.tryOffset <= offset && offset < ehc.tryOffset + ehc.tryLength
        isInside src && not <| isInside dst

    let internal (|Ret|_|) (opCode : OpCode) = if opCode = OpCodes.Ret then Some () else None
    let (|Call|_|) (opCode : OpCode) = if opCode = OpCodes.Call then Some () else None
    let (|CallVirt|_|) (opCode : OpCode) = if opCode = OpCodes.Callvirt then Some () else None
    let (|Calli|_|) (opCode : OpCode) = if opCode = OpCodes.Calli then Some () else None
    let (|TailCall|_|) (opCode : OpCode) = if opCode = OpCodes.Tailcall then Some () else None
    let (|NewObj|_|) (opCode : OpCode) = if opCode = OpCodes.Newobj then Some () else None

    let private methodBytesCache = System.Collections.Generic.Dictionary<MethodBase, byte [] * VSharp.ExceptionHandlingClause []>()

    let private rewriteMethodBytes (m : MethodBase) =
        let methodBody = m.GetMethodBody()
        if methodBody = null then Array.empty, Array.empty
        else
            let ilBytes = methodBody.GetILAsByteArray()
            let methodModule = m.Module
            let moduleName = methodModule.FullyQualifiedName
            let assemblyName = methodModule.Assembly.FullName
            let ehcs = System.Collections.Generic.Dictionary<int, System.Reflection.ExceptionHandlingClause>()
            let props : VSharp.Concolic.rawMethodProperties =
                {token = uint m.MetadataToken; ilCodeSize = uint ilBytes.Length; assemblyNameLength = 0u; moduleNameLength = 0u; maxStackSize = uint methodBody.MaxStackSize; signatureTokensLength = 0u}
            let tokens = System.Runtime.Serialization.FormatterServices.GetUninitializedObject(typeof<VSharp.Concolic.signatureTokens>) :?> VSharp.Concolic.signatureTokens
            let createEH (eh : System.Reflection.ExceptionHandlingClause) : VSharp.Concolic.rawExceptionHandler =
                let matcher = if eh.Flags = ExceptionHandlingClauseOptions.Filter then eh.FilterOffset else eh.HandlerOffset // TODO: need catch type token?
                ehcs.Add(matcher, eh)
                {flags = int eh.Flags; tryOffset = uint eh.TryOffset; tryLength = uint eh.TryLength; handlerOffset = uint eh.HandlerOffset; handlerLength = uint eh.HandlerLength; matcher = uint matcher}
            let ehs = methodBody.ExceptionHandlingClauses |> Seq.map createEH |> Array.ofSeq
            let body : VSharp.Concolic.rawMethodBody =
                {properties = props; assembly = assemblyName; moduleName = moduleName; tokens = tokens; il = ilBytes; ehs = ehs}
            let rewriter = ILRewriter(body)
            rewriter.Import()
            let result = rewriter.Export()
            let parseEH (eh : VSharp.Concolic.rawExceptionHandler) =
                let oldEH = ehcs.[int eh.matcher]
                let ehcType =
                    if oldEH.Flags = ExceptionHandlingClauseOptions.Filter then ehcType.Filter (eh.matcher |> int |> Offset.from)
                    elif oldEH.Flags = ExceptionHandlingClauseOptions.Fault || oldEH.Flags = ExceptionHandlingClauseOptions.Finally then Finally
                    else Catch oldEH.CatchType
                {tryOffset = eh.tryOffset |> int |> Offset.from
                 tryLength = eh.tryLength |> int |> Offset.from
                 handlerOffset = eh.handlerOffset |> int |> Offset.from
                 handlerLength = eh.handlerLength |> int |> Offset.from
                 ehcType = ehcType }
            result.il, Array.map parseEH result.ehs

    let getILBytes (m : MethodBase) : byte [] =
        let result : ref<byte [] * VSharp.ExceptionHandlingClause []> = ref (null, null)
        if methodBytesCache.TryGetValue(m, result) then fst result.Value
        else
            let ilBytes = rewriteMethodBytes m
            assert(ilBytes <> (null, null))
            methodBytesCache.Add(m, ilBytes)
            fst ilBytes

    let getEHSBytes (m : MethodBase) =
        let result : ref<byte [] * VSharp.ExceptionHandlingClause []> = ref (null, null)
        if methodBytesCache.TryGetValue(m, result) then snd result.Value
        else
            let ilBytes = rewriteMethodBytes m
            assert(ilBytes <> (null, null))
            methodBytesCache.Add(m, ilBytes)
            snd ilBytes

    let parseInstruction (m : MethodBase) pos =
        let ilBytes : byte [] = getILBytes m
        OpCodeOperations.getOpCode ilBytes pos

    let (|EndFinally|_|) = function
        | Instruction(offset, m) when parseInstruction m offset = OpCodes.Endfinally -> Some ()
        | _ -> None

    let rec (|InstructionEndingIp|_|) = function
        | Instruction(offset, m)
        | InFilterHandler(offset, m, _, _) -> Some (offset, m)
        | Leave(ip, _, _, _) -> (|InstructionEndingIp|_|) ip
        | _ -> None

    let parseCallSite (m : MethodBase) pos =
        let ilBytes = getILBytes m
        let opCode = OpCodeOperations.getOpCode ilBytes pos
        let calledMethod = TokenResolver.resolveMethodFromMetadata m ilBytes (pos + Offset.from opCode.Size)
        opCode, calledMethod

    let getIpTransition (m : MethodBase) pos =
        let ilBytes = getILBytes m
        let opCode = OpCodeOperations.getOpCode ilBytes pos
        findNextInstructionOffsetAndEdges opCode ilBytes pos

    let unconditionalBranchTarget (m : MethodBase) pos =
        match getIpTransition m pos with
        | UnconditionalBranch target -> target
        | _ -> __unreachable__()

    let fallThroughTarget (m : MethodBase) pos =
        match getIpTransition m pos with
        | FallThrough target -> target
        | _ -> __unreachable__()

    let conditionalBranchTarget (m : MethodBase) pos =
        match getIpTransition m pos with
        | ConditionalBranch(fallThrough, rest) -> fallThrough, rest
        | _ -> __unreachable__()
