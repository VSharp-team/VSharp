namespace VSharp

open System
open VSharp
open System.Reflection
open System.Reflection.Emit
open VSharp.Core

type offset = int
type term = VSharp.Core.term
type state = VSharp.Core.state

type ip = VSharp.Core.ip
type level = VSharp.Core.level

type ipTransition =
    | FallThrough of offset
    | Return
    | UnconditionalBranch of offset
    | ConditionalBranch of offset * offset list
    // TODO: use this thing? #do
    | ExceptionMechanism

module NumberCreator =
    let public extractInt32 (ilBytes : byte []) pos =
        BitConverter.ToInt32(ilBytes, pos)
    let public extractUnsignedInt32 (ilBytes : byte []) pos =
        BitConverter.ToUInt32(ilBytes, pos)
    let public extractUnsignedInt16 (ilBytes : byte []) pos =
        BitConverter.ToUInt16(ilBytes, pos)
    let public extractInt64 (ilBytes : byte []) pos =
        BitConverter.ToInt64(ilBytes, pos)
    let public extractInt8 (ilBytes : byte []) pos =
        ilBytes.[pos] |> sbyte |> int
    let public extractUnsignedInt8 (ilBytes : byte []) pos =
        ilBytes.[pos]
    let public extractFloat64 (ilBytes : byte []) pos =
        BitConverter.ToDouble(ilBytes, pos)
    let public extractFloat32 (ilBytes : byte []) pos =
        BitConverter.ToSingle(ilBytes, pos)

module TokenResolver =
    let private extractToken = NumberCreator.extractInt32

    let resolveFieldFromMetadata methodBase ilBytes = extractToken ilBytes >> Reflection.resolveField methodBase
    let resolveTypeFromMetadata methodBase ilBytes = extractToken ilBytes >> Reflection.resolveType methodBase
    let resolveMethodFromMetadata methodBase ilBytes = extractToken ilBytes >> Reflection.resolveMethod methodBase
    let resolveTokenFromMetadata methodBase ilBytes = extractToken ilBytes >> Reflection.resolveToken methodBase

module Instruction =

    let private operandType2operandSize = [| 4; 4; 4; 8; 4; 0; -1; 8; 4; 4; 4; 4; 4; 4; 2; 1; 1; 4; 1|]

    let private jumpTargetsForNext (opCode : OpCode) _ pos =
        let nextInstruction = pos + opCode.Size + operandType2operandSize.[int opCode.OperandType]
        FallThrough nextInstruction

    let private jumpTargetsForBranch (opCode : OpCode) ilBytes pos =
        let offset =
            match opCode.OperandType with
            | OperandType.InlineBrTarget -> NumberCreator.extractInt32 ilBytes (pos + opCode.Size)
            | _ -> NumberCreator.extractInt8 ilBytes (pos + opCode.Size)

        let nextInstruction = pos + opCode.Size + operandType2operandSize.[int opCode.OperandType]
        if offset = 0 && opCode <> OpCodes.Leave && opCode <> OpCodes.Leave_S
        then UnconditionalBranch nextInstruction
        else UnconditionalBranch <| offset + nextInstruction

    let private inlineBrTarget extract (opCode : OpCode) ilBytes pos =
        let offset = extract ilBytes (pos + opCode.Size)
        let nextInstruction = pos + opCode.Size + operandType2operandSize.[int opCode.OperandType]
        ConditionalBranch(nextInstruction, [nextInstruction + offset])

    let private inlineSwitch (opCode : OpCode) ilBytes pos =
        let n = NumberCreator.extractUnsignedInt32 ilBytes (pos + opCode.Size) |> int
        let nextInstruction = pos + opCode.Size + 4 * n + 4
        let nextOffsets =
            List.init n (fun x -> nextInstruction + NumberCreator.extractInt32 ilBytes (pos + opCode.Size + 4 * (x + 1)))
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
            | OperandType.InlineBrTarget -> inlineBrTarget NumberCreator.extractInt32
            | OperandType.ShortInlineBrTarget -> inlineBrTarget NumberCreator.extractInt8
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
    let isFinallyClause (ehc : ExceptionHandlingClause) =
        match ehc.ehcType with Finally -> true | _ -> false
    let isFilterClause (ehc : ExceptionHandlingClause) =
        match ehc.ehcType with Filter _ -> true | _ -> false
    let isCatchClause (ehc : ExceptionHandlingClause) =
        match ehc.ehcType with Catch _ -> true | _ -> false

    let shouldExecuteFinallyClause (src : offset) (dst : offset) (ehc : ExceptionHandlingClause) =
//        let srcOffset, dstOffset = src.Offset(), dst.Offset()
        let isInside offset = ehc.tryOffset <= offset && offset < ehc.tryOffset + ehc.tryLength
        isInside src && not <| isInside dst

    let internal (|Ret|_|) (opCode : OpCode) = if opCode = OpCodes.Ret then Some () else None
    let (|Call|_|) (opCode : OpCode) = if opCode = OpCodes.Call then Some () else None
    let (|CallVirt|_|) (opCode : OpCode) = if opCode = OpCodes.Callvirt then Some () else None
    let (|Calli|_|) (opCode : OpCode) = if opCode = OpCodes.Calli then Some () else None
    let (|TailCall|_|) (opCode : OpCode) = if opCode = OpCodes.Tailcall then Some () else None
    let (|NewObj|_|) (opCode : OpCode) = if opCode = OpCodes.Newobj then Some () else None

    let private methodBytesCache = System.Collections.Generic.Dictionary<MethodBase, byte [] * ExceptionHandlingClause []>()

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
            let rewriter = VSharp.Concolic.ILRewriter(body)
            rewriter.Import()
            let result = rewriter.Export()
            let parseEH (eh : VSharp.Concolic.rawExceptionHandler) =
                let oldEH = ehcs.[int eh.matcher]
                let ehcType =
                    if oldEH.Flags = ExceptionHandlingClauseOptions.Filter then Filter (int eh.matcher)
                    elif oldEH.Flags = ExceptionHandlingClauseOptions.Fault || oldEH.Flags = ExceptionHandlingClauseOptions.Finally then Finally
                    else Catch oldEH.CatchType
                {tryOffset = int eh.tryOffset; tryLength = int eh.tryLength; handlerOffset = int eh.handlerOffset; handlerLength = int eh.handlerLength; ehcType = ehcType }
            result.il, Array.map parseEH result.ehs

    let getILBytes (m : MethodBase) : byte [] =
        let result : ref<byte [] * ExceptionHandlingClause []> = ref (null, null)
        if methodBytesCache.TryGetValue(m, result) then fst result.Value
        else
            let ilBytes = rewriteMethodBytes m
            assert(ilBytes <> (null, null))
            methodBytesCache.Add(m, ilBytes)
            fst ilBytes

    let getEHSBytes (m : MethodBase) =
        let result : ref<byte [] * ExceptionHandlingClause []> = ref (null, null)
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
        let calledMethod = TokenResolver.resolveMethodFromMetadata m ilBytes (pos + opCode.Size)
        {sourceMethod = m; calledMethod = calledMethod; opCode = opCode; offset = pos}

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
