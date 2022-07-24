namespace VSharp

open System
open System.Reflection
open System.Reflection.Emit
open VSharp

type ipTransition =
    | FallThrough of offset
    | Return
    | UnconditionalBranch of offset
    | ConditionalBranch of offset * offset list
    // TODO: use this thing? #do
    | ExceptionMechanism

type ehcType =
    | Filter of offset
    | Catch of Type
    | Finally

type public ExceptionHandlingClause = { tryOffset : offset; tryLength : offset; handlerOffset : offset; handlerLength : offset; ehcType : ehcType }

type MethodWithBody internal (m : MethodBase) =
    let name = m.Name
    let fullName = Reflection.getFullMethodName m
    let fullGenericMethodName = lazy(Reflection.fullGenericMethodName m)
    let returnType = Reflection.getMethodReturnType m
    let parameters = m.GetParameters()
    let hasThis = Reflection.hasThis m
    let metadataToken = m.MetadataToken
    let isStatic = m.IsStatic
    let isAbstract = m.IsAbstract
    let isVirtual = m.IsVirtual
    let isFinal = m.IsFinal
    let isStaticConstructor = lazy(Reflection.isStaticConstructor m)
    let isConstructor = m.IsConstructor
    let isGenericMethod = m.IsGenericMethod
    let genericArguments = lazy(m.GetGenericArguments())
    let attributes = m.Attributes
    let customAttributes = m.CustomAttributes
    let methodImplementationFlags = lazy(m.GetMethodImplementationFlags())
    let isDelegateConstructor = lazy(Reflection.isDelegateConstructor m)
    let isDelegate = lazy(Reflection.isDelegate m)

    let methodBodyBytes = m.GetMethodBody()
    let localVariables = if methodBodyBytes = null then null else methodBodyBytes.LocalVariables
    let methodBody = lazy(
        if methodBodyBytes = null then None, None, None, None
        else
            let ilBytes = methodBodyBytes.GetILAsByteArray()
            let methodModule = m.Module
            let moduleName = methodModule.FullyQualifiedName
            let assemblyName = methodModule.Assembly.FullName
            let ehcs = System.Collections.Generic.Dictionary<int, System.Reflection.ExceptionHandlingClause>()
            let props : VSharp.Concolic.rawMethodProperties =
                {token = uint m.MetadataToken; ilCodeSize = uint ilBytes.Length; assemblyNameLength = 0u; moduleNameLength = 0u; maxStackSize = uint methodBodyBytes.MaxStackSize; signatureTokensLength = 0u}
            let tokens = System.Runtime.Serialization.FormatterServices.GetUninitializedObject(typeof<VSharp.Concolic.signatureTokens>) :?> VSharp.Concolic.signatureTokens
            let createEH (eh : System.Reflection.ExceptionHandlingClause) : VSharp.Concolic.rawExceptionHandler =
                let matcher = if eh.Flags = ExceptionHandlingClauseOptions.Filter then eh.FilterOffset else eh.HandlerOffset // TODO: need catch type token?
                ehcs.Add(matcher, eh)
                {flags = int eh.Flags; tryOffset = uint eh.TryOffset; tryLength = uint eh.TryLength; handlerOffset = uint eh.HandlerOffset; handlerLength = uint eh.HandlerLength; matcher = uint matcher}
            let ehs = methodBodyBytes.ExceptionHandlingClauses |> Seq.map createEH |> Array.ofSeq
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
            Some result.il, Some (Array.map parseEH result.ehs), Some rewriter, Some (rewriter.CopyInstructions()))

    member x.Name = name
    member x.FullName = fullName
    member x.FullGenericMethodName with get() = fullGenericMethodName.Force()
    member x.Id = int64 m.MethodHandle.Value
    member x.ReturnType = returnType
    member x.Module = m.Module
    member x.DeclaringType = m.DeclaringType
    member x.Parameters = parameters
    member x.LocalVariables = localVariables
    member x.HasThis = hasThis
    member x.MetadataToken = metadataToken
    member x.IsStatic = isStatic
    member x.IsAbstract = isAbstract
    member x.IsVirtual = isVirtual
    member x.IsFinal = isFinal
    member x.IsStaticConstructor with get() = isStaticConstructor.Force()

    member x.IsGenericMethod = isGenericMethod
    member x.GenericArguments with get() = genericArguments.Force()
    member x.GetGenericMethodDefinition() =
        match m with
        | :? MethodInfo as m -> if isGenericMethod then m.GetGenericMethodDefinition() else m
        | _ -> internalfailf $"Asking generic method definition for non-method {x}"
    member x.GetGenericArguments() =
        match m with
        | :? MethodInfo as m -> if isGenericMethod then m.GetGenericArguments() else Array.empty
        | _ -> internalfailf $"Asking generic method definition for non-method {x}"

    member x.Attributes = attributes
    member x.CustomAttributes = customAttributes
    member x.MethodImplementationFlags with get() = methodImplementationFlags.Force()

    member x.IsDelegate with get() = isDelegate.Force()
    member x.IsDelegateConstructor with get() = isDelegateConstructor.Force()

    override x.ToString() = x.FullName
    override x.GetHashCode() = m.MethodHandle.GetHashCode()
    override x.Equals(y : obj) =
        match y with
        | :? MethodWithBody as y -> m.MethodHandle.Value = y.MethodBase.MethodHandle.Value
        | _ -> false

    member x.HasBody = methodBodyBytes <> null

    member x.ILBytes with get() =
        let ilBytes, _, _, _ = methodBody.Force()
        match ilBytes with
        | Some bytes -> bytes
        | None -> internalfailf $"Getting IL bytes of method {x} without body (extern or abstract)"

    member x.ExceptionHandlers with get() =
        let _, exceptionHandlers, _, _ = methodBody.Force()
        match exceptionHandlers with
        | Some handlers -> handlers
        | None -> Array.empty

    member x.ILRewriter with get() =
        let _, _, rewriter, _ = methodBody.Force()
        match rewriter with
        | Some rewriter -> rewriter
        | None -> internalfailf $"Getting IL rewriter of method {x} without body (extern or abstract)"

    member x.ParsedInstructions with get() =
        let _, _, _, instructions = methodBody.Force()
        match instructions with
        | Some instructions -> instructions
        | None -> internalfailf $"Getting instructions of method {x} without body (extern or abstract)"

    // Helps resolving cyclic dependencies between Application and MethodWithBody
    [<DefaultValue>] static val mutable private instantiator : MethodBase -> MethodWithBody
    static member internal InstantiateNew with get() = MethodWithBody.instantiator and set v = MethodWithBody.instantiator <- v

    interface VSharp.Core.IMethod with
        override x.FullName = fullName
        override x.DeclaringType = m.DeclaringType
        override x.Parameters = parameters
        override x.LocalVariables = localVariables
        override x.HasThis = hasThis
        override x.IsConstructor = isConstructor
        override x.GenericArguments with get() = genericArguments.Force()
        override x.SubstituteTypeVariables subst =
            Reflection.concretizeMethodBase m subst |> MethodWithBody.InstantiateNew :> VSharp.Core.IMethod
        override x.CompareTo(y : obj) =
            match y with
            | :? MethodWithBody as y -> Reflection.compareMethods x.MethodBase y.MethodBase
            | _ -> -1

    // TODO: make it private!
    member x.MethodBase : MethodBase = m

    member x.ResolveMethod token = Reflection.resolveMethod m token
    member x.ResolveFieldFromMetadata = NumberCreator.extractInt32 x.ILBytes >> Reflection.resolveField m
    member x.ResolveTypeFromMetadata = NumberCreator.extractInt32 x.ILBytes >> Reflection.resolveType m
    member x.ResolveMethodFromMetadata = NumberCreator.extractInt32 x.ILBytes >> Reflection.resolveMethod m
    member x.ResolveTokenFromMetadata = NumberCreator.extractInt32 x.ILBytes >> Reflection.resolveToken m

    member x.IsEntryPoint with get() =
        m = (m.Module.Assembly.EntryPoint :> MethodBase)

    member x.Generalize() =
        let generalized, genericArgs, genericDefs = Reflection.generalizeMethodBase m
        MethodWithBody.InstantiateNew generalized, genericArgs, genericDefs

    member x.ParseCallSite pos =
        let ilBytes = x.ILBytes
        let opCode = OpCodeOperations.getOpCode ilBytes pos
        let calledMethod = x.ResolveMethodFromMetadata (pos + Offset.from opCode.Size)
        opCode, calledMethod

module MethodBody =

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
    let isFinallyClause (ehc : ExceptionHandlingClause) =
        match ehc.ehcType with Finally -> true | _ -> false
    let isFilterClause (ehc : ExceptionHandlingClause) =
        match ehc.ehcType with ehcType.Filter _ -> true | _ -> false
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

    let parseInstruction (m : MethodWithBody) pos =
        let ilBytes : byte [] = m.ILBytes
        OpCodeOperations.getOpCode ilBytes pos

    let getIpTransition (m : MethodWithBody) pos =
        let ilBytes = m.ILBytes
        let opCode = OpCodeOperations.getOpCode ilBytes pos
        findNextInstructionOffsetAndEdges opCode ilBytes pos

    let unconditionalBranchTarget (m : MethodWithBody) pos =
        match getIpTransition m pos with
        | UnconditionalBranch target -> target
        | _ -> __unreachable__()

    let fallThroughTarget (m : MethodWithBody) pos =
        match getIpTransition m pos with
        | FallThrough target -> target
        | _ -> __unreachable__()

    let conditionalBranchTarget (m : MethodWithBody) pos =
        match getIpTransition m pos with
        | ConditionalBranch(fallThrough, rest) -> fallThrough, rest
        | _ -> __unreachable__()
