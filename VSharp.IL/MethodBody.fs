namespace VSharp

open global.System
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
    | Fault

type public ExceptionHandlingClause = { tryOffset : offset; tryLength : offset; handlerOffset : offset; handlerLength : offset; ehcType : ehcType }

type MethodWithBody internal (m : MethodBase) =
    let desc = Reflection.getMethodDescriptor m
    let name = m.Name
    let fullName = Reflection.getFullMethodName m
    let fullGenericMethodName = lazy(Reflection.fullGenericMethodName m)
    let declaringType = m.DeclaringType
    let returnType = Reflection.getMethodReturnType m
    let parameters = m.GetParameters()
    let hasThis = Reflection.hasThis m
    let hasNonVoidResult = lazy(Reflection.hasNonVoidResult m)
    let isDynamic = m :? DynamicMethod
    let metadataToken = if isDynamic then m.GetHashCode() else m.MetadataToken
    let isStatic = m.IsStatic
    let isAbstract = m.IsAbstract
    let isVirtual = m.IsVirtual
    let isFinal = m.IsFinal
    let isStaticConstructor = lazy(Reflection.isStaticConstructor m)
    let isConstructor = m.IsConstructor
    let isGenericMethod = m.IsGenericMethod
    let isGenericMethodDefinition = m.IsGenericMethodDefinition
    let genericArguments = lazy(if m.IsGenericMethod && not m.IsConstructor then m.GetGenericArguments() else Array.empty)
    let attributes = m.Attributes
    let customAttributes = if isDynamic then Seq.empty else m.CustomAttributes
    let methodImplementationFlags = lazy m.GetMethodImplementationFlags()
    let isDelegateConstructor = lazy(Reflection.isDelegateConstructor m)
    let isDelegate = lazy(Reflection.isDelegate m)
    let tryFSharpInternalCall = lazy(Map.tryFind fullGenericMethodName.Value Loader.FSharpImplementations)
    let isFSharpInternalCall = lazy(Option.isSome tryFSharpInternalCall.Value)
    let isCSharpInternalCall = lazy(Map.containsKey fullGenericMethodName.Value Loader.CSharpImplementations)
    let isShimmed = lazy(Loader.isShimmed fullGenericMethodName.Value)
    let isConcreteCall = lazy(Loader.isInvokeInternalCall fullGenericMethodName.Value)
    let isRuntimeException = lazy(Loader.isRuntimeExceptionsImplementation fullGenericMethodName.Value)
    let runtimeExceptionImpl = lazy(Map.tryFind fullGenericMethodName.Value Loader.runtimeExceptionsConstructors)
    let isNotImplementedIntrinsic =
        lazy(
            let isIntrinsic =
                let intrinsicAttr = "System.Runtime.CompilerServices.IntrinsicAttribute"
                customAttributes |> Seq.exists (fun m -> m.AttributeType.ToString() = intrinsicAttr)
            isIntrinsic && (Array.contains fullGenericMethodName.Value Loader.trustedIntrinsics |> not)
        )
    let isImplementedInternalCall =
        lazy(isFSharpInternalCall.Value || isCSharpInternalCall.Value)
    let isInternalCall =
        lazy (
            int (m.GetMethodImplementationFlags() &&& MethodImplAttributes.InternalCall) <> 0
            || DllManager.isQCall m
        )

    let actualMethod =
        if not isCSharpInternalCall.Value then m
        else Loader.CSharpImplementations[fullGenericMethodName.Value]
    let methodBodyBytes =
        if isFSharpInternalCall.Value then null
        else actualMethod.GetMethodBody()
    let localVariables = if methodBodyBytes = null then null else methodBodyBytes.LocalVariables
    let methodBody = lazy(
        if methodBodyBytes = null then None, None, None, None
        else
            let ilBytes = methodBodyBytes.GetILAsByteArray()
            let methodModule = actualMethod.Module
            let moduleName = methodModule.FullyQualifiedName
            let assemblyName = methodModule.Assembly.FullName
            let ehcs = System.Collections.Generic.Dictionary<int, System.Reflection.ExceptionHandlingClause>()
            let props : rawMethodProperties =
                {
                    token = uint actualMethod.MetadataToken
                    ilCodeSize = uint ilBytes.Length
                    assemblyNameLength = 0u
                    moduleNameLength = 0u
                    maxStackSize = uint methodBodyBytes.MaxStackSize
                    signatureTokensLength = 0u
                }
            let tokens = System.Runtime.Serialization.FormatterServices.GetUninitializedObject(typeof<signatureTokens>) :?> signatureTokens
            let createEH (eh : System.Reflection.ExceptionHandlingClause) : rawExceptionHandler =
                let matcher = if eh.Flags = ExceptionHandlingClauseOptions.Filter then eh.FilterOffset else eh.HandlerOffset // TODO: need catch type token?
                ehcs.Add(matcher, eh)
                {
                    flags = int eh.Flags
                    tryOffset = uint eh.TryOffset
                    tryLength = uint eh.TryLength
                    handlerOffset = uint eh.HandlerOffset
                    handlerLength = uint eh.HandlerLength
                    matcher = uint matcher
                }
            let ehs = methodBodyBytes.ExceptionHandlingClauses |> Seq.map createEH |> Array.ofSeq
            let body : rawMethodBody =
                {properties = props; assembly = assemblyName; moduleName = moduleName; tokens = tokens; il = ilBytes; ehs = ehs}
            let rewriter = ILRewriter(body, actualMethod)
            rewriter.Import()
            let result = rewriter.Export()
            let parseEH (eh : rawExceptionHandler) =
                let oldEH = ehcs[int eh.matcher]
                let ehcType =
                    if oldEH.Flags = ExceptionHandlingClauseOptions.Filter then ehcType.Filter (eh.matcher |> int |> Offset.from)
                    elif oldEH.Flags = ExceptionHandlingClauseOptions.Finally then Finally
                    elif oldEH.Flags = ExceptionHandlingClauseOptions.Fault then Fault
                    else Catch oldEH.CatchType
                {
                    tryOffset = eh.tryOffset |> int |> Offset.from
                    tryLength = eh.tryLength |> int |> Offset.from
                    handlerOffset = eh.handlerOffset |> int |> Offset.from
                    handlerLength = eh.handlerLength |> int |> Offset.from
                    ehcType = ehcType
                }
            Some result.il, Some (Array.map parseEH result.ehs), Some rewriter, Some rewriter.Instructions)

    member x.Name = name
    member x.FullName = fullName
    member x.FullGenericMethodName with get() = fullGenericMethodName.Force()
    member x.Id = desc.GetHashCode()
    member x.ReturnType = returnType
    member x.Module =
        if isCSharpInternalCall.Value then Loader.CSharpImplementations[fullGenericMethodName.Value].Module
        else m.Module
    member x.DeclaringType = declaringType
    member x.ReflectedType = m.ReflectedType
    member x.Parameters = parameters
    member x.HasParameterOnStack =
        x.DeclaringType.IsValueType && not x.IsStatic
        || x.Parameters |> Array.exists (fun p -> p.ParameterType.IsByRef)
    member x.LocalVariables = localVariables
    member x.HasThis = hasThis
    member x.MetadataToken = metadataToken
    member x.IsStatic = isStatic
    member x.IsAbstract = isAbstract
    member x.IsVirtual = isVirtual
    member x.IsFinal = isFinal
    member x.IsStaticConstructor with get() = isStaticConstructor.Force()
    member x.IsConstructor with get() = isConstructor

    member x.ContainsGenericParameters =
        declaringType.ContainsGenericParameters || m.ContainsGenericParameters
    member x.IsGenericMethod = isGenericMethod
    member x.IsGenericMethodDefinition = isGenericMethodDefinition
    member x.GenericArguments with get() = genericArguments.Force()
    member x.GetGenericMethodDefinition() =
        match m with
        | :? MethodInfo as m -> if isGenericMethod then Reflection.getGenericMethodDefinition m else m
        | _ -> internalfailf $"Asking generic method definition for non-method {x}"
    member x.GetGenericArguments() =
        match m with
        | :? MethodInfo as m -> if isGenericMethod then m.GetGenericArguments() else Array.empty
        | _ -> internalfailf $"Asking generic method definition for non-method {x}"

    member x.Attributes = attributes
    member x.CustomAttributes = customAttributes
    member x.MethodImplementationFlags with get() = methodImplementationFlags.Force()

    member x.ReturnParameter with get() =
        match m with
        | :? MethodInfo as m -> Some m.ReturnParameter
        | _ -> None

    member x.IsDelegate with get() = isDelegate.Force()
    member x.IsDelegateConstructor with get() = isDelegateConstructor.Force()

    override x.ToString() = x.FullName
    override x.GetHashCode() = desc.GetHashCode()
    override x.Equals(y : obj) =
        match y with
        | :? MethodWithBody as y -> x.Descriptor = y.Descriptor
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

    member x.IsConcretelyInvokable with get () =
        // All method generic parameters should be provided
        not m.ContainsGenericParameters &&

        // Method is not disallowed by ref-like type
        not returnType.IsByRefLike &&

        // Method is not disallowed by ref type
        (not returnType.IsByRef ||
            let elementType = returnType.GetElementType() in
            not elementType.IsByRefLike && elementType <> typeof<Void>) &&

        // All declaring type generic parameters should be provided, declaring type should not be ref-like
        (m.DeclaringType = null || not m.DeclaringType.ContainsGenericParameters || not m.DeclaringType.IsByRefLike) &&

        // Method should not contain varargs
        (m.CallingConvention &&& CallingConventions.VarArgs) <> CallingConventions.VarArgs

    member x.IsExternalMethod with get() = Reflection.isExternalMethod m
    member x.IsQCall with get() = DllManager.isQCall m

    member x.HasNonVoidResult = hasNonVoidResult.Value

    interface VSharp.Core.IMethod with
        override x.Name = name
        override x.FullName = fullName
        override x.ReturnType = returnType
        override x.ReflectedType = m.ReflectedType
        override x.DeclaringType = m.DeclaringType
        override x.Parameters = parameters
        override x.LocalVariables = localVariables
        override x.HasThis = hasThis
        override x.IsConstructor = isConstructor
        override x.IsExternalMethod with get() = x.IsExternalMethod
        override x.ContainsGenericParameters with get() = x.ContainsGenericParameters
        override x.GenericArguments with get() = genericArguments.Value
        override x.SubstituteTypeVariables subst =
            Reflection.concretizeMethodBase m subst |> MethodWithBody.InstantiateNew :> VSharp.Core.IMethod
        override x.CompareTo(y : obj) =
            match y with
            | :? MethodWithBody as y -> Reflection.compareMethods (x :> Core.IMethod).MethodBase (y :> Core.IMethod).MethodBase
            | _ -> -1
        override x.ResolveOverrideInType t = x.ResolveOverrideInType t
        override x.CanBeOverriddenInType t = x.CanBeOverriden t
        override x.IsImplementedInType t = x.IsImplementedInType t

        // TODO: make it private!
        override x.MethodBase : MethodBase = m

    member private x.Descriptor = desc

    member x.ResolveMethod token = Reflection.resolveMethod actualMethod token
    member x.ResolveFieldFromMetadata = NumberCreator.extractInt32 x.ILBytes >> Reflection.resolveField actualMethod
    member x.ResolveTypeFromMetadata = NumberCreator.extractInt32 x.ILBytes >> Reflection.resolveType actualMethod
    member x.ResolveMethodFromMetadata = NumberCreator.extractInt32 x.ILBytes >> Reflection.resolveMethod actualMethod
    member x.ResolveTokenFromMetadata = NumberCreator.extractInt32 x.ILBytes >> Reflection.resolveToken actualMethod

    member x.IsEntryPoint with get() =
        m = (m.Module.Assembly.EntryPoint :> MethodBase)

    member x.IsInternalCall with get() = isInternalCall.Value
    member x.IsImplementedInternalCall with get () = isImplementedInternalCall.Value

    member x.IsShimmed with get() = isShimmed.Value

    member x.CanCallConcrete with get() = x.IsConcretelyInvokable && isConcreteCall.Value

    member x.IsFSharpInternalCall with get() = isFSharpInternalCall.Value
    member x.IsCSharpInternalCall with get() = isCSharpInternalCall.Value

    member x.GetInternalCall with get() =
        match tryFSharpInternalCall.Value with
        | Some method -> method
        | None -> internalfail $"GetInternalCall: no internal call for method {fullGenericMethodName.Value}"

    member x.IsRuntimeException with get() = isRuntimeException.Value
    member x.HasRuntimeExceptionImpl with get() = Option.isSome runtimeExceptionImpl.Value
    member x.RuntimeExceptionImpl with get() =
        match runtimeExceptionImpl.Value with
        | Some ctor -> ctor
        | None -> internalfail $"RuntimeExceptionImpl: no runtime exception implementation for method {fullGenericMethodName.Value}"

    member x.IsNotImplementedIntrinsic with get() = isNotImplementedIntrinsic.Value

    member x.CanBeOverriden targetType =
        match m with
        | :? ConstructorInfo -> false
        | :? MethodInfo as mi -> Reflection.canOverrideMethod targetType mi
        | _ -> __unreachable__()

    member x.Generalize() =
        let generalized, genericArgs, genericDefs = Reflection.generalizeMethodBase m
        MethodWithBody.InstantiateNew generalized, genericArgs, genericDefs

    member x.Invoke (thisOption : obj option) args =
        let this = Option.toObj thisOption
        m.Invoke(this, args)

    member x.ParseCallSite pos =
        let ilBytes = x.ILBytes
        let opCode = OpCodeOperations.getOpCode ilBytes pos
        let calledMethod = x.ResolveMethodFromMetadata (pos + Offset.from opCode.Size)
        opCode, calledMethod

    member x.ResolveOverrideInType t =
        match m with
        | :? ConstructorInfo when m.DeclaringType = t -> x
        | :? MethodInfo as mi ->
            (Reflection.resolveOverridingMethod t mi :> MethodBase) |> MethodWithBody.InstantiateNew
        | _ -> __unreachable__()

    member x.IsImplementedInType t =
        match m with
        | :? ConstructorInfo -> m.DeclaringType = t
        | :? MethodInfo as mi -> Reflection.typeImplementsMethod t mi
        | _ -> __unreachable__()

module MethodBody =

    let private operandType2operandSize = [| 4<offsets>; 4<offsets>; 4<offsets>; 8<offsets>; 4<offsets>
                                             0<offsets>; -1<offsets>; 8<offsets>; 4<offsets>; 4<offsets>
                                             4<offsets>; 4<offsets>; 4<offsets>; 4<offsets>; 2<offsets>
                                             1<offsets>; 1<offsets>; 4<offsets>; 1<offsets>|]

    let private jumpTargetsForNext (opCode : OpCode) _ (pos : offset) =
        let nextInstruction = pos + Offset.from opCode.Size + operandType2operandSize[int opCode.OperandType]
        FallThrough nextInstruction

    let private jumpTargetsForBranch (opCode : OpCode) ilBytes (pos : offset) =
        let opcodeSize = Offset.from opCode.Size
        let offset =
            match opCode.OperandType with
            | OperandType.InlineBrTarget -> NumberCreator.extractInt32 ilBytes (pos + opcodeSize)
            | _ -> NumberCreator.extractInt8 ilBytes (pos + opcodeSize)

        let nextInstruction = pos + Offset.from opCode.Size + operandType2operandSize[int opCode.OperandType]
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

    // TODO: deal with calli
    let private isCallOpCode (opCode : OpCode) =
        opCode = OpCodes.Call
        //|| opCode = OpCodes.Calli
        || opCode = OpCodes.Callvirt
        || opCode = OpCodes.Tailcall
    let private isNewObjOpCode (opCode : OpCode) =
        opCode = OpCodes.Newobj
    let isDemandingCallOpCode (opCode : OpCode) =
        isCallOpCode opCode || isNewObjOpCode opCode
    let isFinallyClause (ehc : ExceptionHandlingClause) =
        match ehc.ehcType with Finally -> true | _ -> false
    let isFaultClause (ehc : ExceptionHandlingClause) =
        match ehc.ehcType with Fault -> true | _ -> false
    let isFilterClause (ehc : ExceptionHandlingClause) =
        match ehc.ehcType with ehcType.Filter _ -> true | _ -> false
    let isCatchClause (ehc : ExceptionHandlingClause) =
        match ehc.ehcType with Catch _ -> true | _ -> false

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
        | ipTransition -> internalfail $"unconditionalBranchTarget: unexpected ip transition {ipTransition}"

    let fallThroughTarget (m : MethodWithBody) pos =
        match getIpTransition m pos with
        | FallThrough target -> target
        | ipTransition -> internalfail $"fallThroughTarget: unexpected ip transition {ipTransition}"

    let conditionalBranchTarget (m : MethodWithBody) pos =
        match getIpTransition m pos with
        | ConditionalBranch(fallThrough, rest) -> fallThrough, rest
        | ipTransition -> internalfail $"conditionalBranchTarget: unexpected ip transition {ipTransition}"
