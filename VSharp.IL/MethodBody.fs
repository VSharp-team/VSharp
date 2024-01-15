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

type MethodWithBody internal (m : MethodBase) =
    let shouldRewriteIL = false
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
                || declaringType.CustomAttributes |> Seq.exists (fun m -> m.AttributeType.ToString() = intrinsicAttr)
            isIntrinsic && (Array.contains fullGenericMethodName.Value Loader.trustedIntrinsics |> not)
        )
    let isImplementedInternalCall =
        lazy(isFSharpInternalCall.Value || isCSharpInternalCall.Value)
    let isInternalCall =
        lazy (
            int (m.GetMethodImplementationFlags() &&& MethodImplAttributes.InternalCall) <> 0
            || DllManager.isQCall m
        )
    let isExternalCall = lazy Reflection.isExternalMethod m

    let externInvocationForbidden = lazy(
            match DllManager.parseDllImport m with
            | Some info -> Loader.isExternInvocationForbidden info
            | None -> false
        )

    let invocationForbidden =
        lazy (
            Loader.isInvocationForbidden fullGenericMethodName.Value
            || isExternalCall.Value && externInvocationForbidden.Value
        )

    let shouldAnalyseInvokable = lazy not invocationForbidden.Value

    let actualMethod =
        if not isCSharpInternalCall.Value then m
        else Loader.CSharpImplementations[fullGenericMethodName.Value]
    let methodBodyBytes =
        if isFSharpInternalCall.Value then null
        else actualMethod.GetMethodBody()
    let localVariables = if methodBodyBytes = null then null else methodBodyBytes.LocalVariables

    let rawBody = lazy(
        if methodBodyBytes = null then None else
            Some {
                il = methodBodyBytes.GetILAsByteArray()
                ehs = exceptionHandlingClause.CreateArray methodBodyBytes
            })

    let methodBody = lazy(
        match rawBody.Value with
        | Some rawBody when shouldRewriteIL ->
            ILRewriter.rewriteIL rawBody actualMethod |> Some
        | Some rawBody -> Some rawBody
        | None -> None)

    let instructions = lazy(
        match rawBody.Value with
        | Some rawBody -> ILRewriter.instructionsOfMethod rawBody actualMethod
        | None -> internalfailf $"Getting instructions of method {m} without body (extern or abstract)")

    let invocationFlags = lazy (
        let flagsProperty = m.GetType().GetProperty("InvocationFlags", BindingFlags.NonPublic ||| BindingFlags.Instance)
        if flagsProperty <> null then
            flagsProperty.GetMethod.Invoke(m, Array.empty)
        else null)

    let canBeInvoked = lazy (
        let flags = invocationFlags.Value
        flags = null ||
        (flags :?> uint) &&& ((* NoInvoke *) 0x00000002u ||| (* ContainsStackPointers *) 0x00000100u) = 0u)

    let isConcretelyInvokable = lazy (
        // All method generic parameters should be provided
        not m.ContainsGenericParameters &&
        // Method should not return byref-like type
        not returnType.IsByRefLike &&
        // Method should not return pointer or native pointer
        not (returnType.IsPointer || TypeUtils.isNative returnType) &&
        // Method should not return byref type
        not returnType.IsByRef &&
        // Method's 'this' should not be byref-like type
        not (declaringType.IsByRefLike && (hasThis || isConstructor)) &&
        // Method should not be string constructor, because strings are immutable
        not (declaringType = typeof<string> && isConstructor) &&
        // Method's declaring type should not be 'Nullable', because we can not create boxed 'Nullable' struct
        not (TypeUtils.isNullable declaringType) &&
        // Method's arguments should not be byref-like or byref types // TODO: support 'byref' arguments
        parameters |> Array.forall (fun p -> not (p.ParameterType.IsByRefLike || p.ParameterType.IsByRef)) &&
        // All declaring type generic parameters should be provided, declaring type should not be ref-like
        (m.DeclaringType = null || not m.DeclaringType.ContainsGenericParameters || not m.DeclaringType.IsByRefLike) &&
        // Method should not contain varargs
        (m.CallingConvention &&& CallingConventions.VarArgs) <> CallingConventions.VarArgs &&
        // Method is not static constructor
        not isStaticConstructor.Value &&
        // Method should be invokable via Reflection
        canBeInvoked.Value)

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
        x.ReflectedType.IsValueType && not x.IsStatic
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
        match methodBody.Value with
        | Some body -> body.il
        | None -> internalfailf $"Getting IL bytes of method {x} without body (extern or abstract)"

    member x.ExceptionHandlers with get() =
        match methodBody.Value with
        | Some body -> body.ehs
        | None -> Array.empty

    member internal x.AnalyseMethod (failPredicate : analysisEvent -> bool) =
        let rawBody =
            match rawBody.Value with
            | Some rawBody when not isCSharpInternalCall.Value -> rawBody
            | _ -> rawMethodBody.Create m
        ILRewriter.analyseMethod rawBody m failPredicate

    member x.ParsedInstructions with get() =
        assert(methodBodyBytes <> null)
        instructions.Value

    // Helps resolving cyclic dependencies between Application and MethodWithBody
    [<DefaultValue>] static val mutable private instantiator : MethodBase -> MethodWithBody
    static member internal InstantiateNew with get() = MethodWithBody.instantiator and set v = MethodWithBody.instantiator <- v

    member x.IsConcretelyInvokable with get () = isConcretelyInvokable.Value

    member x.IsExternalMethod with get() = isExternalCall.Value
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
        override x.HasParameterOnStack = x.HasParameterOnStack
        override x.IsConstructor = isConstructor
        override x.IsStaticConstructor = x.IsStaticConstructor
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

    member x.CanCallConcrete (changedStaticFields : Collections.Generic.HashSet<fieldId>) =
        let failPredicate (event : analysisEvent) =
            match event with
            | Calli
            | CallVirt _ -> true
            | Ldsfld f -> changedStaticFields.Contains f
            | Stsfld _ -> true
        x.IsConcretelyInvokable &&
        (isConcreteCall.Value || shouldAnalyseInvokable.Value && x.AnalyseMethod failPredicate)

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

    let private operandType2operandSize =
        [|
            4<offsets>; 4<offsets>; 4<offsets>; 8<offsets>; 4<offsets>
            0<offsets>; -1<offsets>; 8<offsets>; 4<offsets>; 4<offsets>
            4<offsets>; 4<offsets>; 4<offsets>; 4<offsets>; 2<offsets>
            1<offsets>; 1<offsets>; 4<offsets>; 1<offsets>
        |]

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
        let nextInstruction = pos + opcodeSize + operandType2operandSize[int opCode.OperandType]
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
    let isFinallyClause (ehc : exceptionHandlingClause) =
        match ehc.ehcType with Finally -> true | _ -> false
    let isFaultClause (ehc : exceptionHandlingClause) =
        match ehc.ehcType with Fault -> true | _ -> false
    let isFilterClause (ehc : exceptionHandlingClause) =
        match ehc.ehcType with ehcType.Filter _ -> true | _ -> false
    let isCatchClause (ehc : exceptionHandlingClause) =
        match ehc.ehcType with Catch _ -> true | _ -> false

    let internal (|Ret|_|) (opCode : OpCode) = if opCode = OpCodes.Ret then Some () else None
    let (|Call|_|) (opCode : OpCode) = if opCode = OpCodes.Call then Some () else None
    let (|CallVirt|_|) (opCode : OpCode) = if opCode = OpCodes.Callvirt then Some () else None
    let (|Calli|_|) (opCode : OpCode) = if opCode = OpCodes.Calli then Some () else None
    let (|TailCall|_|) (opCode : OpCode) = if opCode = OpCodes.Tailcall then Some () else None
    let (|NewObj|_|) (opCode : OpCode) = if opCode = OpCodes.Newobj then Some () else None

    let parseInstruction (m : MethodWithBody) pos =
        let ilBytes : byte[] = m.ILBytes
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
