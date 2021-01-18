namespace VSharp.Core

#nowarn "69"

open VSharp
open System.Collections.Generic
open System.Reflection
open System


[<AbstractClass>]
type public ExplorerBase() =
    static let CurrentlyBeingExploredLocations = HashSet<IFunctionIdentifier>()

    static let DetectUnboundRecursion (funcId : IFunctionIdentifier) (s : state) =
        let isRecursiveFrame (frame : stackFrame) = funcId = frame.func
        s.frames |> Stack.pop |> Stack.exists isRecursiveFrame


    member x.InterpretEntryPoint (id : IFunctionIdentifier) k =
        match id with
        | :? IMethodIdentifier as m ->
            assert(m.IsStatic)
            let state = Memory.initializeStaticMembers Memory.empty (Types.Constructor.fromDotNetType m.DeclaringType)
            x.Invoke id state (List.map (fun (result, state) -> { result = result; state = state }) >> List.toSeq >> k)
        | _ -> internalfailf "unexpected entry point: expected regular method, but got %O" id

    member x.Explore (funcId : IFunctionIdentifier) (k : codeLocationSummary seq -> 'a) =
            let k = API.Reset(); fun x -> API.Restore(); k x
            CurrentlyBeingExploredLocations.Add funcId |> ignore
            let initialStates = x.FormInitialState funcId
            let removePCs this thisIsNotNull =
                List.map (fun (res, state) -> res, if Option.isSome this && thisIsNotNull <> True then Memory.removePathCondition state thisIsNotNull else state)
            let invoke (state, this, thisIsNotNull) = x.Invoke funcId state (removePCs this thisIsNotNull)
            let resultsAndStates =
                initialStates |> List.map invoke |> List.concat
                |> List.map (fun (result, state) -> {result = result; state = state})
            CurrentlyBeingExploredLocations.Remove funcId |> ignore
            k resultsAndStates

    member private x.ReproduceEffectOrUnroll areWeStuck body (id : IFunctionIdentifier) state k =
        if areWeStuck then
            try
                x.ReproduceEffect id state k
            with
            | :? InsufficientInformationException ->
                body state (List.map (fun (rs, s : state) -> rs, {s with currentTime = state.currentTime}) >> k)
        else
            /// explicitly unrolling
            body state k

    member x.EnterRecursiveRegion (funcId : IFunctionIdentifier) state body k =
        let shouldStopUnrolling = x.ShouldStopUnrolling funcId state
        x.ReproduceEffectOrUnroll shouldStopUnrolling body funcId state k

    member x.ShouldStopUnrolling (funcId : IFunctionIdentifier) state =
        match Options.RecursionUnrollingMode () with
        | RecursionUnrollingModeType.SmartUnrolling -> DetectUnboundRecursion funcId state
        | RecursionUnrollingModeType.NeverUnroll -> true
        | RecursionUnrollingModeType.AlwaysUnroll -> false

    member x.ReduceFunction state funcId invoke k =
        // TODO: do concrete invocation if possible!
//        let canUseReflection = API.Marshalling.CanBeCalledViaReflection state funcId this parameters
//        if Options.InvokeConcrete () && canUseReflection then
//            API.Marshalling.CallViaReflection state funcId this parameters k
//        else
            x.EnterRecursiveRegion funcId state invoke k


    member x.ReduceFunctionSignature state (methodBase : MethodBase) this paramValues isEffect k =
        let funcId = x.MakeMethodIdentifier methodBase
        let parameters = methodBase.GetParameters()
        let getParameterType (param : ParameterInfo) = Types.FromDotNetType state param.ParameterType
        let values, areParametersSpecified =
            match paramValues with
            | Specified values -> values, true
            | Unspecified -> [], false
        let localVarsDecl (lvi : LocalVariableInfo) =
            let stackKey = LocalVariableKey(lvi, methodBase)
            (stackKey, Unspecified, Types.FromDotNetType state lvi.LocalType)
        let locals =
            match methodBase.GetMethodBody() with
            | null -> []
            | body -> body.LocalVariables |> Seq.map localVarsDecl |> Seq.toList
        let valueOrFreshConst (param : ParameterInfo option) value =
            match param, value with
            | None, _ -> internalfail "parameters list is longer than expected!"
            | Some param, None ->
                let stackKey = ParameterKey param
                match areParametersSpecified with
                | true when param.HasDefaultValue ->
                    let typ = getParameterType param
                    (stackKey, Specified(Concrete param.DefaultValue typ), typ)
                | true -> internalfail "parameters list is shorter than expected!"
                | _ -> (stackKey, Unspecified, getParameterType param)
            | Some param, Some value -> (ParameterKey param, Specified value, getParameterType param)
        let parameters = List.map2Different valueOrFreshConst parameters values
        let parametersAndThis =
            match this with
            | Some thisValue ->
                let thisKey = ThisKey methodBase
                (thisKey, Specified thisValue, TypeOf thisValue) :: parameters // TODO: incorrect type when ``this'' is Ref to stack
            | None -> parameters
        Memory.NewStackFrame state funcId (parametersAndThis @ locals) isEffect |> k // TODO: need to change FQL in "parametersAndThis" before adding it to stack frames (ClassesSimplePropertyAccess.TestProperty1) #FQLsNotEqual

    member x.ReduceConcreteCall (methodBase : MethodBase) state k =
        let methodId = x.MakeMethodIdentifier methodBase
        let invoke state k = x.Invoke methodId state k
        x.ReduceFunction state methodId invoke k

    member private x.InitStaticFieldWithDefaultValue state (f : FieldInfo) =
        assert(f.IsStatic)
        if f.IsLiteral then
            match f.GetValue(null) with // argument means class with field f, so we have null, because f is a static field
            | null -> state
            | value ->
                let fieldType = f.FieldType |> Types.FromDotNetType state
                let value, state =
                    match value with
                    | :? string as str -> Memory.AllocateString str state
                    | v -> Terms.Concrete v fieldType, state
                let targetType = Types.FromDotNetType state f.DeclaringType
                let fieldId = Reflection.wrapField f
                Memory.writeStaticField state targetType fieldId value
        else state

    member x.InitializeStatics (state : state) (t : Type) (k : state list -> 'a) =
        let fields = t.GetFields(Reflection.staticBindingFlags)
        let staticConstructor = t.GetConstructors(Reflection.staticBindingFlags) |> Array.tryHead
        match t with
        | _ when t.IsGenericParameter -> k (List.singleton state)
        | _ ->
            let termType = t |> Types.FromDotNetType state
            let typeInitialized = Memory.IsTypeInitialized state termType
            match typeInitialized with
            | True -> k (List.singleton state)
            | _ ->
                let state = Memory.AllocateDefaultStatic state termType
                let state = Seq.fold x.InitStaticFieldWithDefaultValue state fields
                let states =
                    match staticConstructor with
                    | Some cctor ->
                        let removeCallSiteResultAndPopStack (stateAfterCallingCCtor : state) =
                            let stateAfterCallingCCtor = Memory.PopStack stateAfterCallingCCtor
                            {stateAfterCallingCCtor with callSiteResults = state.callSiteResults; opStack = state.opStack}
                        x.ReduceFunctionSignature state cctor None (Specified []) false (fun state ->
                        x.ReduceConcreteCall cctor state (List.map (snd >> removeCallSiteResultAndPopStack)))
                    | None -> state |> List.singleton
                k states // TODO: make assumption ``Memory.withPathCondition state (!!typeInitialized)''

    member x.CallAbstractMethod (funcId : IFunctionIdentifier) state k =
        __insufficientInformation__ "Can't call abstract method %O, need more information about the object type" funcId
    member x.FormInitialStateWithoutStatics (funcId : IFunctionIdentifier) =
        let this, state(*, isMethodOfStruct*) =
            match funcId with
            | :? IMethodIdentifier as m ->
                let declaringType = m.DeclaringType |> Types.Constructor.fromDotNetType
                let initialState = Memory.initializeStaticMembers Memory.empty declaringType
                (if m.IsStatic then None else Memory.makeSymbolicThis m.Method |> Some), initialState
            | _ -> __notImplemented__()
        let thisIsNotNull = if Option.isSome this then !!( Pointers.isNull (Option.get this)) else Nop
        let state = if Option.isSome this && thisIsNotNull <> True then Memory.withPathCondition state thisIsNotNull else state
        x.ReduceFunctionSignature state funcId.Method this Unspecified true (fun state ->  state, this, thisIsNotNull)
    member x.FormInitialState (funcId : IFunctionIdentifier) : (state * term option * term) list =
        let state, this, thisIsNotNull = x.FormInitialStateWithoutStatics funcId
        x.InitializeStatics state funcId.Method.DeclaringType (List.map (fun state -> state, this, thisIsNotNull(*, isMethodOfStruct*)))

    abstract CreateInstance : System.Type -> term list -> state -> state list
    default x.CreateInstance exceptionType arguments state =
        x.InitializeStatics state exceptionType (List.map (fun state ->
        let constructors = exceptionType.GetConstructors()
        let argumentsLength = List.length arguments
        let argumentsTypes =
            List.map (TypeOf >> Types.ToDotNetType) arguments
        let ctors =
            constructors
            |> List.ofArray
            |> List.filter (fun (ci : ConstructorInfo)
                             -> ci.GetParameters().Length = argumentsLength
                                && ci.GetParameters()
                                   |> Seq.forall2(fun p1 p2 -> p2.ParameterType.IsAssignableFrom(p1)) argumentsTypes)
        assert(List.length ctors = 1)
        let ctor = List.head ctors
        let methodId = x.MakeMethodIdentifier ctor
        assert (not <| exceptionType.IsValueType)
        let reference, state = Memory.AllocateDefaultClass state (Types.FromDotNetType state exceptionType)
        let invoke = x.Invoke methodId
        let withResult result (state : state) = {state with returnRegister = Some result}
        x.ReduceFunctionSignature state ctor (Some reference) (Specified arguments) false (fun state ->
        x.ReduceFunction state methodId invoke (fun resultsAndStates ->
        resultsAndStates |> List.iter (fun (res, _) -> assert (res = Nop))
        resultsAndStates |> List.map (snd >> withResult reference)))) >> List.concat)

    member x.InvalidProgramException state =
        x.CreateInstance typeof<System.InvalidProgramException> [] state
    member x.NullReferenceException state =
        x.CreateInstance typeof<System.NullReferenceException> [] state
    member x.IndexOutOfRangeException state =
        x.CreateInstance typeof<System.IndexOutOfRangeException> [] state
    member x.ArrayTypeMismatchException state =
        x.CreateInstance typeof<System.ArrayTypeMismatchException> [] state
    member x.DivideByZeroException state =
        x.CreateInstance typeof<System.DivideByZeroException> [] state
    member x.OverflowException state =
        x.CreateInstance typeof<System.OverflowException> [] state
    member x.ArithmeticException state =
        x.CreateInstance typeof<System.ArithmeticException> [] state
    member x.TypeInitializerException qualifiedTypeName innerException state =
        let typeName, state = Memory.AllocateString qualifiedTypeName state
        let args = [typeName; innerException]
        x.CreateInstance typeof<System.TypeInitializationException> args state
    member x.InvalidCastException state =
        let message, state = Memory.AllocateString "Specified cast is not valid." state
        x.CreateInstance typeof<System.InvalidCastException> [message] state

    member x.ExploreAndCompose codeLoc state k =
        x.Explore codeLoc (Seq.map (fun summary ->
            Logger.trace "ExploreAndCompose: got summary state = %s" (Memory.Dump summary.state)
            Logger.trace "ExploreAndCompose: Left state = %s" (Memory.Dump state)
            let newStates = Memory.composeStates state summary.state
            List.iter (Memory.Dump >> (Logger.trace "ExploreAndCompose: Result after composition %s")) newStates

            let result = Memory.fillHoles state summary.result
            List.map (withFst result) newStates) >> List.ofSeq >> List.concat >> k)

    abstract member Invoke : IFunctionIdentifier -> state -> ((term * state) list -> 'a) -> 'a

    abstract member MakeMethodIdentifier : MethodBase -> IMethodIdentifier

    abstract member ReproduceEffect : IFunctionIdentifier -> state -> ((term * state) list -> 'a) -> 'a
    default x.ReproduceEffect funcId state k =
            x.ExploreAndCompose funcId state k

