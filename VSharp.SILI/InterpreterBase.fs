namespace VSharp.Interpreter.IL

open System
open VSharp.Core.API

#nowarn "69"

open VSharp
open VSharp.Core
open CilStateOperations
open System.Collections.Generic
open System.Reflection

type codeLocationSummary = { cilState : cilState } // state.returnRegister is used as result
    with
    member x.Result =
        match x.cilState.state.returnRegister with
        | None -> Nop
        | Some r -> r

type codeLocationSummaries = codeLocationSummary list

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
            let state = Memory.InitializeStaticMembers Memory.EmptyState (Types.FromDotNetType m.DeclaringType)
            let initialState = makeInitialState state
            x.Invoke id initialState (List.map (fun cilState -> { cilState = cilState }) >> List.toSeq >> k)
        | _ -> internalfailf "unexpected entry point: expected regular method, but got %O" id

    member x.Explore (funcId : IFunctionIdentifier) (k : codeLocationSummary seq -> 'a) =
        let k = API.Reset(); fun x -> API.Restore(); k x
        CurrentlyBeingExploredLocations.Add funcId |> ignore
        let initialStates = x.FormInitialState funcId
        let invoke cilState = x.Invoke funcId cilState (List.map (fun cilState -> {cilState = cilState}))
        let resultsAndStates = initialStates |> List.collect invoke
        CurrentlyBeingExploredLocations.Remove funcId |> ignore
        k resultsAndStates

    member private x.ReproduceEffectOrUnroll areWeStuck body (id : IFunctionIdentifier) cilState k =
        if areWeStuck then
            try
                x.ReproduceEffect id cilState k
            with
            | :? InsufficientInformationException ->
                body cilState k
        else
            // explicitly unrolling
            body cilState k

    member x.EnterRecursiveRegion (funcId : IFunctionIdentifier) cilState body k =
        let shouldStopUnrolling = x.ShouldStopUnrolling funcId cilState
        x.ReproduceEffectOrUnroll shouldStopUnrolling body funcId cilState k

    member x.ShouldStopUnrolling (funcId : IFunctionIdentifier) cilState =
        match Options.RecursionUnrollingMode () with
        | RecursionUnrollingModeType.SmartUnrolling -> DetectUnboundRecursion funcId cilState.state
        | RecursionUnrollingModeType.NeverUnroll -> true
        | RecursionUnrollingModeType.AlwaysUnroll -> false

    member x.ReduceFunction initialCilState (methodBase : MethodBase) k =
        // TODO: do concrete invocation if possible!
//        let canUseReflection = API.Marshalling.CanBeCalledViaReflection state funcId this parameters
//        if Options.InvokeConcrete () && canUseReflection then
//            API.Marshalling.CallViaReflection state funcId this parameters k
//        else
        let methodId = x.MakeMethodIdentifier methodBase
        let invoke state k = x.Invoke methodId state k
        let cilState = withOpStack emptyOpStack initialCilState
        let restoreOpStack cilState = withOpStack initialCilState.state.opStack cilState
        x.EnterRecursiveRegion methodId cilState invoke (List.map restoreOpStack >> k)

    member x.ReduceFunctionSignature state (methodBase : MethodBase) this paramValues isEffect k =
        let funcId = x.MakeMethodIdentifier methodBase
        let parameters = methodBase.GetParameters()
        let getParameterType (param : ParameterInfo) = Types.FromDotNetType param.ParameterType
        let values, areParametersSpecified =
            match paramValues with
            | Specified values -> values, true
            | Unspecified -> [], false
        let localVarsDecl (lvi : LocalVariableInfo) =
            let stackKey = LocalVariableKey(lvi, methodBase)
            (stackKey, Unspecified, Types.FromDotNetType lvi.LocalType)
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
        Memory.NewStackFrame state funcId (parametersAndThis @ locals) isEffect |> k

    member private x.InitStaticFieldWithDefaultValue state (f : FieldInfo) =
        assert(f.IsStatic)
        if f.IsLiteral then
            match f.GetValue(null) with // argument means class with field f, so we have null, because f is a static field
            | null -> state
            | value ->
                let fieldType = Types.FromDotNetType f.FieldType
                let value, state =
                    match value with
                    | :? string as str -> Memory.AllocateString str state
                    | v -> Terms.Concrete v fieldType, state
                let targetType = Types.FromDotNetType f.DeclaringType
                let fieldId = Reflection.wrapField f
                Memory.WriteStaticField state targetType fieldId value
        else state

    member x.InitializeStatics (cilState : cilState) (t : System.Type) (k : cilState list -> 'a) =
        let fields = t.GetFields(Reflection.staticBindingFlags)
        let staticConstructor = t.GetConstructors(Reflection.staticBindingFlags) |> Array.tryHead
        match t with
        | _ when t.IsGenericParameter -> k (List.singleton cilState)
        | _ ->
            let termType = Types.FromDotNetType t
            let typeInitialized = Memory.IsTypeInitialized cilState.state termType
            match typeInitialized with
            | True -> k (List.singleton cilState)
            | _ ->
                let state = Memory.InitializeStaticMembers cilState.state termType
                let state = Seq.fold x.InitStaticFieldWithDefaultValue state fields
                let cilStates =
                    match staticConstructor with
                    | Some cctor ->
                        let removeCallSiteResultAndPopStack (cilStateAfterCallingCCtor : cilState) =
                            let stateAfterCallingCCtor = Memory.PopFrame cilStateAfterCallingCCtor.state
                            let stateWithoutCallSiteResult = {stateAfterCallingCCtor with callSiteResults = state.callSiteResults; opStack = state.opStack}
                            {cilStateAfterCallingCCtor with state = stateWithoutCallSiteResult}
                        x.ReduceFunctionSignature state cctor None (Specified []) false (fun state ->
                        x.ReduceFunction {cilState with state = state} cctor (List.map removeCallSiteResultAndPopStack))
                    | None -> {cilState with state = state } |> List.singleton
                k cilStates // TODO: make assumption ``Memory.withPathCondition state (!!typeInitialized)''

    member x.CallAbstractMethod (funcId : IFunctionIdentifier) state k =
        __insufficientInformation__ "Can't call abstract method %O, need more information about the object type" funcId
    member x.FormInitialStateWithoutStatics (funcId : IFunctionIdentifier) =
        let this, state(*, isMethodOfStruct*) =
            match funcId with
            | :? IMethodIdentifier as m ->
                let declaringType = Types.FromDotNetType m.DeclaringType
                let initialState = Memory.InitializeStaticMembers Memory.EmptyState declaringType
                (if m.IsStatic then None else Memory.MakeSymbolicThis m.Method |> Some), initialState
            | _ -> __notImplemented__()
        let state = Option.fold (fun state this -> !!(IsNullReference this) |> WithPathCondition state) state this
        x.ReduceFunctionSignature state funcId.Method this Unspecified true id
    member x.FormInitialState (funcId : IFunctionIdentifier) : cilState list =
        let state = x.FormInitialStateWithoutStatics funcId
        let cilState = makeInitialState state
        x.InitializeStatics cilState funcId.Method.DeclaringType id

    abstract CreateInstance : System.Type -> term list -> cilState -> cilState list
    default x.CreateInstance exceptionType arguments cilState =
        x.InitializeStatics cilState exceptionType (List.map (fun cilState ->
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
        assert (not <| exceptionType.IsValueType)
        let s = cilState.state
        let reference, s = Memory.AllocateDefaultClass s (Types.FromDotNetType exceptionType)
        let withResult result (cilState : cilState) = {cilState with state = {cilState.state with returnRegister = Some result}}
        x.ReduceFunctionSignature s ctor (Some reference) (Specified arguments) false (fun state ->
        x.ReduceFunction {cilState with state = state} ctor (fun cilStates ->
        cilStates |> List.map (withResult reference)))) >> List.concat)

    member x.InvalidProgramException cilState =
        x.CreateInstance typeof<System.InvalidProgramException> [] cilState
    member x.NullReferenceException cilState =
        x.CreateInstance typeof<System.NullReferenceException> [] cilState
    member x.IndexOutOfRangeException cilState =
        x.CreateInstance typeof<System.IndexOutOfRangeException> [] cilState
    member x.ArrayTypeMismatchException cilState =
        x.CreateInstance typeof<System.ArrayTypeMismatchException> [] cilState
    member x.DivideByZeroException cilState =
        x.CreateInstance typeof<System.DivideByZeroException> [] cilState
    member x.OverflowException cilState =
        x.CreateInstance typeof<System.OverflowException> [] cilState
    member x.ArithmeticException cilState =
        x.CreateInstance typeof<System.ArithmeticException> [] cilState
    member x.TypeInitializerException qualifiedTypeName innerException (cilState : cilState) =
        let typeName, state = Memory.AllocateString qualifiedTypeName cilState.state
        let args = [typeName; innerException]
        x.CreateInstance typeof<System.TypeInitializationException> args {cilState with state = state}
    member x.InvalidCastException (cilState : cilState) =
        let message, state = Memory.AllocateString "Specified cast is not valid." cilState.state
        x.CreateInstance typeof<System.InvalidCastException> [message] {cilState with state = state}

    member x.ExploreAndCompose (funcId : IFunctionIdentifier) (cilState : cilState) (k : cilState list -> 'a) =
        let prepareGenericsLessState (methodId : IMethodIdentifier) state =
            let methodBase = methodId.Method
            if not <| Reflection.IsGenericOrDeclaredInGenericType methodBase then methodId :> IFunctionIdentifier, state, false
            else
                let fullyGenericMethod, genericArgs, genericDefs = Reflection.generalizeMethodBase methodBase
                let genericArgs = genericArgs |> Seq.map Types.FromDotNetType |> List.ofSeq
                let genericDefs = genericDefs |> Seq.map Id |> List.ofSeq
                if List.isEmpty genericDefs then methodId :> IFunctionIdentifier, state, false
                else
                    let state = Memory.NewTypeVariables state (List.zip genericDefs genericArgs)
                    (x.MakeMethodIdentifier fullyGenericMethod :> IFunctionIdentifier), state, true

        let newFuncId, cilState, isSubstitutionNeeded =
            match funcId with
            | :? IMethodIdentifier as methodId ->
                let newFunId, state, isSubstitutionNeeded = prepareGenericsLessState methodId cilState.state
                newFunId, {cilState with state = state}, isSubstitutionNeeded
            | _ -> funcId, cilState, false

        let k =
            if isSubstitutionNeeded then
                List.map (fun (cilState : cilState) -> {cilState with state = Memory.PopTypeVariables cilState.state}) >> k
            else k

        x.Explore newFuncId (Seq.map (fun summary ->
            Logger.trace "ExploreAndCompose: Original CodeLoc = %O New CodeLoc = %O\ngot summary state = %s" funcId newFuncId (dump summary.cilState)
            Logger.trace "ExploreAndCompose: Left state = %s" (dump cilState)
            let summaryCilState = withCurrentTime [] summary.cilState
            let resultStates = compose cilState summaryCilState
            List.iter (dump >> (Logger.trace "ExploreAndCompose: Result after composition %s")) resultStates
            resultStates) >> List.ofSeq >> List.concat >> k)

    abstract member Invoke : IFunctionIdentifier -> cilState -> (cilState list -> 'a) -> 'a

    abstract member MakeMethodIdentifier : MethodBase -> IMethodIdentifier

    abstract member ReproduceEffect : IFunctionIdentifier -> cilState -> (cilState list -> 'a) -> 'a
    default x.ReproduceEffect funcId state k = x.ExploreAndCompose funcId state k
