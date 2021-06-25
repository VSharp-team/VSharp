namespace VSharp.Interpreter.IL

open System
open VSharp.Core.API

#nowarn "69"

open VSharp
open VSharp.Core
open CilStateOperations
open System.Collections.Generic
open System.Reflection
open ipOperations

type codeLocationSummary = { cilState : cilState } // state.returnRegister is used as result
    with
    member x.State = withEvaluationStack emptyEvaluationStack x.cilState |> stateOf
    member x.Result =
        if EvaluationStack.Length x.cilState.state.evaluationStack = 0 then Nop
        else pop x.cilState |> fst

type codeLocationSummaries = codeLocationSummary list

[<AbstractClass>]
type public ExplorerBase() =
    static let CurrentlyBeingExploredLocations = HashSet<MethodBase>()

    static let DetectUnboundRecursion (method : MethodBase) (s : state) =
        Memory.CallStackContainsFunction s method

    member x.InterpretEntryPoint (method : MethodBase) k =
        assert(method.IsStatic)
        let state = Memory.InitializeStaticMembers Memory.EmptyState (Types.FromDotNetType method.DeclaringType)
        let initialState = makeInitialState method state
        x.Invoke method initialState (List.map (fun cilState -> { cilState = cilState }) >> List.toSeq >> k)

    member x.Explore (method : MethodBase) (k : codeLocationSummary seq -> 'a) =
        let k = Reset(); fun x -> Restore(); k x
        CurrentlyBeingExploredLocations.Add method |> ignore
        let initialStates = x.FormInitialState method
        let invoke cilState = x.Invoke method cilState (List.map (fun cilState -> {cilState = cilState}))
        let resultsAndStates = initialStates |> List.collect invoke
        CurrentlyBeingExploredLocations.Remove method |> ignore
        k resultsAndStates

    member private x.ReproduceEffectOrUnroll areWeStuck body (id : MethodBase) cilState k =
        // every exploration should be made via searcher
        __unreachable__()
//        if areWeStuck then
//            try
//                x.ReproduceEffect id cilState k
//            with
//            | :? InsufficientInformationException ->
//                body cilState k
//        else
//            // explicitly unrolling
//            body cilState k

    member x.EnterRecursiveRegion (method : MethodBase) cilState body k =
        let shouldStopUnrolling = x.ShouldStopUnrolling method cilState
        x.ReproduceEffectOrUnroll shouldStopUnrolling body method cilState k

    member x.ShouldStopUnrolling (method : MethodBase) cilState =
        match Options.RecursionUnrollingMode () with
        | RecursionUnrollingModeType.SmartUnrolling -> DetectUnboundRecursion method cilState.state
        | RecursionUnrollingModeType.NeverUnroll -> true
        | RecursionUnrollingModeType.AlwaysUnroll -> false

    abstract ReduceFunction : MethodBase -> cilState -> (cilState list -> 'a) -> 'a
    default x.ReduceFunction (methodBase : MethodBase) (cilState : cilState) k =
        // TODO: do nothing, we have queue
        cilState |> List.singleton |> k
        // TODO: do concrete invocation if possible!
//        let canUseReflection = API.Marshalling.CanBeCalledViaReflection state funcId this parameters
//        if Options.InvokeConcrete () && canUseReflection then
//            API.Marshalling.CallViaReflection state funcId this parameters k
//        else
//            let methodId = x.MakeMethodIdentifier methodBase
//            let invoke state k = x.Invoke methodId state k
//            x.EnterRecursiveRegion methodId cilState invoke k

    static member ReduceFunctionSignature state (method : MethodBase) this paramValues k =
        let parameters = method.GetParameters()
        let getParameterType (param : ParameterInfo) = Types.FromDotNetType param.ParameterType
        let values, areParametersSpecified =
            match paramValues with
            | Some values -> values, true
            | None -> [], false
        let localVarsDecl (lvi : LocalVariableInfo) =
            let stackKey = LocalVariableKey(lvi, method)
            (stackKey, None, Types.FromDotNetType lvi.LocalType)
        let locals =
            match method.GetMethodBody() with
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
                    (stackKey, Some(Concrete param.DefaultValue typ), typ)
                | true -> internalfail "parameters list is shorter than expected!"
                | _ -> (stackKey, None, getParameterType param)
            | Some param, Some value -> (ParameterKey param, Some value, getParameterType param)
        let parameters = List.map2Different valueOrFreshConst parameters values
        let parametersAndThis =
            match this with
            | Some thisValue ->
                let thisKey = ThisKey method
                (thisKey, Some thisValue, TypeOf thisValue) :: parameters // TODO: incorrect type when ``this'' is Ref to stack
            | None -> parameters
        Memory.NewStackFrame state method (parametersAndThis @ locals) |> k

    member x.ReduceFunctionSignatureCIL (cilState : cilState) (methodBase : MethodBase) this paramValues k =
        ExplorerBase.ReduceFunctionSignature cilState.state methodBase this paramValues (fun state ->
        cilState |> withState state |> pushToIp (instruction methodBase 0) |> k)

    member private x.InitStaticFieldWithDefaultValue state (f : FieldInfo) =
        assert(f.IsStatic)
        let fieldType = Types.FromDotNetType f.FieldType
        let value, state =
            if f.IsLiteral then
                match f.GetValue(null) with // argument means class with field f, so we have null, because f is a static field
                | null -> NullRef, state
                | :? string as str -> Memory.AllocateString str state
                | v when f.FieldType.IsPrimitive || f.FieldType.IsEnum -> Concrete v fieldType, state
                | _ -> __unreachable__()
            else Memory.DefaultOf fieldType, state
        let targetType = Types.FromDotNetType f.DeclaringType
        let fieldId = Reflection.wrapField f
        Memory.WriteStaticField state targetType fieldId value

    // NOTE: When executing ldsfld, call and so on, we should previously initialize statics.
    // NOTE: Continuation 'whenInitializedCont' contains exploration of current instruction 'I' on ipStack.
    // NOTE: If statics was already initialized or we have no static constructor,
    //       then we can continue to explore instructions in continuation 'whenInitializedCont'.
    // NOTE: Otherwise, we need to add static constructor to ipStack (so queue will explore it instruction by instruction)
    //       without calling continuation 'whenInitializedCont'.
    // NOTE: After exploration of static constructor, queue will contain the same instruction 'I',
    //       that caused statics initialization, so queue will try again to explore this instruction,
    //       but at that moment statics will be already initialized
    member x.InitializeStatics (cilState : cilState) (t : Type) whenInitializedCont =
        let fields = t.GetFields(Reflection.staticBindingFlags)
        match t with
        | _ when t.IsGenericParameter -> whenInitializedCont cilState
        | _ ->
            let termType = Types.FromDotNetType t
            let typeInitialized = Memory.IsTypeInitialized cilState.state termType
            match typeInitialized with
            | True -> whenInitializedCont cilState
            | _ ->
                let staticConstructor = t.GetConstructors(Reflection.staticBindingFlags) |> Array.tryHead
                let state = Memory.InitializeStaticMembers cilState.state termType
                let state = Seq.fold x.InitStaticFieldWithDefaultValue state fields
                let cilState = withState state cilState
                match staticConstructor with
                | Some cctor -> x.ReduceFunctionSignatureCIL cilState cctor None (Some []) List.singleton
                | None -> whenInitializedCont cilState
                // TODO: make assumption ``Memory.withPathCondition state (!!typeInitialized)''

    member x.CallAbstractMethod (method : MethodBase) state k =
        __insufficientInformation__ "Can't call abstract method %O, need more information about the object type" method

    static member FormInitialStateWithoutStatics (method : MethodBase) =
        let this, state(*, isMethodOfStruct*) =
            let declaringType = Types.FromDotNetType method.DeclaringType
            let initialState = Memory.InitializeStaticMembers Memory.EmptyState declaringType
            (if method.IsStatic then None else Memory.MakeSymbolicThis method |> Some), initialState
        let state = Option.fold (fun state this -> !!(IsNullReference this) |> WithPathCondition state) state this
        ExplorerBase.ReduceFunctionSignature state method this None id
    member x.FormInitialState (method : MethodBase) : cilState list =
        let state = ExplorerBase.FormInitialStateWithoutStatics method
        let cilState = makeInitialState method state
        x.InitializeStatics cilState method.DeclaringType List.singleton

    abstract CreateException : System.Type -> term list -> cilState -> cilState list
    default x.CreateException exceptionType arguments cilState =
        assert (not <| exceptionType.IsValueType)
        let cilState = clearEvaluationStackLastFrame cilState
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
        let fullConstructorName = Reflection.getFullMethodName ctor
        assert (Loader.hasRuntimeExceptionsImplementation fullConstructorName)
        let proxyCtor = Loader.getRuntimeExceptionsImplementation fullConstructorName
        x.ReduceFunctionSignatureCIL cilState proxyCtor None (Some arguments) List.singleton

    member x.InvalidProgramException cilState =
        x.CreateException typeof<System.InvalidProgramException> [] cilState
    member x.NullReferenceException cilState =
        x.CreateException typeof<System.NullReferenceException> [] cilState
    member x.IndexOutOfRangeException cilState =
        x.CreateException typeof<System.IndexOutOfRangeException> [] cilState
    member x.ArrayTypeMismatchException cilState =
        x.CreateException typeof<System.ArrayTypeMismatchException> [] cilState
    member x.DivideByZeroException cilState =
        x.CreateException typeof<System.DivideByZeroException> [] cilState
    member x.OverflowException cilState =
        x.CreateException typeof<System.OverflowException> [] cilState
    member x.ArithmeticException cilState =
        x.CreateException typeof<System.ArithmeticException> [] cilState
    member x.TypeInitializerException qualifiedTypeName innerException (cilState : cilState) =
        let typeName, state = Memory.AllocateString qualifiedTypeName cilState.state
        let args = [typeName; innerException]
        x.CreateException typeof<System.TypeInitializationException> args {cilState with state = state}
    member x.InvalidCastException (cilState : cilState) =
        let message, state = Memory.AllocateString "Specified cast is not valid." cilState.state
        x.CreateException typeof<System.InvalidCastException> [message] {cilState with state = state}

    member x.ExploreAndCompose (method : MethodBase) (cilState : cilState) (k : cilState list -> 'a) =
        let prepareGenericsLessState (method : MethodBase) state =
            if not <| Reflection.isGenericOrDeclaredInGenericType method then method, state, false
            else
                let fullyGenericMethod, genericArgs, genericDefs = Reflection.generalizeMethodBase method
                let genericArgs = genericArgs |> Seq.map Types.FromDotNetType |> List.ofSeq
                let genericDefs = genericDefs |> Seq.map Id |> List.ofSeq
                if List.isEmpty genericDefs then method, state, false
                else
                    let state = Memory.NewTypeVariables state (List.zip genericDefs genericArgs)
                    fullyGenericMethod, state, true
        let newMethod, state, isSubstitutionNeeded = prepareGenericsLessState method cilState.state
        let cilState = withState state cilState
        let k =
            if isSubstitutionNeeded then
                List.map (fun (cilState : cilState) -> {cilState with state = Memory.PopTypeVariables cilState.state}) >> k
            else k
        x.Explore newMethod (Seq.map (fun summary ->
//            Logger.trace "ExploreAndCompose: Original CodeLoc = %O New CodeLoc = %O\ngot summary state = %s" funcId newFuncId (dump summary.cilState)
//            Logger.trace "ExploreAndCompose: Left state = %s" (dump cilState)
            let summaryCilState = withCurrentTime [] summary.cilState
            let resultStates = compose cilState summaryCilState
//            List.iter (dump >> (Logger.trace "ExploreAndCompose: Result after composition %s")) resultStates
            resultStates) >> List.ofSeq >> List.concat >> k)

    abstract member Invoke : MethodBase -> cilState -> (cilState list -> 'a) -> 'a

    abstract member ReproduceEffect : MethodBase -> cilState -> (cilState list -> 'a) -> 'a
    default x.ReproduceEffect method state k = x.ExploreAndCompose method state k
