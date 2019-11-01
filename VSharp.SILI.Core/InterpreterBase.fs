namespace VSharp.Core

#nowarn "69"

open VSharp
open System.Collections.Generic
open System.Reflection
open System.Text.RegularExpressions
open System


module public TokenCreator =
    let private lvtokenPattern = "LocalVariable:"
    let private lvtokenName = "__loc__"
    let internal stackKeyIsLocalVariable (name, token) =
        Regex.Match(name, lvtokenName + "-?\d+").Success
        && Regex.Match(token, lvtokenPattern + "-?\d+").Success

    let private getThisTokenBy (node : MethodBase) = node.ToString()
    let private getTokenByLocalVariable (lvi : LocalVariableInfo) =
        sprintf "%s%i" lvtokenPattern (lvi.ToString().GetHashCode())
    let private getTokenByParameter (pi : ParameterInfo) =
        sprintf "MethodParameter:%i" (Microsoft.FSharp.Core.LanguagePrimitives.PhysicalHash(pi))

    let private nameLocalVar (lvi : LocalVariableInfo) = lvtokenName + lvi.LocalIndex.ToString()

    let StackKeyOfThis methodBase : stackKey = ("this", getThisTokenBy methodBase)
    let StackKeyOfParameter (param : ParameterInfo) : stackKey = (param.Name, getTokenByParameter param)
    let StackKeyOfLocalVariable lvi : stackKey = (nameLocalVar lvi, getTokenByLocalVariable lvi)

[<AbstractClass>]
type public ExplorerBase() =
    static let CurrentlyBeingExploredLocations = new HashSet<ICodeLocation>()
    static let CurrentlyCalledLocations = new HashSet<ICodeLocation>()

    static let DetectUnboundRecursion (codeLoc : ICodeLocation) (s : state) =
        match codeLoc with
        | :? IFunctionIdentifier as id ->
            let isRecursiveFrame (frame : stackFrame) =
                match frame.func with
                | Some(id', _) when id = id' -> true
                | _ -> false
            let bottomOccurence = Stack.tryFindBottom isRecursiveFrame s.frames.f
            match bottomOccurence with
            | None -> false
            | Some { func = Some(_, p'); entries = _ } when s.pc = p' -> false
            | _ -> true
        ///  TODO: add more logic
        | :? ILCodePortion as ilcode when CurrentlyCalledLocations.Contains ilcode -> true
        | :? ILCodePortion as ilcode -> //alwaysUnrollValue
            let lastFrame = List.head ilcode.Frames.f
            lastFrame.entries
            |> List.exists (fun (entry : entry) ->
                if TokenCreator.stackKeyIsLocalVariable entry.key then false else
                let reference = Memory.ReferenceLocalVariable entry.key
                let value, _ = Memory.Dereference s reference
                match value.term with
                | Concrete _ -> false
                | _ -> true)
        | _ -> internalfail "Some new ICodeLocation"

    interface IActivator with
        member x.CreateInstance _ exceptionType arguments state =
            x.InitEntryPoint state exceptionType (fun state ->
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
            let reference, state = Memory.AllocateDefaultBlock state (Types.FromDotNetType state exceptionType)
            let invoke state k = x.Invoke methodId state (Some reference) k
            x.ReduceFunction state (Some reference) (Specified arguments) methodId ctor invoke (fun (res, state) ->
            assert (res = Nop)
            reference, state))

    member x.NullReferenceException state =
        (x :> IActivator).CreateInstance () typeof<System.NullReferenceException> [] state

    member x.InterpretEntryPoint (id : IFunctionIdentifier) k =
        let initialState = State.emptyRestricted
        match id with
        | :? IMethodIdentifier as m ->
            assert(m.IsStatic)
            x.InitEntryPoint initialState m.DeclaringType (fun state ->
            x.Invoke id state None (fun (result, state) -> k { result = result; state = state }))
        | _ -> internalfailf "unexpected entry point: expected regular method, but got %O" id

    member x.Explore (codeLoc : ICodeLocation) k =
        match Database.querySummary codeLoc with
        | Some r -> k r
        | None ->
            let k = API.Reset(); fun x -> API.Restore(); k x
            let metadata = Metadata.empty
            CurrentlyBeingExploredLocations.Add codeLoc |> ignore
            let initClosure frames =
                let state = List.foldBack (fun frame state ->
                    let fr = frame.entries |> List.map (fun e -> e.key, Unspecified, e.typ)
                    match frame.func with
                    | Some(f, p) ->
                        let state = {state with pc = p}
                        Memory.NewStackFrame state f fr
                    | None -> Memory.NewScope state fr) frames.f State.empty
                { state with pc = List.empty; frames = frames}
            match codeLoc with
            | :? IFunctionIdentifier as funcId ->
                let this, state, isMethodOfStruct =
                    match funcId with
                    | :? IMethodIdentifier as m ->
                        let declaringType = m.DeclaringType |> Types.Constructor.fromDotNetType
                        let initialState = { State.empty with statics = State.Defined false (Explorer.formInitialStatics metadata declaringType) }
                        if m.IsStatic then (None, initialState, false)
                        else
                            Memory.makeSymbolicThis metadata initialState m.Token declaringType
                            |> (fun (f, s, flag) -> Some f, s, flag)
                    | :? IDelegateIdentifier as dlgt ->
                        // TODO: Create dummy frame
                        (None, initClosure dlgt.ContextFrames, false)
                    | _ -> __notImplemented__()
                let thisIsNotNull = if Option.isSome this then !!( Pointers.isNull metadata (Option.get this)) else Nop
                let state = if Option.isSome this && thisIsNotNull <> True then State.withPathCondition state thisIsNotNull else state
                let state = x.FormInitialState funcId state
                x.Invoke funcId state this (fun (res, state) ->
                    let state = if Option.isSome this && thisIsNotNull <> True then State.popPathCondition state else state
                    let state = if isMethodOfStruct then State.popStack state else state
                    CurrentlyBeingExploredLocations.Remove funcId |> ignore
                    Database.report funcId res state |> k)
            | :? ILCodePortion as ilcode ->
                let state = initClosure ilcode.Frames
                x.Invoke ilcode state None (fun (res, state) ->
                    CurrentlyBeingExploredLocations.Remove ilcode |> ignore
                    Database.report ilcode res state |> k)
            | _ -> __notImplemented__()

    member x.ReproduceEffect (codeLoc : ICodeLocation) state k =
        let mtd = Metadata.empty
        let addr = [Memory.freshAddress()]
        if CurrentlyBeingExploredLocations.Contains codeLoc then
            Explorer.recursionApplication mtd codeLoc state addr k
        else
            let ctx : compositionContext = { mtd = mtd; addr = addr }
            x.Explore codeLoc (fun summary ->
            let result = Memory.fillHoles ctx state summary.result
            let state = Memory.composeStates ctx state summary.state
            k (result, state))

    member private x.ReproduceEffectOrUnroll areWeStuck body id state setup teardown k =
        if areWeStuck then
            x.ReproduceEffect id state k
        else
            /// explicitly unrolling
            setup id
            body state (fun (result, state) ->
            teardown id
            k (result, state))

    member x.EnterRecursiveRegion (codeLoc : IFunctionIdentifier) state body k =
        let shouldStopUnrolling = x.ShouldStopUnrolling codeLoc state
        let setup, teardown =
            match Options.RecursionUnrollingMode () with
            | RecursionUnrollingModeType.NeverUnroll -> CurrentlyCalledLocations.Add >> ignore, CurrentlyCalledLocations.Remove >> ignore
            | _ -> ignore, ignore
        x.ReproduceEffectOrUnroll shouldStopUnrolling body codeLoc state setup teardown k

    member x.ShouldStopUnrolling (codeLoc : ICodeLocation) state =
        match Options.RecursionUnrollingMode () with
        | RecursionUnrollingModeType.SmartUnrolling -> DetectUnboundRecursion codeLoc state
        | RecursionUnrollingModeType.NeverUnroll ->
            CurrentlyCalledLocations.Contains codeLoc ||
            not <| CurrentlyBeingExploredLocations.Contains codeLoc ||
            Database.reported codeLoc
        | RecursionUnrollingModeType.AlwaysUnroll -> false

    member x.ReduceFunction state this parameters funcId (methodBase : MethodBase) invoke k =
        x.ReduceFunctionSignature funcId state methodBase this parameters (fun state ->
        x.EnterRecursiveRegion funcId state invoke (fun (result, state) ->
        k (result, Memory.PopStack state)))

    member x.ReduceFunctionSignature (funcId : IFunctionIdentifier) state (methodBase : MethodBase) this paramValues k =
        let parameters = methodBase.GetParameters()
        let getParameterType (param : ParameterInfo) = Types.FromDotNetType state param.ParameterType
        let values, areParametersSpecified =
            match paramValues with
            | Specified values -> values, true
            | Unspecified -> [], false
        let localVarsDecl (lvi : LocalVariableInfo) =
            let stackKey = TokenCreator.StackKeyOfLocalVariable lvi
            (stackKey, Unspecified, Types.FromDotNetType state lvi.LocalType)
        let locals = methodBase.GetMethodBody().LocalVariables |> Seq.map localVarsDecl |> Seq.toList
        let valueOrFreshConst (param : ParameterInfo option) value =
            match param, value with
            | None, _ -> internalfail "parameters list is longer than expected!"
            | Some param, None ->
                let stackKey = TokenCreator.StackKeyOfParameter param
                match areParametersSpecified with
                | true when param.HasDefaultValue ->
                    let typ = getParameterType param
                    (stackKey, Specified(Concrete param.DefaultValue typ), typ)
                | true -> internalfail "parameters list is shorter than expected!"
                | _ -> (stackKey, Unspecified, getParameterType param)
            | Some param, Some value -> (TokenCreator.StackKeyOfParameter param, Specified value, getParameterType param)
        let parameters = List.map2Different valueOrFreshConst parameters values
        let parametersAndThis =
            match this with
            | Some thisValue ->
                let thisKey = TokenCreator.StackKeyOfThis methodBase
                (thisKey, Specified thisValue, TypeOf thisValue) :: parameters // TODO: incorrect type when ``this'' is Ref to stack
            | None -> parameters
        Memory.NewStackFrame state funcId (parametersAndThis @ locals) |> k // TODO: need to change FQL in "parametersAndThis" before adding it to stack frames (ClassesSimplePropertyAccess.TestProperty1) #FQLsNotEqual

    member x.ReduceConcreteCall (methodBase : MethodBase) state this (args : term list symbolicValue) k =
        let methodId = x.MakeMethodIdentifier methodBase
        let invoke state k = x.Invoke methodId state this k
        x.ReduceFunction state this args methodId methodBase invoke k

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
                let fullName = Reflection.getFullNameOfField f
                let address = Memory.ReferenceStaticField targetType fullName fieldType
                Memory.Mutate state address value |> snd
        else state

    member x.InitEntryPoint state (t : Type) k =
        let fields = t.GetFields(Reflection.staticBindingFlags)
        let staticConstructor = t.GetConstructors(Reflection.staticBindingFlags) |> Array.tryHead
        match t with
        | _ when t.IsGenericParameter -> k state
        | _ ->
            let termType = t |> Types.FromDotNetType state
            BranchStatements state
                (fun state k -> k (Memory.IsTypeNameInitialized termType state, state))
                (fun state k -> k (Nop, state))
                (fun state k ->
                    let state = Memory.AllocateDefaultStatic state termType
                    let state = Seq.fold x.InitStaticFieldWithDefaultValue state fields
                    match staticConstructor with
                    | Some cctor -> x.ReduceConcreteCall cctor state None (Specified []) k
                    | None -> k (Nop, state))
                (snd >> k)

    member x.CallAbstractMethod (caller : locationBinding) (funcId : IFunctionIdentifier) state k =
        let k = Enter caller state k
        HigherOrderApply funcId state k

    abstract member Invoke : ICodeLocation -> state -> term option -> (term * state -> 'a) -> 'a
    abstract member FormInitialState : IFunctionIdentifier -> state -> state
    abstract member MakeMethodIdentifier : MethodBase -> IMethodIdentifier


type public IInterpreterState<'InterpreterState when 'InterpreterState :> IInterpreterState<'InterpreterState>> =
    abstract member InternalState : state
    abstract member SetState : state -> 'InterpreterState
    abstract member ResultTerm : term option
    abstract member SetResultTerm : term option -> 'InterpreterState

[<AbstractClass>]
type public InterpreterBase<'InterpreterState when 'InterpreterState :> IInterpreterState<'InterpreterState>>() =

    member x.Interpret start =
        let merge (x : #IInterpreterState<'a>) (y : #IInterpreterState<'a>) : 'a =
            let rec findCommonSuffix common pc1 pc2 =
                match pc1, pc2 with
                | [], [] -> [], [], common
                | [], rest2 -> [], rest2, common
                | rest1, [] -> rest1, [], common
                | x :: xs, y :: ys when x = y -> findCommonSuffix (y :: common) xs ys
                | _ -> pc1, pc2, common
            let st1 = x.InternalState
            let st2 = y.InternalState
            let cond1List, cond2List, commonPc = findCommonSuffix [] (List.rev st1.pc) (List.rev st2.pc)
            let cond1 = List.fold (&&&) True cond1List
            let cond2 = List.fold (&&&) True cond2List
            let result =
                match x.ResultTerm, y.ResultTerm with
                | None, None -> None
                | Some t1, Some t2 -> Some <| Merging.merge2Terms cond1 cond2 t1 t2
                | _ -> internalfail "only one state has result"
            let mergedInternalState = Merging.merge2States cond1 cond2 {st1 with pc = commonPc} {st2 with pc = commonPc}
            let newSt = x.SetState mergedInternalState
            newSt.SetResultTerm(result)
        let rec interpret' current =
            let states = x.EvaluateOneStep current
            states |> List.iter (fun state ->
                if x.IsResultState state then
                    match x.GetResultState() with
                    | None -> x.SetResultState state
                    | Some res -> x.SetResultState (merge res state)
                else
                    let newState =
                        if x.IsRecursiveState state then
                            x.ExploreInIsolation state
                        else state
                    match x.FindSimilar newState with
                    | None -> x.Add newState
                    | Some similar -> x.Add (merge newState similar)
                )
            if x.HasNextState () then
                let newSt = x.PickNext ()
                interpret' newSt
            else
                x.GetResultState ()
        let res = interpret' start
        match res with
        | Some st when x.IsRecursiveState start ->
            let ist = x.MakeRecursiveState st
            x.MakeEpsilonState ist |> merge ist |> Some
        | _ -> res

    abstract member EvaluateOneStep : 'InterpreterState -> 'InterpreterState list
    abstract member ExploreInIsolation : 'InterpreterState -> 'InterpreterState
    abstract member HasNextState : unit -> bool
    abstract member FindSimilar : 'InterpreterState -> 'InterpreterState option
    abstract member Add : 'InterpreterState -> unit
    abstract member GetResultState : unit -> 'InterpreterState option
    abstract member PickNext : unit -> 'InterpreterState
    abstract member IsRecursiveState : 'InterpreterState -> bool
    abstract member MakeEpsilonState : 'InterpreterState -> 'InterpreterState
    abstract member IsResultState : 'InterpreterState -> bool
    abstract member SetResultState : 'InterpreterState -> unit
    abstract member MakeRecursiveState : 'InterpreterState -> 'InterpreterState
