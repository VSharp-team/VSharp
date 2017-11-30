namespace VSharp

open JetBrains.Decompiler.Ast
open JetBrains.Metadata.Reader.API
open System.Collections.Generic

type WriteDependence =
    | UnboundedMutation of Term * Term
    | UnboundedAllocation of Term * Term

module Functions =
    type internal SymbolicLambda<'a> = LocationBinding -> State.state -> Term list State.SymbolicValue -> (StatementResult * State.state -> 'a) -> 'a

    let internal MakeLambda metadata state (metadataMethod : IMetadataMethod) (lambda : SymbolicLambda<'a>) =
        let typ = Types.FromMetadataMethodSignature metadataMethod in
        let term = Concrete lambda typ metadata in
        Memory.allocateInHeap metadata state term

    let internal MakeLambdaTerm metadata (signature : IFunctionSignature) (returnMetadataType : IMetadataType) (lambda : SymbolicLambda<'a>) =
        let typ = Types.FromDecompiledSignature signature returnMetadataType in
        Concrete lambda typ metadata

    let internal MakeLambda2 metadata state (signature : IFunctionSignature) (returnMetadataType : IMetadataType) (lambda : SymbolicLambda<'a>) =
        let term = MakeLambdaTerm metadata signature returnMetadataType lambda in
        if Transformations.isInlinedSignatureCall signature
            then Memory.allocateInHeap metadata state term
            else term, state

    let (|Lambda|_|) = function
        | Concrete(lambda, t) when Types.IsFunction t && (lambda :? SymbolicLambda<'a>) ->
            Some(Lambda(lambda :?> SymbolicLambda<'a>))
        | _ -> None

    module UnboundedRecursionExplorer =

        type SymbolicEffect = State.state -> State.state

        let private unboundedApproximationAttempts = new Dictionary<FunctionIdentifier, int>()
        let private initialStates = new Dictionary<FunctionIdentifier, State.state>()
        let private symbolicEffects = new Dictionary<FunctionIdentifier, SymbolicEffect list>()
        let private symbolicResults = new Dictionary<FunctionIdentifier, StatementResult>()
        let private terminationRelatedState = new Dictionary<FunctionIdentifier, Term seq>()

        type internal IInterpreter =
            abstract member Initialize : State.state -> (State.state -> 'a) -> 'a
            abstract member InitializeStaticMembers : State.state -> string -> (StatementResult * State.state -> 'a) -> 'a
            abstract member Invoke : FunctionIdentifier -> State.state -> Term option -> (StatementResult * State.state -> 'a) -> 'a
        type private NullActivator() =
            interface IInterpreter with
                member x.InitializeStaticMembers _ _ _ =
                    internalfail "interpreter for unbounded recursion is not ready"
                member x.Invoke _ _ _ _ =
                    internalfail "interpreter for unbounded recursion is not ready"
                member this.Initialize _ _ =
                    internalfail "interpreter for unbounded recursion is not ready"
        let mutable internal interpreter : IInterpreter = new NullActivator() :> IInterpreter

//        let private findReadDependencies terms =
//            let filterMapConstant deps = function
//                | Constant(_, LazyInstantiation location, _) as term when not (List.exists (fst >> ((=) location)) deps) ->
//                    Some (location, term)
//                | _ -> None
//            let unsorted = Terms.filterMapConstants filterMapConstant terms in
//            List.sortWith (fun (loc1, _) (loc2, _) -> Memory.compareRefs loc1 loc2) unsorted
//
//        let private overwriteReadWriteDependencies id readDeps writeDeps =
//            let currentWriteDeps = writeDependencies.[id] in
//            readDependencies.[id] <- readDeps
//            writeDependencies.[id] <- writeDeps
//            let result = List.length currentWriteDeps = List.length writeDeps
//            unboundedApproximationFinished.[id] <- result
//            result
//
//        let private exceptionsFirst (xG, xS) (yG, yS) =
//            match xS, yS with
//            | _ when xS = yS -> 0
//            | Throw _, _ -> -1
//            | _, _ -> 1
//
//        let private bubbleUpExceptions = function
//            | Guarded gvs -> List.sortWith exceptionsFirst gvs |> Guarded
//            | r -> r
//
//        let rec private makeMutuallyExclusiveGuards acc res xs =
//            match xs with
//            | [] -> Terms.MakeNAry OperationType.LogicalAnd (List.map (fun t -> Terms.Negate t) acc) false Bool |> fun g -> g::res
//            | x::xs -> Terms.MakeNAry OperationType.LogicalAnd (x::(List.map (fun t -> Terms.Negate t) acc)) false Bool |> fun g -> makeMutuallyExclusiveGuards (x::acc) (g::res) xs
//
//        let private mutuallyExclusiveGuards guards =
//            match guards with
//            | [] -> internalfail "empty guard"
//            | [x] -> guards
//            | x::xs -> makeMutuallyExclusiveGuards [] [] (List.rev xs)
//
//        let rec private symbolizeUnboundedResult source id = function
//            | NoResult
//            | Break
//            | Return Nop -> NoResult
//            | Return term ->
//                let resultName = IdGenerator.startingWith(toString id + "%%res") in
//                // TODO: time!
//                let result = State.makeSymbolicInstance 0u source resultName (Terms.TypeOf term) in
//                Return result
//            | Throw e ->
//                let resultName = IdGenerator.startingWith(toString id + "%%err") in
//                // TODO: time!
//                let error = State.makeSymbolicInstance 0u source resultName (Terms.TypeOf e) in
//                Throw error
//            | Guarded gvs ->
//                let guards, results = List.unzip gvs in
//                let symbolizedGuards = List.map (fun _ -> VSharp.Constant(IdGenerator.startingWith(toString id + "%%guard"), source, Bool)) guards in
//                let symbolizedGuards = mutuallyExclusiveGuards symbolizedGuards in
//                let symbolizedResults = List.map (symbolizeUnboundedResult source id) results in
//                Guarded(List.zip symbolizedGuards symbolizedResults)
//            | r -> internalfail ("unexpected result of the unbounded encoding " + toString r)
//
//        let internal invokeUnboundedRecursion state id k =
//            let sourceRef = ref Nop in
//            let readDepsLocations = readDependencies.[id] |> List.unzip |> fst in
//            let writeDepsLocations = writeDependencies.[id] in
//            let readDeps = readDepsLocations |> List.map (Memory.deref state) |> List.unzip |> fst in
////            let writeDeps, state' = writeDepsLocations |> Memory.symbolizeLocations state sourceRef in
//            let writeDeps, state' = [], state in
//
//            let result = symbolizeUnboundedResult (UnboundedRecursion (TermRef sourceRef)) id unboundedFunctionResult.[id] in
//            let isSymbolizedConstant _ = function
//                | Constant (_, UnboundedRecursion (TermRef r), _) as c when LanguagePrimitives.PhysicalEquality r sourceRef -> Some c
//                | _ -> None
//            in
//            let resultConstants = Terms.filterMapConstants isSymbolizedConstant [ControlFlow.resultToTerm result] in
//            let allResults = List.append resultConstants writeDeps in
//
//            let applicationTerm = Expression(Application id, List.append readDeps allResults, Bool) in
//            sourceRef := applicationTerm
//            k (result, state')

        let internal reproduceEffect id state k =
            k (NoResult Metadata.empty, state)

        let internal approximate id (result, state) =
            let attempt = unboundedApproximationAttempts.[id] + 1 in
            if attempt > Options.WriteDependenciesApproximationTreshold() then
                failwith "Approximating iterations limit exceeded! Either this is an iternal error or some function is really complex. Consider either increasing approximation treshold or reporing it."
            else
                unboundedApproximationAttempts.[id] <- attempt
                let initialState = initialStates.[id] in
                true
//                let writeDependencies = Memory.diff symbolicState state in
//                let writeDependenciesTerms = writeDependencies |> List.map (function | Memory.Mutation (_, t) -> t | Memory.Allocation (_, t) -> t)
//                let result = bubbleUpExceptions result in
//                let readDependencies = findReadDependencies ((ControlFlow.resultToTerm result)::writeDependenciesTerms) in
//                unboundedFunctionResult.[id] <- result
//                overwriteReadWriteDependencies id readDependencies writeDependencies

        let rec private doExplore id state this k =
            interpreter.Invoke id state this (fun result ->
            if approximate id result then k result
            else doExplore id state this k)

        let internal explore id k =
            let metadata = Metadata.empty in
            unboundedApproximationAttempts.[id] <- 0
            symbolicEffects.[id] <- []
            let initialSymbolicResult =
                NoResult metadata
//                match returnType with
//                | Void -> NoResult
//                | _ ->
//                    let resultName = IdGenerator.startingWith(toString id + "%%initial-res") in
//                    // TODO: time!
//                    State.makeSymbolicInstance 0u (UnboundedRecursion (TermRef (ref Nop))) resultName returnType |> Return
            in symbolicResults.[id] <- initialSymbolicResult
            interpreter.Initialize State.empty (fun newState ->
            let this, state =
                match id with
                | MetadataMethodIdentifier mm ->
                    interpreter.InitializeStaticMembers newState mm.DeclaringType.AssemblyQualifiedName (fun (_, state) ->
                    match mm with
                    | _ when mm.IsStatic -> (None, state)
                    | _ ->
                        // TODO: declaring type should be symbolic here
                        let declaringType = mm.DeclaringType.AssemblyQualifiedName |> System.Type.GetType |> Types.Constructor.FromConcreteDotNetType in
                        let instance, state = Memory.allocateSymbolicInstance metadata state declaringType in
                        if Terms.IsHeapRef instance then (Some instance, state)
                        else
                            let key = ("external data", mm.Token.ToString()) in
                            let state = Memory.newStackFrame state metadata (MetadataMethodIdentifier null) [(key, State.Specified instance, Some declaringType)] in
                            (Some <| Memory.referenceLocalVariable metadata state key true, state))
                | DelegateIdentifier ast ->
                    __notImplemented__()
                | StandardFunctionIdentifier _ -> __notImplemented__()
            in initialStates.[id] <- state
            doExplore id state this k)

        let internal exploreIfShould id k =
            if unboundedApproximationAttempts.ContainsKey id then k ()
            else
                explore id (fun _ -> k ())
