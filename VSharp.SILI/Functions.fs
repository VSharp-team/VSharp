namespace VSharp

open JetBrains.Decompiler.Ast
open JetBrains.Metadata.Reader.API
open System.Collections.Generic

module Functions =
    type internal SymbolicLambda<'a> = LocationBinding -> State.state -> Term list State.SymbolicValue -> (StatementResult * State.state -> 'a) -> 'a

    let internal MakeLambda metadata state (metadataMethod : IMetadataMethod) (lambda : SymbolicLambda<'a>) =
        let typ = Types.FromMetadataMethodSignature metadataMethod in
        let term = Concrete metadata lambda typ in
        Memory.allocateInHeap metadata state term

    let internal MakeLambdaTerm metadata (signature : IFunctionSignature) (returnMetadataType : IMetadataType) (lambda : SymbolicLambda<'a>) =
        let typ = Types.FromDecompiledSignature signature returnMetadataType in
        Concrete metadata lambda typ

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

        let private unboundedApproximationAttempts = new Dictionary<FunctionIdentifier, int>()
//        let private initialStates = new Dictionary<FunctionIdentifier, State.state>()
        let private startTimes = new Dictionary<FunctionIdentifier, Timestamp>()
        let private terminationRelatedState = new Dictionary<FunctionIdentifier, Term seq>()
        let private recursiveFunctions = new HashSet<FunctionIdentifier>()

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

        let internal markAsRecursive id =
            recursiveFunctions.Add(id) |> ignore

        let internal reproduceEffect mtd id state k =
            Effects.invoke mtd id {state = state; address = [Memory.freshAddress()]; time = Memory.tick() } k

        let internal approximate mtd id (result, state) =
            let attempt = unboundedApproximationAttempts.[id] + 1 in
            if attempt > Options.WriteDependenciesApproximationTreshold() then
                failwith "Approximating iterations limit exceeded! Either this is an iternal error or some function is really complex. Consider either increasing approximation treshold or reporing it."
            else
                unboundedApproximationAttempts.[id] <- attempt
                let startTime = startTimes.[id] in
                Effects.parseEffects mtd id startTime result state

        let rec private doExplore mtd id state this k =
            interpreter.Invoke id state this (fun result ->
            if not <| recursiveFunctions.Contains(id) || approximate mtd id result then k result
            else doExplore mtd id state this k)

        let internal explore id k =
            let metadata = Metadata.empty in
            unboundedApproximationAttempts.[id] <- 0
//<<<<<<< HEAD
//            //Effects.initialize id
//=======
//            symbolicEffects.[id] <- []
//            let initialSymbolicResult =
//                NoResult metadata
////                match returnType with
////                | Void -> NoResult
////                | _ ->
////                    let resultName = IdGenerator.startingWith(toString id + "%%initial-res") in
////                    // TODO: time!
////                    State.makeSymbolicInstance 0u (UnboundedRecursion (TermRef (ref Nop))) resultName returnType |> Return
//            in symbolicResults.[id] <- initialSymbolicResult
            interpreter.Initialize State.empty (fun newState ->
//>>>>>>> b89c9c788a11f871171ea79322805e3b3fd9954e
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
//<<<<<<< HEAD
//            in initialStates.[id] <- state
            in startTimes.[id] <- Memory.tick()
            doExplore metadata id state this k)
//=======
//            in initialStates.[id] <- state
//            doExplore id state this k)
//>>>>>>> b89c9c788a11f871171ea79322805e3b3fd9954e

        let internal exploreIfShould id k =
            if unboundedApproximationAttempts.ContainsKey id then k ()
            else
                explore id (fun _ -> k ())
