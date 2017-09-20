namespace VSharp

open JetBrains.Decompiler.Ast
open JetBrains.Metadata.Reader.API
open System.Collections.Generic

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

        let private unboundedApproximationAttempts = new Dictionary<FunctionIdentifier, int>()
//        let private initialStates = new Dictionary<FunctionIdentifier, State.state>()
        let private startTimes = new Dictionary<FunctionIdentifier, Timestamp>()
        let private terminationRelatedState = new Dictionary<FunctionIdentifier, Term seq>()
        let private recursiveFunctions = new HashSet<FunctionIdentifier>()

        type IInterpreter =
            abstract member InitializeStaticMembers : State.state -> string -> (StatementResult * State.state -> 'a) -> 'a
            abstract member Invoke : FunctionIdentifier -> State.state -> Term option -> (StatementResult * State.state -> 'a) -> 'a
        type private NullActivator() =
            interface IInterpreter with
                member x.InitializeStaticMembers _ _ _ =
                    internalfail "interpreter for unbounded recursion is not ready"
                member x.Invoke _ _ _ _ =
                    internalfail "interpreter for unbounded recursion is not ready"
        let mutable interpreter : IInterpreter = new NullActivator() :> IInterpreter

        let internal markAsRecursive id =
            recursiveFunctions.Add(id) |> ignore

        let internal reproduceEffect mtd id state k =
            Effects.apply mtd id state k

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
            //Effects.initialize id
            let this, state =
                match id with
                | MetadataMethodIdentifier mm ->
                    interpreter.InitializeStaticMembers State.empty mm.DeclaringType.AssemblyQualifiedName (fun (_, state) ->
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
//            in initialStates.[id] <- state
            in startTimes.[id] <- Memory.tick()
            doExplore metadata id state this k

        let internal exploreIfShould id k =
            if unboundedApproximationAttempts.ContainsKey id then k ()
            else
                explore id (fun _ -> k ())
