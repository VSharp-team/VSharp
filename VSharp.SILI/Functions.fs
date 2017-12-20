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

        let private currentlyExploredFunctions = new HashSet<FunctionIdentifier>()

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

        let internal explore id k =
            let metadata = Metadata.empty in
            currentlyExploredFunctions.Add id |> ignore
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
            interpreter.Invoke id state this (fun r ->
                currentlyExploredFunctions.Remove id |> ignore
                Database.report id r
                k r))

        let internal reproduceEffect mtd funcId (state : State.state) k =
            let addr = [Memory.freshAddress()] in
            let time = Memory.tick() in
            if currentlyExploredFunctions.Contains funcId then
                // TODO: this is just a temporary hack!!
                let recursiveResult = NoResult mtd in
                let recursiveState = { state with heap = State.RecursiveApplication(funcId, addr, time) } in
                k (recursiveResult, recursiveState)
            else
                let exploredResult, exploredState = Database.query funcId ||?? lazy(explore funcId id) in
                let result = Memory.fillHoles mtd addr time state (ControlFlow.resultToTerm exploredResult) |> ControlFlow.throwOrReturn in
                let state = Memory.composeStates mtd addr time state exploredState in
                k (result, state)
