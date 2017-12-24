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

    module Explorer =

        let private currentlyExploredFunctions = new HashSet<FunctionIdentifier>()

        type internal IInterpreter =
            abstract member Reset : unit -> unit
            abstract member Invoke : FunctionIdentifier -> State.state -> Term option -> (StatementResult * State.state -> 'a) -> 'a
        type private NullInterpreter() =
            interface IInterpreter with
                member this.Reset() =
                    internalfail "interpreter is not ready"
                member this.Invoke _ _ _ _ =
                    internalfail "interpreter is not ready"
        let mutable internal interpreter : IInterpreter = new NullInterpreter() :> IInterpreter

        let private formInitialStatics metadata typ typeName =
                let staticMemoryKey = Terms.MakeStringKey typeName
                let staticMemoryEntry = Struct metadata Heap.empty typ
                Heap.empty.Add(staticMemoryKey, (staticMemoryEntry, Timestamp.zero, Timestamp.zero))

        let internal invoke id state this k =
            interpreter.Invoke id state this k

        let internal explore id k =
            interpreter.Reset()
            let metadata = Metadata.empty
            currentlyExploredFunctions.Add id |> ignore
            let this, state =
                match id with
                | MetadataMethodIdentifier mm ->
                    let declaringQualifiedName = mm.DeclaringType.AssemblyQualifiedName
                    let declaringType = declaringQualifiedName |> System.Type.GetType |> Types.Constructor.FromConcreteDotNetType in
                    let initialState = { State.empty with statics = State.Defined false (formInitialStatics metadata declaringType declaringQualifiedName) }
                    match mm with
                    | _ when mm.IsStatic -> (None, initialState)
                    | _ ->
                        // TODO: declaring type should be symbolic here
                        let instance, state = Memory.allocateSymbolicInstance metadata initialState declaringType
                        if Terms.IsHeapRef instance then (Some instance, state)
                        else
                            let key = ("external data", mm.Token.ToString()) in
                            let state = Memory.newStackFrame state metadata (MetadataMethodIdentifier null) [(key, State.Specified instance, Some declaringType)] in
                            (Some <| Memory.referenceLocalVariable metadata state key true, state)
                | DelegateIdentifier ast ->
                    __notImplemented__()
                | StandardFunctionIdentifier _ -> __notImplemented__()
            invoke id state this (fun r ->
                currentlyExploredFunctions.Remove id |> ignore
                Database.report id r
                k r)

        let internal reproduceEffect mtd funcId (state : State.state) k =
            let addr = [Memory.freshAddress()] in
            let time = Memory.tick() in
            if currentlyExploredFunctions.Contains funcId then
                // TODO: this is just a temporary hack!!
                let recursiveResult = NoResult mtd in
                let recursiveState = { state with heap = State.RecursiveApplication(funcId, addr, time); statics = State.RecursiveApplication(funcId, addr, time) } in
                k (recursiveResult, recursiveState)
            else
                let exploredResult, exploredState = Database.query funcId ||?? lazy(explore funcId id) in
                let result = Memory.fillHoles mtd addr time state (ControlFlow.resultToTerm exploredResult) |> ControlFlow.throwOrReturn in
                let state = Memory.composeStates mtd addr time state exploredState in
                k (result, state)
