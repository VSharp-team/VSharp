namespace VSharp.Interpreter.IL
//
//open System.Reflection
//open System.Collections.Generic
//
//open CFG
//open FSharpx.Collections
//open VSharp
//open VSharp.Core
//
//module Properties =
//    let internal exitVertexOffset    = -1
//    let internal exitVertexNumber    = -1
//
//    let internal initialVertexOffset = 0
//
//module public Engine =
//    let mutable stepInterpreterOption : ILInterpreter option = None
//    let configureInterpreter itp = stepInterpreterOption <- Some itp
//
//    let private formInitialState (m : MethodBase) =
//         match stepInterpreterOption with
//         | None -> __unreachable__()
//         | Some itp ->
//            let state, this, thisIsNotNull, _ = itp.FormInitialState (itp.MakeMethodIdentifier m)
//            let initialState =
//                match this with
//                | None -> state
//                | Some _ -> AddConditionToState state thisIsNotNull
//            initialState, this
//    let private makeFunctionResultConstant (m : MethodBase) =
//        match stepInterpreterOption with
//        | None -> __unreachable__()
//        | Some itp -> API.Terms.MakeFunctionResultConstant (itp.MakeMethodIdentifier m) m
//    type Guard = term
//
//    type Lemma = Guard
//
//    type Formula = Guard
//    type executionState = {
//        state : state
//        exceptionFlag : term option
//    }
//
//    type Model =
//        class
//        end
//
//    type Query =
//        { property: Guard
//          level: int }
//
//    [<AllowNullLiteral>]
//    [<AbstractClass>]
//    type Vertex() =
//        let sigma: Dictionary<int, HashSet<Lemma>> = Dictionary<_, _>()
//        let rho: Dictionary<int, HashSet<executionState>> = Dictionary<_, _>()
//        let queries: Dictionary<int, HashSet<Query>> = Dictionary<_, _>()
//        let incomingEdges: List<Edge> = List<_>()
//        let outgoingEdges: List<Edge> = List<_>()
//        member x.Sigma = sigma
//        member x.Rho = rho
//        member x.Queries = queries
//        member x.IncomingEdges = incomingEdges
//        member x.OutgoingEdges = outgoingEdges
//        abstract member Offset : int
//        abstract member IsStartVertex : bool
//        override x.ToString() =
//            sprintf "Offset = %d\n" x.Offset +
//            "Edges: \n" + Seq.fold (fun acc edge -> acc + edge.ToString()) "" x.OutgoingEdges
//    and Edge =
//        { action: Action
//          src: Vertex
//          dst: Vertex }
//        with
//            override x.ToString() =
//                x.src.Offset.ToString() + " -> " + x.dst.Offset.ToString() + "\n"
//                + "action = " + x.action.ToString() + "\n"
//    and
//        [<AbstractClass>]
//        Action() =
//            let lemmaCandidates: Dictionary<Query, Lemma> = Dictionary<_, _>()
//            member x.AlreadyAnswered = lemmaCandidates.ContainsKey
//            member x.AddLemmaCandidate = lemmaCandidates.Add
//            abstract member IsCall: unit -> bool
//    and
//        MethodRepresentation =
//            { method: MethodBase
//              vertices: Dictionary<offset, Vertex>
//              initialVertex: Vertex
//              exitVertex: Vertex }
//        with
//            override x.ToString() =
//                "Method: " + x.method.ToString() +
//                Seq.fold (fun acc (pair : KeyValuePair<_,Vertex>) -> acc + "\n" + pair.Value.ToString()) "" x.vertices
//
//    type SMTResult =
//        { isUnSat: bool
//          model: Model
//          rhoUsed: Dictionary<Vertex, state>
//          sigmaUsed: Dictionary<Vertex, Lemma> }
//
//    type CFGVertex(offset) =
//        inherit Vertex()
//        override x.IsStartVertex = offset = Properties.initialVertexOffset
//        override x.Offset = offset
//    type PreCatchVertex(exceptionType, offset, catchOffset, failedOffset) =
//        inherit Vertex()
//        override x.IsStartVertex = false
//        override x.Offset = offset
//        member x.CatchOffset = catchOffset
//        member x.ExceptionType = exceptionType
//        member x.FailedOffset = failedOffset
//    type ExitVertex(offset) =
//        inherit Vertex()
//        override x.IsStartVertex = false
//        override x.Offset = offset
//
//    type SimpleAction(state: state) =
//        inherit Action()
//        override x.IsCall() = false
//        member x.State = state
//        member x.VisibleVariables() = __notImplemented__()
//        override x.ToString() =
//            "Simple Action \nstate = "
//            + API.Memory.Dump state + "\n"
//
//    type CallAction(methodBase: MethodBase, actualArgs : term list, computeRepresentation) =
//        inherit Action()
//
//        let computeExitNodeForMethod() =
//            if methodBase.IsAbstract then None
//            else
//                let state, this = formInitialState methodBase
//                let representation = computeRepresentation state this methodBase
//                Some representation
//        override x.IsCall() = true
//        member x.ExitNodeForCall() = lazy (
//            let representationOption = computeExitNodeForMethod()
//            representationOption.Value.exitVertex
//                                          )
//        member x.CallVariables() = __notImplemented__()
//        member x.ActualArgs () = actualArgs
//        override x.ToString() =
//            "Call Action \n method = " + methodBase.ToString() + "\n"
//            + (List.fold (fun acc arg -> acc + arg.ToString() + " ") "args = " actualArgs) + "\n"
//
//    type ReturnResultAction(result : term option) =
//        inherit Action()
//        override x.IsCall() = false
//        override x.ToString() =
//            "ReturnResultAction \n result = "
//            + (if Option.isSome result then (result |> Option.get).ToString() else "void")
//            + "\n"
//    type ThrowExceptionAction(state : state, failedOffset) =
//        inherit Action()
//        override x.IsCall() = false
//        override x.ToString() = sprintf "ThrowExceptionAction \n offset = %d\n" failedOffset
//
//    module SolverInteraction =
//        let checkSat (_: List<Vertex>) (_: Formula) (_: Query): SMTResult =
//            __notImplemented__()
//
//    module MethodRepresentationBuilder =
//        let private alreadyComputedRepresentations = Dictionary<MethodBase, MethodRepresentation>()
//
//        let private createEmptyRepresentation () =
//            { method = null
//              vertices = Dictionary<offset, Vertex>()
//              initialVertex = null
//              exitVertex = null }
//
//        let private addVertex (representation : MethodRepresentation) offset v =
//            if not <| representation.vertices.ContainsKey offset then representation.vertices.Add(offset, v)
//        let private addEdgeBetweenOffsets (representation : MethodRepresentation) action (v : offset) (w : offset) =
//            let edge =
//                { action = action
//                  src = representation.vertices.[v]
//                  dst = representation.vertices.[w] }
//            representation.vertices.[v].OutgoingEdges.Add edge
//            representation.vertices.[w].IncomingEdges.Add edge
//        let private executeInstructions (interpreter : ILInterpreter) cfg cilState =
//            assert (cilState.currentVertex <> destination.Exit)
//            let lastOffset = Seq.last cfg.sortedOffsets
//            let startingOffset = cilState.currentVertex.Vertex ()
//            let u = cfg.sortedOffsets.BinarySearch startingOffset
//            let endOffset =
//                if startingOffset = lastOffset then lastOffset + 1
//                else cfg.sortedOffsets.[u + 1]
//            let isOffsetOfCurrentVertex (offset : destination) = startingOffset <= offset.Vertex() && offset.Vertex() < endOffset
//            let rec executeAllInstructions (offset : destination) cilState : cilState list * cilState list =
//                let allStates = interpreter.ExecuteInstruction cfg (offset.Vertex()) cilState
//                let okStates, errorStates = allStates |> List.fold (fun (oks, errors) ((_, cilState : cilState) as dc) ->
//                        if cilState.HasException then oks, dc :: errors
//                        else dc :: oks, errors) ([], [])
//                let errorStates = errorStates |> List.map (fun (_, cilState) -> {cilState with currentVertex = offset})
//                match okStates with
//                | list when List.forall (fst >> (=) destination.Exit) list -> (List.map (fun (_, state) -> { state with currentVertex = destination.Exit}) list), errorStates
//                | (nextOffset, _)::xs as list when isOffsetOfCurrentVertex nextOffset
//                                                   && List.forall (fun (offset, _) -> offset = nextOffset && isOffsetOfCurrentVertex offset) xs ->
//                    List.fold (fun (oks, errors) (d, cilState) ->
//                        let oks', errors' = executeAllInstructions d cilState
//                        (oks @ oks'), (errors @ errors')) ([], errorStates) list
//                | list -> list |> List.map (fun (offset, cilSt) -> {cilSt with currentVertex = Intermediate (offset.Vertex())}), errorStates
//            executeAllInstructions (Intermediate startingOffset) cilState
//        let private findNextHandlerOffsetAndAddPreCatchVertexIfNeeded (representation : MethodRepresentation) previousHandlerOffset failedOffset =
//            let mb = representation.method.GetMethodBody()
//            let findHandlerStartOffset (c : ExceptionHandlingClause) =
//                match c.Flags with
//                | ExceptionHandlingClauseOptions.Filter -> c.FilterOffset
//                | ExceptionHandlingClauseOptions.Fault -> __notImplemented__()
//                | _ -> c.HandlerOffset
//            let clauses = mb.ExceptionHandlingClauses
//                          |> Seq.filter (fun c -> failedOffset >= c.TryOffset
//                                                  && failedOffset <= c.TryOffset + c.TryLength - 1
//                                                  && findHandlerStartOffset c > previousHandlerOffset)
//                          |> Seq.sortWith (fun c c' ->
//                              match findHandlerStartOffset c, findHandlerStartOffset c' with
//                              | s, s' when s > s' -> 1
//                              | s, s' when s = s' -> 0
//                              | _ -> -1)
//                          |> List.ofSeq
//            let c =
//                match clauses with
//                | [] -> None
//                | c :: _ -> Some c
//            let nextOffset =
//                match clauses with
//                | [] -> Properties.exitVertexOffset
//                | c :: _ when c.Flags = ExceptionHandlingClauseOptions.Filter -> c.FilterOffset
//                | c :: _ when c.Flags = ExceptionHandlingClauseOptions.Clause ->
//                    let negativeOffsetForImaginaryVertex = - c.HandlerOffset
//                    let preCatchVertex = PreCatchVertex(c.CatchType, negativeOffsetForImaginaryVertex, c.HandlerOffset, failedOffset)
//                    addVertex representation negativeOffsetForImaginaryVertex preCatchVertex
//                    negativeOffsetForImaginaryVertex
//                | c :: _ when c.Flags = ExceptionHandlingClauseOptions.Finally ->
//                    // TODO: this is wrong in theory, because control-flow bypasses frames and then returns to finally!
//                    c.HandlerOffset
//                | c :: _ when c.Flags = ExceptionHandlingClauseOptions.Fault -> __notImplemented__()
//                | _ -> __unreachable__()
//            nextOffset, c
//
//        let rec private computeAction (interpreter : ILInterpreter) (representation : MethodRepresentation) (cfg : cfgData) (offset, (state : state), this, opStack) =
//            match offset < 0 with
//            | true ->
//                assert (Option.isSome state.raisedException)
//                let error = Option.get state.raisedException
//                let vertex : PreCatchVertex = representation.vertices.[offset] :?> PreCatchVertex
//                let catchCondition = API.Types.IsCast (API.Types.FromDotNetType state vertex.ExceptionType) error
//                let action = SimpleAction (AddConditionToState state catchCondition)
//                addEdgeBetweenOffsets representation action offset vertex.CatchOffset
//
//                let action = ThrowExceptionAction (AddConditionToState state !!catchCondition, vertex.FailedOffset)
//                let newOffset, clause = findNextHandlerOffsetAndAddPreCatchVertexIfNeeded representation vertex.CatchOffset vertex.FailedOffset
//                addEdgeBetweenOffsets representation action offset newOffset
//                (Intermediate vertex.CatchOffset, None)
//                :: (Intermediate newOffset, clause)
//                :: []
//
////            | _ when cfg.IsCallOrNewObjOffset offset->
////                let opCode = Instruction.parseInstruction cfg.ilBytes offset
////                let calledMethod = InstructionsSet.resolveMethodFromMetadata cfg (offset + opCode.Size)
////                let argumentsNumber, neededResultOnStack =
////                    match calledMethod with
////                    | :? ConstructorInfo  -> calledMethod.GetParameters().Length, true
////                    | :? MethodInfo as mi ->
////                        let addThis = if not calledMethod.IsStatic || opCode = OpCodes.Callvirt then 1 else 0
////                        addThis + calledMethod.GetParameters().Length, (mi.ReturnType <> typedefof<System.Void>)
////                    | _ -> __notImplemented__()
////                let actualArgs, stackAfterCall =
////                    let rec pick n acc stack =
////                        if n = 0 then acc, stack else pick (n - 1) (List.head stack :: acc) (List.tail stack)
////                    pick argumentsNumber [] cilState.opStack
////                let action = CallAction(calledMethod, actualArgs, (computeRepresentation ilintpr))
////                let nextOffset =
////                    match Instruction.findNextInstructionOffsetAndEdges opCode cfg.ilBytes offset with
////                    | Choice1Of2 nextOffset -> nextOffset
////                    | _ -> __unreachable__()
////                addEdgeBetweenOffsets repr action offset nextOffset
////                (Intermediate nextOffset
////                    , if neededResultOnStack
////                      then makeFunctionResultConstant calledMethod :: stackAfterCall
////                      else stackAfterCall
////                    /// why there's no exceptions?
////                    , None)
////                |> List.singleton
//            | _ ->
//                let cilState = { cilState.MakeEmpty (Intermediate offset) destination.AnyWhere state
//                                 with this = this; opStack = opStack }
//                let okStates, errorStates = executeInstructions interpreter cfg cilState
//                let res1 = errorStates |> List.map (fun (cilState : cilState) ->
//                    let failedOffset = cilState.currentVertex.Vertex()
//                    let handlerOffset, clause = findNextHandlerOffsetAndAddPreCatchVertexIfNeeded representation -1 failedOffset
//                    let action = ThrowExceptionAction (cilState.state, failedOffset)
//                    addEdgeBetweenOffsets representation action offset handlerOffset
//                    Intermediate handlerOffset, clause)
//                let res2 = okStates |> List.map (fun (cilState : cilState) ->
//                    match cilState.currentVertex with
//                    | Exit ->
//                        assert (cilState.opStack = [])
//                        let action = ReturnResultAction(cilState.functionResult)
//                        addEdgeBetweenOffsets representation action offset Properties.exitVertexOffset
//                    | Intermediate w ->
//                        let action = SimpleAction(cilState.state)
//                        addEdgeBetweenOffsets representation action offset w
//                    | destination.AnyWhere -> __unreachable__()
//                    cilState.currentVertex, None
//                )
//                res1 @ res2
//        and computeRepresentation (interpreter : ILInterpreter) emptyStateForMethod (this : term option) (methodBase: MethodBase) : MethodRepresentation =
//            match alreadyComputedRepresentations.ContainsKey methodBase with
//            | true -> alreadyComputedRepresentations.[methodBase]
//            | _ ->
//                let cfg = CFG.build methodBase
//                let representation = { createEmptyRepresentation () with method = methodBase }
//
//                let addVertices representation =
//                    cfg.sortedOffsets |> Seq.iter (fun offset -> addVertex representation offset (CFGVertex(offset)))
//                    { representation with initialVertex = representation.vertices.[Properties.initialVertexOffset] }
//
//                let addExitVertex representation =
//                    addVertex representation Properties.exitVertexOffset (ExitVertex(Properties.exitVertexOffset))
//                    { representation with exitVertex = representation.vertices.[Properties.exitVertexOffset] }
//
//                let initQueue () =
//                    let used = HashSet<offset>()
//                    used.Add 0 |> ignore
//                    let q = System.Collections.Generic.Queue<offset * ExceptionHandlingClause option>()
//                    q.Enqueue(0, None)
//                    used, q
//
//                let representation = addVertices representation
//                let representation = addExitVertex representation
//                let used, q = initQueue()
//
//                let rec bfs () =
//                    if q.Count = 0 then ()
//                    else let offset, clause = q.Dequeue()
//                         let allocateException state offset (c : ExceptionHandlingClause option) =
//                             match c with
//                             | None -> state, []
//                             | Some c ->
//                                 let t =
//                                     match c.Flags with
//                                     | f when f = ExceptionHandlingClauseOptions.Clause && offset > 0 -> c.CatchType
//                                     | f when f = ExceptionHandlingClauseOptions.Finally -> __notImplemented__() // TODO: should raised exception be Some _ or None?
//                                     | _ -> typeof<System.Exception>
//                                 let error, state = Memory.AllocateException state (Types.FromDotNetType state t)
//                                 state, [error]
//                         let state, operationalStack = allocateException emptyStateForMethod offset clause
//                         computeAction interpreter representation cfg (offset, state, this, operationalStack)
//                         |> List.iter (fun (nextOffset, clause) ->
//                             match nextOffset with
//                             | Exit -> ()
//                             | Intermediate -1 -> () // TODO: [Return] and [Intermediate -1] are equivalent
//                             | Intermediate nextOffset ->
//                                 if not <| used.Contains nextOffset then
//                                     used.Add nextOffset |> ignore
//                                     q.Enqueue (nextOffset, clause)
//                             | _ -> __unreachable__()
//                         )
//                         bfs ()
//                bfs ()
//                alreadyComputedRepresentations.[methodBase] <- representation
//                representation
//
