namespace VSharp.Analyzer

open System.Reflection
open System.Collections.Generic
open System.Reflection.Emit

open VSharp.Interpreter.IL
open CFG
open FSharpx.Collections
open VSharp
open VSharp.Core

module Properties =
    let internal exitVertexOffset = -1
    let internal exitVertexNumber = -1

    let internal initialVertexOffset = 0

module public CFA =
    let mutable stepItp : ILInterpreter option = None
    let configureInterpreter itp = stepItp <- Some itp

    let private formInitialState (m : MethodBase) =
         match stepItp with
         | None -> __unreachable__()
         | Some itp ->
            let ilmm = itp.MakeMethodIdentifier m
            let state, this, thisIsNotNull, _ = itp.FormInitialState ilmm
            let initialState =
                match this with
                | None -> state
                | Some _ -> AddConditionToState state thisIsNotNull
            initialState, this

    let private makeFunctionResultConstant (m : MethodBase) =
        match stepItp with
        | None -> __unreachable__()
        | Some itp ->
            let ilmm = itp.MakeMethodIdentifier m
            API.Terms.MakeFunctionResultConstant ilmm m

    [<AllowNullLiteral>]
    type Vertex(m : MethodBase, number, offset) =
        let lemmas = Lemmas(m, offset)
        let paths = Paths(m, offset)
        let queries = Queries(m, offset)
        let solver = Solver.SolverPool.mkSolver()
        let incomingEdges: List<Edge> = List<_>()
        let outgoingEdges: List<Edge> = List<_>()

        member x.Lemmas = lemmas
        member x.Paths = paths
        member x.Queries = queries
        member x.Solver = solver
        member x.IncomingEdges = incomingEdges
        member x.OutgoingEdges = outgoingEdges
        member x.Offset = offset
        member x.Number = number
        member x.IsStartVertex () = offset = 0
        override x.ToString() =
            sprintf "Number = %d Offset = %d\n" number offset +
            "Edges: \n" + Seq.fold (fun acc edge -> acc + edge.ToString()) "" x.OutgoingEdges

    and
        [<AbstractClass>]
        Edge(src : Vertex, dst : Vertex) =
            member x.Src = src
            member x.Dst = dst

    and
        MethodRepresentation =
            { method: MethodBase
              vertices: Dictionary<offset, Vertex>
              initialVertex: Vertex
              exitVertex: Vertex }
        with
            override x.ToString() =
                "Method: " + x.method.ToString() +
                Seq.fold (fun acc (pair : KeyValuePair<_,Vertex>) -> acc + "\n" + pair.Value.ToString()) "" x.vertices


    type StepEdge(src : Vertex, dst : Vertex, effect : state) =
        inherit Edge(src, dst)
        member x.Effect = effect
        member x.VisibleVariables() = __notImplemented__()

        override x.ToString() =
            sprintf "Step Edge [%O --> %O] \n effect = %O\n" x.Src.Number x.Dst.Number (API.Memory.Dump effect)

    type CallEdge(src : Vertex, dst : Vertex, methodBase: MethodBase, actualArgs : term list, computeRepr) =
        inherit Edge(src, dst)

        let computeExitNodeForMethod() =
            if methodBase.IsAbstract then
                None
            else
                match stepItp with
                | None -> __unreachable__()
                | Some itp ->
                    let state, this = formInitialState methodBase
                    let repr = computeRepr state this methodBase
                    Some repr

        member x.ExitNodeForCall() = lazy (
            let repr = computeExitNodeForMethod()
            repr.Value.exitVertex
                                          )
        member x.CallVariables() = __notImplemented__()
        member x.ActualArgs () = actualArgs
        override x.ToString() =
            sprintf "Call Edge [%O --> %O] \n method = %O\n%s\n" x.Src.Number x.Dst.Number methodBase
                (List.fold (fun acc arg -> acc + arg.ToString() + " ") "args = " actualArgs)

    type ReturnResultEdge(src : Vertex, dst : Vertex, methodBase: MethodBase, result : term option) =
        inherit Edge(src, dst)
        override x.ToString() =
            sprintf "ReturnResult Edge [%O --> %O] \n result = %s\n" x.Src.Number x.Dst.Number
                (match result with | Some res -> res.ToString() | _ -> "void")

    module MethodRepresentationBuilder =
        let private alreadyComputedReprs = Dictionary<MethodBase, MethodRepresentation>()

        let private createEmptyRepresentation () =
            { method = null
              vertices = Dictionary<offset, Vertex>()
              initialVertex = null
              exitVertex = null }

        let private addEdge (edge : Edge) =
            edge.Src.OutgoingEdges.Add edge
            edge.Dst.IncomingEdges.Add edge

        let private executeInstructions (ilintpr : ILInterpreter) cfg cilState =
            assert (cilState.currentVertex <> destination.Return)
            let lastOffset = Seq.last cfg.sortedOffsets
            let u = cilState.currentVertex.Vertex ()
            let startingOffset = cfg.sortedOffsets.[u]
            let endOffset =
                if startingOffset = lastOffset then lastOffset + 1
                else cfg.sortedOffsets.[u + 1]
            let isOffsetOfCurrentVertex (offset : destination) = startingOffset <= offset.Vertex() && offset.Vertex() < endOffset
            let rec executeAllInstructions (offset : destination) cilState =
                let cilStates =
                    ilintpr.ExecuteInstruction cfg (offset.Vertex()) cilState
                    |> List.filter (fun (_, cilState : cilState) -> not cilState.HasException) //TODO: implement exceptions
                match cilStates with
                | list when List.forall (fst >> (=) destination.Return) list -> List.map (fun (_, state) -> { state with currentVertex = destination.Return}) list
                | (nextOffset, _)::xs as list when isOffsetOfCurrentVertex nextOffset
                                                   && List.forall (fun (offset, _) -> offset = nextOffset && isOffsetOfCurrentVertex offset) xs ->
                    List.collect ((<||) executeAllInstructions) list
                | list -> list |> List.map (fun (offset, cilSt) -> {cilSt with currentVertex = Intermediate (cfg.sortedOffsets.BinarySearch (offset.Vertex()))})
            executeAllInstructions (Intermediate startingOffset) cilState

        let rec private computeAction (ilintpr : ILInterpreter) (repr: MethodRepresentation) (cfg: cfgData) v outgoingEdges (cilState: cilState) =
            let offset = cfg.sortedOffsets.[v]
            let srcVertex = repr.vertices.[v]
            match cfg.IsCallOrNewObjOffset offset with
            | true ->
                let opcode = Instruction.parseInstruction cfg.ilBytes offset
                let calledMethod = InstructionsSet.resolveMethodFromMetadata cfg (offset + opcode.Size)
                let actualArgs, stackAfterCall =
                    let argumentsNumber =
                        if not calledMethod.IsStatic || opcode = OpCodes.Callvirt then 1 else 0
                        + calledMethod.GetParameters().Length
                        - if opcode = OpCodes.Newobj then 1 else 0
                    let rec pick n acc stack =
                        if n = 0 then acc, stack else pick (n - 1) (List.head stack :: acc) (List.tail stack)
                    pick argumentsNumber [] cilState.opStack
//                let ilintpr = ILInterpreter()
//                let methodId = ilintpr.MakeMethodIdentifier calledMethod
//                let state, _, _, _ = ilintpr.FormInitialState methodId
                assert (Seq.length outgoingEdges = 1)
                let w = Seq.head outgoingEdges
                new CallEdge(srcVertex, repr.vertices.[w], calledMethod, actualArgs, computeRepresentation ilintpr) |> addEdge
                let functionResult = makeFunctionResultConstant calledMethod
                (Intermediate w, functionResult :: stackAfterCall) :: []
            | _ ->
//                let methodId = ilintpr.MakeMethodIdentifier (repr.method)
//                let cpIntpr = CodePortionInterpreter(ilintpr, methodId, cfg, [])
                let newStates = executeInstructions ilintpr cfg cilState
                newStates |> List.map (fun (st : cilState) ->
                    match st.currentVertex with
                    | Return ->
                        new ReturnResultEdge(srcVertex, repr.vertices.[Properties.exitVertexOffset], repr.method, st.functionResult) |> addEdge
                        st.currentVertex, []
                    | Intermediate w ->
                        new StepEdge(srcVertex, repr.vertices.[w], st.state) |> addEdge
                        Intermediate w, st.opStack
                    | destination.AnyWhere -> __unreachable__()
                )

        and computeRepresentation (ilintptr : ILInterpreter) (emptyStateForMethod : state) (this : term option) (methodBase: MethodBase) : MethodRepresentation =
            match alreadyComputedReprs.ContainsKey methodBase with
            | true -> alreadyComputedReprs.[methodBase]
            | _ ->
                let cfg = CFG.build methodBase
                let size = cfg.graph.Count
                let repr = { createEmptyRepresentation () with method = methodBase }

                let addVertices repr =
                    cfg.sortedOffsets |> Seq.iteri (fun i offset ->
                        if repr.vertices.TryAdd(i, Vertex(repr.method, i, offset)) then ()
                        else ()
                    )
//                        repr.vertices.Add(i, Vertex(i, offset)))
                    { repr with initialVertex = repr.vertices.[Properties.initialVertexOffset] }

                let addExitVertex repr =
                    let exit = Vertex(repr.method, Properties.exitVertexNumber, Properties.exitVertexOffset)
                    repr.vertices.Add(Properties.exitVertexOffset, exit)
                    { repr with exitVertex = exit }

                let initQueue () =
                    let used = Seq.init size (fun _ -> false) |> Seq.toArray
                    used.[0] <- true
                    let q = System.Collections.Generic.Queue<offset * operationalStack>()
                    q.Enqueue(0, [])
                    used, q

                let repr = repr |> addVertices |> addExitVertex
                let used, q = initQueue()

                let rec bfs () =
                    if q.Count = 0 then ()
                    else let v, opStack = q.Dequeue()
                         let cilState = InstructionsSet.makeEmptyState (Intermediate v) destination.AnyWhere emptyStateForMethod
                         let cilState = { cilState with opStack = opStack; this = this }
                         computeAction ilintptr repr cfg v cfg.graph.[v] cilState
                         |> List.iter (fun (w, opStack) ->
                             match w with
                             | Return -> ()
                             | Intermediate w ->
                                 if not <| used.[w] then
                                     used.[w] <- true
                                     q.Enqueue (w, opStack)
                             | _ -> __unreachable__()
                         )
                         bfs ()
                bfs ()
                repr

type StepInterpreter() =
    inherit ILInterpreter()
    override x.CreateInstance exceptionType arguments state = Nop, state
    override x.Invoke codeLoc oldState this k =
        match codeLoc with
        | :? ILMethodMetadata as ilmm ->
            CFA.configureInterpreter x
            let state, this, thisIsNotNull, _ = x.FormInitialState ilmm
            let initialState =
                match this with
                | None -> state
                | Some _ -> AddConditionToState state thisIsNotNull
            let methodRepr = CFA.MethodRepresentationBuilder.computeRepresentation x initialState this ilmm.methodBase
            Logger.printLog Logger.Trace "Computed Method Representation: %s" (methodRepr.ToString())
            k (Terms.Nop, oldState)

//            let interpreter = new CodePortionInterpreter(x, ilmm, findCfg ilmm, [])
//            interpreter.Invoke state this k
        | _ -> internalfail "unhandled ICodeLocation instance"
