namespace VSharp.Interpreter.IL

open System.Reflection
open System.Collections.Generic

open System.Collections
open System.Reflection
open CFG
open FSharpx.Collections
open VSharp
open VSharp.Core

module Properties =
    let internal exitVertexOffset    = -1
    let internal exitVertexNumber    = -1

    let internal initialVertexOffset = 0

module public rec Engine =
    type Guard = term

    type Lemma = Guard

    type Formula = Guard

    type Model =
        class
        end

    type Query =
        { property: Guard
          level: int }

    [<AllowNullLiteral>]
    type Vertex(number, offset) =
        let sigma: Dictionary<int, HashSet<Lemma>> = Dictionary<_, _>()
        let rho: Dictionary<int, HashSet<state>> = Dictionary<_, _>()
        let queries: Dictionary<int, HashSet<Query>> = Dictionary<_, _>()
        let incomingEdges: List<Edge> = List<_>()
        let outgoingEdges: List<Edge> = List<_>()
        member x.Sigma = sigma
        member x.Rho = rho
        member x.Queries = queries
        member x.IncomingEdges = incomingEdges
        member x.OutgoingEdges = outgoingEdges
        member x.Offset = offset
        member x.Number = number
        member x.IsStartVertex () = offset = 0
        override x.ToString() =
            sprintf "Number = %d Offset = %d\n" number offset +
            "Edges: \n" + Seq.fold (fun acc edge -> acc + edge.ToString()) "" x.OutgoingEdges

    type SMTResult =
        { isUnSat: bool
          model: Model
          rhoUsed: Dictionary<Vertex, state>
          sigmaUsed: Dictionary<Vertex, Lemma> }

    [<AbstractClass>]
    type Action() =
        let lemmaCandidates: Dictionary<Query, Lemma> = Dictionary<_, _>()
        member x.AlreadyAnswered = lemmaCandidates.ContainsKey
        member x.AddLemmaCandidate = lemmaCandidates.Add
        abstract IsCall: unit -> bool

    type SimpleAction(state: state) =
        inherit Action()
        override x.IsCall() = false
        member x.State = state
        member x.VisibleVariables() = __notImplemented__()
        override x.ToString() =
            "Simple Action \nstate = "
            + API.Memory.Dump state + "\n"

    type CallAction(emptyStateForMethod : state, methodBase: MethodBase, actualArgs : term list) =
        inherit Action()

        let computeExitNodeForMethod() =
            if methodBase.IsAbstract then
                None
            else
                let repr = MethodRepresentationBuilder.computeRepresentation emptyStateForMethod methodBase
                Some repr

        override x.IsCall() = true
        member x.ExitNodeForCall() = lazy (computeExitNodeForMethod())
        member x.CallVariables() = __notImplemented__()
        member x.ActualArgs () = actualArgs
        override x.ToString() =
            "Call Action \n method = " + methodBase.ToString() + "\n"
            + (List.fold (fun acc arg -> acc + arg.ToString() + " ") "args = " actualArgs) + "\n"

    type ReturnResultAction(methodBase: MethodBase, result : term option) =
        inherit Action()
        override x.IsCall() = false
        override x.ToString() =
            "ReturnResultAction \n result = "
            + (if Option.isSome result then (result |> Option.get).ToString() else "void")
            + "\n"

    type Edge =
        { action: Action
          src: Vertex
          dst: Vertex }
    with
        override x.ToString() =
            x.src.Number.ToString() + " -> " + x.dst.Number.ToString() + "\n"
            + "action = " + x.action.ToString() + "\n"

    module SolverInteraction =
        let checkSat (vertices: List<Vertex>) (phi: Formula) (q: Query): SMTResult =
            __notImplemented__()

    module MethodRepresentationBuilder =
        type MethodRepresentation =
            { method: MethodBase
              vertices: Dictionary<offset, Vertex>
              initialVertex: Vertex
              exitVertex: Vertex }
        with
            override x.ToString() =
                "Method: " + x.method.ToString() +
                Seq.fold (fun acc (pair : KeyValuePair<_,Vertex>) -> acc + "\n" + pair.Value.ToString()) "" x.vertices

        let private alreadyComputedReprs = Dictionary<MethodBase, MethodRepresentation>()

        let private createEmptyRepresentation () =
            { method = null
              vertices = Dictionary<offset, Vertex>()
              initialVertex = null
              exitVertex = null }

        let private addEdge (repr : MethodRepresentation) action v w =
            let edge =
                { action = action
                  src = repr.vertices.[v]
                  dst = repr.vertices.[w] }
            repr.vertices.[v].OutgoingEdges.Add edge
            repr.vertices.[w].IncomingEdges.Add edge
        let private computeAction (repr: MethodRepresentation) (cfg: cfgData) v outgoingEdges (cilState: cilState) =
            let offset = cfg.sortedOffsets.[v]
            match cfg.IsCallOffset offset with
            | true ->
                let opcode = Instruction.parseInstruction cfg.ilBytes offset
                let calledMethod = InstructionsSet.resolveMethodFromMetadata cfg (offset + opcode.Size)
                let actualArgs, stackAfterCall =
                    let rec pick n acc stack =
                        if n = 0 then acc, stack else pick (n - 1) (List.head stack :: acc) (List.tail stack)
                    pick (calledMethod.GetParameters().Length) [] cilState.opStack
                let ilintpr = ILInterpreter()
                let methodId = ilintpr.MakeMethodIdentifier calledMethod
                let state, _, _, _ = ilintpr.FormInitialState methodId
                let action = CallAction(state, calledMethod, actualArgs)
                assert (Seq.length outgoingEdges = 1)
                let w = Seq.head outgoingEdges
                addEdge repr action v w
                let functionResult = API.Terms.MakeFunctionResultConstant methodId calledMethod
                (Intermediate w, functionResult :: stackAfterCall) :: []
            | _ ->
                let ilintpr = ILInterpreter()
                let methodId = ilintpr.MakeMethodIdentifier (repr.method)
                let cpIntpr = CodePortionInterpreter(ilintpr, methodId, cfg, [])
                let newStates = cpIntpr.EvaluateOneStep cilState
                newStates |> List.map (fun (st : cilState) ->
                    match st.currentVertex with
                    | Return ->
                        let action = ReturnResultAction(repr.method, st.functionResult)
                        addEdge repr action v Properties.exitVertexOffset
                        st.currentVertex, []
                    | Intermediate w ->
                        let action = SimpleAction(st.state)
                        addEdge repr action v w
                        Intermediate w, st.opStack
                    | destination.AnyWhere -> __unreachable__()
                )

        let computeRepresentation (emptyStateForMethod : state) (methodBase: MethodBase) : MethodRepresentation =
            match alreadyComputedReprs.ContainsKey methodBase with
            | true -> alreadyComputedReprs.[methodBase]
            | _ ->
                let cfg = CFG.build methodBase
                let size = cfg.graph.Count
                let repr = { createEmptyRepresentation () with method = methodBase }

                let addVertices repr =
                    cfg.sortedOffsets |> Seq.iteri (fun i offset ->
                        if repr.vertices.TryAdd(i, Vertex(i, offset)) then ()
                        else ()
                    )
//                        repr.vertices.Add(i, Vertex(i, offset)))
                    { repr with initialVertex = repr.vertices.[Properties.initialVertexOffset] }

                let addExitVertex repr =
                    let exit = Vertex(Properties.exitVertexNumber, Properties.exitVertexOffset)
                    repr.vertices.Add(Properties.exitVertexOffset, exit)
                    { repr with exitVertex = exit }

                let initQueue () =
                    let used = Seq.init size (fun _ -> false) |> Seq.toArray
                    used.[0] <- true
                    let q = System.Collections.Generic.Queue<offset * operationalStack>()
                    q.Enqueue(0, [])
                    used, q

                let repr = addVertices repr
                let repr = addExitVertex repr
                let used, q = initQueue()

                let rec bfs () =
                    if q.Count = 0 then ()
                    else let v, opStack = q.Dequeue()
                         let cilState = InstructionsSet.makeEmptyState (Intermediate v) destination.AnyWhere emptyStateForMethod
                         let cilState = { cilState with opStack = opStack }
                         computeAction repr cfg v cfg.graph.[v] cilState
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

