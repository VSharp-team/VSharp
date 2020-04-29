namespace VSharp.Interpreter.IL

open System.Reflection
open System.Collections.Generic

open System.Collections
open System.Reflection.Emit
open CFG
open FSharpx.Collections
open VSharp
open VSharp.Core

module Properties =
    let internal exitVertexOffset    = -1
    let internal exitVertexNumber    = -1

    let internal initialVertexOffset = 0

module public Engine =
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
    type Guard = term

    type Lemma = Guard

    type Formula = Guard
    type executionState = {
        state : state
        exceptionFlag : term option
    }

    type Model =
        class
        end

    type Query =
        { property: Guard
          level: int }

    [<AllowNullLiteral>]
    type Vertex(number, offset) =
        let sigma: Dictionary<int, HashSet<Lemma>> = Dictionary<_, _>()
        let rho: Dictionary<int, HashSet<executionState>> = Dictionary<_, _>()
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
    and Edge =
        { action: Action
          src: Vertex
          dst: Vertex }
        with
            override x.ToString() =
                x.src.Number.ToString() + " -> " + x.dst.Number.ToString() + "\n"
                + "action = " + x.action.ToString() + "\n"
    and
        [<AbstractClass>]
        Action() =
            let lemmaCandidates: Dictionary<Query, Lemma> = Dictionary<_, _>()
            member x.AlreadyAnswered = lemmaCandidates.ContainsKey
            member x.AddLemmaCandidate = lemmaCandidates.Add
            abstract IsCall: unit -> bool
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

    type SMTResult =
        { isUnSat: bool
          model: Model
          rhoUsed: Dictionary<Vertex, state>
          sigmaUsed: Dictionary<Vertex, Lemma> }



    type SimpleAction(state: state) =
        inherit Action()
        override x.IsCall() = false
        member x.State = state
        member x.VisibleVariables() = __notImplemented__()
        override x.ToString() =
            "Simple Action \nstate = "
            + API.Memory.Dump state + "\n"

    type CallAction(methodBase: MethodBase, actualArgs : term list, computeRepr) =
        inherit Action()

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

        override x.IsCall() = true
        member x.ExitNodeForCall() = lazy (
            let repr = computeExitNodeForMethod()
            repr.Value.exitVertex
                                          )
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
    type ThrowExceptionAction(state : state, methodBase: MethodBase, offsetOfThrowingInstruction) =
        inherit Action()
        override x.IsCall() = false
        override x.ToString() = sprintf "ThrowExceptionAction \n offset = %d\n" offsetOfThrowingInstruction

    module SolverInteraction =
        let checkSat (vertices: List<Vertex>) (phi: Formula) (q: Query): SMTResult =
            __notImplemented__()

    module MethodRepresentationBuilder =
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
        let private executeInstructions (ilintpr : ILInterpreter) cfg cilState =
            assert (cilState.currentVertex <> destination.Return)
            let lastOffset = Seq.last cfg.sortedOffsets
            let u = cilState.currentVertex.Vertex ()
            let startingOffset = cfg.sortedOffsets.[u]
            let endOffset =
                if startingOffset = lastOffset then lastOffset + 1
                else cfg.sortedOffsets.[u + 1]
            let isOffsetOfCurrentVertex (offset : destination) = startingOffset <= offset.Vertex() && offset.Vertex() < endOffset
            let rec executeAllInstructions (offset : destination) cilState : cilState list * cilState list =
                let allStates = ilintpr.ExecuteInstruction cfg (offset.Vertex()) cilState
                let okStates, errorStates = allStates |> List.fold (fun (oks, errors) ((_, cilState : cilState) as dc) ->
                        if cilState.HasException then oks, dc :: errors
                        else dc :: oks, errors) ([], [])
                let errorStates = errorStates |> List.map (fun (_, cilState) -> {cilState with currentVertex = offset})
                match okStates with
                | list when List.forall (fst >> (=) destination.Return) list -> (List.map (fun (_, state) -> { state with currentVertex = destination.Return}) list), errorStates
                | (nextOffset, _)::xs as list when isOffsetOfCurrentVertex nextOffset
                                                   && List.forall (fun (offset, _) -> offset = nextOffset && isOffsetOfCurrentVertex offset) xs ->
                    List.fold (fun (oks, errors) (d, cilState) ->
                        let oks', errors' = executeAllInstructions d cilState
                        (oks @ oks'), (errors @ errors')) ([], errorStates) list
                | list -> list |> List.map (fun (offset, cilSt) -> {cilSt with currentVertex = Intermediate (cfg.sortedOffsets.BinarySearch (offset.Vertex()))}), errorStates
            executeAllInstructions (Intermediate startingOffset) cilState
        let rec private computeAction (ilintpr : ILInterpreter) (repr : MethodRepresentation) (cfg : cfgData) v outgoingEdges (cilState : cilState) =
            let offset = cfg.sortedOffsets.[v]
            match cfg.IsCallOrNewObjOffset offset with
            | true ->
                let opCode = Instruction.parseInstruction cfg.ilBytes offset
                let calledMethod = InstructionsSet.resolveMethodFromMetadata cfg (offset + opCode.Size)
                let argumentsNumber, neededResultOnStack =
                    match calledMethod with
                    | :? ConstructorInfo  -> calledMethod.GetParameters().Length, true
                    | :? MethodInfo as mi ->
                        let addThis = if not calledMethod.IsStatic || opCode = OpCodes.Callvirt then 1 else 0
                        addThis + calledMethod.GetParameters().Length, (mi.ReturnType <> typedefof<System.Void>)
                    | _ -> __notImplemented__()
                let actualArgs, stackAfterCall =
                    let rec pick n acc stack =
                        if n = 0 then acc, stack else pick (n - 1) (List.head stack :: acc) (List.tail stack)
                    pick argumentsNumber [] cilState.opStack
                let action = CallAction(calledMethod, actualArgs, (computeRepresentation ilintpr))
                assert (Seq.length outgoingEdges = 1)
                let w = Seq.head outgoingEdges
                addEdge repr action v w
                (Intermediate w, if neededResultOnStack
                                 then makeFunctionResultConstant calledMethod :: stackAfterCall
                                 else stackAfterCall)
                |> List.singleton
            | _ ->
                let okStates, errorStates = executeInstructions ilintpr cfg cilState
                errorStates |> List.iter (fun (st : cilState) ->
                    let action = ThrowExceptionAction(st.state, repr.method, st.currentVertex.Vertex())
                    addEdge repr action v Properties.exitVertexOffset )

                okStates |> List.map (fun (st : cilState) ->
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

        and computeRepresentation (ilintptr : ILInterpreter) (emptyStateForMethod : state) (this : term option) (methodBase: MethodBase) : MethodRepresentation =
            match alreadyComputedReprs.ContainsKey methodBase with
            | true -> alreadyComputedReprs.[methodBase]
            | _ ->
                let cfg = CFG.build methodBase
                let size = cfg.graph.Count
                let repr = { createEmptyRepresentation () with method = methodBase }

                let addVertices repr =
                    cfg.sortedOffsets |> Seq.iteri (fun i offset -> repr.vertices.Add(i, Vertex(i, offset)))
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

