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
    let internal initialVertexNumber = 0

module public CFA =
    let mutable stepItp : ILInterpreter option = None
    let configureInterpreter itp = stepItp <- Some itp

    type vertexLabel =
        | FromCFG of offset
        | PreCatchVertex of offset * ExceptionHandlingClause
        | MethodCommonExit
        with
        member x.Foo() = ()

    // TODO: use vertexLabel instead of offset
//    [<AllowNullLiteral;CustomEquality;CustomComparison>]
//    [<CustomEquality;CustomComparison>]
    type Vertex private(id, m : MethodBase, offset, opStack : operationalStack) =
        static let ids : Dictionary<MethodBase, int> = Dictionary<_,_>()
        let lemmas = Lemmas(m, offset)
        let paths = Paths(m, offset)
//        let paths : Paths persistent = let r = new persistent<_>(always (Paths(m, offset)), id) in r.Reset(); r
        let queries = Queries(m, offset)
        let solver = null //Solver.SolverPool.mkSolver()
        let errors = List<cilState>()
        let incomingEdges: List<Edge> = List<_>()
        let outgoingEdges: List<Edge> = List<_>()

        override x.GetHashCode() = (m, offset).GetHashCode()
        override x.Equals(o : obj) =
            match o with
            | :? Vertex as other -> x.Method = other.Method && x.Offset = other.Offset
            | _ -> false
        interface System.IComparable with
            override x.CompareTo(other) =
                match other with
                | :? Vertex as other when x.Method.Equals(other.Method) -> x.Offset.CompareTo(other.Offset)
                | :? Vertex as other -> x.Method.MetadataToken.CompareTo(other.Method.MetadataToken)
                | _ -> -1
        member x.AddErroredStates (newErrors : cilState list) =
            errors.AddRange(newErrors)

        member x.Id with get() = id
        member x.Lemmas = lemmas
//        member x.Reset () = paths.Reset()
//        member x.Restore () = paths.Restore()
        member x.Paths with get () = paths
//        member x.Paths with get() =
//            paths.Value
        member x.Queries = queries
        member x.Solver = solver
        member x.IncomingEdges = incomingEdges
        member x.OutgoingEdges = outgoingEdges
        member x.Offset with get() = offset
        member x.OpStack with get() = opStack
        member x.Method with get() = m
        member x.IsMethodStartVertex with get() = offset = 0
        member x.IsMethodExitVertex with get() = offset = -1
        override x.ToString() =
            sprintf "(Method = %O, Offset = %x, id = %d)\n" m offset id +
            sprintf "Edges count = %d\n" x.OutgoingEdges.Count +
            "Edges: \n" + Seq.fold (fun acc edge -> acc + edge.ToString() + "\n") "" x.OutgoingEdges
        static member CreateVertex method offset opStack =
            let id = Dict.getValueOrUpdate ids method (always 0)
            ids.[method] <- id + 1
            Vertex(id, method, offset, opStack)
    and
        [<AbstractClass>]
        Edge(src : Vertex, dst : Vertex) =
        abstract member Type : string
        abstract member PropagatePath : path -> bool
        member x.PrintLog msg obj = Logger.printLog Logger.Trace "[%s] %s: %O" (x.commonToString()) msg obj
        member x.Src = src
        member x.Dst = dst
        member x.CommonPropagatePath lvl state =
            let pc = List.filter (fun cond -> cond <> Terms.True) state.pc
            let state = {state with pc = pc}
            let newPc = List.fold (fun pc cond -> pc &&& cond) Terms.True state.pc
            if newPc <> Terms.False then
                if dst.IsMethodExitVertex then ()
                dst.Paths.Add {lvl = lvl; state = state}
                true
            else false
        override x.ToString() = x.commonToString()
        member x.commonToString() = sprintf "%s ID:[%d --> %d] Offset:[%x --> %x]" x.Type x.Src.Id x.Dst.Id x.Src.Offset x.Dst.Offset


    type 'a unitBlock =
        {
            entity : 'a
            entryPoint : Vertex
            exitVertex : Vertex
            vertices : Dictionary<int, Vertex>
        }
        with
        member x.AddVertex (v : Vertex) =
            if not <| x.vertices.ContainsKey v.Id then
                x.vertices.Add(v.Id, v)
        member x.WithEntryPoint v = {x with entryPoint = v}
        member x.WithExitVertex v = {x with exitVertex = v}
        member x.FindCallVertex id = x.vertices.[id]

        static member private CreateEmpty entity method entryOffset exitOffset =
            let entry = Vertex.CreateVertex method entryOffset []
            let exit = Vertex.CreateVertex method exitOffset []
            let vertices = Dictionary<int, Vertex>()
            vertices.Add(entry.Id, entry)
            vertices.Add(exit.Id, exit)
            {
                entity = entity
                entryPoint = entry
                exitVertex = exit
                vertices = vertices
            }
        static member CreateEmptyForMethod (method : MethodBase) =
            unitBlock<'a>.CreateEmpty method method Properties.initialVertexOffset Properties.exitVertexOffset
        static member CreateEmptyForFinallyClause (m : MethodBase) (ehc : ExceptionHandlingClause) =
            let entryOffset = ehc.HandlerOffset
            // TODO: check if this formula is forever true
            let exitOffset = ehc.HandlerOffset + ehc.HandlerLength - 1
            unitBlock<'a>.CreateEmpty ehc m entryOffset exitOffset
        override x.ToString() =
            Seq.fold (fun acc vertex -> acc + "\n" + vertex.ToString()) "" x.vertices.Values

    type cfa =
            {
              cfg : cfgData
              body : MethodBase unitBlock
              finallyHandlers : Dictionary<offset, ExceptionHandlingClause unitBlock>
            }
        with
            member x.Method = x.cfg.methodBase
            member x.FindOrCreateFinallyHandler (ehc : ExceptionHandlingClause) =
                let offset = ehc.HandlerOffset
                if x.finallyHandlers.ContainsKey offset then x.finallyHandlers.[offset]
                else
                    let block = unitBlock<ExceptionHandlingClause>.CreateEmptyForFinallyClause x.Method ehc
                    x.finallyHandlers.Add(offset, block)
                    block

//            member x.Reset() =
//                let resetCollection (dict : Dictionary<offset, Vertex>) = dict |> Seq.iter (fun kvp -> kvp.Value.Reset())
//                resetCollection x.body.vertices
//                x.finallyHandlers |> Seq.iter (fun b -> resetCollection b.Value.vertices)

//            member x.Restore() =
//                let restoreCollection (dict : Dictionary<offset, Vertex>) = dict |> Seq.iter (fun kvp -> kvp.Value.Restore())
//                restoreCollection x.body.vertices
//                x.finallyHandlers |> Seq.iter (fun b -> restoreCollection b.Value.vertices)

            member x.WithEntryPoint v = {x with body = x.body.WithEntryPoint v}
            member x.WithExitVertex v = {x with body = x.body.WithExitVertex v}
            member x.WithFinallyEntryPoint offset v = x.finallyHandlers.[offset].WithEntryPoint v |> ignore; x
            member x.WithFinallyExitVertex offset v = x.finallyHandlers.[offset].WithExitVertex v |> ignore; x
            override x.ToString() =
                let separator = "\n"
                "Method: " + x.Method.ToString() + separator +
                x.body.ToString() + separator +
                Seq.fold (fun acc block -> acc + block.ToString() + separator) "" (x.finallyHandlers.Values)



    type StepEdge(src : Vertex, dst : Vertex, effect : state) =
        inherit Edge(src, dst)
        do
            Prelude.releaseAssert(Map.isEmpty effect.callSiteResults)
        override x.Type = "StepEdge"
        override x.PropagatePath (path : path) =

            Memory.ComposeStates path.state effect (fun state ->
//                x.PrintLog "composition left" path.state
//                x.PrintLog "composition right" effect
//                x.PrintLog "composition result" state
                x.CommonPropagatePath (path.lvl + 1u) state)

        member x.Effect = effect
        member x.VisibleVariables() = __notImplemented__()

        override x.ToString() =
            sprintf "%s\neffect = %O\npc = %O\n" (base.ToString()) (API.Memory.Dump effect) effect.pc

    type CallEdge(src : Vertex, dst : Vertex, callSite : callSite, thisOption : term option, args : term list) =
        inherit Edge(src, dst)
        override x.Type = "Call"
        override x.PropagatePath (path : path) =
            let addCallSiteResult callSiteResults callSite res = Map.add callSite res callSiteResults
            let k cilStates =
                let states = List.map (fun (cilState : cilState) -> cilState.state) cilStates
                let propagateStateAfterCall acc state =
                    assert(path.state.frames = state.frames)
                    assert(path.state.stack = state.stack)
//                    x.PrintLog "composition left" path.state
//                    x.PrintLog "composition this" thisOption
//                    x.PrintLog "composition args" args
//                    x.PrintLog "composition result" state
                    let result' = x.CommonPropagatePath (path.lvl + 1u) {state with callSiteResults = addCallSiteResult path.state.callSiteResults callSite state.returnRegister
                                                                                    returnRegister = None}
                    acc || result'
                List.fold propagateStateAfterCall false states
            let k1 results = results |> List.unzip |> snd |> k
            Prelude.releaseAssert (Option.isSome stepItp)
            let interpreter = stepItp |> Option.get
            let fillTerm term = Memory.FillHoles path.state term fst
            let concreteArgs = List.map fillTerm args
            let concreteThis = Option.map fillTerm thisOption
            let concreteCilState = cilState.MakeEmpty (Instruction src.Offset) (Instruction dst.Offset) path.state


            match callSite.opCode with
            | Instruction.Call     -> interpreter.CommonCall callSite.calledMethod concreteCilState concreteThis concreteArgs k1
            | Instruction.NewObj   -> interpreter.CommonNewObj callSite.calledMethod {concreteCilState with opStack = List.rev concreteArgs} (fun cilStates ->
                cilStates |> List.map (fun (cilState : cilState) -> {cilState with state = {cilState.state with returnRegister = cilState.opStack |> List.head |> Some}}) |> k)
            | Instruction.CallVirt -> interpreter.CommonCallVirt callSite.calledMethod concreteCilState (Option.get concreteThis) concreteArgs k1
            | _ ->  __notImplemented__()


//            let oldState = path.state
//            let makeStateWithOldFrames state =
//                {state with stack = oldState.stack; frames = oldState.frames; returnRegister = None
//                            callSiteResults = addCallSiteResult oldState.callSiteResults callSite state.returnRegister}
//            let methodId = interpreter.MakeMethodIdentifier callSite.methodBase


//            let concreteCall _ state this args k =
//                let emptyState, emptyThis, _, _ = interpreter.FormInitialState methodId
//                interpreter.Invoke methodId emptyState emptyThis (fun state ->
//                let stateWithConcreteArgsOnFrame = interpreter.ReduceFunctionSignature methodId Memory.EmptyState callSite.methodBase this (Specified args) (fun state ->
//                Memory.ComposeStates oldState state k))


//            interpreter.ReduceMethodBaseCall callSite.methodBase emptyCilState this Unspecified concreteCall (fun resCilStates ->
//                let _, cilStates = List.unzip resCilStates
//                let resStates = List.map (fun (cilState : cilState) -> cilState.state) cilStates
//                resStates |> List.fold (fun acc resState ->
//                    let propagated = Memory.ComposeStates stateWithConcreteArgsOnFrame resState ((makeStateWithOldFrames >> x.CommonPropagatePath (path.lvl + 1u)))
//                    acc || propagated) false)

//                Memory.ComposeStates oldState stateWithArgsOnFrame (fun stateWithConcreteArgs ->
//                let stateWithoutCallSiteResults = {stateWithConcreteArgs with callSiteResults = Map.empty}
//                interpreter.Invoke methodId stateWithoutCallSiteResults this (fun (_, state) ->
//                let stateWithOldStack = {state with stack = oldState.stack; frames = oldState.frames
//                                                    callSiteResults = addCallSiteResult oldState.callSiteResults callSite state.returnRegister}
//                x.CommonPropagatePath (path.lvl + 1u) {stateWithOldStack with returnRegister = None}
//                ))
        member x.ExitNodeForCall() = __notImplemented__()
        member x.CallVariables() = __notImplemented__()
        override x.ToString() =
//            let framesCount = List.length stateWithArgsOnFrame.frames.f
//            Prelude.releaseAssert(framesCount <= 3)
            base.ToString()
//                (List.fold (fun acc arg -> acc + arg.ToString() + " ") "args = " actualArgs)

//    type ReturnResultEdge(src : Vertex, dst : Vertex, methodBase: MethodBase, result : term option) =
//        inherit Edge(src, dst)
//        override x.ToString() =
//            sprintf "ReturnResult Edge [%O --> %O] \n result = %s\n" x.Src.Number x.Dst.Number
//                (match result with | Some res -> res.ToString() | _ -> "void")

    module cfaBuilder =
        let private alreadyComputedCFAs = Dictionary<MethodBase, cfa>()

        let private createEmptyCFA cfg method =
            {
              cfg = cfg
              body = unitBlock<MethodBase>.CreateEmptyForMethod method
              finallyHandlers = Dictionary<_,_>()
            }

        let private addEdge (edge : Edge) =
            edge.Src.OutgoingEdges.Add edge
            edge.Dst.IncomingEdges.Add edge

        let private executeInstructions (ilintpr : ILInterpreter) cfg cilState =
            assert (cilState.ip.CanBeExpanded())
            let startingOffset = cilState.ip.Vertex ()
            let endOffset =
                let lastOffset = Seq.last cfg.sortedOffsets
                if startingOffset = lastOffset then cfg.ilBytes.Length
                else
                    let index = cfg.sortedOffsets.BinarySearch startingOffset
                    cfg.sortedOffsets.[index + 1]
            let isOffsetOfCurrentVertex (offset : ip) = startingOffset <= offset.Vertex() && offset.Vertex() < endOffset
            let rec executeAllInstructions erroredStates (offset : ip) cilState : cilState list=
                let allStates = ilintpr.ExecuteInstruction cfg (offset.Vertex()) cilState
                let goodStates = allStates |> List.filter (fun (_, cilState) -> not cilState.HasException)
                let newErrors = allStates |> List.filter (fun (_, cilState) -> cilState.HasException)

                let allErrors = erroredStates @ List.map (fun (erroredOffset, cilState) -> {cilState with ip = erroredOffset}) newErrors

                match goodStates with
                | list when List.forall (fst >> (=) ip.Exit) list -> List.map (fun (_, state) -> { state with ip = ip.Exit}) list @ allErrors
                | (nextOffset, _)::xs as list when isOffsetOfCurrentVertex nextOffset
                                                   && List.forall (fun (offset, _) -> offset = nextOffset && isOffsetOfCurrentVertex offset) xs ->
                    List.collect ((<||) (executeAllInstructions allErrors)) list
                | list -> allErrors @ (list |> List.map (fun (offset, cilState) -> {cilState with ip = offset}))
            executeAllInstructions [] (Instruction startingOffset) cilState

        let computeCFAForMethod (ilintptr : ILInterpreter) (cilState1 : cilState) (cfa : cfa) (used : Dictionary<offset * operationalStack, int>) (block : MethodBase unitBlock) =
            let cfg = cfa.cfg
            let executeSeparatedOpCode offset (opCode : System.Reflection.Emit.OpCode) (cilState : cilState) =
                let calledMethod = InstructionsSet.resolveMethodFromMetadata cfg (offset + opCode.Size)
                let callSite = { sourceMethod = cfg.methodBase; offset = offset; calledMethod = calledMethod; opCode = opCode }
                let args, stackWithoutCallArgs, neededResult, this =
                    let args, cilState = InstructionsSet.retrieveActualParameters calledMethod cilState
                    let this, cilState, neededResult =
                        match calledMethod with
                        | :? ConstructorInfo when opCode = OpCodes.Newobj -> None, cilState, true
                        | :? ConstructorInfo ->
                            let this, cilState = InstructionsSet.popStack cilState
                            this, cilState, false
                        | :? MethodInfo as methodInfo when not calledMethod.IsStatic || opCode = System.Reflection.Emit.OpCodes.Callvirt -> // TODO: check if condition `opCode = OpCodes.Callvirt` needed
                            let this, cilState = InstructionsSet.popStack cilState
                            this, cilState, methodInfo.ReturnType <> typedefof<System.Void>
                        | :? MethodInfo as methodInfo ->
                            None, cilState, methodInfo.ReturnType <> typedefof<System.Void>
                        | _ -> internalfailf "unknown methodBase %O" calledMethod
                    args, cilState.opStack, neededResult, this
                let nextOffset =
                    match Instruction.findNextInstructionOffsetAndEdges opCode cfg.ilBytes offset with
                    | FallThrough nextOffset -> nextOffset
                    | _ -> __unreachable__()
                let stackAfterCall =
                    if neededResult then Terms.MakeFunctionResultConstant callSite :: stackWithoutCallArgs
                    else stackWithoutCallArgs
                // TODO: why no exceptions?
                nextOffset, this, args, { cilState with opStack = stackAfterCall }

            let rec computeCFAForBlock (block : unitBlock<'a>) =
                let createOrGetVertex = function
                    | Instruction offset, opStack when used.ContainsKey(offset, opStack) ->
                        block.vertices.[used.[offset, opStack]]
                    | Instruction offset, opStack ->
                        let vertex = Vertex.CreateVertex cfg.methodBase offset opStack
                        block.AddVertex vertex
                        vertex
                    | Exit, [] -> block.exitVertex
                    | _ -> __unreachable__()

                // note: entry point and exit vertex must be added to unit block
                let rec bypass (vertex : Vertex) =
                    let id, offset, opStack = vertex.Id, vertex.Offset, vertex.OpStack
                    if used.ContainsKey (offset, opStack) || vertex = block.exitVertex then
                        Logger.printLog Logger.Trace "Again went to offset = %x\nnopStack = %O" offset opStack
                    else
                    let wasAdded = used.TryAdd((offset, opStack), id)
                    Prelude.releaseAssert(wasAdded)
                    let srcVertex = block.vertices.[id]
                    Logger.printLog Logger.Trace "[Starting computing cfa for offset = %x]\nopStack = %O" offset opStack
                    match cfg.offsetsDemandingCall.ContainsKey offset with
                    | true ->
                        let opCode, calledMethod = cfg.offsetsDemandingCall.[offset]
                        let callSite = {sourceMethod = cfg.methodBase; offset = offset; calledMethod = calledMethod; opCode = opCode}
                        let nextOffset, this, args, cilState' = executeSeparatedOpCode offset opCode {cilState1 with ip = Instruction offset; opStack = opStack}
                        let dstVertex = createOrGetVertex (Instruction nextOffset, cilState'.opStack)
                        block.AddVertex dstVertex
                        addEdge <| CallEdge(srcVertex, dstVertex, callSite, this, args)
                        bypass dstVertex
                    | _ ->
                        let newStates = executeInstructions ilintptr cfg {cilState1 with ip = Instruction offset; opStack = opStack}
                        let goodStates = List.filter (fun (cilState : cilState) -> not cilState.HasException) newStates
                        let erroredStates = List.filter (fun (cilState : cilState) -> cilState.HasException) newStates
                        goodStates |> List.iter (fun (cilState' : cilState) ->
                            let dstVertex = createOrGetVertex (cilState'.ip, cilState'.opStack)
                            if not cilState'.leaveInstructionExecuted then addEdge <| StepEdge(srcVertex, dstVertex, cilState'.state)
                            else
                                Prelude.releaseAssert(List.isEmpty cilState'.opStack)
                                addEdgesToFinallyBlocks cilState1.state srcVertex dstVertex
                            bypass dstVertex)
                        srcVertex.AddErroredStates erroredStates
                bypass block.entryPoint
            and addEdgesToFinallyBlocks emptyEffect (srcVertex : Vertex) (dstVertex : Vertex) =
                let method = cfg.methodBase
                let ehcs = method.GetMethodBody().ExceptionHandlingClauses
                           |> Seq.filter Instruction.isFinallyClause
                           |> Seq.filter (Instruction.shouldExecuteFinallyClause srcVertex.Offset dstVertex.Offset)
                           |> Seq.sortWith (fun ehc1 ehc2 -> ehc1.HandlerOffset - ehc2.HandlerOffset)
                let chainSequentialFinallyBlocks prevVertex (ehc : ExceptionHandlingClause) =
                    let finallyBlock = cfa.FindOrCreateFinallyHandler ehc
                    addEdge <| StepEdge(prevVertex, finallyBlock.entryPoint, emptyEffect)
                    computeCFAForBlock finallyBlock
                    finallyBlock.exitVertex
                let lastVertex = ehcs |> Seq.fold chainSequentialFinallyBlocks srcVertex
                addEdge <| StepEdge (lastVertex, dstVertex, emptyEffect)
            computeCFAForBlock block

        let computeCFA (ilintptr : ILInterpreter) (ilmm: ILMethodMetadata) : cfa =
            let methodBase = ilmm.methodBase
            match alreadyComputedCFAs.ContainsKey methodBase with
            | true -> alreadyComputedCFAs.[methodBase]
            | _ ->
                let emptyState, this, _, _ = ilintptr.FormInitialState ilmm
                Prelude.releaseAssert(Map.isEmpty emptyState.callSiteResults)
                Prelude.releaseAssert(Option.isNone emptyState.returnRegister)

//                Logger.printLog Logger.Trace "emptyState for method %O = %O" methodBase emptyState
                let cfg = CFG.build methodBase
                let cfa = createEmptyCFA cfg methodBase

                let used = Dictionary<offset * operationalStack, int>()
                let cilState = cilState.MakeEmpty ip.Exit ip.Exit emptyState //DIRTY: ip.Exit ip.Exit
                let cilState = {cilState with this = this}
                computeCFAForMethod ilintptr cilState cfa used cfa.body
                alreadyComputedCFAs.[methodBase] <- cfa
                Logger.printLog Logger.Trace "Computed cfa: %O" cfa
                cfa

type StepInterpreter() =
    inherit ILInterpreter()
    let visitedVertices : persistent<Map<CFA.Vertex, uint32>> =
        let r = new persistent<_>(always Map.empty, id) in r.Reset(); r
    override x.ReproduceEffect codeLoc state k = x.ExploreAndCompose codeLoc state k
    override x.CreateInstance exceptionType arguments state =
        let error = Nop
        error, {state with exceptionRegister = Unhandled error}
    member x.ForwardExploration (cfa : CFA.cfa) codeLoc initialState this k =
        let k =
            visitedVertices.Save()
            let k x = visitedVertices.Restore(); k x
            k

        let maxBorder = 50u
        let used (vertex : CFA.Vertex) =
            if vertex.IsMethodExitVertex then true
            elif visitedVertices.Value.ContainsKey vertex then
                visitedVertices.Value.[vertex] >= maxBorder
            else visitedVertices.Mutate (Map.add vertex 1u visitedVertices.Value)
                 false

        let visit vertex =
            match visitedVertices.Value.ContainsKey vertex with
            | true ->
                let cnt = Map.find vertex visitedVertices.Value
                visitedVertices.Mutate (Map.add vertex (cnt + 1u) visitedVertices.Value)
            | _ -> visitedVertices.Mutate (Map.add vertex 1u visitedVertices.Value)
        let rec dfs lvl (vertex : CFA.Vertex) =
            if used vertex then ()
            else
                visit vertex
                let edges = vertex.OutgoingEdges
                let paths : path list = vertex.Paths.OfLevel false lvl
                let newDsts = edges |> Seq.fold (fun acc (edge : CFA.Edge) ->
                    let propagated = Seq.map edge.PropagatePath paths |> Seq.fold (||) false
                    if propagated then (edge.Dst :: acc) else acc) []
                List.iter (fun dst -> dfs (lvl + 1u) dst) newDsts
        cfa.body.entryPoint.Paths.Add {lvl = 0u; state = initialState}
        dfs 0u cfa.body.entryPoint
        let resultStates = List.init (maxBorder |> int) (fun lvl -> cfa.body.exitVertex.Paths.OfLevel true (lvl |> uint32) |> List.ofSeq)
                         |> List.concat
                         |> List.map (fun (path : path) -> path.state)
        let resState = resultStates
//                       |> List.mapi (fun idx state -> Logger.printLog Logger.Trace "index = %d\nstate = %O\n" idx state; state)
                       |> List.reduce (fun state1 state2 -> Memory.Merge2States state1 state2)
        let tempResult =
            match resState.returnRegister with
            | None -> Nop
            | Some res -> res
        match resState.returnRegister with
        | None -> k (Nop, resState)
        | Some res -> k (res, resState)

    override x.Invoke codeLoc =
        let (|StaticConstructorMethodMetadata|_|) (codeLoc : ICodeLocation) =
            match codeLoc with
            | :? ILMethodMetadata as ilmm when ilmm.methodBase.IsStatic && (ilmm.methodBase :? ConstructorInfo) -> Some ilmm
            | _ -> None

        match codeLoc with
//        | StaticConstructorMethodMetadata _ -> base.Invoke codeLoc
        | :? ILMethodMetadata as ilmm ->
            CFA.configureInterpreter x
            let cfa = CFA.cfaBuilder.computeCFA x ilmm
            x.ForwardExploration cfa codeLoc
        | _ -> internalfail "unhandled ICodeLocation instance"

