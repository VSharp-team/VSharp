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

    type Vertex private(id, m : MethodBase, ip : ip, opStack : operationalStack) =
        static let ids : Dictionary<MethodBase, int> = Dictionary<_,_>()
        let lemmas = Lemmas(m, ip)
        let paths = Paths(m, ip)
        let queries = Queries(m, ip)
        let solver = null //Solver.SolverPool.mkSolver()
        let errors = List<cilState>()
        let incomingEdges: List<Edge> = List<_>()
        let outgoingEdges: List<Edge> = List<_>()

        member x.AddErroredStates (newErrors : cilState list) =
            errors.AddRange(newErrors)

        member x.Id with get() = id
        member x.Lemmas = lemmas
        member x.Paths with get () = paths
        member x.Queries = queries
        member x.Solver = solver
        member x.IncomingEdges = incomingEdges
        member x.OutgoingEdges = outgoingEdges
        member x.Ip with get() = ip
        member x.OpStack with get() = opStack
        member x.Method with get() = m
        member x.IsMethodStartVertex with get() = ip = Instruction 0
        member x.IsMethodExitVertex with get() = ip = ExitPointer
        override x.ToString() =
            sprintf "(Method = %s, IP = %O, id = %d)\n" (Reflection.GetFullMethodName m) ip id +
            sprintf "Edges count = %d\n" x.OutgoingEdges.Count +
            "Edges: \n" + Seq.fold (fun acc edge -> acc + edge.ToString() + "\n") "" x.OutgoingEdges
        static member CreateVertex method ip opStack =
            let id = Dict.getValueOrUpdate ids method (always 0)
            ids.[method] <- id + 1
            Vertex(id, method, ip, opStack)
    and
        [<AbstractClass>]
        Edge(src : Vertex, dst : Vertex) =
        abstract member Type : string
        abstract member PropagatePath : path -> bool
        member x.PrintLog msg obj = Logger.printLog Logger.Trace "[%s]\n%s: %O" (x.commonToString()) msg obj
        member x.Src = src
        member x.Dst = dst
        member x.Method = x.Src.Method
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
        member x.commonToString() =
            sprintf "%s ID:[%d --> %d] IP:[%O --> %O] Method:%s"
                x.Type x.Src.Id x.Dst.Id x.Src.Ip x.Dst.Ip (Reflection.GetFullMethodName x.Method)


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

        static member private CreateEmpty entity method entryIp exitIp =
            let entry = Vertex.CreateVertex method entryIp []
            let exit = Vertex.CreateVertex method exitIp []
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
            unitBlock<'a>.CreateEmpty method method (Instruction Properties.initialVertexOffset) ExitPointer
        static member CreateEmptyForFinallyClause (m : MethodBase) (ehc : ExceptionHandlingClause) =
            let entryOffset = ehc.HandlerOffset
            // TODO: check if this formula is forever true
            let exitOffset = ehc.HandlerOffset + ehc.HandlerLength - 1
            unitBlock<'a>.CreateEmpty ehc m (Instruction entryOffset) (Instruction exitOffset)
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

            member x.WithEntryPoint v = {x with body = x.body.WithEntryPoint v}
            member x.WithExitVertex v = {x with body = x.body.WithExitVertex v}
            member x.WithFinallyEntryPoint offset v = x.finallyHandlers.[offset].WithEntryPoint v |> ignore; x
            member x.WithFinallyExitVertex offset v = x.finallyHandlers.[offset].WithExitVertex v |> ignore; x
            override x.ToString() =
                let separator = "\n"
                "Method: " + x.Method.ToString() + separator +
                x.body.ToString() + separator +
                Seq.fold (fun acc block -> acc + block.ToString() + separator) "" (x.finallyHandlers.Values)



    type InsufficientInformationEdge(startingOffset, interpreter : ILInterpreter, cfg : cfg, src : Vertex, dst : Vertex) =
        inherit Edge(src, dst)
        override x.Type = "InsufficientInformationEdge"
        override x.PropagatePath (path : path) =
            let canBePropagated (ip : ip) = ip <> dst.Ip
            let cilState = cilState.MakeEmpty (Instruction startingOffset) ip.ExitPointer path.state
            let iieWasThrownAgain, resultStates = interpreter.ExecuteInstructionsWhile canBePropagated cfg startingOffset cilState
            if Option.isSome iieWasThrownAgain then raise <| Option.get iieWasThrownAgain
            resultStates
            |> List.map (fun (cilState : cilState) ->
                assert(List.length cilState.opStack = 0)
                x.CommonPropagatePath (path.lvl + 1u) cilState.state)
            |> List.fold (||) false

    type StepEdge(src : Vertex, dst : Vertex, effect : state) =
        inherit Edge(src, dst)
        do
            Prelude.releaseAssert(Map.isEmpty effect.callSiteResults)
        override x.Type = "StepEdge"
        override x.PropagatePath (path : path) =

            Memory.ComposeStates path.state effect (fun states ->
                x.PrintLog "composition left"  <| Memory.Dump path.state
                x.PrintLog "composition right" <| Memory.Dump effect
                x.PrintLog (sprintf "composition resulted in %d states" <| List.length states) <| (List.map Memory.Dump states |> join "\n")
                assert(List.forall (fun state -> path.state.frames = state.frames) states)
                // Do NOT turn this List.fold into List.exists to be sure that EVERY returned state is propagated
                List.fold (fun acc state -> acc || x.CommonPropagatePath (path.lvl + 1u) state) false states)

        member x.Effect = effect
        member x.VisibleVariables() = __notImplemented__()

        override x.ToString() =
            sprintf "%s\neffect = %O\npc = %O\n" (base.ToString()) (API.Memory.Dump effect) effect.pc

    type CallEdge(src : Vertex, dst : Vertex, callSite : callSite, stateWithArgsOnFrameAndAllocatedType : state) =
        inherit Edge(src, dst)
        do
           assert(List.length stateWithArgsOnFrameAndAllocatedType.frames = 2)
        override x.Type = "Call"
        override x.PropagatePath (path : path) =
            let addCallSiteResult callSiteResults (callSite : callSite) (res : term option) =
                if callSite.HasNonVoidResult then
                    assert(Option.isSome res)
                    Map.add callSite res callSiteResults
                else callSiteResults
            let k states =
                let propagateStateAfterCall acc state =
                    assert(path.state.frames = state.frames)
                    x.PrintLog "propagation through callEdge" callSite
                    x.PrintLog "call edge: composition left" (Memory.Dump path.state)
                    x.PrintLog "call edge: composition result" (Memory.Dump state)
                    let result' = x.CommonPropagatePath (path.lvl + 1u) {state with callSiteResults = addCallSiteResult path.state.callSiteResults callSite state.returnRegister
                                                                                    returnRegister = None}
                    acc || result'
                List.fold propagateStateAfterCall false states
//            let k1 results = results |> List.unzip |> snd |> k
            Prelude.releaseAssert (Option.isSome stepItp)
            let interpreter = stepItp |> Option.get
            let states = Memory.ComposeStates path.state stateWithArgsOnFrameAndAllocatedType id
            match states with
            | [state] ->
                match callSite.opCode with
                | Instruction.NewObj   when Reflection.IsDelegateConstructor callSite.calledMethod -> k [Memory.PopStack state]
                | Instruction.NewObj   when Reflection.IsArrayConstructor callSite.calledMethod -> k [Memory.PopStack state]
                | Instruction.Call
                | Instruction.NewObj   ->
                    interpreter.CommonCall callSite.calledMethod state k
                | Instruction.CallVirt -> interpreter.CommonCallVirt callSite.calledMethod state k
                | _ ->  __notImplemented__()
            | _ -> internalfailf "Calling %s: composition with frames unexpectedly forked!" callSite.calledMethod.Name
        member x.ExitNodeForCall() = __notImplemented__()
        member x.CallVariables() = __notImplemented__()
        override x.ToString() =
            sprintf "%s\nstate = %O\n" (base.ToString()) (API.Memory.Dump stateWithArgsOnFrameAndAllocatedType)

    module cfaBuilder =
        let private alreadyComputedCFAs = Dictionary<MethodBase, cfa>()

        type bypassData =
            { ip : ip
              opStack : operationalStack
              allocatedTypes : pdict<concreteHeapAddress, symbolicType>
              lengths : pdict<arrayType, vectorRegion>
              lowerBounds : pdict<arrayType, vectorRegion> }

        let createData ip opStack allocatedTypes lengths lowerBounds =
            {
                ip = ip; opStack = opStack; allocatedTypes = allocatedTypes; lengths = lengths; lowerBounds = lowerBounds
            }

        let private createEmptyCFA cfg method =
            {
              cfg = cfg
              body = unitBlock<MethodBase>.CreateEmptyForMethod method
              finallyHandlers = Dictionary<_,_>()
            }

        let private addEdge (edge : Edge) =
            edge.Src.OutgoingEdges.Add edge
            edge.Dst.IncomingEdges.Add edge

        let private executeInstructions (stepInterpreter : ILInterpreter) cfg (cilState : cilState) =
            assert (cilState.ip.CanBeExpanded())
            let startingOffset = cilState.ip.Offset
            let endOffset =
                 let lastOffset = Seq.last cfg.sortedOffsets
                 if startingOffset = lastOffset then cfg.ilBytes.Length
                 else
                     let index = cfg.sortedOffsets.BinarySearch startingOffset
                     cfg.sortedOffsets.[index + 1]
            let isOffsetOfCurrentVertex (ip : ip) = ip <> ExitPointer && startingOffset <= ip.Offset && ip.Offset < endOffset
            stepInterpreter.ExecuteInstructionsWhile isOffsetOfCurrentVertex cfg startingOffset cilState

        let computeCFAForMethod (ilintptr : ILInterpreter) (initialState : state) (cfa : cfa) (used : Dictionary<bypassData, int>) (block : MethodBase unitBlock) =
            let cfg = cfa.cfg
            let mutable currentTime = initialState.currentTime
            let executeSeparatedOpCode offset (opCode : System.Reflection.Emit.OpCode) (cilStateWithArgs : cilState) =
                let calledMethod = InstructionsSet.resolveMethodFromMetadata cfg (offset + opCode.Size)
                let callSite = { sourceMethod = cfg.methodBase; offset = offset; calledMethod = calledMethod; opCode = opCode }
                let pushFunctionResultOnOpStackIfNeeded (cilState : cilState) (methodInfo : System.Reflection.MethodInfo) =
                    if methodInfo.ReturnType = typeof<System.Void> then cilState
                    else {cilState with opStack = Terms.MakeFunctionResultConstant cilState.state callSite :: cilState.opStack}

                let args, cilStateWithoutArgs = InstructionsSet.retrieveActualParameters calledMethod cilStateWithArgs
                let this, cilState =
                    match calledMethod with
                    | _ when opCode = OpCodes.Newobj ->
                        let state = ilintptr.CommonNewObj false (calledMethod :?> ConstructorInfo) cilStateWithoutArgs.state args id |> List.head // TODO: what if newobj returns a lot of references and states?
                        assert(Option.isSome state.returnRegister)
                        let reference = Option.get state.returnRegister
                        Some reference, {cilStateWithoutArgs with state = {state with returnRegister = None}; opStack = reference :: cilStateWithoutArgs.opStack}
                    | :? ConstructorInfo -> InstructionsSet.popOperationalStack cilStateWithoutArgs
                    | :? MethodInfo as methodInfo when not calledMethod.IsStatic || opCode = System.Reflection.Emit.OpCodes.Callvirt -> // TODO: check if condition `opCode = OpCodes.Callvirt` needed
                        let this, cilState = InstructionsSet.popOperationalStack cilStateWithoutArgs
                        this, pushFunctionResultOnOpStackIfNeeded cilState methodInfo
                    | :? MethodInfo as methodInfo ->
                        None, pushFunctionResultOnOpStackIfNeeded cilStateWithoutArgs methodInfo
                    | _ -> internalfailf "unknown methodBase %O" calledMethod

                let nextOffset =
                    assert(cfg.graph.[offset].Count = 1)
                    cfg.graph.[offset].[0]

                // TODO: why no exceptions?
                nextOffset, this, args, cilState

            let rec computeCFAForBlock (block : unitBlock<'a>) =
                let createOrGetVertex (bypassData : bypassData) =
                    match bypassData.ip with
                    | ExitPointer when bypassData.opStack = [] -> block.exitVertex
                    | _ when used.ContainsKey(bypassData) -> block.vertices.[used.[bypassData]]
                    | ip ->
                        let vertex = Vertex.CreateVertex cfg.methodBase ip bypassData.opStack
                        block.AddVertex vertex
                        vertex
                let isIpDemandingCall = function
                    | Instruction offset -> if cfg.offsetsDemandingCall.ContainsKey offset then Some offset else None
                    | _                  -> None

                let isInsideCycle opStack (src : offset) (dst : ip) =
                    match dst with
                    | Instruction dst ->
                        if cfg.topologicalTimes.[src] >= cfg.topologicalTimes.[dst] then
                            Seq.tryFind (fun (bypassData : bypassData) -> bypassData.ip = Instruction dst && bypassData.opStack = opStack) used.Keys
                            |> Option.map (fun bypassData -> block.vertices.[used.[bypassData]])
                        else None
                    | _ -> None

                // note: entry point and exit vertex must be added to unit block
                let rec bypass (vertex : Vertex) allocatedTypes lengths lowerBounds =
                    let id, ip, opStack = vertex.Id, vertex.Ip, vertex.OpStack
                    let bypassData = createData ip opStack allocatedTypes lengths lowerBounds
                    if used.ContainsKey bypassData || vertex = block.exitVertex then
                        Logger.printLog Logger.Trace "Again went to ip = %O\nnopStack = %O" ip opStack
                    else
                    let wasAdded = used.TryAdd(bypassData, id)
                    Prelude.releaseAssert(wasAdded)
                    let srcVertex = block.vertices.[id]
                    Logger.printLog Logger.Trace "[Starting computing cfa for ip = %O]\nopStack = %O" ip opStack
                    System.Console.WriteLine("Making initial CFA state with STARTING TIME = {0}", currentTime)
                    let modifiedState = {initialState with allocatedTypes = allocatedTypes; lengths = lengths; lowerBounds = lowerBounds
                                                           currentTime = currentTime; startingTime = currentTime}
                    let initialCilState = {cilState.Empty with state = modifiedState}
                    let offset = ip.Offset
                    match cfg.offsetsDemandingCall.ContainsKey offset with
                    | true ->
                        let opCode, calledMethod = cfg.offsetsDemandingCall.[offset]
                        let callSite = {sourceMethod = cfg.methodBase; offset = offset; calledMethod = calledMethod; opCode = opCode}
                        let nextOffset, this, args, cilState' = executeSeparatedOpCode offset opCode {initialCilState with ip = Instruction offset; opStack = opStack}
                        let dstVertex =
                            let s = cilState'.state
                            createOrGetVertex (createData (Instruction nextOffset) cilState'.opStack s.allocatedTypes s.lengths s.lowerBounds)
                        block.AddVertex dstVertex
                        let stateWithArgsOnFrame = ilintptr.ReduceFunctionSignature cilState'.state calledMethod this (Specified args) false (fun x -> x)
                        currentTime <- VectorTime.max currentTime stateWithArgsOnFrame.currentTime
                        addEdge <| CallEdge(srcVertex, dstVertex, callSite, stateWithArgsOnFrame)
                        bypass dstVertex stateWithArgsOnFrame.allocatedTypes stateWithArgsOnFrame.lengths stateWithArgsOnFrame.lowerBounds
                    | _ ->
                        let offset = ip.Offset
                        let insufficientExceptionThrown, newStates = executeInstructions ilintptr cfg {initialCilState with ip = ip; opStack = opStack}
                        if Option.isSome insufficientExceptionThrown then
                            Prelude.releaseAssert(cfg.graph.[offset].Count <= 1)
                            let dstVertex =
                                let ip =
                                    if cfg.graph.[offset].Count = 0 then ExitPointer
                                    else Instruction cfg.graph.[offset].[0]
                                let opStack = [] // this is a hack!
                                createOrGetVertex (createData ip opStack allocatedTypes lengths lowerBounds)
                            addEdge <| InsufficientInformationEdge(ip.Offset, ILInterpreter(), cfg, srcVertex, dstVertex)

                        newStates |> List.iter (fun state -> currentTime <- VectorTime.max currentTime state.state.currentTime)
                        let goodStates = List.filter (fun (cilState : cilState) -> not cilState.HasException) newStates
                        let erroredStates = List.filter (fun (cilState : cilState) -> cilState.HasException) newStates
                        goodStates |> List.iter (fun (cilState' : cilState) ->
                            match isInsideCycle cilState'.opStack offset cilState'.ip with
                            | Some vertex ->
                                assert (not cilState'.leaveInstructionExecuted)
                                addEdge <| StepEdge(srcVertex, vertex, cilState'.state)
                            | None ->
                                let s = cilState'.state
                                let dstVertex = createOrGetVertex (createData cilState'.ip cilState'.opStack s.allocatedTypes s.lengths s.lowerBounds)
                                if not cilState'.leaveInstructionExecuted then addEdge <| StepEdge(srcVertex, dstVertex, cilState'.state)
                                else
                                    Prelude.releaseAssert(List.isEmpty cilState'.opStack)
                                    addEdgesToFinallyBlocks initialCilState.state srcVertex dstVertex
                                bypass dstVertex cilState'.state.allocatedTypes cilState'.state.lengths cilState'.state.lowerBounds)
                        srcVertex.AddErroredStates erroredStates
                bypass block.entryPoint initialState.allocatedTypes initialState.lengths initialState.lowerBounds
            and addEdgesToFinallyBlocks emptyEffect (srcVertex : Vertex) (dstVertex : Vertex) =
                let method = cfg.methodBase
                let ehcs = method.GetMethodBody().ExceptionHandlingClauses
                           |> Seq.filter Instruction.isFinallyClause
                           |> Seq.filter (Instruction.shouldExecuteFinallyClause srcVertex.Ip dstVertex.Ip)
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
                let initialState, this, _ = ilintptr.FormInitialStateWithoutStatics ilmm
                Prelude.releaseAssert(Map.isEmpty initialState.callSiteResults && Option.isNone initialState.returnRegister)

//                Logger.printLog Logger.Trace "emptyState for method %O = %O" methodBase emptyState
                let cfg = CFG.build methodBase
                let cfa = createEmptyCFA cfg methodBase

                let used = Dictionary<bypassData, int>()
                computeCFAForMethod ilintptr initialState cfa used cfa.body
                alreadyComputedCFAs.[methodBase] <- cfa
                Logger.printLog Logger.Trace "Computed cfa: %O" cfa
                cfa

type StepInterpreter() =
    inherit ILInterpreter()
    let visitedVertices : persistent<Dictionary<CFA.Vertex, uint32>> =
        let r = new persistent<_>(always (Dictionary<_,_>()), id) in r.Reset(); r
    let incrementCounter vertex =
        if not <| visitedVertices.Value.ContainsKey vertex then
            visitedVertices.Value.[vertex] <- 1u
        else visitedVertices.Value.[vertex] <- 1u + visitedVertices.Value.[vertex]
    override x.ReproduceEffect codeLoc state k = x.ExploreAndCompose codeLoc state k
    override x.CreateInstance exceptionType arguments state : state list =
        let error = Nop
        {state with exceptionsRegister = Unhandled error} |> List.singleton
    member x.ForwardExploration (cfa : CFA.cfa) codeLoc initialState (k : (term * state) list -> 'a) =
        let k =
            visitedVertices.Save()
            let k x = visitedVertices.Restore(); k x
            k

        let maxBorder = 50u
        let used (vertex : CFA.Vertex) =
            if vertex.IsMethodExitVertex then true
            elif visitedVertices.Value.ContainsKey vertex then
                visitedVertices.Value.[vertex] >= maxBorder
            else false

        let rec dfs lvl (vertex : CFA.Vertex) =
            if used vertex then ()
            else
                incrementCounter vertex
                let edges = vertex.OutgoingEdges
                let paths : path list = vertex.Paths.OfLevel lvl
                let newDsts = edges |> Seq.fold (fun acc (edge : CFA.Edge) ->
                    let propagated = Seq.map edge.PropagatePath paths |> Seq.fold (||) false
                    if propagated then (edge.Dst :: acc) else acc) []
                List.iter (dfs (lvl + 1u)) newDsts
        cfa.body.entryPoint.Paths.Add {lvl = 0u; state = initialState}
        Logger.trace "starting Forward exploration for %O" codeLoc
        dfs 0u cfa.body.entryPoint
        let resultStates = List.init (maxBorder |> int) (fun lvl -> cfa.body.exitVertex.Paths.OfLevel (lvl |> uint32) |> List.ofSeq)
                         |> List.concat
                         |> List.map (fun (path : path) ->
                             let state = path.state
                             match state.returnRegister with
                             | None -> Nop, state
                             | Some res -> res, state)
        if List.length resultStates = 0 then internalfailf "No states were obtained. Most likely such a situation is a bug. Check it!"
        k resultStates

    override x.Invoke codeLoc =
        match codeLoc with
        | :? ILMethodMetadata as ilmm ->
            CFA.configureInterpreter x

            try
                let cfa = CFA.cfaBuilder.computeCFA x ilmm
                x.ForwardExploration cfa codeLoc
            with
            | :? InsufficientInformationException -> base.Invoke codeLoc

        | _ -> internalfail "unhandled ICodeLocation instance"

