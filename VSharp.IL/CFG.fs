namespace VSharp

open System
open System.Reflection
open System.Collections.Generic
open FSharpx.Collections
open Microsoft.FSharp.Collections
open VSharp

[<Struct>]
type internal temporaryCallInfo = {callee: MethodWithBody; callFrom: offset; returnTo: offset}

type private BasicBlock (startVertex: offset) =
    let innerVertices = ResizeArray<offset>()
    let mutable finalVertex = None
    member this.StartVertex = startVertex
    member this.InnerVertices with get () = innerVertices
    member this.AddVertex v = innerVertices.Add v
    member this.FinalVertex
        with get () =
                match finalVertex with
                | Some v -> v
                | None -> failwith "Final vertex of this basic block is not specified yet."
        and set v = finalVertex <- Some v

type internal CfgTemporaryData (method : MethodWithBody) =
    let () = assert method.HasBody
    let ilBytes = method.ILBytes
    let exceptionHandlers = method.ExceptionHandlers
    let sortedOffsets = ResizeArray<offset>()
    let edges = Dictionary<offset, HashSet<offset>>()
    let sinks = ResizeArray<_>()
    let calls = ResizeArray<temporaryCallInfo>()
    let loopEntries = HashSet<offset>()
    let offsetsWithSiblings = HashSet<offset>()

    let dfs (startVertices : array<offset>) =
        let used = HashSet<offset>()
        let verticesOffsets = HashSet<offset> startVertices
        let addVertex v = verticesOffsets.Add v |> ignore
        let greyVertices = HashSet<offset>()
        let vertexToBasicBloc: array<Option<BasicBlock>> = Array.init ilBytes.Length (fun _ -> None)

        let splitEdge edgeStart edgeFinal intermediatePoint =
            let isRemoved = edges.[edgeStart].Remove edgeFinal
            assert isRemoved
            let isAdded = edges.[edgeStart].Add intermediatePoint
            assert isAdded
            edges.Add(intermediatePoint, HashSet<_>[|edgeFinal|])

        let splitBasicBlock (block:BasicBlock) intermediatePoint =
            let newBlock = BasicBlock(intermediatePoint)
            newBlock.FinalVertex <- block.FinalVertex
            let tmp = ResizeArray block.InnerVertices
            for v in tmp do
                if v > intermediatePoint
                then
                    let isRemoved = block.InnerVertices.Remove v
                    assert isRemoved
                    newBlock.AddVertex v
                    vertexToBasicBloc.[int v] <- Some newBlock
            block.FinalVertex <- intermediatePoint
            let isRemoved = block.InnerVertices.Remove intermediatePoint
            assert isRemoved

        let addEdge src dst =
            addVertex src
            addVertex dst
            if src <> dst
            then
                let exists,outgoingEdges = edges.TryGetValue src
                if exists
                then outgoingEdges.Add dst |> ignore
                else edges.Add(src, HashSet [|dst|])
                match vertexToBasicBloc.[int dst] with
                | None -> ()
                | Some block ->
                    if block.InnerVertices.Contains dst && block.FinalVertex <> dst
                    then
                        splitEdge block.StartVertex block.FinalVertex dst
                        splitBasicBlock block dst


        let rec dfs' (currentBasicBlock : BasicBlock) (currentVertex : offset) =

            vertexToBasicBloc.[int currentVertex] <- Some currentBasicBlock

            if used.Contains currentVertex
            then
                currentBasicBlock.FinalVertex <- currentVertex
                addEdge currentBasicBlock.StartVertex currentVertex
                if greyVertices.Contains currentVertex
                then loopEntries.Add currentVertex |> ignore
            else
                greyVertices.Add currentVertex |> ignore
                used.Add currentVertex |> ignore
                let opCode = MethodBody.parseInstruction method currentVertex

                let dealWithJump src dst =
                    addVertex src
                    addVertex dst
                    addEdge src dst
                    dfs' (BasicBlock dst)  dst

                let processCall callee callFrom returnTo =
                    calls.Add({callee=callee; callFrom=callFrom; returnTo=returnTo})
                    currentBasicBlock.FinalVertex <- currentVertex
                    addEdge currentBasicBlock.StartVertex currentVertex
                    addEdge callFrom returnTo
                    dfs' (BasicBlock returnTo) returnTo

                let ipTransition = MethodBody.findNextInstructionOffsetAndEdges opCode ilBytes currentVertex

                match ipTransition with
                | FallThrough offset when MethodBody.isDemandingCallOpCode opCode ->
                    let opCode', calleeBase = method.ParseCallSite currentVertex
                    assert(opCode' = opCode)
                    let callee = MethodWithBody.InstantiateNew calleeBase
                    if callee.HasBody then
                        processCall callee currentVertex offset
                    else
                        currentBasicBlock.AddVertex offset
                        dfs' currentBasicBlock offset
                | FallThrough offset ->
                    currentBasicBlock.AddVertex offset
                    dfs' currentBasicBlock offset
                | ExceptionMechanism ->
                    // TODO: gsv fix it.
//                    currentBasicBlock.FinalVertex <- currentVertex
//                    addEdge currentBasicBlock.StartVertex currentVertex
//                    calls.Add(CallInfo(null, currentVertex, currentVertex + 1))
                    ()
                | Return ->
                    addVertex currentVertex
                    sinks.Add currentVertex
                    currentBasicBlock.FinalVertex <- currentVertex
                    addEdge currentBasicBlock.StartVertex currentVertex
                | UnconditionalBranch target ->
                    currentBasicBlock.FinalVertex <- currentVertex
                    addEdge currentBasicBlock.StartVertex currentVertex
                    dealWithJump currentVertex target
                | ConditionalBranch (fallThrough, offsets) ->
                    currentBasicBlock.FinalVertex <- currentVertex
                    addEdge currentBasicBlock.StartVertex currentVertex
                    dealWithJump currentVertex fallThrough
                    offsets |> List.iter (dealWithJump currentVertex)

                greyVertices.Remove currentVertex |> ignore

        startVertices
        |> Array.iter (fun v -> dfs' (BasicBlock v) v)

        verticesOffsets
        |> Seq.sort
        |> Seq.iter sortedOffsets.Add

    let cfgDistanceFrom = GraphUtils.distanceCache<offset>()

    let findDistanceFrom node =
        Dict.getValueOrUpdate cfgDistanceFrom node (fun () ->
        let dist = GraphUtils.incrementalSourcedDijkstraAlgo node edges cfgDistanceFrom
        let distFromNode = Dictionary<offset, uint>()
        for i in dist do
            if i.Value <> GraphUtils.infinity then
                distFromNode.Add(i.Key, i.Value)
        distFromNode)

    do
        let startVertices =
            [|
             yield 0<offsets>
             for handler in exceptionHandlers do
                 yield handler.handlerOffset
                 match handler.ehcType with
                 | ehcType.Filter offset -> yield offset
                 | _ -> ()
            |]

        dfs startVertices
        sortedOffsets |> Seq.iter (fun bb ->
            if edges.ContainsKey bb then
                let outgoing = edges.[bb]
                if outgoing.Count > 1 then
                    offsetsWithSiblings.UnionWith outgoing
            else edges.Add(bb, HashSet<_>()))

    member this.ILBytes = ilBytes
    member this.SortedOffsets = sortedOffsets
    member this.Edges = edges
    member this.Calls = calls
    member this.Sinks = sinks.ToArray()
    member this.LoopEntries = loopEntries
    member this.BlocksWithSiblings = offsetsWithSiblings
    member this.DistancesFrom offset = findDistanceFrom offset

[<Struct>]
type CallInfo =
    val Callee: Method
    val CallFrom: offset
    val ReturnTo: offset
    new (callee, callFrom, returnTo) =
        {
            Callee = callee
            CallFrom = callFrom
            ReturnTo = returnTo
        }

and CfgInfo internal (cfg : CfgTemporaryData) =
    let resolveBasicBlockIndex offset =
        let rec binSearch (sortedOffsets : ResizeArray<offset>) offset l r =
            if l >= r then l
            else
                let mid = (l + r) / 2
                let midValue = sortedOffsets.[mid]
                let leftIsLefter = midValue <= offset
                let rightIsRighter = mid + 1 >= sortedOffsets.Count || sortedOffsets.[mid + 1] > offset
                if leftIsLefter && rightIsRighter then mid
                elif not rightIsRighter
                    then binSearch sortedOffsets offset (mid + 1) r
                    else binSearch sortedOffsets offset l (mid - 1)

        binSearch cfg.SortedOffsets offset 0 (cfg.SortedOffsets.Count - 1)

    let resolveBasicBlock offset = cfg.SortedOffsets.[resolveBasicBlockIndex offset]

    let sinks = cfg.Sinks |> Array.map resolveBasicBlock
    let loopEntries = cfg.LoopEntries
    let calls =
        let res = Dictionary<_,_>()
        cfg.Calls
        |> ResizeArray.iter (fun tmpCallInfo ->
            let callInfo = CallInfo(tmpCallInfo.callee :?> Method, tmpCallInfo.callFrom, tmpCallInfo.returnTo)
            res.Add(tmpCallInfo.callFrom, callInfo))
        res

    member this.IlBytes = cfg.ILBytes
    member this.SortedOffsets = cfg.SortedOffsets
    member this.Edges = cfg.Edges
    member this.Sinks = sinks
    member this.Calls = calls
    member this.IsLoopEntry offset = loopEntries.Contains offset
    member internal this.ResolveBasicBlockIndex offset = resolveBasicBlockIndex offset
    member this.ResolveBasicBlock offset = resolveBasicBlock offset
    member this.IsBasicBlockStart offset = resolveBasicBlock offset = offset
    // Returns dictionary of shortest distances, in terms of basic blocks (1 step = 1 basic block transition)
    member this.DistancesFrom offset =
        let bb = resolveBasicBlock offset
        cfg.DistancesFrom bb
    member this.HasSiblings offset =
        this.IsBasicBlockStart offset && cfg.BlocksWithSiblings.Contains offset

and Method internal (m : MethodBase) as this =
    inherit MethodWithBody(m)
    let cfg = lazy(
        if this.HasBody then
            Logger.trace $"Add CFG for {this}."
            let cfg = CfgTemporaryData this
            Method.ReportCFGLoaded this
            cfg |> CfgInfo |> Some
        else None)

    member x.CFG with get() =
        match cfg.Force() with
        | Some cfg -> cfg
        | None -> internalfailf $"Getting CFG of method {x} without body (extern or abstract)"

    // Helps resolving cyclic dependencies between Application and MethodWithBody
    [<DefaultValue>] static val mutable private cfgReporter : Method -> unit
    static member internal ReportCFGLoaded with get() = Method.cfgReporter and set v = Method.cfgReporter <- v

    // Returns a sequence of strings, one per instruction in basic block
    member x.BasicBlockToString (offset : offset) : string seq =
        let cfg = x.CFG
        let idx = cfg.ResolveBasicBlockIndex offset
        let offset = cfg.SortedOffsets.[idx]
        let parsedInstrs = x.ParsedInstructions
        let mutable instr = parsedInstrs |> Array.find (fun instr -> Offset.from (int instr.offset) = offset)
        let endInstr =
            if idx + 1 < cfg.SortedOffsets.Count then
                let nextBBOffset = cfg.SortedOffsets.[idx + 1]
                parsedInstrs |> Array.find (fun instr -> Offset.from (int instr.offset) = nextBBOffset)
            else parsedInstrs.[parsedInstrs.Length - 1].next
        seq {
            while not <| LanguagePrimitives.PhysicalEquality instr endInstr do
                yield ILRewriter.PrintILInstr None None x.MethodBase instr
                instr <- instr.next
        }

[<CustomEquality; CustomComparison>]
type public codeLocation = {offset : offset; method : Method}
    with
    override x.Equals y =
        match y with
        | :? codeLocation as y -> x.offset = y.offset && x.method.Equals(y.method)
        | _ -> false
    override x.GetHashCode() = (x.offset, x.method).GetHashCode()
    override x.ToString() = sprintf "[method = %s\noffset = %s]" x.method.FullName ((int x.offset).ToString("X"))
    interface IComparable with
        override x.CompareTo y =
            match y with
            | :? codeLocation as y when x.method.Equals(y.method) -> compare x.offset y.offset
            | :? codeLocation as y -> (x.method :> IComparable).CompareTo(y.method)
            | _ -> -1

type IGraphTrackableState =
    abstract member CodeLocation: codeLocation
    abstract member CallStack: list<Method>

type private ApplicationGraphMessage =
    | ResetQueryEngine
    | AddGoals of array<codeLocation>
    | RemoveGoal of codeLocation
    | SpawnStates of seq<IGraphTrackableState>
    | AddForkedStates of parentState:IGraphTrackableState * forkedStates:seq<IGraphTrackableState>
    | MoveState of positionForm:codeLocation * positionTo: IGraphTrackableState
    | AddCFG of Option<AsyncReplyChannel<CfgInfo>> *  Method
    | AddCallEdge of callForm:codeLocation * callTo: codeLocation
    | GetShortestDistancesToGoals
        of AsyncReplyChannel<ResizeArray<codeLocation * codeLocation * int>> * array<codeLocation>
    | GetReachableGoals
        of AsyncReplyChannel<Dictionary<codeLocation,HashSet<codeLocation>>> * array<codeLocation>
    | GetDistanceToNearestGoal
        of AsyncReplyChannel<seq<IGraphTrackableState * int>> * seq<IGraphTrackableState>

type ApplicationGraph() as this =

    let addCallEdge (callSource:codeLocation) (callTarget:codeLocation) =
        Logger.trace "Add call edge."
        //__notImplemented__()

    let moveState (initialPosition: codeLocation) (stateWithNewPosition: IGraphTrackableState) =
        Logger.trace "Move state."
        //__notImplemented__()

    let addStates (parentState:Option<IGraphTrackableState>) (states:array<IGraphTrackableState>) =
        Logger.trace "Add states."
        //__notImplemented__()

    let getShortestDistancesToGoals (states:array<codeLocation>) =
        __notImplemented__()

    let messagesProcessor = MailboxProcessor.Start(fun inbox ->
        let tryGetCfgInfo (method : Method) =
            if method.HasBody then
                let cfg = method.CFG
                // TODO: add cfg if needed
                Some cfg
            else None

        async{
            while true do
                let! message = inbox.Receive()
                try
                    match message with
                    | ResetQueryEngine ->
                        __notImplemented__()
                    | AddCFG (replyChannel, method) ->
                        let reply cfgInfo =
                            match replyChannel with
                            | Some ch -> ch.Reply cfgInfo
                            | None -> ()
                        assert method.HasBody
                        let cfg = tryGetCfgInfo method |> Option.get
                        reply cfg
                    | AddCallEdge (_from, _to) ->
                        match tryGetCfgInfo _from.method, tryGetCfgInfo _to.method with
                        | Some _, Some _ ->  addCallEdge _from _to
                        | _ -> ()
                    | AddGoals positions ->
                        Logger.trace "Add goals."
                        //__notImplemented__()
                    | RemoveGoal pos ->
                        Logger.trace "Remove goal."
                        //__notImplemented__()
                    | SpawnStates states -> Array.ofSeq states |> addStates None
                    | AddForkedStates (parentState, forkedStates) ->
                        addStates (Some parentState) (Array.ofSeq forkedStates)
                    | MoveState (_from,_to) ->
                        tryGetCfgInfo _to.CodeLocation.method |> ignore
                        moveState _from _to
                    | GetShortestDistancesToGoals (replyChannel, states) ->
                        Logger.trace "Get shortest distances."
                        __notImplemented__()
                    | GetDistanceToNearestGoal (replyChannel, states) ->
                        replyChannel.Reply []
                        //__notImplemented__()
                    | GetReachableGoals (replyChannel, states) -> __notImplemented__()
                with
                | e ->
                    Logger.error $"Something wrong in application graph messages processor: \n %A{e} \n %s{e.Message} \n %s{e.StackTrace}"
                    match message with
                    | AddCFG (Some ch, _) -> ch.Reply Unchecked.defaultof<CfgInfo>
                    | _ -> ()
        }
    )

    do
        messagesProcessor.Error.Add(fun e ->
            Logger.error $"Something wrong in application graph messages processor: \n %A{e} \n %s{e.Message} \n %s{e.StackTrace}"
            raise e
            )

    member this.RegisterMethod (method: Method) =
        messagesProcessor.Post (AddCFG (None, method))

    member this.AddCallEdge (sourceLocation : codeLocation) (targetLocation : codeLocation) =
        messagesProcessor.Post <| AddCallEdge (sourceLocation, targetLocation)

    member this.SpawnState (state:IGraphTrackableState) =
        messagesProcessor.Post <| SpawnStates [|state|]

    member this.SpawnStates (states:seq<IGraphTrackableState>) =
        messagesProcessor.Post <| SpawnStates states

    member this.AddForkedStates (parentState:IGraphTrackableState) (states:seq<IGraphTrackableState>) =
        messagesProcessor.Post <| AddForkedStates (parentState,states)

    member this.MoveState (fromLocation : codeLocation) (toLocation : IGraphTrackableState) =
        messagesProcessor.Post <| MoveState (fromLocation, toLocation)

    member x.AddGoal (location:codeLocation) =
        messagesProcessor.Post <| AddGoals [|location|]

    member x.AddGoals (locations:array<codeLocation>) =
        messagesProcessor.Post <| AddGoals locations

    member x.RemoveGoal (location:codeLocation) =
        messagesProcessor.Post <| RemoveGoal location

    member this.GetShortestDistancesToAllGoalsFromStates (states: array<codeLocation>) =
        messagesProcessor.PostAndReply (fun ch -> GetShortestDistancesToGoals(ch, states))

    member this.GetDistanceToNearestGoal (states: seq<IGraphTrackableState>) =
        messagesProcessor.PostAndReply (fun ch -> GetDistanceToNearestGoal(ch, states))

    member this.GetGoalsReachableFromStates (states: array<codeLocation>) =
        messagesProcessor.PostAndReply (fun ch -> GetReachableGoals(ch, states))

    member this.ResetQueryEngine() =
        messagesProcessor.Post ResetQueryEngine



type IVisualizer =
    abstract AddState : IGraphTrackableState -> unit
    abstract TerminateState : IGraphTrackableState -> unit
    abstract VisualizeGraph : unit -> unit
    abstract VisualizeStep : codeLocation -> IGraphTrackableState -> IGraphTrackableState seq -> unit

type NullVisualizer() =
    interface IVisualizer with
        override x.AddState _ = ()
        override x.TerminateState _ = ()
        override x.VisualizeGraph () = ()
        override x.VisualizeStep _ _ _ = ()



module Application =
    let private methods = Dictionary<MethodBase, Method>()
    let private _loadedMethods = HashSet<_>()
    let loadedMethods = _loadedMethods :> seq<_>
    let graph = ApplicationGraph()
    let mutable visualizer : IVisualizer = NullVisualizer()

    let getMethod (m : MethodBase) : Method =
        Dict.getValueOrUpdate methods m (fun () -> Method(m))

    let setVisualizer (v : IVisualizer) =
        visualizer <- v

    let spawnStates states =
        graph.SpawnStates states
        states |> Seq.iter (fun state -> visualizer.AddState state)
        visualizer.VisualizeGraph()

    let moveState fromLoc toState forked =
        graph.MoveState fromLoc toState
        graph.AddForkedStates toState forked
        visualizer.VisualizeStep fromLoc toState forked

    let terminateState state =
        // TODO: gsv: propagate this into application graph
        visualizer.TerminateState state

    let addCallEdge = graph.AddCallEdge
    let addGoal = graph.AddGoal
    let addGoals = graph.AddGoals
    let removeGoal = graph.RemoveGoal

    do
        MethodWithBody.InstantiateNew <- fun m -> getMethod m :> MethodWithBody
        Method.ReportCFGLoaded <- fun m ->
            graph.RegisterMethod m
            let added = _loadedMethods.Add(m) in assert added
