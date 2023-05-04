namespace VSharp

open System.Collections.Concurrent
open VSharp.GraphUtils
open global.System
open System.Reflection
open System.Collections.Generic
open FSharpx.Collections
open Microsoft.FSharp.Collections
open VSharp

type ICallGraphNode =
    inherit IGraphNode<ICallGraphNode>

type IReversedCallGraphNode =
    inherit IGraphNode<IReversedCallGraphNode>

module CallGraph =
    let callGraphDistanceFrom = Dictionary<Assembly, distanceCache<ICallGraphNode>>()
    let callGraphDistanceTo = Dictionary<Assembly, distanceCache<IReversedCallGraphNode>>()

type [<Measure>] terminalSymbol

type ICfgNode =
    inherit IGraphNode<ICfgNode>
    abstract Offset : offset

[<Struct>]
type internal temporaryCallInfo = {callee: MethodWithBody; callFrom: offset; returnTo: offset}

type BasicBlock (method: MethodWithBody, startOffset: offset) =
    let mutable finalOffset = startOffset
    let mutable startOffset = startOffset
    let mutable isGoal = false
    let mutable isCovered = false
    let associatedStates = HashSet<IGraphTrackableState>()
    let incomingCFGEdges = HashSet<BasicBlock>()
    let incomingCallEdges = HashSet<BasicBlock>()
    let outgoingEdges = Dictionary<int<terminalSymbol>, HashSet<BasicBlock>>()
    member this.StartOffset
        with get () = startOffset
        and set v = startOffset <- v
    member this.Method = method
    member this.OutgoingEdges = outgoingEdges
    member this.IncomingCFGEdges = incomingCFGEdges
    member this.IncomingCallEdges = incomingCallEdges
    member this.AssociatedStates = associatedStates
    member this.IsCovered
        with get () = isCovered
        and set v = isCovered <- v
    member this.IsGoal
        with get () = isGoal
        and set v = isGoal <- v
    member this.HasSiblings
        with get () =
            let siblings = HashSet<BasicBlock>()
            for bb in incomingCFGEdges do
                for kvp in bb.OutgoingEdges do
                        siblings.UnionWith kvp.Value
            siblings.Count > 1

    member this.FinalOffset
        with get () = finalOffset
        and set (v : offset) = finalOffset <- v

    member this.ToString() =
        let parsedInstructions = method.ParsedInstructions
        let mutable instr = parsedInstructions |> Array.find (fun instr -> Offset.from (int instr.offset) = this.StartOffset)
        let endInstr = parsedInstructions |> Array.find (fun instr -> Offset.from (int instr.offset) = this.FinalOffset)
        let mutable notEnd = true
        seq {
            while notEnd do
                notEnd <- not <| LanguagePrimitives.PhysicalEquality instr endInstr
                yield ILRewriter.PrintILInstr None None (method :> Core.IMethod).MethodBase instr
                instr <- instr.next
        }

    interface ICfgNode with
        member this.OutgoingEdges
            with get () =
                let exists, cfgEdges = outgoingEdges.TryGetValue CfgInfo.TerminalForCFGEdge
                if exists
                then cfgEdges |> Seq.cast<ICfgNode>
                else Seq.empty
        member this.Offset = startOffset

and [<Struct>] CallInfo =
    val Callee: Method
    val CallFrom: offset
    val ReturnTo: offset
    new (callee, callFrom, returnTo) =
        {
            Callee = callee
            CallFrom = callFrom
            ReturnTo = returnTo
        }

and CfgInfo internal (method : MethodWithBody) =
    let () = assert method.HasBody
    let ilBytes = method.ILBytes
    let exceptionHandlers = method.ExceptionHandlers
    let sortedBasicBlocks = ResizeArray<BasicBlock>()
    let sinks = ResizeArray<_>()
    let calls = Dictionary<_,_>()
    let loopEntries = HashSet<offset>()

    let dfs (startVertices : array<offset>) =
        let used = HashSet<offset>()
        let basicBlocks = HashSet<BasicBlock>()
        let addBasicBlock v = basicBlocks.Add v |> ignore
        let greyVertices = HashSet<offset>()
        let vertexToBasicBlock: array<Option<BasicBlock>> = Array.init ilBytes.Length (fun _ -> None)

        let findFinalVertex intermediatePoint block =
            let mutable index = 0
            let mutable currentIndex = int intermediatePoint - 1
            let mutable found = false
            while not found do
                match vertexToBasicBlock[currentIndex] with
                | Some basicBlock when basicBlock = block ->
                    found <- true
                    index <- currentIndex
                | _ -> currentIndex <- currentIndex - 1
            Offset.from index

        let splitBasicBlock (block : BasicBlock) intermediatePoint =

            let newBlock = BasicBlock(method, block.StartOffset)
            addBasicBlock newBlock
            block.StartOffset <- intermediatePoint

            newBlock.FinalOffset <- findFinalVertex intermediatePoint block
            for v in int newBlock.StartOffset .. int intermediatePoint - 1 do
                vertexToBasicBlock[v] <- Some newBlock

            for parent in block.IncomingCFGEdges do
                let removed =
                    parent.OutgoingEdges
                    |> Seq.map (fun kvp -> kvp.Key, kvp.Value.Remove block)
                    |> Seq.filter snd
                    |> Array.ofSeq
                assert(removed.Length = 1)
                let added = parent.OutgoingEdges[fst removed[0]].Add newBlock
                assert added
                let added = newBlock.IncomingCFGEdges.Add parent
                assert added
            block.IncomingCFGEdges.Clear()
            let added = block.IncomingCFGEdges.Add newBlock
            assert added
            newBlock.OutgoingEdges.Add(CfgInfo.TerminalForCFGEdge, HashSet[|block|])
            block

        let makeNewBasicBlock startVertex =
            match vertexToBasicBlock[int startVertex] with
            | None ->
                let newBasicBlock = BasicBlock(method, startVertex)
                vertexToBasicBlock[int startVertex] <- Some newBasicBlock
                addBasicBlock newBasicBlock
                newBasicBlock
            | Some block ->
                if block.StartOffset = startVertex then block
                else splitBasicBlock block startVertex

        let addEdge (src : BasicBlock) (dst : BasicBlock) =
            let added = dst.IncomingCFGEdges.Add src
            assert added
            let exists, edges = src.OutgoingEdges.TryGetValue CfgInfo.TerminalForCFGEdge
            if exists
            then
                let added = edges.Add dst
                assert added
            else
                src.OutgoingEdges.Add(CfgInfo.TerminalForCFGEdge, HashSet [|dst|])

        let rec dfs' (currentBasicBlock : BasicBlock) (currentVertex : offset) =
            if used.Contains currentVertex
            then
                let existingBasicBlock = vertexToBasicBlock[int currentVertex]
                if currentBasicBlock <> existingBasicBlock.Value
                then
                    currentBasicBlock.FinalOffset <- findFinalVertex currentVertex currentBasicBlock
                    addEdge currentBasicBlock existingBasicBlock.Value
                if greyVertices.Contains currentVertex
                then loopEntries.Add currentVertex |> ignore
            else
                vertexToBasicBlock[int currentVertex] <- Some currentBasicBlock
                let added = greyVertices.Add currentVertex
                assert added
                let added = used.Add currentVertex
                assert added
                let opCode = MethodBody.parseInstruction method currentVertex

                let dealWithJump srcBasicBlock dst =
                    let newBasicBlock = makeNewBasicBlock dst
                    addEdge srcBasicBlock newBasicBlock
                    dfs' newBasicBlock  dst

                let processCall (callee: MethodWithBody) callFrom returnTo =
                    calls.Add(currentBasicBlock, CallInfo(callee :?> Method, callFrom, returnTo))
                    currentBasicBlock.FinalOffset <- callFrom
                    let newBasicBlock = makeNewBasicBlock returnTo
                    addEdge currentBasicBlock newBasicBlock
                    dfs' newBasicBlock returnTo

                let ipTransition = MethodBody.findNextInstructionOffsetAndEdges opCode ilBytes currentVertex

                match ipTransition with
                | FallThrough offset when MethodBody.isDemandingCallOpCode opCode ->
                    let opCode', calleeBase = method.ParseCallSite currentVertex
                    assert (opCode' = opCode)
                    let callee = MethodWithBody.InstantiateNew calleeBase
                    if callee.HasBody
                    then processCall callee currentVertex offset
                    else
                        currentBasicBlock.FinalOffset <- offset
                        dfs' currentBasicBlock offset
                | FallThrough offset ->
                    currentBasicBlock.FinalOffset <- offset
                    dfs' currentBasicBlock offset
                | ExceptionMechanism ->
                    currentBasicBlock.FinalOffset <- currentVertex
                | Return ->
                    sinks.Add currentBasicBlock
                    currentBasicBlock.FinalOffset <- currentVertex
                | UnconditionalBranch target ->
                    currentBasicBlock.FinalOffset <- currentVertex
                    dealWithJump currentBasicBlock target
                | ConditionalBranch (fallThrough, offsets) ->
                    currentBasicBlock.FinalOffset <- currentVertex
                    HashSet(fallThrough :: offsets) |> Seq.iter (dealWithJump currentBasicBlock)

                let removed = greyVertices.Remove currentVertex
                assert removed

        startVertices
        |> Array.iter (fun v -> dfs' (makeNewBasicBlock v) v)

        basicBlocks
        |> Seq.sortBy (fun b -> b.StartOffset)
        |> Seq.iter sortedBasicBlocks.Add


    let cfgDistanceFrom = GraphUtils.distanceCache<ICfgNode>()

    let findDistanceFrom node =
        Dict.getValueOrUpdate cfgDistanceFrom node (fun () ->
        let dist = incrementalSourcedDijkstraAlgo node cfgDistanceFrom
        let distFromNode = Dictionary<ICfgNode, uint>()
        for i in dist do
            if i.Value <> infinity then
                distFromNode.Add(i.Key, i.Value)
        distFromNode)

    let resolveBasicBlockIndex offset =
        let rec binSearch (sortedOffsets : ResizeArray<BasicBlock>) offset l r =
            if l >= r then l
            else
                let mid = (l + r) / 2
                let midValue = sortedOffsets[mid].StartOffset
                let leftIsLefter = midValue <= offset
                let rightIsRighter = mid + 1 >= sortedOffsets.Count || sortedOffsets[mid + 1].StartOffset > offset
                if leftIsLefter && rightIsRighter then mid
                elif not rightIsRighter
                    then binSearch sortedOffsets offset (mid + 1) r
                    else binSearch sortedOffsets offset l (mid - 1)

        binSearch sortedBasicBlocks offset 0 (sortedBasicBlocks.Count - 1)

    let resolveBasicBlock offset = sortedBasicBlocks[resolveBasicBlockIndex offset]

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

    static member TerminalForCFGEdge = 0<terminalSymbol>
    member this.SortedBasicBlocks = sortedBasicBlocks
    member this.IlBytes = ilBytes
    member this.EntryPoint = sortedBasicBlocks[0]
    member this.Sinks = sinks
    member this.Calls = calls
    member this.IsLoopEntry offset = loopEntries.Contains offset
    member internal this.ResolveBasicBlockIndex offset = resolveBasicBlockIndex offset
    member this.ResolveBasicBlock offset = resolveBasicBlock offset
    member this.IsBasicBlockStart offset = (resolveBasicBlock offset).StartOffset = offset
    // Returns dictionary of shortest distances, in terms of basic blocks (1 step = 1 basic block transition)
    member this.DistancesFrom offset =
        let bb = resolveBasicBlock offset
        findDistanceFrom (bb :> ICfgNode)
    member this.HasSiblings offset =
        let basicBlock = resolveBasicBlock offset
        basicBlock.HasSiblings

and Method internal (m : MethodBase) as this =
    inherit MethodWithBody(m)
    let cfg = lazy(
        if this.HasBody then
            Logger.trace $"Add CFG for {this}."
            let cfg = this |> CfgInfo |> Some
            Method.ReportCFGLoaded this
            cfg
        else None)

    member x.CFG with get() = cfg.Force()

    member x.ForceCFG with get() =
        match x.CFG with
        | Some cfg -> cfg
        | None -> internalfailf $"Getting CFG of method {x} without body (extern or abstract)"

    member x.BasicBlocks with get() =
        match x.CFG with
        | Some cfg -> cfg.SortedBasicBlocks
        | None -> ResizeArray()

    // Helps resolving cyclic dependencies between Application and MethodWithBody
    [<DefaultValue>] static val mutable private cfgReporter : Method -> unit
    static member internal ReportCFGLoaded with get() = Method.cfgReporter and set v = Method.cfgReporter <- v
    static member val internal CoverageZone : Method -> bool = fun _ -> true with get, set

    member x.InCoverageZone with get() = Method.CoverageZone x

    interface ICallGraphNode with
        member this.OutgoingEdges with get () =
            let edges = HashSet<_>()
            for bb in this.ForceCFG.Sinks do
                for kvp in bb.OutgoingEdges do
                    if kvp.Key <> CfgInfo.TerminalForCFGEdge
                    then
                        for target in kvp.Value do
                            let added = edges.Add target.Method
                            assert added
            edges |> Seq.cast<ICallGraphNode>

    interface IReversedCallGraphNode with
        member this.OutgoingEdges with get () =
            let edges = HashSet<_>()
            for bb in this.ForceCFG.EntryPoint.IncomingCallEdges do
                edges.Add bb.Method |> ignore
            edges |> Seq.cast<IReversedCallGraphNode>

    static member val internal AttributesZone : Method -> bool = fun _ -> true with get, set

    member x.CheckAttributes with get() = Method.AttributesZone x

    member x.BasicBlocksCount with get() = x.BasicBlocks.Count

    member x.CallGraphDistanceFromMe with get () =
        let assembly = x.Module.Assembly
        let callGraphDistanceFrom = CallGraph.callGraphDistanceFrom
        let mutable callGraphDist = null
        let exists, value = callGraphDistanceFrom.TryGetValue assembly
        if not exists then
            let newDict = Dictionary<_, _>()
            callGraphDistanceFrom[assembly] <- newDict
            callGraphDist <- newDict
        else callGraphDist <- value

        let exists, value = callGraphDist.TryGetValue x
        if not exists then
            let dist = incrementalSourcedDijkstraAlgo (x :> ICallGraphNode) callGraphDist
            let distFromNode = Dictionary<ICallGraphNode, uint>()
            for i in dist do
                if i.Value <> infinity then
                    distFromNode.Add(i.Key, i.Value)
            callGraphDist[x] <- distFromNode
            distFromNode
        else value

    member x.CallGraphDistanceToMe with get () =
        let assembly = x.Module.Assembly
        let callGraphDistanceTo = CallGraph.callGraphDistanceTo
        let mutable callGraphDist = null
        let exists, value = callGraphDistanceTo.TryGetValue assembly
        if not exists then
            let newDict = Dictionary<_, _>()
            callGraphDistanceTo[assembly] <- newDict
            callGraphDist <- newDict
        else callGraphDist <- value

        let exists, value = callGraphDist.TryGetValue x
        let mutable res = null
        if not exists then
            let dist = incrementalSourcedDijkstraAlgo (x :> IReversedCallGraphNode) callGraphDist
            let distToNode = Dictionary<IReversedCallGraphNode, uint>()
            for i in dist do
                if i.Value <> infinity then
                    distToNode.Add(i.Key, i.Value)
            callGraphDist[x] <- distToNode
            res <- distToNode
        else res <- value
        res

and
    [<CustomEquality; CustomComparison>]
    public codeLocation = {offset : offset; method : Method}
        with
        member x.BasicBlock =
            Option.map (fun (cfg : CfgInfo) -> cfg.ResolveBasicBlock x.offset) x.method.CFG
        member x.ForceBasicBlock with get() = x.method.ForceCFG.ResolveBasicBlock x.offset
        override x.Equals y =
            match y with
            | :? codeLocation as y -> x.offset = y.offset && x.method.Equals(y.method)
            | _ -> false
        override x.GetHashCode() = (x.offset, x.method).GetHashCode()
        override x.ToString() =
            sprintf "[method = %s\noffset = %s]" x.method.FullName ((int x.offset).ToString("X"))
        interface IComparable with
            override x.CompareTo y =
                match y with
                | :? codeLocation as y when x.method.Equals(y.method) -> compare x.offset y.offset
                | :? codeLocation as y -> (x.method :> IComparable).CompareTo(y.method)
                | _ -> -1

and IGraphTrackableState =
    abstract member CodeLocation: codeLocation
    abstract member CallStack: list<Method>

module public CodeLocation =

    let hasSiblings (blockStart : codeLocation) =
        match blockStart.method.CFG with
        | Some cfg -> cfg.HasSiblings blockStart.offset
        | None -> false

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

type ApplicationGraph() =

    let dummyTerminalForCallEdge = 1<terminalSymbol>
    let dummyTerminalForReturnEdge = 2<terminalSymbol>

    let addCallEdge (callSource : codeLocation) (callTarget : codeLocation) =
        let callerMethodCfgInfo = callSource.method.ForceCFG
        let calledMethodCfgInfo = callTarget.method.ForceCFG
        let callFrom = callSource.ForceBasicBlock
        let callTo = calledMethodCfgInfo.EntryPoint
        let exists, location = callerMethodCfgInfo.Calls.TryGetValue callSource.ForceBasicBlock
        if not <| callTo.IncomingCallEdges.Contains callFrom then
            let mutable returnTo = callFrom
            // if not exists then it should be from exception mechanism
            if not callTarget.method.IsStaticConstructor && exists then
                returnTo <- callerMethodCfgInfo.ResolveBasicBlock location.ReturnTo
                let exists, callEdges = callFrom.OutgoingEdges.TryGetValue dummyTerminalForCallEdge
                if exists then
                    let added = callEdges.Add callTo
                    assert added
                else callFrom.OutgoingEdges.Add(dummyTerminalForCallEdge, Seq.singleton callTo |> HashSet)

            for returnFrom in calledMethodCfgInfo.Sinks do
                let outgoingEdges = returnFrom.OutgoingEdges
                let exists, returnEdges = outgoingEdges.TryGetValue dummyTerminalForReturnEdge
                if exists then
                    let added = returnEdges.Add returnTo
                    assert added
                else outgoingEdges.Add(dummyTerminalForReturnEdge, Seq.singleton returnTo |> HashSet)
                let added = returnTo.IncomingCallEdges.Add returnFrom
                assert added

            let added = callTo.IncomingCallEdges.Add callFrom
            assert added

    let moveState (initialPosition : codeLocation) (stateWithNewPosition : IGraphTrackableState) =
        ()

    let addStates (parentState : Option<IGraphTrackableState>) (states : array<IGraphTrackableState>) =
        ()

    let getShortestDistancesToGoals (states : array<codeLocation>) =
        __notImplemented__()

    let messagesProcessor = MailboxProcessor.Start(fun inbox ->
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
                        let cfg = method.CFG |> Option.get
                        reply cfg
                    | AddCallEdge (src, dst) ->
                        match src.method.CFG, dst.method.CFG with
                        | Some _, Some _ ->  addCallEdge src dst
                        | _ -> ()
                    | AddGoals positions ->
                        Logger.trace "Add goals."
                    | RemoveGoal pos ->
                        Logger.trace "Remove goal."
                    | SpawnStates states -> Array.ofSeq states |> addStates None
                    | AddForkedStates (parentState, forkedStates) ->
                        addStates (Some parentState) (Array.ofSeq forkedStates)
                    | MoveState (src, dst) ->
                        moveState src dst
                    | GetShortestDistancesToGoals (replyChannel, states) ->
                        Logger.trace "Get shortest distances."
                        __notImplemented__()
                    | GetDistanceToNearestGoal (replyChannel, states) ->
                        replyChannel.Reply []
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
        addCallEdge sourceLocation targetLocation

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
    abstract DrawInterproceduralEdges: bool
    abstract AddState : IGraphTrackableState -> unit
    abstract TerminateState : IGraphTrackableState -> unit
    abstract VisualizeGraph : unit -> unit
    abstract VisualizeStep : codeLocation -> IGraphTrackableState -> IGraphTrackableState seq -> unit

type NullVisualizer() =
    interface IVisualizer with
        override x.DrawInterproceduralEdges = false
        override x.AddState _ = ()
        override x.TerminateState _ = ()
        override x.VisualizeGraph () = ()
        override x.VisualizeStep _ _ _ = ()

module Application =
    let private methods = ConcurrentDictionary<methodDescriptor, Method>()
    let private _loadedMethods = ConcurrentDictionary<Method, unit>()
    let loadedMethods = _loadedMethods :> seq<_>
    let graph = ApplicationGraph()
    // TODO: if visualizer is not set, decline all 'ApplicationGraph' calls
    let mutable visualizer : IVisualizer = NullVisualizer()

    let getMethod (m : MethodBase) : Method =
        let desc = Reflection.getMethodDescriptor m
        Dict.getValueOrUpdate methods desc (fun () -> Method(m))

    let setCoverageZone (zone : Method -> bool) =
        Method.CoverageZone <- zone

    let setAttributesZone (zone : Method -> bool) =
        Method.AttributesZone <- zone

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
            let added = _loadedMethods.TryAdd(m, ())
            assert added
