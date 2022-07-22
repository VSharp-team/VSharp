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

and CfgInfo internal (cfg:CfgTemporaryData) =
    let resolveBasicBlock offset =
        let rec binSearch (sortedOffsets : ResizeArray<offset>) offset l r =
            if l >= r then sortedOffsets.[l]
            else
                let mid = (l + r) / 2
                let midValue = sortedOffsets.[mid]
                if midValue = offset
                then midValue
                elif midValue < offset
                then binSearch sortedOffsets offset (mid + 1) r
                else binSearch sortedOffsets offset l (mid - 1)

        binSearch cfg.SortedOffsets offset 0 (cfg.SortedOffsets.Count - 1)

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
    member this.Sinks = sinks
    member this.Calls = calls
    member this.IsLoopEntry offset = loopEntries.Contains offset
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
            cfg |> CfgInfo |> Some
        else None)

    member x.CFG with get() =
        match cfg.Force() with
        | Some cfg -> cfg
        | None -> internalfailf $"Getting CFG of method {x} without body (extern or abstract)"

    // Helps resolving cyclic dependencies between Application and MethodWithBody
    [<DefaultValue>] static val mutable private cfgReporter : Method -> unit
    static member internal ReportCFGLoaded with get() = Method.cfgReporter and set v = Method.cfgReporter <- v

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

type private ApplicationGraphMessage =
    | ResetQueryEngine
    | AddGoals of array<codeLocation>
    | RemoveGoal of codeLocation
    | AddStates of seq<IGraphTrackableState>
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
                    | AddStates states -> Array.ofSeq states |> addStates None
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

    member this.AddState (state:IGraphTrackableState) =
        messagesProcessor.Post <| AddStates [|state|]

    member this.AddStates (states:seq<IGraphTrackableState>) =
        messagesProcessor.Post <| AddStates states

    member this.AddForkedStates (parentState:IGraphTrackableState, states:seq<IGraphTrackableState>) =
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

module Application =
    let private methods = Dictionary<MethodBase, Method>()
    let applicationGraph = ApplicationGraph()

    let getMethod (m : MethodBase) : Method =
        Dict.getValueOrUpdate methods m (fun () -> Method(m))

    let () =
        MethodWithBody.InstantiateNew <- fun m -> getMethod m :> MethodWithBody
        Method.ReportCFGLoaded <- applicationGraph.RegisterMethod

// TODO: ideally, we don't need CallGraph: it gets lazily bundled into application graph. Get rid of it when CFL reachability is ready.
module CallGraph =
    type callGraph =
        {
            graph : GraphUtils.graph<Method>
            reverseGraph : GraphUtils.graph<Method>
        }

    let graph = GraphUtils.graph<Method>()
    let reverseGraph = GraphUtils.graph<Method>()

    let callGraphs = Dictionary<Assembly, callGraph>()
    let callGraphDijkstras = Dictionary<Assembly, Dictionary<Method * Method, uint>>()
    let callGraphFloyds = Dictionary<Assembly, Dictionary<Method * Method, uint>>()
    let callGraphDistanceFrom = Dictionary<Assembly, GraphUtils.distanceCache<Method>>()
    let callGraphDistanceTo = Dictionary<Assembly, GraphUtils.distanceCache<Method>>()

    let private fromCurrentAssembly assembly (current : Method) = current.Module.Assembly = assembly

    let private addToDict (dict : Dictionary<'a, HashSet<'b>> ) (k : 'a ) (v : 'b) =
        let mutable refSet = ref null
        if not (dict.TryGetValue(k, refSet)) then
            refSet <- ref (HashSet<_>())
            dict.Add(k, refSet.Value)
        refSet.Value.Add(v) |> ignore

    let private addCall (methods : HashSet<Method>) methodsReachability caller callee =
        methods.Add callee |> ignore
        addToDict methodsReachability caller callee

    let private buildMethodsReachabilityForAssembly (assembly : Assembly) (entryMethod : Method) =
        let methods = HashSet<Method>()
        let methodsReachability = Dictionary<Method, HashSet<Method>>()
        let rec exit processedMethods = function
            | [] -> ()
            | m :: q' -> dfs processedMethods q' m
        and dfs (processedMethods : Method list) (methodsQueue : Method list) (current : Method) =
            methods.Add current |> ignore
            if List.contains current processedMethods || not (fromCurrentAssembly assembly current) then exit processedMethods methodsQueue
            else
                let processedMethods = current :: processedMethods
                if current.HasBody then
                    current.CFG.Calls |> Seq.iter (fun kvp ->
                        let m = kvp.Value.Callee
                        addCall methods methodsReachability current m)
                let newQ =
                    if methodsReachability.ContainsKey(current) then List.ofSeq methodsReachability.[current] @ methodsQueue
                    else methodsQueue
                exit processedMethods newQ
        let mq = List<Method>()
        dfs [] (List.ofSeq mq) entryMethod
        methods, methodsReachability

    let private buildCallGraph (assembly : Assembly) (entryMethod : Method) =
        let methods, methodsReachability = buildMethodsReachabilityForAssembly assembly entryMethod
        let graph = GraphUtils.graph<Method>()
        let reverseGraph = GraphUtils.graph<Method>()
        for i in methods do
            graph.Add (i, HashSet<_>())
            reverseGraph.Add (i, HashSet<_>())
        for i in methodsReachability do
            for j in i.Value do
                let added = graph.[i.Key].Add j in assert added
                let added = reverseGraph.[j].Add i.Key in assert added
        { graph = graph; reverseGraph = reverseGraph }

    let private findCallGraph (assembly : Assembly) (entryMethod : Method) =
        let callGraph = Dict.getValueOrUpdate callGraphs assembly (fun () -> buildCallGraph assembly entryMethod)
        if not <| callGraph.graph.ContainsKey entryMethod then
            let updateCallGraph = buildCallGraph assembly entryMethod
            for i in updateCallGraph.graph do
                callGraph.graph.TryAdd(i.Key, i.Value) |> ignore
            for i in updateCallGraph.reverseGraph do
                callGraph.reverseGraph.TryAdd(i.Key, i.Value) |> ignore
        callGraph

    let private buildCallGraphDistance (assembly : Assembly) (entryMethod : Method) =
        let methods, methodsReachability = buildMethodsReachabilityForAssembly assembly entryMethod
        let callGraph = GraphUtils.graph<Method>()
        for i in methods do
            callGraph.Add (i, HashSet<_>())
        for i in methodsReachability do
            for j in i.Value do
                let added = callGraph.[i.Key].Add j
                assert added
        GraphUtils.dijkstraAlgo methods callGraph

    let findCallGraphDistance (assembly : Assembly) (entryMethod : Method) =
        let callGraphDist = Dict.getValueOrUpdate callGraphDijkstras assembly (fun () -> buildCallGraphDistance assembly entryMethod)
        if not <| callGraphDist.ContainsKey (entryMethod, entryMethod) then
            for i in buildCallGraphDistance assembly entryMethod do
                callGraphDist.TryAdd(i.Key, i.Value) |> ignore
        callGraphDist

    let findCallGraphDistanceFrom (method : Method) =
        let assembly = method.Module.Assembly
        let callGraphDist = Dict.getValueOrUpdate callGraphDistanceFrom assembly (fun () -> Dictionary<_, _>())
        Dict.getValueOrUpdate callGraphDist method (fun () ->
        let callGraph = findCallGraph assembly method
        let dist = GraphUtils.incrementalSourcedDijkstraAlgo method callGraph.graph callGraphDist
        let distFromNode = Dictionary<Method, uint>()
        for i in dist do
            if i.Value <> GraphUtils.infinity then
                distFromNode.Add(i.Key, i.Value)
        distFromNode)

    let findCallGraphDistanceTo (method : Method) =
        let assembly = method.Module.Assembly
        let callGraphDist = Dict.getValueOrUpdate callGraphDistanceTo assembly (fun () -> Dictionary<_, _>())
        Dict.getValueOrUpdate callGraphDist method (fun () ->
        let callGraph = findCallGraph assembly method
        let dist = GraphUtils.incrementalSourcedDijkstraAlgo method callGraph.reverseGraph callGraphDist
        let distToNode = Dictionary<Method, uint>()
        for i in dist do
            if i.Value <> GraphUtils.infinity then
                distToNode.Add(i.Key, i.Value)
        distToNode)
