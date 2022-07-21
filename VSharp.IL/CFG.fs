namespace VSharp

open System.Reflection
open System.Collections.Generic
open FSharpx.Collections
open Microsoft.FSharp.Collections
open VSharp
open VSharp.Core

type IGraphTrackableState =
    abstract member CodeLocation: codeLocation

[<Struct>]
type CallInfo =
    val Callee: MethodBase
    val CallFrom: offset
    val ReturnTo: offset
    new (callee, callFrom, returnTo) =
        {
            Callee = callee
            CallFrom = callFrom
            ReturnTo = returnTo
        }

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

type CfgTemporaryData (methodBase : MethodBase) =
    let ilBytes = Instruction.getILBytes methodBase
    let exceptionHandlers = Instruction.getEHSBytes methodBase
    let sortedOffsets = ResizeArray<offset>()
    let edges = Dictionary<offset, HashSet<offset>>()
    let sinks = ResizeArray<_>()
    let calls = ResizeArray<CallInfo>()
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
                    vertexToBasicBloc.[v] <- Some newBlock
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
                match vertexToBasicBloc.[dst] with
                | None -> ()
                | Some block ->
                    if block.InnerVertices.Contains dst && block.FinalVertex <> dst
                    then
                        splitEdge block.StartVertex block.FinalVertex dst
                        splitBasicBlock block dst


        let rec dfs' (currentBasicBlock : BasicBlock) (currentVertex : offset) =

            vertexToBasicBloc.[currentVertex] <- Some currentBasicBlock

            if used.Contains currentVertex
            then
                currentBasicBlock.FinalVertex <- currentVertex
                addEdge currentBasicBlock.StartVertex currentVertex
                if greyVertices.Contains currentVertex
                then loopEntries.Add currentVertex |> ignore
            else
                greyVertices.Add currentVertex |> ignore
                used.Add currentVertex |> ignore
                let opCode = Instruction.parseInstruction methodBase currentVertex

                let dealWithJump src dst =
                    addVertex src
                    addVertex dst
                    addEdge src dst
                    dfs' (BasicBlock dst)  dst

                let processCall callee callFrom returnTo =
                    calls.Add(CallInfo(callee, callFrom,returnTo))
                    currentBasicBlock.FinalVertex <- currentVertex
                    addEdge currentBasicBlock.StartVertex currentVertex
                    addEdge callFrom returnTo
                    dfs' (BasicBlock returnTo) returnTo

                let ipTransition = Instruction.findNextInstructionOffsetAndEdges opCode ilBytes currentVertex

                match ipTransition with
                | FallThrough offset when Instruction.isDemandingCallOpCode opCode ->
                    let calledMethod = TokenResolver.resolveMethodFromMetadata methodBase ilBytes (currentVertex + opCode.Size)
                    if not <| Reflection.isExternalMethod calledMethod
                    then processCall calledMethod currentVertex offset
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
             yield 0
             for handler in exceptionHandlers do
                 yield handler.handlerOffset
                 match handler.ehcType with
                 | Filter offset -> yield offset
                 | _ -> ()
            |]

        dfs startVertices
        sortedOffsets |> Seq.iter (fun bb ->
            if edges.ContainsKey bb then
                let outgoing = edges.[bb]
                if outgoing.Count > 1 then
                    offsetsWithSiblings.UnionWith outgoing
            else edges.Add(bb, HashSet<_>()))

    member this.MethodBase = methodBase
    member this.ILBytes = ilBytes
    member this.SortedOffsets = sortedOffsets
    member this.Edges = edges
    member this.Calls = calls
    member this.Sinks = sinks.ToArray()
    member this.LoopEntries = loopEntries
    member this.BlocksWithSiblings = offsetsWithSiblings
    member this.DistancesFrom offset = findDistanceFrom offset


type CfgInfo (cfg:CfgTemporaryData) =
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
        |> ResizeArray.iter (fun callInfo -> res.Add(callInfo.CallFrom, callInfo))
        res

    member this.MethodBase = cfg.MethodBase
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


type private ApplicationGraphMessage =
    | ResetQueryEngine
    | AddGoals of array<codeLocation>
    | RemoveGoal of codeLocation
    | AddStates of seq<IGraphTrackableState>
    | AddForkedStates of parentState:IGraphTrackableState * forkedStates:seq<IGraphTrackableState>
    | MoveState of positionForm:codeLocation * positionTo: IGraphTrackableState
    | AddCFG of Option<AsyncReplyChannel<CfgInfo>> *  MethodBase
    | AddCallEdge of callForm:codeLocation * callTo: codeLocation
    | GetShortestDistancesToGoals
        of AsyncReplyChannel<ResizeArray<codeLocation * codeLocation * int>> * array<codeLocation>
    | GetReachableGoals
        of AsyncReplyChannel<Dictionary<codeLocation,HashSet<codeLocation>>> * array<codeLocation>
    | GetDistanceToNearestGoal
        of AsyncReplyChannel<seq<IGraphTrackableState * int>> * seq<IGraphTrackableState>

type ApplicationGraph() as this =

    let cfgs = Dictionary<MethodBase, CfgInfo>()

    let buildCFG (methodBase:MethodBase) =
        Logger.trace $"Add CFG for %A{methodBase.Name}."
        let cfg = CfgTemporaryData methodBase
        cfg

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
        let tryGetCfgInfo methodBase =
            let exists,cfgInfo = cfgs.TryGetValue methodBase
            if not exists
            then
                let cfg = buildCFG methodBase
                let cfgInfo = CfgInfo cfg
                cfgs.Add(methodBase, cfgInfo)
                cfgInfo
            else cfgInfo

        async{
            while true do
                let! message = inbox.Receive()
                try
                    match message with
                    | ResetQueryEngine ->
                        __notImplemented__()
                    | AddCFG (replyChannel, methodBase) ->
                        let reply cfgInfo =
                            match replyChannel with
                            | Some ch -> ch.Reply cfgInfo
                            | None -> ()
                        let cfgInfo = tryGetCfgInfo methodBase
                        reply cfgInfo
                    | AddCallEdge (_from, _to) ->
                        tryGetCfgInfo _from.method |> ignore
                        tryGetCfgInfo _to.method |> ignore
                        addCallEdge _from _to
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
                    | AddCFG (Some ch, _) -> ch.Reply (Unchecked.defaultof<CfgInfo>)
                    | _ -> ()
        }
    )

    do
        messagesProcessor.Error.Add(fun e ->
            Logger.error $"Something wrong in application graph messages processor: \n %A{e} \n %s{e.Message} \n %s{e.StackTrace}"
            raise e
            )

    member this.GetCfg (methodBase: MethodBase) =
         messagesProcessor.PostAndReply (fun ch -> AddCFG (Some ch, methodBase))

    member this.RegisterMethod (methodBase: MethodBase) =
        messagesProcessor.Post (AddCFG (None, methodBase))

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

module CFG =
    let applicationGraph = ApplicationGraph()

    // TODO: ideally, we don't need everything below. Call graph gets lazily bundled into application graph. Get rid of it when CFL reachability is ready.

    type callGraph =
        {
            graph : GraphUtils.graph<MethodBase>
            reverseGraph : GraphUtils.graph<MethodBase>
        }

    let callGraphs = Dictionary<Assembly, callGraph>()
    let callGraphDijkstras = Dictionary<Assembly, Dictionary<MethodBase * MethodBase, uint>>()
    let callGraphFloyds = Dictionary<Assembly, Dictionary<MethodBase * MethodBase, uint>>()
    let callGraphDistanceFrom = Dictionary<Assembly, GraphUtils.distanceCache<MethodBase>>()
    let callGraphDistanceTo = Dictionary<Assembly, GraphUtils.distanceCache<MethodBase>>()

    let private fromCurrentAssembly assembly (current : MethodBase) = current.Module.Assembly = assembly

    let private addToDict (dict : Dictionary<'a, HashSet<'b>> ) (k : 'a ) (v : 'b) =
        let mutable refSet = ref null
        if not (dict.TryGetValue(k, refSet)) then
            refSet <- ref (HashSet<_>())
            dict.Add(k, refSet.Value)
        refSet.Value.Add(v) |> ignore

    let private addCall (methods : HashSet<MethodBase>) methodsReachability caller callee =
        methods.Add callee |> ignore
        addToDict methodsReachability caller callee

    let private buildMethodsReachabilityForAssembly (assembly : Assembly) (entryMethod : MethodBase) =
        let methods = HashSet<MethodBase>()
        let methodsReachability = Dictionary<MethodBase, HashSet<MethodBase>>()
        let rec exit processedMethods = function
            | [] -> ()
            | m :: q' -> dfs processedMethods q' m
        and dfs (processedMethods : MethodBase list) (methodsQueue : MethodBase list) (current : MethodBase) =
            methods.Add current |> ignore
            if List.contains current processedMethods || not (fromCurrentAssembly assembly current) then exit processedMethods methodsQueue
            else
                let processedMethods = current :: processedMethods
                if current.GetMethodBody() <> null then
                    let cfg = applicationGraph.GetCfg current
                    cfg.Calls |> Seq.iter (fun kvp ->
                        let m = kvp.Value.Callee
                        addCall methods methodsReachability current m)
                let newQ =
                    if methodsReachability.ContainsKey(current) then List.ofSeq methodsReachability.[current] @ methodsQueue
                    else methodsQueue
                exit processedMethods newQ
        let mq = List<MethodBase>()
        dfs [] (List.ofSeq mq) entryMethod
        methods, methodsReachability

    let private buildCallGraph (assembly : Assembly) (entryMethod : MethodBase) =
        let methods, methodsReachability = buildMethodsReachabilityForAssembly assembly entryMethod
        let graph = GraphUtils.graph<MethodBase>()
        let reverseGraph = GraphUtils.graph<MethodBase>()
        for i in methods do
            graph.Add (i, HashSet<_>())
            reverseGraph.Add (i, HashSet<_>())
        for i in methodsReachability do
            for j in i.Value do
                let added = graph.[i.Key].Add j in assert added
                let added = reverseGraph.[j].Add i.Key in assert added
        { graph = graph; reverseGraph = reverseGraph }

    let private findCallGraph (assembly : Assembly) (entryMethod : MethodBase) =
        let callGraph = Dict.getValueOrUpdate callGraphs assembly (fun () -> buildCallGraph assembly entryMethod)
        if not <| callGraph.graph.ContainsKey entryMethod then
            let updateCallGraph = buildCallGraph assembly entryMethod
            for i in updateCallGraph.graph do
                callGraph.graph.TryAdd(i.Key, i.Value) |> ignore
            for i in updateCallGraph.reverseGraph do
                callGraph.reverseGraph.TryAdd(i.Key, i.Value) |> ignore
        callGraph

    let private buildCallGraphDistance (assembly : Assembly) (entryMethod : MethodBase) =
        let methods, methodsReachability = buildMethodsReachabilityForAssembly assembly entryMethod
        let callGraph = GraphUtils.graph<MethodBase>()
        for i in methods do
            callGraph.Add (i, HashSet<_>())
        for i in methodsReachability do
            for j in i.Value do
                let added = callGraph.[i.Key].Add j
                assert added
        GraphUtils.dijkstraAlgo methods callGraph

    let findCallGraphDistance (assembly : Assembly) (entryMethod : MethodBase) =
        let callGraphDist = Dict.getValueOrUpdate callGraphDijkstras assembly (fun () -> buildCallGraphDistance assembly entryMethod)
        if not <| callGraphDist.ContainsKey (entryMethod, entryMethod) then
            for i in buildCallGraphDistance assembly entryMethod do
                callGraphDist.TryAdd(i.Key, i.Value) |> ignore
        callGraphDist

    let findCallGraphDistanceFrom (method : MethodBase) =
        let assembly = method.Module.Assembly
        let callGraphDist = Dict.getValueOrUpdate callGraphDistanceFrom assembly (fun () -> Dictionary<_, _>())
        Dict.getValueOrUpdate callGraphDist method (fun () ->
        let callGraph = findCallGraph assembly method
        let dist = GraphUtils.incrementalSourcedDijkstraAlgo method callGraph.graph callGraphDist
        let distFromNode = Dictionary<MethodBase, uint>()
        for i in dist do
            if i.Value <> GraphUtils.infinity then
                distFromNode.Add(i.Key, i.Value)
        distFromNode)

    let findCallGraphDistanceTo (method : MethodBase) =
        let assembly = method.Module.Assembly
        let callGraphDist = Dict.getValueOrUpdate callGraphDistanceTo assembly (fun () -> Dictionary<_, _>())
        Dict.getValueOrUpdate callGraphDist method (fun () ->
        let callGraph = findCallGraph assembly method
        let dist = GraphUtils.incrementalSourcedDijkstraAlgo method callGraph.reverseGraph callGraphDist
        let distToNode = Dictionary<MethodBase, uint>()
        for i in dist do
            if i.Value <> GraphUtils.infinity then
                distToNode.Add(i.Key, i.Value)
        distToNode)
