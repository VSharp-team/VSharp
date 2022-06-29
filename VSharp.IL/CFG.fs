namespace VSharp

open System
open System.Reflection
open System.Collections.Generic

open System.Reflection.Emit
open CFPQ_GLL
open CFPQ_GLL.GLL
open CFPQ_GLL.InputGraph
open CFPQ_GLL.RSM
open FSharpx.Collections
open VSharp
open VSharp.Core

[<Struct>]
type CallInfo =
    val CallFrom: offset
    val ReturnTo: offset
    val MethodToCall: MethodBase
    new (callFrom, returnTo, methodToCall) =
        {
            CallFrom = callFrom
            ReturnTo = returnTo
            MethodToCall = methodToCall
        }

[<Struct>]
type private CFGTemporaryData =
    val VerticesOffsets : HashSet<int>
    val FallThroughOffset : array<Option<offset>>
    val Edges : Dictionary<offset, HashSet<offset>>
    val OffsetsDemandingCall : Dictionary<offset, OpCode * MethodBase>    
    new (verticesOffset, fallThroughOffset, edges, offsetsDemandingCall) =
        {
            VerticesOffsets = verticesOffset
            FallThroughOffset = fallThroughOffset
            Edges = edges
            OffsetsDemandingCall = offsetsDemandingCall            
        }
    
type CFG (methodBase : MethodBase) =
    let ilBytes = Instruction.getILBytes methodBase
    let exceptionHandlers = Instruction.getEHSBytes methodBase
    let sortedOffsets = ResizeArray<offset>()
    let edges = Dictionary<offset, ResizeArray<offset>>()
    let offsetsDemandingCall = Dictionary<offset,_>()
    let sinks = ResizeArray<_>()
    let callsToAdd = ResizeArray<CallInfo>()
    let loopEntries = HashSet<offset>()
    
    let addVerticesAndEdges (temporaryData : CFGTemporaryData) =
        temporaryData.VerticesOffsets
        |> Seq.sort
        |> Seq.iter (fun v ->
            sortedOffsets.Add v
            edges.Add(v, ResizeArray<_>()))

        let addEdge src dst =
            edges.[src].Add dst            

        let used = HashSet<offset>()
        let rec addEdges currentVertex src =
            if used.Contains src then ()
            else
                let wasAdded = used.Add src
                assert wasAdded
                let exists, callInfo = offsetsDemandingCall.TryGetValue src
                if exists
                then
                    addEdge currentVertex src                    
                    let dst = temporaryData.FallThroughOffset.[src].Value
                    CallInfo (src, dst, snd callInfo)
                    |> callsToAdd.Add
                    addEdges dst dst                    
                else 
                   match temporaryData.FallThroughOffset.[src] with                
                    | Some dst when sortedOffsets.Contains dst ->
                        addEdge currentVertex dst
                        addEdges dst dst
                    | Some dst -> addEdges currentVertex dst
                    | None when temporaryData.Edges.ContainsKey src ->
                        temporaryData.Edges.[src] |> Seq.iter (fun dst ->
                            if sortedOffsets.Contains dst
                            then
                                addEdge currentVertex dst
                                addEdges dst dst
                            else
                                addEdges currentVertex dst
                        )
                    | None -> ()
        
        let returnPoints = callsToAdd |> ResizeArray.map (fun callInfo -> callInfo.ReturnTo)         
        returnPoints.AddRange sortedOffsets
        returnPoints |> ResizeArray.iter (fun offset -> addEdges offset offset)        

    let dfs (startVertices : array<offset>) =
        let used = HashSet<offset>()        
        let verticesOffsets = HashSet<offset> startVertices
        let addVertex v = verticesOffsets.Add v |> ignore
        let edges = Dictionary<offset, HashSet<offset>>()
        let fallThroughOffset = Array.init ilBytes.Length (fun _ -> None)
        
        let addEdge src dst =
            let exists,outgoingEdges = edges.TryGetValue src
            if exists
            then outgoingEdges.Add dst |> ignore
            else edges.Add(src, HashSet [|dst|])
        
        let greyVertices = HashSet<offset>()
            
        let rec dfs' (v : offset) =
            if used.Contains v
            then
                if greyVertices.Contains v
                then loopEntries.Add v |> ignore
            else 
                greyVertices.Add v |> ignore
                used.Add v |> ignore
                let opCode = Instruction.parseInstruction methodBase v                

                let dealWithJump src dst =
                    addVertex src
                    addVertex dst 
                    addEdge src dst
                    dfs' dst

                let ipTransition = Instruction.findNextInstructionOffsetAndEdges opCode ilBytes v
                match ipTransition with
                | FallThrough offset when Instruction.isDemandingCallOpCode opCode ->
                    let calledMethod = TokenResolver.resolveMethodFromMetadata methodBase ilBytes (v + opCode.Size)
                    offsetsDemandingCall.Add(v, (opCode, calledMethod))
                    addVertex v
                    addVertex offset
                    fallThroughOffset.[v] <- Some offset
                    dfs' offset
                | FallThrough offset ->
                    fallThroughOffset.[v] <- Some offset
                    dfs' offset
                | ExceptionMechanism ->
                    Logger.trace $"Exception mechanism: %A{v}"
                    ()
                | Return ->
                    addVertex v
                    sinks.Add v
                | UnconditionalBranch target -> dealWithJump v target
                | ConditionalBranch (fallThrough, offsets) ->
                    dealWithJump v fallThrough
                    offsets |> List.iter (dealWithJump v)
                greyVertices.Remove v |> ignore
        
        startVertices
        |> Array.iter dfs'
        
        CFGTemporaryData(verticesOffsets, fallThroughOffset, edges, offsetsDemandingCall)
        
    do
        let startVertices =
            [|
             yield 0
             yield! exceptionHandlers |> Array.choose (fun handler -> match handler.ehcType with | Filter offset -> Some offset | _ -> None)
             yield! exceptionHandlers |> Array.map (fun ehc -> ehc.handlerOffset)
            |]
        
        let temporaryData = dfs startVertices
        
        addVerticesAndEdges temporaryData
        
    member this.MethodBase = methodBase
    member this.ILBytes = ilBytes
    member this.SortedOffsets = sortedOffsets
    member this.Edges = edges
    member this.CallsToAdd = callsToAdd
    member this.Sinks = sinks.ToArray()
    member this.LoopEntries = loopEntries

type CfgInfo (cfg:CFG) =
    let resolveBasicBlock offset =
        let rec binSearch (sortedOffsets : List<offset>) offset l r =
            if l >= r then sortedOffsets.[l]
            else
                let mid = (l + r) / 2
                let midValue = sortedOffsets.[mid]
                if midValue = offset then midValue
                elif midValue < offset then
                    binSearch sortedOffsets offset (mid + 1) r
                else
                    binSearch sortedOffsets offset l (mid - 1)
    
        binSearch cfg.SortedOffsets offset 0 (cfg.SortedOffsets.Count - 1)
    
    let sinks = cfg.Sinks |> Array.map resolveBasicBlock
    let loopEntries = cfg.LoopEntries   
    let callReturnPairs =
        let res = Dictionary<_,_>()
        cfg.CallsToAdd
        |> ResizeArray.iter (fun callInfo -> res.Add(callInfo.CallFrom, callInfo.ReturnTo))
        res
    member this.MethodBase = cfg.MethodBase
    member this.IlBytes = cfg.ILBytes
    member this.SortedOffsets = cfg.SortedOffsets
    member this.Sinks = sinks 
    member this.CallReturnPairs = callReturnPairs
    member this.IsLoopEntry offset = loopEntries.Contains offset
    member this.ResolveBasicBlock offset =
        resolveBasicBlock offset

[<Struct>]
type PositionInApplicationGraph =
    val Method: MethodBase
    val Offset: offset
    new (method, offset) = {Method = method; Offset = offset}
    
type private ApplicationGraphMessage =
    | AddGoal of PositionInApplicationGraph
    | RemoveGoal of PositionInApplicationGraph
    | AddState of PositionInApplicationGraph
    | MoveState of positionForm:PositionInApplicationGraph * positionTo: PositionInApplicationGraph
    | AddCFG of Option<AsyncReplyChannel<CfgInfo>> *  MethodBase
    | AddCallEdge of callForm:PositionInApplicationGraph * callTo: PositionInApplicationGraph
    | GetShortestDistancesToGoals
        of AsyncReplyChannel<ResizeArray<PositionInApplicationGraph * PositionInApplicationGraph * int>> * array<PositionInApplicationGraph>
    | GetReachableGoals
        of AsyncReplyChannel<Dictionary<PositionInApplicationGraph,HashSet<PositionInApplicationGraph>>> * array<PositionInApplicationGraph>
    
type ApplicationGraph() as this =        
    let mutable firstFreeVertexId = 0<inputGraphVertex>        
    let terminalForCFGEdge = 0<terminalSymbol>
    let mutable firstFreeCallTerminalId = 1<terminalSymbol>
    let cfgToFirstVertexIdMapping = Dictionary<MethodBase,int<inputGraphVertex>>()
    let callEdgesTerminals = Dictionary<_,Dictionary<_,int<terminalSymbol>>>()        
    let statesToInnerGraphVerticesMap = Dictionary<PositionInApplicationGraph, int<inputGraphVertex>>()
    let innerGraphVerticesToStatesMap = Dictionary<int<inputGraphVertex>, PositionInApplicationGraph>()
    let goalsToInnerGraphVerticesMap = Dictionary<PositionInApplicationGraph, int<inputGraphVertex>>()
    let innerGraphVerticesToGoalsMap = Dictionary<int<inputGraphVertex>, PositionInApplicationGraph>()    
    let cfgs = Dictionary<MethodBase, CfgInfo>()
    let vertices = Dictionary<int<inputGraphVertex>, ResizeArray<InputGraphEdge>>()
    
    let buildQuery () =
        let startBox =
            RSMBox(
                0<rsmState>,
                HashSet [|0<rsmState>|],
                [|
                    yield RSMEdges.TerminalEdge(0<rsmState>, terminalForCFGEdge, 0<rsmState>)
                    yield RSMEdges.NonTerminalEdge(0<rsmState>, 1<rsmState>, 0<rsmState>)
                    for callSymbol in 1<terminalSymbol> .. 2<terminalSymbol> .. firstFreeCallTerminalId - 1<terminalSymbol> do
                      yield RSMEdges.TerminalEdge(0<rsmState>, callSymbol, 0<rsmState>)
                |]
                )
        let balancedBracketsBox =
          let mutable firstFreeRsmState = 3<rsmState>
          RSMBox(
              1<rsmState>,
              HashSet [|1<rsmState>; 2<rsmState>|],
              [|
                  yield RSMEdges.TerminalEdge(1<rsmState>, terminalForCFGEdge, 1<rsmState>)
                  for callSymbol in 1<terminalSymbol> .. 2<terminalSymbol> .. firstFreeCallTerminalId - 1<terminalSymbol> do
                      yield RSMEdges.TerminalEdge(1<rsmState>, callSymbol, firstFreeRsmState)
                      yield RSMEdges.NonTerminalEdge(firstFreeRsmState, 0<rsmState>, firstFreeRsmState + 1<rsmState>)
                      yield RSMEdges.TerminalEdge(firstFreeRsmState + 1<rsmState>, callSymbol + 1<terminalSymbol>, 2<rsmState>)
                      firstFreeRsmState <- firstFreeRsmState + 2<rsmState>
              |])
        RSM([|startBox; balancedBracketsBox|], startBox)    
        
    
    let buildCFG (methodBase:MethodBase) =
        Logger.trace $"Add CFG for %A{methodBase.Name}."
        let cfg = CFG(methodBase)
        for kvp in cfg.Edges do
            let edges =
                [|
                    for targetOffset in kvp.Value do
                        yield InputGraphEdge(
                                             terminalForCFGEdge,
                                             firstFreeVertexId + targetOffset * 1<inputGraphVertex>
                                             )        
                |]
            vertices.Add(firstFreeVertexId + kvp.Key * 1<inputGraphVertex>, ResizeArray edges)       
            
        cfgToFirstVertexIdMapping.Add(methodBase, firstFreeVertexId)
        firstFreeVertexId <- firstFreeVertexId + cfg.ILBytes.Length * 1<inputGraphVertex>
        
        cfg
        
    let getVertex (pos:PositionInApplicationGraph) =
        cfgToFirstVertexIdMapping.[pos.Method] + cfgs.[pos.Method].ResolveBasicBlock pos.Offset * 1<inputGraphVertex>
        
    let addGoal (pos:PositionInApplicationGraph) = 
        Logger.trace $"Add goal: %A{pos.Method.Name}, %A{pos.Offset}"
        let vertexInInnerGraph = getVertex pos
        goalsToInnerGraphVerticesMap.Add (pos, vertexInInnerGraph)
        innerGraphVerticesToGoalsMap.Add (vertexInInnerGraph, pos)
        
    let removeGoal (pos:PositionInApplicationGraph) =
        Logger.trace $"Remove goal: %A{pos.Method.Name}, %A{pos.Offset}"
        let vertexInInnerGraph = getVertex pos
        goalsToInnerGraphVerticesMap.Remove pos |> ignore
        innerGraphVerticesToGoalsMap.Remove vertexInInnerGraph |> ignore
        
    let addCallEdge (callSource:PositionInApplicationGraph) (callTarget:PositionInApplicationGraph) =
        Logger.trace $"Add call edge from %A{callSource.Method}, %i{callSource.Offset} to %A{callTarget.Method}."       
        let callerMethodCfgInfo = cfgs.[callSource.Method]
        let calledMethodCfgInfo = cfgs.[callTarget.Method]
        let callFrom = getVertex callSource
        let callTo = getVertex callTarget
        let returnTo = getVertex (PositionInApplicationGraph(callSource.Method, callerMethodCfgInfo.CallReturnPairs.[callSource.Offset]))
        if not (vertices.ContainsKey callFrom && vertices.[callFrom].Exists (fun edg -> edg.TargetVertex = callTo))
        then
            InputGraphEdge (firstFreeCallTerminalId, callTo)
            |> vertices.[callFrom].Add
            let returnFromPoints = calledMethodCfgInfo.Sinks |>  Array.map(fun sink -> getVertex (PositionInApplicationGraph (callTarget.Method, sink)))  
            for returnFrom in returnFromPoints do
                InputGraphEdge (firstFreeCallTerminalId + 1<terminalSymbol>, returnTo)
                |> vertices.[returnFrom].Add
                
            firstFreeCallTerminalId <- firstFreeCallTerminalId + 2<terminalSymbol>            
        
    let moveState (initialPosition: PositionInApplicationGraph) (finalPosition: PositionInApplicationGraph) =
        Logger.trace $"Move state form %A{initialPosition.Method.Name}, %A{initialPosition.Offset} to %A{finalPosition.Method.Name}, %A{finalPosition.Offset}"
        let initialVertexInInnerGraph = getVertex initialPosition            
        let finalVertexInnerGraph = getVertex finalPosition            
        if initialVertexInInnerGraph <> finalVertexInnerGraph
        then
            statesToInnerGraphVerticesMap.Remove initialPosition |> ignore
            innerGraphVerticesToStatesMap.Remove initialVertexInInnerGraph |> ignore
            if not <| statesToInnerGraphVerticesMap.ContainsKey finalPosition
            then statesToInnerGraphVerticesMap.Add(finalPosition, finalVertexInnerGraph)
            if not <| innerGraphVerticesToStatesMap.ContainsKey finalVertexInnerGraph
            then innerGraphVerticesToStatesMap.Add(finalVertexInnerGraph,finalPosition)
            
    let addState (pos:PositionInApplicationGraph) =
        Logger.trace $"Add state: %A{pos.Method.Name}, %A{pos.Offset}"
        let vertexInInnerGraph = getVertex pos
        if not <| statesToInnerGraphVerticesMap.ContainsKey pos
        then statesToInnerGraphVerticesMap.Add(pos, vertexInInnerGraph)
        if not <| innerGraphVerticesToStatesMap.ContainsKey vertexInInnerGraph
        then innerGraphVerticesToStatesMap.Add(vertexInInnerGraph, pos)
        
    let getReachableGoals (states:array<PositionInApplicationGraph>) =
        Logger.trace $"Get reachable goals for %A{states}."
        let query = buildQuery()
        let statesInInnerGraph =
            states
            |> Array.map (fun state -> cfgToFirstVertexIdMapping.[state.Method] + state.Offset * 1<inputGraphVertex>)
        let res = GLL.eval this statesInInnerGraph query Mode.ReachabilityOnly
        match res with
        | QueryResult.ReachabilityFacts facts ->
            let res = Dictionary<_,_>()
            for fact in facts do
                res.Add(innerGraphVerticesToStatesMap.[fact.Key], HashSet<_>())
                for reachable in fact.Value do
                    let exists, goalPosition = innerGraphVerticesToGoalsMap.TryGetValue reachable
                    if exists then res.[innerGraphVerticesToStatesMap.[fact.Key]].Add goalPosition |> ignore
            res
            
        | _ -> failwith "Impossible!"
        
    let getShortestDistancesToGoal (states:array<PositionInApplicationGraph>) =
        Logger.trace $"Get shortest distances for %A{states}."
        let query = buildQuery()
        let statesInInnerGraph =
            states
            |> Array.map (fun state -> cfgToFirstVertexIdMapping.[state.Method] + state.Offset * 1<inputGraphVertex>)
        let goalsInInnerGraph =
            goalsToInnerGraphVerticesMap
            |> Seq.map (fun kvp -> kvp.Value)
        let res = GLL.eval this statesInInnerGraph query Mode.AllPaths
        match res with
        | QueryResult.ReachabilityFacts _ ->
            failwith "Impossible!"
        | QueryResult.MatchedRanges ranges ->
            ranges.GetShortestDistances(statesInInnerGraph, goalsInInnerGraph)
            |> ResizeArray.map (fun (_from,_to,distance) ->
                innerGraphVerticesToStatesMap.[_from],
                innerGraphVerticesToGoalsMap.[_to],
                distance
                )
        
    let messagesProcessor = MailboxProcessor.Start(fun inbox ->            
        async{            
            while true do
                try
                    let! message = inbox.Receive()
                    match message with
                    | AddCFG (replyChannel, methodBase) ->
                        let reply cfgInfo =
                            match replyChannel with
                            | Some ch -> ch.Reply cfgInfo
                            | None -> ()
                        let exists, cfgInfo = cfgs.TryGetValue methodBase                    
                        if exists
                        then reply cfgInfo
                        else
                            let cfg = buildCFG methodBase
                            let cfgInfo = CfgInfo cfg
                            reply cfgInfo
                            cfgs.Add(methodBase, cfgInfo)
                            Logger.trace $"Vertices in application graph: %i{vertices.Count}"
                            
                    | AddCallEdge (_from, _to) -> addCallEdge _from _to
                    | AddGoal pos -> addGoal pos
                    | RemoveGoal pos -> removeGoal pos
                    | AddState pos -> addState pos
                    | MoveState (_from,_to) -> moveState _from _to
                    | GetShortestDistancesToGoals (replyChannel, states) -> replyChannel.Reply (getShortestDistancesToGoal states)
                    | GetReachableGoals (replyChannel, states) -> replyChannel.Reply (getReachableGoals states)
                with
                | e -> Logger.error $"Something wrong in application graph messages processor: \n %A{e} \n %s{e.Message} \n %s{e.StackTrace}"
        }            
    )

    do
        messagesProcessor.Error.Add(fun e ->
            Logger.error $"Something wrong in application graph messages processor: \n %A{e} \n %s{e.Message} \n %s{e.StackTrace}"
            )

    interface IInputGraph with
        member this.GetOutgoingEdges v =
            vertices.[v]
    member this.GetCfg (methodBase: MethodBase) =        
            messagesProcessor.PostAndReply (fun ch -> AddCFG (Some ch, methodBase))
    
    member this.AddCallEdge (sourceMethod : MethodBase) (sourceOffset : offset) (targetMethod : MethodBase) =
        if not <| cfgs.ContainsKey targetMethod
        then messagesProcessor.Post (AddCFG (None, targetMethod))
        messagesProcessor.Post <| AddCallEdge (PositionInApplicationGraph(sourceMethod, sourceOffset), PositionInApplicationGraph(targetMethod, 0))

    member this.AddState (method : MethodBase) (offset : offset) =            
        messagesProcessor.Post <| AddState (PositionInApplicationGraph(method, offset))
        
    member this.MoveState (fromMethod : MethodBase) (fromOffset : offset) (toMethod : MethodBase) (toOffset : offset) =
        
        //Add query here
        messagesProcessor.Post <| MoveState (PositionInApplicationGraph(fromMethod, fromOffset), PositionInApplicationGraph(toMethod,toOffset))

    member x.AddGoal (method : MethodBase) (offset : offset) =
        messagesProcessor.Post <| AddGoal (PositionInApplicationGraph(method, offset))    

    member x.RemoveGoal (method : MethodBase) (offset : offset) =
        messagesProcessor.Post <| RemoveGoal (PositionInApplicationGraph(method, offset))
    
    member this.GetShortestDistancesToAllGoalsFromStates (states: array<PositionInApplicationGraph>) =
        messagesProcessor.PostAndReply (fun ch -> GetShortestDistancesToGoals(ch, states))
            
    member this.GetGoalsReachableFromStates (states: array<PositionInApplicationGraph>) =            
        messagesProcessor.PostAndReply (fun ch -> GetReachableGoals(ch, states))

module CFG =
    let applicationGraph = ApplicationGraph()