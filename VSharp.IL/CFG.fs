namespace VSharp

open System.Reflection
open System.Collections.Generic

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
    val IsExternalStaticCall: bool
    new (callFrom, returnTo) =
        {
            CallFrom = callFrom
            ReturnTo = returnTo
            IsExternalStaticCall = false
        }
    new (callFrom, returnTo, isExternalStatic) =
        {
            CallFrom = callFrom
            ReturnTo = returnTo
            IsExternalStaticCall = isExternalStatic
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
                        splitBasicBlock block dst
                        splitEdge block.StartVertex block.FinalVertex dst                
            
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
                
                let processCall callFrom returnTo isStaticCall =
                    calls.Add(CallInfo(callFrom,returnTo,isStaticCall))
                    currentBasicBlock.FinalVertex <- currentVertex
                    addEdge currentBasicBlock.StartVertex currentVertex
                    if isStaticCall
                    then addEdge callFrom returnTo
                    dfs' (BasicBlock returnTo) returnTo
                
                let ipTransition = Instruction.findNextInstructionOffsetAndEdges opCode ilBytes currentVertex

                match ipTransition with
                | FallThrough offset when Instruction.isDemandingCallOpCode opCode ->
                    let calledMethod = TokenResolver.resolveMethodFromMetadata methodBase ilBytes (currentVertex + opCode.Size)
                    if not <| Reflection.isExternalMethod calledMethod
                    then processCall currentVertex offset false
                    elif calledMethod.IsStatic 
                    then processCall currentVertex offset true
                    else
                        currentBasicBlock.AddVertex offset
                        dfs' currentBasicBlock offset
                | FallThrough offset ->                    
                    currentBasicBlock.AddVertex offset
                    dfs' currentBasicBlock offset
                | ExceptionMechanism ->
                    //TODO gsv fix it.
                    Logger.trace $"Exception mechanism: %A{currentVertex}"
                    currentBasicBlock.FinalVertex <- currentVertex
                    addEdge currentBasicBlock.StartVertex currentVertex
                    calls.Add(CallInfo(currentVertex,currentVertex+1))
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
        
    member this.MethodBase = methodBase
    member this.ILBytes = ilBytes
    member this.SortedOffsets = sortedOffsets
    member this.Edges = edges
    member this.Calls = calls
    member this.Sinks = sinks.ToArray()
    member this.LoopEntries = loopEntries

type CfgInfo (cfg:CfgTemporaryData) =
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
    member this.ResolveBasicBlock offset =
        resolveBasicBlock offset

type private ApplicationGraphMessage =
    | AddGoal of codeLocation
    | RemoveGoal of codeLocation
    | AddState of codeLocation
    | MoveState of positionForm:codeLocation * positionTo: codeLocation
    | AddCFG of Option<AsyncReplyChannel<CfgInfo>> *  MethodBase
    | AddCallEdge of callForm:codeLocation * callTo: codeLocation
    | GetShortestDistancesToGoals
        of AsyncReplyChannel<ResizeArray<codeLocation * codeLocation * int>> * array<codeLocation>
    | GetReachableGoals
        of AsyncReplyChannel<Dictionary<codeLocation,HashSet<codeLocation>>> * array<codeLocation>
    
type ApplicationGraph() as this =        
    let mutable firstFreeVertexId = 0<inputGraphVertex>        
    let terminalForCFGEdge = 0<terminalSymbol>
    let mutable firstFreeCallTerminalId = 1<terminalSymbol>
    let cfgToFirstVertexIdMapping = Dictionary<MethodBase,int<inputGraphVertex>>()
    let callEdgesTerminals = Dictionary<_,Dictionary<_,int<terminalSymbol>>>()        
    let statesToInnerGraphVerticesMap = Dictionary<codeLocation, int<inputGraphVertex>>()
    let innerGraphVerticesToStatesMap = Dictionary<int<inputGraphVertex>, codeLocation>()
    let goalsToInnerGraphVerticesMap = Dictionary<codeLocation, int<inputGraphVertex>>()
    let innerGraphVerticesToGoalsMap = Dictionary<int<inputGraphVertex>, codeLocation>()    
    let cfgs = Dictionary<MethodBase, CfgInfo>()
    let vertices = Dictionary<int<inputGraphVertex>, ResizeArray<InputGraphEdge>>()
    
    let toDot filePath =        
        let content =
            seq{
               yield "digraph G"
               yield "{"
               yield "node [shape = plaintext]"
               for kvp in vertices do
                for e in kvp.Value do
                    yield $"%i{kvp.Key} -> %i{e.TargetVertex} [label=%A{e.TerminalSymbol}]"
               yield "}"
            }
        System.IO.File.WriteAllLines(filePath, content)
    
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
        let cfg = CfgTemporaryData(methodBase)
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
        
    let getVertex (pos:codeLocation) =
        cfgToFirstVertexIdMapping.[pos.method] + cfgs.[pos.method].ResolveBasicBlock pos.offset * 1<inputGraphVertex>
        
    let addGoal (pos:codeLocation) = 
        Logger.trace $"Add goal: %A{pos.method.Name}, %A{pos.offset}"
        let vertexInInnerGraph = getVertex pos
        goalsToInnerGraphVerticesMap.Add (pos, vertexInInnerGraph)
        innerGraphVerticesToGoalsMap.Add (vertexInInnerGraph, pos)
        
    let removeGoal (pos:codeLocation) =
        Logger.trace $"Remove goal: %A{pos.method.Name}, %A{pos.offset}"
        let vertexInInnerGraph = getVertex pos
        goalsToInnerGraphVerticesMap.Remove pos |> ignore
        innerGraphVerticesToGoalsMap.Remove vertexInInnerGraph |> ignore
        
    let addCallEdge (callSource:codeLocation) (callTarget:codeLocation) =
        Logger.trace $"Add call edge from %A{callSource.method}, %i{callSource.offset} to %A{callTarget.method}."       
        let callerMethodCfgInfo = cfgs.[callSource.method]
        let calledMethodCfgInfo = cfgs.[callTarget.method]
        let callFrom = getVertex callSource
        let callTo = getVertex callTarget
        let returnTo =
            if Reflection.isStaticConstructor callTarget.method
            then callFrom
            else
                let exists, location = callerMethodCfgInfo.Calls.TryGetValue callSource.offset
                if exists
                then getVertex {callSource with offset = location.ReturnTo}
                else getVertex {callSource with offset = callerMethodCfgInfo.Sinks.[0]}            

        if not (vertices.ContainsKey callFrom && vertices.[callFrom].Exists (fun edg -> edg.TargetVertex = callTo))
        then
            if not <| vertices.ContainsKey callFrom
            then vertices.Add(callFrom, ResizeArray())
            InputGraphEdge (firstFreeCallTerminalId, callTo)
            |> vertices.[callFrom].Add
            let returnFromPoints = calledMethodCfgInfo.Sinks |>  Array.map(fun sink -> getVertex {callTarget with offset = sink})  
            for returnFrom in returnFromPoints do
                if not <| vertices.ContainsKey returnFrom
                then vertices.Add(returnFrom, ResizeArray())
                InputGraphEdge (firstFreeCallTerminalId + 1<terminalSymbol>, returnTo)
                |> vertices.[returnFrom].Add
                
            firstFreeCallTerminalId <- firstFreeCallTerminalId + 2<terminalSymbol>            
        
    let moveState (initialPosition: codeLocation) (finalPosition: codeLocation) =
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
            
    let addState (pos:codeLocation) =
        Logger.trace $"Add state: %A{pos.method.Name}, %A{pos.offset}"
        let vertexInInnerGraph = getVertex pos
        if not <| statesToInnerGraphVerticesMap.ContainsKey pos
        then statesToInnerGraphVerticesMap.Add(pos, vertexInInnerGraph)
        if not <| innerGraphVerticesToStatesMap.ContainsKey vertexInInnerGraph
        then innerGraphVerticesToStatesMap.Add(vertexInInnerGraph, pos)
        
    let getReachableGoals (states:array<codeLocation>) =
        Logger.trace $"Get reachable goals for %A{states}."
        let query = buildQuery()
        let statesInInnerGraph =
            states
            |> Array.map (fun state -> cfgToFirstVertexIdMapping.[state.method] + state.offset * 1<inputGraphVertex>)
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
        
    let getShortestDistancesToGoal (states:array<codeLocation>) =
        Logger.trace $"Get shortest distances for %A{states}."
        let query = buildQuery()
        let statesInInnerGraph =
            states
            |> Array.map (fun state -> cfgToFirstVertexIdMapping.[state.method] + state.offset * 1<inputGraphVertex>)
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
                        //toDot "cfg.dot"
                    | AddGoal pos -> addGoal pos
                    | RemoveGoal pos -> removeGoal pos
                    | AddState pos -> addState pos
                    | MoveState (_from,_to) ->
                        tryGetCfgInfo _to.method |> ignore                            
                        moveState _from _to
                    | GetShortestDistancesToGoals (replyChannel, states) -> replyChannel.Reply (getShortestDistancesToGoal states)
                    | GetReachableGoals (replyChannel, states) -> replyChannel.Reply (getReachableGoals states)
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

    interface IInputGraph with
        member this.GetOutgoingEdges v =
            vertices.[v]

    member this.GetCfg (methodBase: MethodBase) =        
            messagesProcessor.PostAndReply (fun ch -> AddCFG (Some ch, methodBase))
    
    member this.AddCallEdge (sourceLocation : codeLocation) (targetLocation : codeLocation) =        
        messagesProcessor.Post <| AddCallEdge (sourceLocation, targetLocation)

    member this.AddState (location:codeLocation) =            
        messagesProcessor.Post <| AddState location
        
    member this.MoveState (fromLocation : codeLocation) (toLocation : codeLocation) =
        //Add query here        
        messagesProcessor.Post <| MoveState (fromLocation, toLocation)

    member x.AddGoal (location:codeLocation) =
        messagesProcessor.Post <| AddGoal location    

    member x.RemoveGoal (location:codeLocation) =
        messagesProcessor.Post <| RemoveGoal location
    
    member this.GetShortestDistancesToAllGoalsFromStates (states: array<codeLocation>) =
        messagesProcessor.PostAndReply (fun ch -> GetShortestDistancesToGoals(ch, states))
            
    member this.GetGoalsReachableFromStates (states: array<codeLocation>) =            
        messagesProcessor.PostAndReply (fun ch -> GetReachableGoals(ch, states))

module CFG =
    let applicationGraph = ApplicationGraph()