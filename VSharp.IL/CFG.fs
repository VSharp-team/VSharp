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
    let callsToAdd = ResizeArray<_>()
    
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
                    callsToAdd.Add (src, dst, snd callInfo)
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
                
        sortedOffsets |> Seq.iter (fun offset -> addEdges offset offset)        

    let dfs (startVertices : array<offset>) =
        let used = HashSet<offset>()        
        let verticesOffsets = HashSet<offset> startVertices
        let addVertex v = verticesOffsets.Add |> ignore
        let edges = Dictionary<offset, HashSet<offset>>()
        let fallThroughOffset = Array.init ilBytes.Length (fun _ -> None)
        
        let addEdge src dst =
            let exists,outgoingEdges = edges.TryGetValue src
            if exists
            then outgoingEdges.Add dst |> ignore
            else edges.Add(src, HashSet [|dst|])

        let rec dfs' (v : offset) =
            if not <| used.Contains v
            then
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
                | ExceptionMechanism -> ()
                | Return ->
                    addVertex v
                    sinks.Add v
                | UnconditionalBranch target -> dealWithJump v target
                | ConditionalBranch (fallThrough, offsets) ->
                    dealWithJump v fallThrough
                    offsets |> List.iter (dealWithJump v)
        
        startVertices
        |> Array.iter dfs'
        
        CFGTemporaryData(verticesOffsets, fallThroughOffset, edges, offsetsDemandingCall)
        
    do
        let startVertices =
            [|
             yield 0
             yield! exceptionHandlers |> Array.choose (fun handler -> match handler.ehcType with | Filter offset -> Some offset | _ -> None)
            |]
        
        let temporaryData = dfs startVertices
        
        addVerticesAndEdges temporaryData
        
    member this.MethodBase = methodBase
    member this.ILBytes = ilBytes
    member this.ResolveBasicBlock offset =
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
        
            binSearch sortedOffsets offset 0 (sortedOffsets.Count - 1)

(*
type ApplicationGraph() =
    
    let buildCfgGraph (methodBase:MethodBase) =
*)

module public CFG =
    type internal graph = Dictionary<offset, List<offset>>

    [<CustomEquality; CustomComparison>]
    type public cfgData =        
        {
            methodBase : MethodBase
            ilBytes : byte []
            sortedOffsets : List<offset>
            dfsOut : Dictionary<offset, int>
            sccOut : Dictionary<offset, int>               // maximum tOut of SCC-vertices
            graph : graph
            reverseGraph : graph
            clauses : ExceptionHandlingClause []
            offsetsDemandingCall : Dictionary<offset, OpCode * MethodBase>
        }        
        interface IComparable with
            override x.CompareTo (obj : obj) =
                match obj with
                | :? cfgData as other -> x.methodBase.GetHashCode().CompareTo(other.GetHashCode())
                | _ -> -1
        override x.Equals (obj : obj) =
            match obj with
            | :? cfgData as other -> x.methodBase = other.methodBase
            | _ -> false
        override x.GetHashCode() = x.methodBase.GetHashCode()
        
        member this.ResolveBasicBlock offset =
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
        
            binSearch this.sortedOffsets offset 0 (this.sortedOffsets.Count - 1)
        
        member this.GetSinks () =
            [|
                for kvp in this.graph do
                    if kvp.Value.Count = 0
                    then yield kvp.Key
            |]


    type private interimData = {
        opCodes : OpCode [] //  for debug
        verticesOffsets : int HashSet
        fallThroughOffset : offset option []
        edges : Dictionary<offset, List<offset>>
        offsetsDemandingCall : Dictionary<offset, OpCode * MethodBase>
    }
    with
        member x.AddEdge src dst =
            if not <| x.edges.ContainsKey src then
                x.edges.Add (src, List<_>())
                x.edges.[src].Add dst
            elif x.edges.[src].Contains dst |> not then x.edges.[src].Add dst
    
    [<Struct>]
    type PositionInApplicationGraph =
        val Cfg: cfgData
        val Offset: offset
        new (cfg, offset) = {Cfg = cfg; Offset = offset}
    type ApplicationGraphMessage =
        | AddGoal of PositionInApplicationGraph
        | RemoveGoal of PositionInApplicationGraph
        | AddState of PositionInApplicationGraph
        | MoveState of positionForm:PositionInApplicationGraph * positionTo: PositionInApplicationGraph
        | AddCFG of cfgData
        | AddCallEdge of callForm:PositionInApplicationGraph * callTo: PositionInApplicationGraph
        | GetShortestDistancesToGoals
            of AsyncReplyChannel<ResizeArray<PositionInApplicationGraph * PositionInApplicationGraph * int>> * array<PositionInApplicationGraph>
        | GetReachableGoals
            of AsyncReplyChannel<Dictionary<PositionInApplicationGraph,HashSet<PositionInApplicationGraph>>> * array<PositionInApplicationGraph>
        
    type ApplicationGraph() =        
        let mutable firstFreeVertexId = 0<graphVertex>        
        let terminalForCFGEdge = 0<terminalSymbol>
        let mutable firstFreeCallTerminalId = 1<terminalSymbol>
        let cfgToFirstVertexIdMapping = Dictionary<cfgData,int<graphVertex>>()
        let callEdgesTerminals = Dictionary<_,Dictionary<_,int<terminalSymbol>>>()        
        let statesToInnerGraphVerticesMap = Dictionary<PositionInApplicationGraph, int<graphVertex>>()
        let innerGraphVerticesToStatesMap = Dictionary<int<graphVertex>, PositionInApplicationGraph>()
        let goalsToInnerGraphVerticesMap = Dictionary<PositionInApplicationGraph, int<graphVertex>>()
        let innerGraphVerticesToGoalsMap = Dictionary<int<graphVertex>, PositionInApplicationGraph>()
        
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
        let getNewInnerGraphEdgesForCfgData (cfg:cfgData) =
            let edges = 
                [|
                   for kvp in cfg.graph do
                       for targetOffset in kvp.Value do
                           yield InputGraph.TerminalEdge(
                                               firstFreeVertexId + kvp.Key * 1<graphVertex>,
                                               terminalForCFGEdge,
                                               firstFreeVertexId + targetOffset * 1<graphVertex>)
                |]
            cfgToFirstVertexIdMapping.Add(cfg, firstFreeVertexId)
            firstFreeVertexId <- firstFreeVertexId + cfg.ilBytes.Length * 1<graphVertex>
            edges
     
        let innerGraphForCFPQ = InputGraph.InputGraph()
        
        let addCFG cfg =
            Logger.trace "Add CFG"
            innerGraphForCFPQ.AddEdges (getNewInnerGraphEdgesForCfgData cfg)
            
        let addGoal (pos:PositionInApplicationGraph) = 
            Logger.trace "Add goal"
            let vertexInInnerGraph = cfgToFirstVertexIdMapping.[pos.Cfg] + pos.Cfg.ResolveBasicBlock pos.Offset * 1<graphVertex>
            goalsToInnerGraphVerticesMap.Add (pos, vertexInInnerGraph)
            innerGraphVerticesToGoalsMap.Add(vertexInInnerGraph, pos)
            
        let removeGoal (pos:PositionInApplicationGraph) =
            Logger.trace "Remove goal"
            let vertexInInnerGraph = cfgToFirstVertexIdMapping.[pos.Cfg] + pos.Cfg.ResolveBasicBlock pos.Offset * 1<graphVertex>
            goalsToInnerGraphVerticesMap.Remove pos |> ignore
            innerGraphVerticesToGoalsMap.Remove vertexInInnerGraph |> ignore
            
        let addCallEdge (callSource:PositionInApplicationGraph) (callTarget:PositionInApplicationGraph) =
            Logger.trace "Add call edge"
            let sourceBasicBlock = cfgToFirstVertexIdMapping.[callSource.Cfg] + callSource.Cfg.ResolveBasicBlock callSource.Offset * 1<graphVertex>
            let callFrom = cfgToFirstVertexIdMapping.[callSource.Cfg] + callSource.Offset * 1<graphVertex>
            if not (callEdgesTerminals.ContainsKey callFrom && callEdgesTerminals.[callFrom].ContainsKey callTarget.Cfg)
            then
                let targetBlockSinks =
                   callSource.Cfg.GetSinks()
                   |>  Array.map (( * ) 1<graphVertex> >> (+) cfgToFirstVertexIdMapping.[callSource.Cfg])
                let callTo = cfgToFirstVertexIdMapping.[callSource.Cfg]
                let returnTo = callFrom + 1<graphVertex>
                
                let edgesToAdd =
                        [|
                            yield InputGraph.TerminalEdge (callFrom, firstFreeCallTerminalId, callTo)
                            for returnFrom in targetBlockSinks do
                                yield InputGraph.TerminalEdge (returnFrom, firstFreeCallTerminalId + 1<terminalSymbol>, returnTo)
                            if not (callEdgesTerminals.ContainsKey callFrom)
                            then
                                // TODO: Fix this case. Sequential calls from single basic block handled incorrect for now.
                                callEdgesTerminals.Add(callFrom, Dictionary<_,_>())
                                let outgoingFromCall =
                                    innerGraphForCFPQ.OutgoingTerminalEdges sourceBasicBlock
                                    |> ResizeArray.map unpackInputGraphTerminalEdge
                                innerGraphForCFPQ.RemoveOutgoingEdges sourceBasicBlock                                                        
                                if sourceBasicBlock <> callFrom
                                then yield InputGraph.TerminalEdge (sourceBasicBlock, terminalForCFGEdge, callFrom)
                                for edge in outgoingFromCall do
                                    yield InputGraph.TerminalEdge (returnTo, edge.TerminalSymbol, edge.Vertex)
                        |]
 
                callEdgesTerminals.[callFrom].Add(callTarget.Cfg, firstFreeCallTerminalId)
                
                firstFreeCallTerminalId <- firstFreeCallTerminalId + 2<terminalSymbol>
                innerGraphForCFPQ.AddEdges edgesToAdd
            
        let moveState (initialPosition: PositionInApplicationGraph) (finalPosition: PositionInApplicationGraph) =
            Logger.trace "Move state"
            let initialVertexInInnerGraph = cfgToFirstVertexIdMapping.[initialPosition.Cfg] + initialPosition.Cfg.ResolveBasicBlock initialPosition.Offset * 1<graphVertex>            
            let finalVertexInnerGraph = cfgToFirstVertexIdMapping.[finalPosition.Cfg] + finalPosition.Cfg.ResolveBasicBlock finalPosition.Offset * 1<graphVertex>            
            if initialVertexInInnerGraph <> finalVertexInnerGraph
            then
                statesToInnerGraphVerticesMap.Remove initialPosition |> ignore
                innerGraphVerticesToStatesMap.Remove initialVertexInInnerGraph |> ignore
                if not <| statesToInnerGraphVerticesMap.ContainsKey finalPosition
                then statesToInnerGraphVerticesMap.Add(finalPosition, finalVertexInnerGraph)
                if not <| innerGraphVerticesToStatesMap.ContainsKey finalVertexInnerGraph
                then innerGraphVerticesToStatesMap.Add(finalVertexInnerGraph,finalPosition)
                
        let addState (pos:PositionInApplicationGraph) =
            Logger.trace "Add state"
            let vertexInInnerGraph = cfgToFirstVertexIdMapping.[pos.Cfg] + pos.Cfg.ResolveBasicBlock pos.Offset * 1<graphVertex>
            if not <| statesToInnerGraphVerticesMap.ContainsKey pos
            then statesToInnerGraphVerticesMap.Add(pos, vertexInInnerGraph)
            if not <| innerGraphVerticesToStatesMap.ContainsKey vertexInInnerGraph
            then innerGraphVerticesToStatesMap.Add(vertexInInnerGraph, pos)
            
        let getReachableGoals (states:array<PositionInApplicationGraph>) =
            let query = buildQuery()
            let statesInInnerGraph =
                states
                |> Array.map (fun state -> cfgToFirstVertexIdMapping.[state.Cfg] + state.Offset * 1<graphVertex>)
            let res = GLL.eval innerGraphForCFPQ statesInInnerGraph query Mode.ReachabilityOnly
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
            let query = buildQuery()
            let statesInInnerGraph =
                states
                |> Array.map (fun state -> cfgToFirstVertexIdMapping.[state.Cfg] + state.Offset * 1<graphVertex>)
            let goalsInInnerGraph =
                goalsToInnerGraphVerticesMap
                |> Seq.map (fun kvp -> kvp.Value)
            let res = GLL.eval innerGraphForCFPQ statesInInnerGraph query Mode.AllPaths
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
                        let! message = inbox.Receive()
                        match message with
                        | AddCFG cfg -> addCFG cfg
                        | AddCallEdge (_from, _to) -> addCallEdge _from _to
                        | AddGoal pos -> addGoal pos
                        | RemoveGoal pos -> removeGoal pos
                        | AddState pos -> addState pos
                        | MoveState (_from,_to) -> moveState _from _to
                        | GetShortestDistancesToGoals (replyChannel, states) -> replyChannel.Reply (getShortestDistancesToGoal states)
                        | GetReachableGoals (replyChannel, states) -> replyChannel.Reply (getReachableGoals states)                    
                }            
            )

        do
            messagesProcessor.Error.Add(fun e -> Logger.error $"Something wrong in application graph messages processor: \n %A{e} \n %s{e.Message} \n %s{e.StackTrace}")
            
        member x.AddCfg cfg =
            messagesProcessor.Post (AddCFG cfg)

        member x.AddCallEdge (sourceCfg : cfgData) (sourceOffset : offset) (targetCfg : cfgData) =
            messagesProcessor.Post <| AddCallEdge (PositionInApplicationGraph(sourceCfg, sourceOffset), PositionInApplicationGraph(targetCfg,0))

        member this.AddState (cfg : cfgData) (offset : offset) =            
            messagesProcessor.Post <| AddState (PositionInApplicationGraph(cfg, offset))
            
        member this.MoveState (fromCfg : cfgData) (fromOffset : offset) (toCfg : cfgData) (toOffset : offset) =
            messagesProcessor.Post <| MoveState (PositionInApplicationGraph(fromCfg, fromOffset), PositionInApplicationGraph(toCfg,toOffset))

        member x.AddGoal (cfg : cfgData) (offset : offset) =
            messagesProcessor.Post <| AddGoal (PositionInApplicationGraph(cfg, offset))    

        member x.RemoveGoal (cfg : cfgData) (offset : offset) =
            messagesProcessor.Post <| RemoveGoal (PositionInApplicationGraph(cfg, offset))
        
        member this.GetShortestDistancesToAllGoalsFromStates (states: array<PositionInApplicationGraph>) =
            messagesProcessor.PostAndReply (fun ch -> GetShortestDistancesToGoals(ch, states))
                
        member this.GetGoalsReachableFromStates (states: array<PositionInApplicationGraph>) =            
            messagesProcessor.PostAndReply (fun ch -> GetReachableGoals(ch, states))
            

    let private createData (methodBase : MethodBase) (ilBytes : byte []) ehsBytes =
        let size = ilBytes.Length
        let interim = {
            fallThroughOffset = Array.init size (fun _ -> None)
            verticesOffsets = HashSet<_>()
            edges = Dictionary<_, _>()
            opCodes = Array.init size (fun _ -> OpCodes.Prefix1)
            offsetsDemandingCall = Dictionary<_,_>()
        }
        let cfg = {
            methodBase = methodBase
            ilBytes = ilBytes
            sortedOffsets = List<_>()
            dfsOut = Dictionary<_,_>()
            sccOut = Dictionary<_,_>()
            graph = Dictionary<_,_>()
            reverseGraph = Dictionary<_,_>()
            clauses = ehsBytes
            offsetsDemandingCall = Dictionary<_,_>()
        }
        interim, cfg

    let createVertex (cfgData : cfgData) offset =
        cfgData.sortedOffsets.Add offset
        cfgData.graph.Add <| (offset, List<_>())
        cfgData.reverseGraph.Add <| (offset, List<_>())

    let private addVerticesAndEdges (cfgData : cfgData) (interimData : interimData) =
        interimData.verticesOffsets
        |> Seq.sort
        |> Seq.iter (createVertex cfgData)

        let addEdge src dst =
            cfgData.graph.[src].Add dst
            cfgData.reverseGraph.[dst].Add src

        let used = HashSet<offset>()
        let rec addEdges currentVertex src =
            if used.Contains src then ()
            else
                let wasAdded = used.Add src
                assert wasAdded
                match interimData.fallThroughOffset.[src] with
                | Some dst when interimData.offsetsDemandingCall.ContainsKey dst ->
                    addEdge currentVertex dst
                    addEdges dst dst
                | Some dst when cfgData.sortedOffsets.Contains dst ->
                    addEdge currentVertex dst
                    addEdges dst dst
                | Some dst -> addEdges currentVertex dst
                | None when interimData.edges.ContainsKey src ->
                    interimData.edges.[src] |> Seq.iter (fun dst ->
                        if cfgData.sortedOffsets.Contains dst then
                            addEdge currentVertex dst
                            addEdges dst dst
                        else
                            addEdges currentVertex dst
                    )
                | None -> ()
        cfgData.sortedOffsets |> Seq.iter (fun offset -> addEdges offset offset)
        { cfgData with offsetsDemandingCall = interimData.offsetsDemandingCall }

    let private markVertex (set : HashSet<offset>) vOffset =
        set.Add vOffset |> ignore

    let private dfs (methodBase : MethodBase) (data : interimData) (used : HashSet<int>) (ilBytes : byte []) (v : offset) =
        let rec dfs' (v : offset) =
            if used.Contains v then ()
            else
                let wasAdded = used.Add(v)
                assert wasAdded
                let opCode = Instruction.parseInstruction methodBase v
                data.opCodes.[v] <- opCode

                let dealWithJump src dst =
                    markVertex data.verticesOffsets src
                    markVertex data.verticesOffsets dst
                    data.AddEdge src dst
                    dfs' dst

                let ipTransition = Instruction.findNextInstructionOffsetAndEdges opCode ilBytes v
                match ipTransition with
                | FallThrough offset when Instruction.isDemandingCallOpCode opCode ->
                    let calledMethod = TokenResolver.resolveMethodFromMetadata methodBase ilBytes (v + opCode.Size)
                    data.offsetsDemandingCall.Add(v, (opCode, calledMethod))
                    markVertex data.verticesOffsets v
                    markVertex data.verticesOffsets offset
                    data.fallThroughOffset.[v] <- Some offset
                    dfs' offset
                | FallThrough offset ->
                    data.fallThroughOffset.[v] <- Some offset
                    dfs' offset
                | ExceptionMechanism -> ()
                | Return -> markVertex data.verticesOffsets v
                | UnconditionalBranch target -> dealWithJump v target
                | ConditionalBranch (fallThrough, offsets) -> fallThrough :: offsets |> List.iter (dealWithJump v)
        dfs' v

    let private dfsComponent methodBase (data : interimData) used (ilBytes : byte []) startOffset =
        markVertex data.verticesOffsets startOffset
        dfs methodBase data used ilBytes startOffset

    let private dfsExceptionHandlingClause methodBase (data : interimData) used (ilBytes : byte []) (ehc : ExceptionHandlingClause) =
        match ehc.ehcType with
        | Filter offset -> dfsComponent methodBase data used ilBytes offset
        | _ -> ()
        dfsComponent methodBase data used ilBytes ehc.handlerOffset // some catch handlers may be nested

    let orderEdges (used : HashSet<offset>) (cfg : cfgData) : unit =
        let rec bypass acc (u : offset) =
            used.Add u |> ignore
            let vertices, tOut = cfg.graph.[u] |> Seq.fold (fun acc v -> if used.Contains v then acc else bypass acc v) acc
            cfg.dfsOut.[u] <- tOut
            u::vertices, tOut + 1
        let propagateMaxTOutForSCC (usedForSCC : HashSet<offset>) max v =
            let rec helper v =
                usedForSCC.Add v |> ignore
                cfg.sccOut.[v] <- max
                cfg.reverseGraph.[v] |> Seq.iter (fun u -> if not <| usedForSCC.Contains u then helper u)
            helper v
        let vertices, _ = Seq.fold (fun acc u -> if used.Contains u then acc else bypass acc u)  ([], 1) cfg.sortedOffsets
        let usedForSCC = HashSet<offset>()
        vertices |> List.iter (fun v -> if not <| usedForSCC.Contains v then propagateMaxTOutForSCC usedForSCC cfg.dfsOut.[v] v)

    let floydAlgo (cfg : cfgData) infty =
//        let infty = 10000
        let dist = Dictionary<offset * offset, int>()
        let offsets = cfg.sortedOffsets
        for i in offsets do
            for j in offsets do
                dist.Add((i, j), infty)

        for i in offsets do
            dist.[(i, i)] <- 0
            for j in cfg.graph.[i] do
                dist.[(i, j)] <- 1

        for k in offsets do
            for i in offsets do
                for j in offsets do
                    if dist.[i, j] > dist.[i, k] + dist.[k, j] then
                        dist.[(i,j)] <- dist.[i, k] + dist.[k, j]
        dist

    let cfgs = Dictionary<MethodBase, cfgData>()
    let appGraph = ApplicationGraph()
    let floyds = Dictionary<cfgData, Dictionary<offset * offset, int>>()

    let private build (methodBase : MethodBase) =
        let ilBytes = Instruction.getILBytes methodBase
        let ehs = Instruction.getEHSBytes methodBase
        let interimData, cfgData = createData methodBase ilBytes ehs
        let used = HashSet<offset>()
        dfsComponent methodBase interimData used ilBytes 0
        Seq.iter (dfsExceptionHandlingClause methodBase interimData used ilBytes) ehs
        let cfg = addVerticesAndEdges cfgData interimData
        orderEdges (HashSet<offset>()) cfg
        appGraph.AddCfg cfg
        cfg

    let findCfg m = Dict.getValueOrUpdate cfgs m (fun () -> build m)

    let findDistance cfg = Dict.getValueOrUpdate floyds cfg (fun () -> floydAlgo cfg 100000)

    let private fromCurrentAssembly assembly (current : MethodBase) = current.Module.Assembly = assembly
