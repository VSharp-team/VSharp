namespace VSharp

open System.Collections.Concurrent
open VSharp.GraphUtils
open global.System
open System.Reflection
open System.Collections.Generic
open FSharpx.Collections
open Microsoft.FSharp.Collections
open VSharp

type coverageType =
    | ByTest
    | ByEntryPointTest

type [<Measure>] terminalSymbol

module CallGraph =
    let callGraphDistanceFrom = Dictionary<Assembly, GraphUtils.distanceCache<ICallGraphNode>>()
    let callGraphDistanceTo = Dictionary<Assembly, GraphUtils.distanceCache<IReversedCallGraphNode>>()
    let dummyTerminalForCallShortcut = 3<terminalSymbol>


type ICfgNode =
        abstract OutgoingEdges : seq<ICfgNode> with get
        abstract Offset : offset 

type IInterproceduralCfgNode =
        abstract OutgoingEdges : seq<IInterproceduralCfgNode> with get
        abstract IsCovered : bool with get
        abstract IsVisited : bool with get
        abstract IsTouched : bool with get
        abstract IsGoal : bool with get
        abstract IsSink : bool with get


[<RequireQualifiedAccess>]
type EdgeType =
    | CFG
    | ShortcutForCall
    | Call of int<terminalSymbol>
    | Return of int<terminalSymbol>
    
[<Struct>]
type EdgeLabel =
    val EdgeType: EdgeType
    val IsCovered: bool
    val IsVisited: bool 
    
[<Struct>]
type internal temporaryCallInfo = {callee: MethodWithBody; callFrom: offset; returnTo: offset}

type BasicBlock (method: MethodWithBody, startOffset: offset) =    
    //inherit InputGraphVertexBase ()    
    let mutable finalOffset = Some startOffset
    let mutable startOffset = startOffset
    let mutable isGoal = false
    let mutable isCovered = false
    let mutable isVisited = false
    let mutable isSink = false
    let mutable visitedInstructions = 0u
    let associatedStates = HashSet<IGraphTrackableState>()
    let incomingCFGEdges = HashSet<BasicBlock>()
    let incomingCallEdges = HashSet<BasicBlock>()
    let outgoingEdges = Dictionary<int<terminalSymbol>,HashSet<BasicBlock>>()
    member this.StartOffset 
        with get () = startOffset
        and set v = startOffset <- v
    member this.Method = method
    member this.OutgoingEdges = outgoingEdges
    member this.IncomingCFGEdges = incomingCFGEdges
    member this.IncomingCallEdges = incomingCallEdges
    member this.AssociatedStates = associatedStates
    member this.VisitedInstructions
        with get () = visitedInstructions
        and set v = visitedInstructions <- v
    
    member this.IsCovered
        with get () = isCovered
        and set v = isCovered <- v  
    member this.IsVisited
        with get () = isVisited
        and set v = isVisited <- v
    member this.IsTouched
        with get () = visitedInstructions > 0u
        
    member this.IsGoal
        with get () = isGoal
        and set v = isGoal <- v
        
    member this.IsSink
        with get () = isSink
        and set v = isSink <- v
    member this.HasSiblings
        with get () =
            let siblings = HashSet<BasicBlock>()
            for bb in incomingCFGEdges do
                for kvp in bb.OutgoingEdges do
                        siblings.UnionWith kvp.Value
            siblings.Count > 1

    member this.FinalOffset
        with get () =
                match finalOffset with
                | Some v -> v
                | None -> failwith "Final vertex of this basic block is not specified yet."
        and set (v: offset) = finalOffset <- Some v
        
    member this.Instructions =
        method.ParsedInstructions
        |> Array.filter (fun instr -> Offset.from (int instr.offset) >= this.StartOffset && Offset.from (int instr.offset) <= this.FinalOffset)
        
    member this.ToString () =
        this.Instructions
        |> Array.map (fun instr -> ILRewriter.PrintILInstr None None (method :> Core.IMethod).MethodBase instr)
    
    interface ICfgNode with
        member this.OutgoingEdges
            with get () =
                let exists1,cfgEdges = outgoingEdges.TryGetValue CfgInfo.TerminalForCFGEdge
                let exists2,cfgSpecialEdges = outgoingEdges.TryGetValue CallGraph.dummyTerminalForCallShortcut
                seq{
                    if exists1
                    then yield! cfgEdges |> Seq.cast<ICfgNode>
                    if exists2
                    then yield! cfgSpecialEdges |> Seq.cast<ICfgNode>
                }
        member this.Offset = startOffset
        
    interface IInterproceduralCfgNode with
        member this.OutgoingEdges
            with get () =
                seq{
                    for kvp in outgoingEdges do
                        if kvp.Key <> CallGraph.dummyTerminalForCallShortcut
                        then yield! kvp.Value |> Seq.cast<IInterproceduralCfgNode>
                }
        member this.IsCovered with get() = this.IsCovered
        member this.IsVisited with get() = this.IsVisited
        member this.IsTouched with get() = this.IsTouched
        member this.IsGoal with get() = this.IsGoal
        member this.IsSink with get() = this.IsSink
        
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
        let basicBlocks = HashSet<BasicBlock> ()
        let addBasicBlock v = basicBlocks.Add v |> ignore
        let greyVertices = HashSet<offset>()
        let vertexToBasicBlock: array<Option<BasicBlock>> = Array.init ilBytes.Length (fun _ -> None)
    
        let findFinalVertex intermediatePoint block =
            let mutable index = 0
            let mutable currentIndex = int intermediatePoint - 1
            let mutable found = false
            while not found do
                if vertexToBasicBlock.[currentIndex].IsSome
                   && vertexToBasicBlock.[currentIndex].Value = block
                then
                    found <- true
                    index <- currentIndex
                else currentIndex <- currentIndex - 1
            index * 1<offsets>
        
        let splitBasicBlock (block:BasicBlock) intermediatePoint =
            
            let newBlock = BasicBlock (method, block.StartOffset)
            addBasicBlock newBlock
            block.StartOffset <- intermediatePoint
            
            newBlock.FinalOffset <- findFinalVertex intermediatePoint block
            for v in int newBlock.StartOffset .. int intermediatePoint - 1  do
                vertexToBasicBlock.[v] <- Some newBlock
            
            for parent in block.IncomingCFGEdges do
                let removed =
                    parent.OutgoingEdges |> Seq.map (fun kvp -> kvp.Key, kvp.Value.Remove block)
                    |> Seq.filter snd
                    |> Array.ofSeq
                assert (removed.Length = 1)
                let added = parent.OutgoingEdges.[fst removed.[0]].Add newBlock                
                assert added
                let added = newBlock.IncomingCFGEdges.Add parent
                assert added
            block.IncomingCFGEdges.Clear()
            let added = block.IncomingCFGEdges.Add newBlock
            assert added
            newBlock.OutgoingEdges.Add(CfgInfo.TerminalForCFGEdge, HashSet[|block|])
            block

        let makeNewBasicBlock startVertex =
            match vertexToBasicBlock.[int startVertex] with
            | None ->
                let newBasicBlock = BasicBlock (method, startVertex)
                vertexToBasicBlock.[int startVertex] <- Some newBasicBlock
                addBasicBlock newBasicBlock                
                newBasicBlock
            | Some block ->
                if block.StartOffset = startVertex
                then block
                else splitBasicBlock block startVertex
            

        let addEdge (src:BasicBlock) (dst:BasicBlock) =
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
                let existingBasicBlock = vertexToBasicBlock.[int currentVertex]
                if currentBasicBlock <> existingBasicBlock.Value
                then
                    currentBasicBlock.FinalOffset <- findFinalVertex currentVertex currentBasicBlock
                    addEdge currentBasicBlock existingBasicBlock.Value                                
                if greyVertices.Contains currentVertex
                then loopEntries.Add currentVertex |> ignore
            else
                vertexToBasicBlock.[int currentVertex] <- Some currentBasicBlock
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
                    sinks.Add currentBasicBlock
                    currentBasicBlock.FinalOffset <- currentVertex
                    //addEdge currentBasicBlock.StartVertex currentVertex
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
        let dist = GraphUtils.incrementalSourcedDijkstraAlgo node cfgDistanceFrom
        let distFromNode = Dictionary<ICfgNode, uint>()
        for i in dist do
            if i.Value <> GraphUtils.infinity then
                distFromNode.Add(i.Key, i.Value)
        distFromNode)
        
    let resolveBasicBlockIndex offset =
        let rec binSearch (sortedOffsets : ResizeArray<BasicBlock>) offset l r =
            if l >= r then l
            else
                let mid = (l + r) / 2
                let midValue = sortedOffsets.[mid].StartOffset
                let leftIsLefter = midValue <= offset
                let rightIsRighter = mid + 1 >= sortedOffsets.Count || sortedOffsets.[mid + 1].StartOffset > offset
                if leftIsLefter && rightIsRighter then mid
                elif not rightIsRighter
                    then binSearch sortedOffsets offset (mid + 1) r
                    else binSearch sortedOffsets offset l (mid - 1)

        binSearch sortedBasicBlocks offset 0 (sortedBasicBlocks.Count - 1)

    let resolveBasicBlock offset = sortedBasicBlocks.[resolveBasicBlockIndex offset]
        
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
        sinks |> ResizeArray.iter (fun basicBlock -> basicBlock.IsSink <- true)
    
    static member TerminalForCFGEdge = 0<terminalSymbol>
    member this.SortedBasicBlocks = sortedBasicBlocks
    member this.IlBytes  = ilBytes
    member this.EntryPoint = sortedBasicBlocks.[0]
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
        basicBlock.StartOffset = offset
        && basicBlock.HasSiblings

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

    // Helps resolving cyclic dependencies between Application and MethodWithBody
    [<DefaultValue>] static val mutable private cfgReporter : Method -> unit
    static member internal ReportCFGLoaded with get() = Method.cfgReporter and set v = Method.cfgReporter <- v
    static member val internal CoverageZone : Method -> bool = fun _ -> false with get, set

    member x.InCoverageZone with get() = Method.CoverageZone x
    
    interface ICallGraphNode with
        member this.OutgoingEdges with get () =
            let edges = HashSet<_>()
            match this.CFG with
            | Some cfg -> 
                for bb in cfg.SortedBasicBlocks do 
                    for kvp in bb.OutgoingEdges do
                        if kvp.Key <> CfgInfo.TerminalForCFGEdge
                        then
                            for target in kvp.Value do
                                edges.Add target.Method |> ignore
            | None -> ()
            edges |> Seq.cast<ICallGraphNode>
            
    member this.CallGraphDistanceToMe
        with get () =
            let assembly = this.Module.Assembly
            let callGraphDist = Dict.getValueOrUpdate CallGraph.callGraphDistanceTo assembly (fun () -> Dictionary<_, _>())
            Dict.getValueOrUpdate callGraphDist this (fun () ->        
            let dist = incrementalSourcedDijkstraAlgo (this :> IReversedCallGraphNode) callGraphDist
            let distToNode = Dictionary<IReversedCallGraphNode, uint>()
            for i in dist do
                if i.Value <> infinity then
                    distToNode.Add(i.Key, i.Value)
            distToNode)
            
    interface IReversedCallGraphNode with
        member this.OutgoingEdges with get () =
            let edges = HashSet<_>()
            match this.CFG with
            | Some cfg -> 
                for bb in cfg.EntryPoint.IncomingCallEdges do                
                    edges.Add bb.Method |> ignore
            | None -> ()
            edges |> Seq.cast<IReversedCallGraphNode>

    static member val internal AttributesZone : Method -> bool = fun _ -> true with get, set

    member x.CheckAttributes with get() = Method.AttributesZone x

    member x.BasicBlocksCount with get() =
        match x.CFG with
        | Some cfg -> cfg.SortedBasicBlocks |> Seq.length |> uint
        | None -> 0u

and [<CustomEquality; CustomComparison>] public codeLocation = {offset : offset; method : Method}
    with
    member this.BasicBlock =
        match this.method.CFG with
        | Some cfg -> cfg.ResolveBasicBlock this.offset
        | None -> Unchecked.defaultof<_>
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

and IGraphTrackableState =
    abstract member CodeLocation: codeLocation
    abstract member CallStack: list<Method>
    abstract member Id: uint<VSharp.ML.GameServer.Messages.stateId>
    abstract member PathConditionSize: uint
    abstract member PredictedUsefulness: float with get
    abstract member VisitedNotCoveredVerticesInZone: uint with get
    abstract member VisitedNotCoveredVerticesOutOfZone: uint with get
    abstract member VisitedAgainVertices: uint with get
    abstract member History:  Dictionary<BasicBlock,uint>
    abstract member Children: array<IGraphTrackableState>
    
    

module public CodeLocation =
    let hasSiblings (blockStart : codeLocation) =
        let method = blockStart.method
        match method.CFG with
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
    
    let addCallEdge (callSource:codeLocation) (callTarget:codeLocation) =   
        let callerMethodCfgInfo = callSource.method.ForceCFG
        let calledMethodCfgInfo = callTarget.method.ForceCFG
        let callFrom = callSource.BasicBlock
        let callTo = calledMethodCfgInfo.EntryPoint
        let exists, location = callerMethodCfgInfo.Calls.TryGetValue callSource.BasicBlock  
        let mutable needInvalidate = false
        if not <| callTo.IncomingCallEdges.Contains callFrom
        then
            let returnTo =
                if callTarget.method.IsStaticConstructor || not exists // if not exists then it should be from exception mechanism
                then callFrom
                else
                    let returnTo = callerMethodCfgInfo.ResolveBasicBlock location.ReturnTo
                    
                    needInvalidate <-
                        callFrom.OutgoingEdges.Remove CfgInfo.TerminalForCFGEdge
                        && returnTo.IncomingCFGEdges.Remove callFrom
                    if needInvalidate
                    then
                        callFrom.OutgoingEdges.Add(CallGraph.dummyTerminalForCallShortcut, HashSet<_>([returnTo]))
                    returnTo
                    
                                        
            if not (callTarget.method.IsStaticConstructor || not exists)
            then
                let exists,callEdges = callFrom.OutgoingEdges.TryGetValue dummyTerminalForCallEdge
                if exists
                then
                    let added = callEdges.Add callTo
                    assert added
                else
                    callFrom.OutgoingEdges.Add(dummyTerminalForCallEdge, HashSet [|callTo|])
            
            calledMethodCfgInfo.Sinks                
            |> ResizeArray.iter (fun returnFrom ->
                let exists,returnEdges = returnFrom.OutgoingEdges.TryGetValue dummyTerminalForReturnEdge
                if exists
                then
                    let added = returnEdges.Add returnTo
                    assert added
                else
                    returnFrom.OutgoingEdges.Add(dummyTerminalForReturnEdge, HashSet [|returnTo|])
                let added = returnTo.IncomingCallEdges.Add returnFrom
                assert added
                    )
            
            let added = callTo.IncomingCallEdges.Add callFrom
            assert added
        else ()

    let moveState (initialPosition: codeLocation) (stateWithNewPosition: IGraphTrackableState) =
        let removed = initialPosition.BasicBlock.AssociatedStates.Remove stateWithNewPosition
        if removed        
        then
            let added = stateWithNewPosition.CodeLocation.BasicBlock.AssociatedStates.Add stateWithNewPosition
            assert added 
        if stateWithNewPosition.History.ContainsKey stateWithNewPosition.CodeLocation.BasicBlock
        then
             if initialPosition.BasicBlock <> stateWithNewPosition.CodeLocation.BasicBlock
             then stateWithNewPosition.History[stateWithNewPosition.CodeLocation.BasicBlock]
                    <- stateWithNewPosition.History[stateWithNewPosition.CodeLocation.BasicBlock] + 1u
        else stateWithNewPosition.History.Add(stateWithNewPosition.CodeLocation.BasicBlock, 1u)
        stateWithNewPosition.CodeLocation.BasicBlock.VisitedInstructions <-
            max
                stateWithNewPosition.CodeLocation.BasicBlock.VisitedInstructions
                (uint ((stateWithNewPosition.CodeLocation.BasicBlock.Instructions
                      |> Array.findIndex (fun instr -> Offset.from (int instr.offset) = stateWithNewPosition.CodeLocation.offset)) + 1))       
        stateWithNewPosition.CodeLocation.BasicBlock.IsVisited <-
            stateWithNewPosition.CodeLocation.BasicBlock.IsVisited
            || stateWithNewPosition.CodeLocation.offset = stateWithNewPosition.CodeLocation.BasicBlock.FinalOffset
    
    let addStates (parentState:Option<IGraphTrackableState>) (states:array<IGraphTrackableState>) =
        for newState in states do
            newState.CodeLocation.BasicBlock.AssociatedStates.Add newState
            if newState.History.ContainsKey newState.CodeLocation.BasicBlock
            then newState.History[newState.CodeLocation.BasicBlock] <- newState.History[newState.CodeLocation.BasicBlock] + 1u
            else newState.History.Add(newState.CodeLocation.BasicBlock, 1u)
            newState.CodeLocation.BasicBlock.VisitedInstructions <-
                max
                    newState.CodeLocation.BasicBlock.VisitedInstructions
                    (uint ((newState.CodeLocation.BasicBlock.Instructions
                          |> Array.findIndex (fun instr -> Offset.from (int instr.offset) = newState.CodeLocation.offset)) + 1))
            newState.CodeLocation.BasicBlock.IsVisited <-
                newState.CodeLocation.BasicBlock.IsVisited
                || newState.CodeLocation.offset = newState.CodeLocation.BasicBlock.FinalOffset

    let getShortestDistancesToGoals (states:array<codeLocation>) =
        __notImplemented__()

    let tryGetCfgInfo (method : Method) =
        if method.HasBody then
            // TODO: enabling this currently crushes V# as we asynchronously change Application.methods! Fix it
            // TODO: fix it
            let cfg = method.CFG
            Some cfg
        else None
        
    (*let messagesProcessor = MailboxProcessor.Start(fun inbox ->
        let tryGetCfgInfo (method : Method) =
            if method.HasBody then
                // TODO: enabling this currently crushes V# as we asynchronously change Application.methods! Fix it
                // TODO: fix it
                let cfg = method.CFG
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
                        let cfg = method.CFG |> Option.get
                        reply cfg
                    | AddCallEdge (_from, _to) ->
                        match _from.method.CFG, _to.method.CFG with
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
                        _to.CodeLocation.method.CFG |> ignore
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
*)
    member this.RegisterMethod (method: Method) =
        assert method.HasBody
        //let cfg = tryGetCfgInfo method |> Option.get
        //cfg
        //messagesProcessor.Post (AddCFG (None, method))

    member this.AddCallEdge (sourceLocation : codeLocation) (targetLocation : codeLocation) =
         addCallEdge sourceLocation targetLocation

    member this.SpawnState (state:IGraphTrackableState) =
        [|state|] |> addStates None
        //messagesProcessor.Post <| SpawnStates [|state|]

    member this.SpawnStates (states:seq<IGraphTrackableState>) =
        Array.ofSeq states |> addStates None
        //messagesProcessor.Post <| SpawnStates states

    member this.AddForkedStates (parentState:IGraphTrackableState) (forkedStates:seq<IGraphTrackableState>) =
        addStates (Some parentState) (Array.ofSeq forkedStates)
        //messagesProcessor.Post <| AddForkedStates (parentState,states)

    member this.MoveState (fromLocation : codeLocation) (toLocation : IGraphTrackableState) =
        tryGetCfgInfo toLocation.CodeLocation.method |> ignore
        moveState fromLocation toLocation
        //messagesProcessor.Post <| MoveState (fromLocation, toLocation)

    member x.AddGoal (location:codeLocation) =
        ()
        //messagesProcessor.Post <| AddGoals [|location|]

    member x.AddGoals (locations:array<codeLocation>) =
        ()
        //messagesProcessor.Post <| AddGoals locations

    member x.RemoveGoal (location:codeLocation) =
        ()
        //messagesProcessor.Post <| RemoveGoal location

    member this.GetShortestDistancesToAllGoalsFromStates (states: array<codeLocation>) =
        ()
        //messagesProcessor.PostAndReply (fun ch -> GetShortestDistancesToGoals(ch, states))

    member this.GetDistanceToNearestGoal (states: seq<IGraphTrackableState>) =
        ()
        //messagesProcessor.PostAndReply (fun ch -> GetDistanceToNearestGoal(ch, states))

    member this.GetGoalsReachableFromStates (states: array<codeLocation>) =
        ()
        //messagesProcessor.PostAndReply (fun ch -> GetReachableGoals(ch, states))

    member this.ResetQueryEngine() =
        ()
        //messagesProcessor.Post ResetQueryEngine



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
    let mutable graph = ApplicationGraph()
    let mutable visualizer : IVisualizer = NullVisualizer()

    let reset () =
        graph <- ApplicationGraph()
        methods.Clear()
        _loadedMethods.Clear()
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
