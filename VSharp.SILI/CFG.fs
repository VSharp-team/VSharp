namespace VSharp.Interpreter.IL

open System
open System.Reflection
open System.Collections.Generic

open System.Reflection.Emit
open FSharpx.Collections
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL

module public CFG =
    type internal genericGraph<'a> = Dictionary<'a, List<'a>>
    type internal genericDistance<'a> = Dictionary<'a, Dictionary<'a, uint>>
    type internal graph = genericGraph<offset>
    let private infinity = UInt32.MaxValue

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
            retOffsets : List<offset>
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

    type private interimData = {
        opCodes : OpCode [] //  for debug
        verticesOffsets : int HashSet
        fallThroughOffset : offset option []
        edges : Dictionary<offset, List<offset>>
        offsetsDemandingCall : Dictionary<offset, OpCode * MethodBase>
        retOffsets : List<offset>
    }
    with
        member x.AddEdge src dst =
            if not <| x.edges.ContainsKey src then
                x.edges.Add (src, List<_>())
                x.edges.[src].Add dst
            elif x.edges.[src].Contains dst |> not then x.edges.[src].Add dst

    let private createData (methodBase : MethodBase) (ilBytes : byte []) ehsBytes =
        let size = ilBytes.Length
        let interim = {
            fallThroughOffset = Array.init size (fun _ -> None)
            verticesOffsets = HashSet<_>()
            edges = Dictionary<_, _>()
            opCodes = Array.init size (fun _ -> OpCodes.Prefix1)
            offsetsDemandingCall = Dictionary<_,_>()
            retOffsets = List<_>()
        }
        let cfg = {
            methodBase = methodBase
            ilBytes = ilBytes
            sortedOffsets = List<_>()
            dfsOut = Dictionary<_,_>()
            sccOut = Dictionary<_,_>()
            graph = Dictionary<_, _>()
            reverseGraph = Dictionary<_,_>()
            clauses = ehsBytes
            offsetsDemandingCall = Dictionary<_,_>()
            retOffsets = List<_>()
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
                assert(wasAdded)
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
        { cfgData with offsetsDemandingCall = interimData.offsetsDemandingCall; retOffsets = interimData.retOffsets }

    let private markVertex (set : HashSet<offset>) vOffset =
        set.Add vOffset |> ignore

    let private dfs (methodBase : MethodBase) (data : interimData) (used : HashSet<int>) (ilBytes : byte []) (v : offset) =
        let rec dfs' (v : offset) =
            if used.Contains v then ()
            else
                //TODO: remove next line of code when generic pobs-generating mechanism is coded: for now ``markVertex''
                //TODO: is done intentionally to bypass all opcodes and find ``hard-coded Throw'' that would be pobs
//                markVertex data.verticesOffsets v
                let wasAdded = used.Add(v)
                assert(wasAdded)
                let opCode = Instruction.parseInstruction methodBase v
//                Logger.trace "CFG.dfs: Method = %s went to %d opCode = %O" (Reflection.getFullMethodName methodBase) v opCode
                data.opCodes.[v] <- opCode
                if opCode = OpCodes.Ret then
                    data.retOffsets.Add v

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

    let orderEdges (cfg : cfgData) : unit =
        let used = HashSet<offset>()
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

    let floydAlgo nodes (graph : genericGraph<'a>) =
        let dist = Dictionary<'a * 'a, uint>()
        let offsets = nodes
        let infinitySum a b =
            if a = infinity || b = infinity then infinity else a + b
        for i in offsets do
            for j in offsets do
                dist.Add((i, j), infinity)

        for i in offsets do
            dist.[(i, i)] <- 0u
            for j in graph.[i] do
                dist.[(i, j)] <- 1u

        for k in offsets do
            for i in offsets do
                for j in offsets do
                    if dist.[i, j] > infinitySum dist.[i, k] dist.[k, j] then
                        dist.[(i,j)] <- infinitySum dist.[i, k] dist.[k, j]
        dist

    let sourcedDijkstraAlgo source (graph : genericGraph<'a>) =
        let dist = Dictionary<'a, uint>()
        dist.Add (source, 0u)
        let queue = Queue<_>()
        queue.Enqueue source
        while not <| Seq.isEmpty queue do
            let parent = queue.Dequeue()
            for children in graph.[parent] do
                if not <| dist.ContainsKey (children) then
                    dist.Add (children, dist.[parent] + 1u)
                    queue.Enqueue children
        dist

    let incrementalSourcedDijkstraAlgo source (graph : genericGraph<'a>) (allPairDist : Dictionary<'a, Dictionary<'a, uint>>) =
        let dist = Dictionary<'a, uint>()
        dist.Add (source, 0u)
        let queue = Queue<_>()
        queue.Enqueue source
        while not <| Seq.isEmpty queue do
            let parent = queue.Dequeue()
            if allPairDist.ContainsKey parent then
                for parentDist in allPairDist.[parent] do
                    if not <| dist.ContainsKey parentDist.Key then
                        dist.Add (parentDist.Key, dist.[parent] + parentDist.Value)
            else
                for children in graph.[parent] do
                    if dist.ContainsKey children && dist.[parent] + 1u < dist.[children] then
                        dist.Remove children |> ignore
                    if not <| dist.ContainsKey children then
                        dist.Add (children, dist.[parent] + 1u)
                        queue.Enqueue children
        dist

    let dijkstraAlgo nodes (graph : genericGraph<'a>) =
        let dist = Dictionary<'a * 'a, uint>()
        for i in nodes do
            let iDist = sourcedDijkstraAlgo i graph
            for kvpair in iDist do
                dist.Add ((i, kvpair.Key), kvpair.Value)
        dist

    let foydAlgoCFG cfg =
        let nodes = cfg.sortedOffsets
        let graph = cfg.graph
        floydAlgo nodes graph

    let dijkstraAlgoCFG cfg =
        let nodes = cfg.sortedOffsets
        let graph = cfg.graph
        dijkstraAlgo nodes graph

    let private build (methodBase : MethodBase) =
        let ilBytes = Instruction.getILBytes methodBase
        let ehs = Instruction.getEHSBytes methodBase
        let interimData, cfgData = createData methodBase ilBytes ehs
        let used = HashSet<offset>()
        dfsComponent methodBase interimData used ilBytes 0
        Seq.iter (dfsExceptionHandlingClause methodBase interimData used ilBytes) ehs
        let cfg = addVerticesAndEdges cfgData interimData
        orderEdges cfg
        cfg

    let cfgs = Dictionary<MethodBase, cfgData>()
    let cfgFloyds = Dictionary<cfgData, Dictionary<offset * offset, uint>>()
    let cfgDijkstras = Dictionary<cfgData, Dictionary<offset * offset, uint>>()
    let cfgDistanceFrom = Dictionary<cfgData, genericDistance<offset>>()
    let cfgDistanceTo = Dictionary<cfgData, genericDistance<offset>>()

    let findCfg m = Dict.getValueOrUpdate cfgs m (fun () -> build m)

    let findDistance cfg = Dict.getValueOrUpdate cfgDijkstras cfg (fun () -> dijkstraAlgoCFG cfg)

    let findDistanceFrom cfg node =
        let cfgDist = Dict.getValueOrUpdate cfgDistanceFrom cfg (fun () -> Dictionary<_, _>())
        Dict.getValueOrUpdate cfgDist node (fun () ->
        let dist = incrementalSourcedDijkstraAlgo node cfg.graph cfgDist
        let distFromNode = Dictionary<offset, uint>()
        for i in dist do
            if i.Value <> infinity then
                distFromNode.Add(i.Key, i.Value)
        distFromNode)

    let findDistanceTo cfg node =
        let cfgDist = Dict.getValueOrUpdate cfgDistanceTo cfg (fun () -> Dictionary<_, _>())
        Dict.getValueOrUpdate cfgDist node (fun () ->
        let dist = incrementalSourcedDijkstraAlgo node cfg.reverseGraph cfgDist
        let distToNode = Dictionary<offset, uint>()
        for i in dist do
            if i.Value <> infinity then
                distToNode.Add(i.Key, i.Value)
        distToNode)

    let vertexOf (method : MethodBase) (offset : offset) =
        if not method.IsAbstract then
            let cfg = findCfg method
            cfg.sortedOffsets
             |> Seq.filter (fun vertex -> vertex <= offset)
             |> Seq.max
             |> Some
        else None

    let isVertex (method : MethodBase) (offset : offset) =
        if not method.IsAbstract then
            let cfg = findCfg method
            cfg.sortedOffsets.BinarySearch(offset) >= 0
        else false

    let isReachingReturn (method : MethodBase) (offset : offset) =
        if not method.IsAbstract then
            let cfg = findCfg method
            let vertex = cfg.sortedOffsets |> Seq.filter (fun vertex -> vertex <= offset) |> Seq.max
            let dist = findDistanceFrom cfg vertex
            Seq.exists (fun offset -> dist.ContainsKey offset && dist.[offset] <> 0u) cfg.retOffsets
        else false

    let isRecursiveVertex (method : MethodBase) (offset : offset) =
        if not method.IsAbstract then
            let cfg = findCfg method
            if cfg.dfsOut.ContainsKey offset then
                let t1 = cfg.dfsOut.[offset]
                cfg.reverseGraph.[offset] |> Seq.exists (fun w -> cfg.dfsOut.[w] <= t1)
            else false
        else false

    let hasSiblings (method : MethodBase) (offset : offset) =
        if not method.IsAbstract then
            let cfg = findCfg method
            if cfg.sortedOffsets.BinarySearch(offset) >= 0 then
                let parents = cfg.reverseGraph.[offset]
                parents |> Seq.exists (fun parent ->
                cfg.graph.[parent].Count > 1)
            else false
        else false

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

    type callGraph =
        {
            graph : genericGraph<MethodBase>
            reverseGraph : genericGraph<MethodBase>
        }

    let callGraphs = Dictionary<Assembly, callGraph>()
    let callGraphDijkstras = Dictionary<Assembly, Dictionary<MethodBase * MethodBase, uint>>()
    let callGraphFloyds = Dictionary<Assembly, Dictionary<MethodBase * MethodBase, uint>>()
    let callGraphDistanceFrom = Dictionary<Assembly, genericDistance<MethodBase>>()
    let callGraphDistanceTo = Dictionary<Assembly, genericDistance<MethodBase>>()

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
                    let cfg = findCfg current
                    Seq.iter (fun (_, m) -> addCall methods methodsReachability current m) cfg.offsetsDemandingCall.Values
                let newQ =
                    if methodsReachability.ContainsKey(current) then List.ofSeq methodsReachability.[current] @ methodsQueue
                    else methodsQueue
                exit processedMethods newQ
        let mq = List<MethodBase>()
        dfs [] (List.ofSeq mq) entryMethod
        methods, methodsReachability

    let private buildCallGraph (assembly : Assembly) (entryMethod : MethodBase) =
        let methods, methodsReachability = buildMethodsReachabilityForAssembly assembly entryMethod
        let graph = genericGraph<MethodBase>()
        let reverseGraph = genericGraph<MethodBase>()
        for i in methods do
            graph.Add (i, List<_>())
            reverseGraph.Add (i, List<_>())
        for i in methodsReachability do
            for j in i.Value do
                graph.[i.Key].Add j
                reverseGraph.[j].Add i.Key
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
        let callGraph = genericGraph<MethodBase>()
        for i in methods do
            callGraph.Add (i, List<_>())
        for i in methodsReachability do
            for j in i.Value do
                callGraph.[i.Key].Add j
        dijkstraAlgo methods callGraph

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
        let dist = incrementalSourcedDijkstraAlgo method callGraph.graph callGraphDist
        let distFromNode = Dictionary<MethodBase, uint>()
        for i in dist do
            if i.Value <> infinity then
                distFromNode.Add(i.Key, i.Value)
        distFromNode)

    let findCallGraphDistanceTo (method : MethodBase) =
        let assembly = method.Module.Assembly
        let callGraphDist = Dict.getValueOrUpdate callGraphDistanceTo assembly (fun () -> Dictionary<_, _>())
        Dict.getValueOrUpdate callGraphDist method (fun () ->
        let callGraph = findCallGraph assembly method
        let dist = incrementalSourcedDijkstraAlgo method callGraph.reverseGraph callGraphDist
        let distToNode = Dictionary<MethodBase, uint>()
        for i in dist do
            if i.Value <> infinity then
                distToNode.Add(i.Key, i.Value)
        distToNode)
