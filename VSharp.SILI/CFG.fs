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
            graph = Dictionary<_, _>()
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
        { cfgData with offsetsDemandingCall = interimData.offsetsDemandingCall }

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

    let private build (methodBase : MethodBase) =
        let ilBytes = Instruction.getILBytes methodBase
        let ehs = Instruction.getEHSBytes methodBase
        let interimData, cfgData = createData methodBase ilBytes ehs
        let used = HashSet<offset>()
        dfsComponent methodBase interimData used ilBytes 0
        Seq.iter (dfsExceptionHandlingClause methodBase interimData used ilBytes) ehs
        let cfg = addVerticesAndEdges cfgData interimData
        orderEdges (HashSet<offset>()) cfg
        cfg

    let cfgs = Dictionary<MethodBase, cfgData>()
    let floyds = Dictionary<cfgData, Dictionary<offset * offset, int>>()
    let findCfg m = Dict.getValueOrUpdate cfgs m (fun () -> build m)

    let findDistance cfg = Dict.getValueOrUpdate floyds cfg (fun () -> floydAlgo cfg 100000)

    let private fromCurrentAssembly assembly (current : MethodBase) = current.Module.Assembly = assembly

    let private addToDict (dict : Dictionary<'a, HashSet<'b>> ) (k : 'a ) (v : 'b) =
        let mutable refSet = ref null
        if not (dict.TryGetValue(k, refSet)) then
            refSet <- ref (HashSet<_>())
            dict.Add(k, refSet.Value)
        refSet.Value.Add(v) |> ignore

    let private addCall methodsReachability inverseMethodsReachability caller callee =
        addToDict methodsReachability caller callee
        addToDict inverseMethodsReachability callee caller

    let buildMethodsReachabilityForAssembly (entryMethod : MethodBase) =
        let assembly = entryMethod.Module.Assembly
        let methodsReachability = Dictionary<MethodBase, HashSet<MethodBase>>()
        let inverseMethodsReachability = Dictionary<MethodBase, HashSet<MethodBase>>()
        let rec exit processedMethods = function
            | [] -> ()
            | m :: q' -> dfs (processedMethods, q') m
        and dfs (processedMethods : MethodBase list, methodsQueue : MethodBase list) (current : MethodBase) =
            if List.contains current processedMethods || not (fromCurrentAssembly assembly current) then exit processedMethods methodsQueue
            else
                let processedMethods = current :: processedMethods
                if current.GetMethodBody() <> null then
                    let cfg = findCfg current
                    Seq.iter (fun (_, m) -> addCall methodsReachability inverseMethodsReachability current m) cfg.offsetsDemandingCall.Values
                let newQ =
                    if methodsReachability.ContainsKey(current) then List.ofSeq methodsReachability.[current] @ methodsQueue
                    else methodsQueue
                exit processedMethods newQ
        dfs ([],[]) entryMethod
        methodsReachability, inverseMethodsReachability
