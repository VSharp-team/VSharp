namespace VSharp.Interpreter.IL

open System.Reflection
open System.Collections.Generic

open System.Reflection.Emit
open System.Runtime.InteropServices
open VSharp

module public CFG =
    type internal graph = Dictionary<offset, List<offset>>

    type public cfgData = {
        methodBase : MethodBase
        ilBytes : byte []
        sortedOffsets : List<offset>
        topologicalTimes : Dictionary<offset, int>
        graph : graph
        reverseGraph : graph
        clauses : ExceptionHandlingClause list
        offsetsDemandingCall : Dictionary<offset, OpCode * MethodBase>
    }
//    with
//        member x.IsCallOrNewObjOffset offset =
//            Seq.tryFind ((=) offset) x.offsetsDemandingCall |> Option.isSome

    type private interimData = {
        opCodes : OpCode [] //  for debug
        verticesOffsets : int HashSet
//        possibleOffsets : int HashSet
        fallThroughOffset : offset option []
//        visitedOffsetsOperationalStackBalance : int []
        edges : Dictionary<offset, List<offset>>
        offsetsDemandingCall : Dictionary<offset, OpCode * MethodBase>
    }
    with
        member x.AddEdge src dst =
            if not <| x.edges.ContainsKey src then
                x.edges.Add (src, List<_>())
            x.edges.[src].Add dst

    let private createData (methodBase : MethodBase) =
        let mb = methodBase.GetMethodBody()
        let size = mb.GetILAsByteArray().Length
        let interim = {
            fallThroughOffset = Array.init size (fun _ -> None)
//            visitedOffsetsOperationalStackBalance = Array.init size (always -1)
            verticesOffsets = HashSet<_>()
//            possibleOffsets = HashSet<_>()
            edges = Dictionary<_, _>()
            opCodes = Array.init size (fun _ -> OpCodes.Prefix1)
            offsetsDemandingCall = Dictionary<_,_>()
        }
        let cfg = {
            methodBase = methodBase
            ilBytes = mb.GetILAsByteArray()
            sortedOffsets = List<_>()
            topologicalTimes = null
            graph = Dictionary<_, _>()
            reverseGraph = Dictionary<_,_>()
            clauses = List.ofSeq mb.ExceptionHandlingClauses
            offsetsDemandingCall = Dictionary<_,_>()
        }
        interim, cfg

    let createVertex (cfgData : cfgData) offset =
        cfgData.sortedOffsets.Add offset
        cfgData.graph.Add <| (offset, List<_>())
        cfgData.reverseGraph.Add <| (offset, List<_>())

    let private addVerticesAndEdges (cfgData : cfgData) (interimData : interimData) =
        interimData.verticesOffsets
//        |> Seq.filter (fun offset -> interimData.visitedOffsetsOperationalStackBalance.[offset] = 0)
//        |> Seq.append interimData.verticesOffsets
//        |> Seq.distinct
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
                Prelude.releaseAssert2 (wasAdded, "offset %d couldn't be added")
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
        let rec dfs'  (v : offset) = //(v : offset, balance)  =
            if used.Contains v
            then () // Prelude.releaseAssert(balance = data.visitedOffsetsOperationalStackBalance.[v])
            else
                let wasAdded = used.Add(v)
                assert(wasAdded)
//                data.visitedOffsetsOperationalStackBalance.[v] <- balance
                let opCode = Instruction.parseInstruction ilBytes v
                Logger.trace "CFG.dfs: Method = %s went to %d opCode = %O" (Reflection.GetFullMethodName methodBase) v opCode
                data.opCodes.[v] <- opCode

                let dealWithJump src dst =
                    markVertex data.verticesOffsets src
                    markVertex data.verticesOffsets dst
                    data.AddEdge src dst
//                    let newBalance = Instruction.countOperationalStackBalance opCode None balance
                    dfs' dst

                let ipTransition = Instruction.findNextInstructionOffsetAndEdges opCode ilBytes v
                match ipTransition with
                | FallThrough offset when Instruction.isDemandingCallOpCode opCode ->
                    let calledMethod = Instruction.resolveMethodFromMetadata methodBase ilBytes (v + opCode.Size)
                    data.offsetsDemandingCall.Add(v, (opCode, calledMethod))
                    markVertex data.verticesOffsets v
                    markVertex data.verticesOffsets offset
                    data.fallThroughOffset.[v] <- Some offset
//                    let newBalance = Instruction.countOperationalStackBalance opCode (Some calledMethod) balance
                    dfs' offset
                | FallThrough offset ->
                    data.fallThroughOffset.[v] <- Some offset
//                    let newBalance = Instruction.countOperationalStackBalance opCode None balance
                    dfs' offset
                | ExceptionMechanism
                | Return -> ()
                | UnconditionalBranch target -> dealWithJump v target
                | ConditionalBranch offsets -> offsets |> List.iter (dealWithJump v)
        dfs' v
    let private dfsComponent methodBase (data : interimData) used (ilBytes : byte []) startOffset =
        markVertex data.verticesOffsets startOffset
        dfs methodBase data used ilBytes startOffset

    let private dfsExceptionHandlingClause methodBase (data : interimData) used (ilBytes : byte []) (ehc : ExceptionHandlingClause) =
        if ehc.Flags = ExceptionHandlingClauseOptions.Filter
        then dfsComponent methodBase data used ilBytes ehc.FilterOffset
        dfsComponent methodBase data used ilBytes ehc.HandlerOffset // some catch handlers may be nested

    // TODO: rewrite this code in functional style!
    let topDfs cnt (gr : graph) =
        let mutable t = cnt
        let topTm = Dictionary<offset, int>()
        let rec helperH v =
            topTm.Add(v, 0)
            gr.[v] |> Seq.iter (fun u -> if not <| topTm.ContainsKey u then helperH u)
            topTm.[v] <- t
            t <- t - 1
        gr |> Seq.iter (fun kvp -> if not <| topTm.ContainsKey kvp.Key then helperH kvp.Key)
        topTm

    let build (methodBase : MethodBase) =
        let interimData, cfgData = createData methodBase
        let methodBody = methodBase.GetMethodBody()
        let ilBytes = methodBody.GetILAsByteArray()
        let used = HashSet<offset>()
        dfsComponent methodBase interimData used ilBytes 0
        Seq.iter (dfsExceptionHandlingClause methodBase interimData used ilBytes) methodBody.ExceptionHandlingClauses
        let cfg = addVerticesAndEdges cfgData interimData
        {cfg with topologicalTimes = topDfs cfg.sortedOffsets.Count cfg.graph}
