namespace VSharp.Interpreter.IL

open System.Reflection
open System.Collections.Generic

open System.Reflection.Emit
open System.Runtime.InteropServices
open FSharpx.Collections
open VSharp

module public CFG =
    type internal graph = Dictionary<offset, List<offset>>

    type public cfgData = {
        methodBase : MethodBase
        ilBytes : byte []
        sortedOffsets : List<offset>
        dfsOut : Dictionary<offset, int>
        sccOut : Dictionary<offset, int>               // maximum tOut of SCC-vertices
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
            dfsOut = Dictionary<_,_>()
            sccOut = Dictionary<_,_>()
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
        let rec dfs' (v : offset) =
            if used.Contains v then ()
            else
                let wasAdded = used.Add(v)
                assert(wasAdded)
                let opCode = Instruction.parseInstruction ilBytes v
                Logger.trace "CFG.dfs: Method = %s went to %d opCode = %O" (Reflection.GetFullMethodName methodBase) v opCode
                data.opCodes.[v] <- opCode

                let dealWithJump src dst =
                    markVertex data.verticesOffsets src
                    markVertex data.verticesOffsets dst

                    if Instruction.isLeaveOpCode opCode then
                        let ehcs = methodBase.GetMethodBody().ExceptionHandlingClauses
                                   |> Seq.filter Instruction.isFinallyClause
                                   |> Seq.filter (Instruction.shouldExecuteFinallyClause (Instruction src) (Instruction dst))
                                   |> Seq.sortWith (fun ehc1 ehc2 -> ehc1.HandlerOffset - ehc2.HandlerOffset)
                        let chainSequentialFinallyBlocks prevOffset (ehc : ExceptionHandlingClause) =
                            let startOffset = ehc.HandlerOffset
                            let endOffset = ehc.HandlerOffset + ehc.HandlerLength - 1
                            markVertex data.verticesOffsets startOffset
                            data.AddEdge prevOffset startOffset
                            dfs' startOffset
                            markVertex data.verticesOffsets endOffset
                            endOffset
                        let lastVertex = ehcs |> Seq.fold chainSequentialFinallyBlocks src
                        data.AddEdge lastVertex dst
                    else data.AddEdge src dst
                    dfs' dst

                let ipTransition = Instruction.findNextInstructionOffsetAndEdges opCode ilBytes v
                match ipTransition with
                | FallThrough offset when Instruction.isDemandingCallOpCode opCode ->
                    let calledMethod = Instruction.resolveMethodFromMetadata methodBase ilBytes (v + opCode.Size)
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
                | ConditionalBranch offsets -> offsets |> List.iter (dealWithJump v)
        dfs' v
    let private dfsComponent methodBase (data : interimData) used (ilBytes : byte []) startOffset =
        markVertex data.verticesOffsets startOffset
        dfs methodBase data used ilBytes startOffset

    let private dfsExceptionHandlingClause methodBase (data : interimData) used (ilBytes : byte []) (ehc : ExceptionHandlingClause) =
        if ehc.Flags = ExceptionHandlingClauseOptions.Filter
        then dfsComponent methodBase data used ilBytes ehc.FilterOffset
        dfsComponent methodBase data used ilBytes ehc.HandlerOffset // some catch handlers may be nested

    let orderEdges (used : HashSet<offset>) (cfg : cfgData) : unit =
        let rec bypass acc (u : offset) =
            used.Add u |> ignore
            let vertices, tOut = cfg.graph.[u] |> Seq.fold (fun acc v -> if used.Contains v then acc else bypass acc v) acc
            cfg.dfsOut.[u] <- tOut
            u::vertices, tOut + 1
        let propagateMaxTOutForSCC (used : HashSet<offset>) max v =
            let rec helper v =
                used.Add v |> ignore
                cfg.sccOut.[v] <- max
                cfg.reverseGraph.[v] |> Seq.iter (fun u -> if not <| used.Contains u then helper u)
            helper v
        let vertices, _ = bypass ([], 1) 0 // TODO: what about final handlers (they are separated from main) ?
        let used = HashSet<offset>()
        vertices |> List.iter (fun v -> if not <| used.Contains v then propagateMaxTOutForSCC used cfg.dfsOut.[v] v)

    let build (methodBase : MethodBase) =
        let interimData, cfgData = createData methodBase
        let methodBody = methodBase.GetMethodBody()
        let ilBytes = methodBody.GetILAsByteArray()
        let used = HashSet<offset>()
        dfsComponent methodBase interimData used ilBytes 0
        Seq.iter (dfsExceptionHandlingClause methodBase interimData used ilBytes) methodBody.ExceptionHandlingClauses
        let cfg = addVerticesAndEdges cfgData interimData
        orderEdges (HashSet<offset>()) cfg
        cfg
