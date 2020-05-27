namespace VSharp.Interpreter.IL

open System.Reflection
open System.Collections.Generic

open VSharp

module public CFG =
    type internal offset = int
    type internal graph = List<List<int>>

    type public cfgData = {
        methodBase : MethodBase
        ilBytes : byte []
        sortedOffsets : List<offset>
        topologicalTimes : int []
        graph : graph
        reverseGraph : graph
        clauses : ExceptionHandlingClause list
        offsetsDemandingCall : List<offset>
    }
    with
        member x.IsCallOrNewObjOffset offset =
            Seq.tryFind ((=) offset) x.offsetsDemandingCall |> Option.isSome

    type private interimData = {
        verticesOffsets : bool []
        previousVertexOffset : offset []
        nextVertexOffset : offset []
        visitedOffsets : bool []
        edges : (int * int) List
    }
    let private createData (methodBase : MethodBase) =
        let mb = methodBase.GetMethodBody()
        let size = mb.GetILAsByteArray().Length
        let interim = {
            previousVertexOffset = Array.init size id
            nextVertexOffset = Array.init size id
            visitedOffsets = Array.zeroCreate size
            verticesOffsets = Array.zeroCreate size
            edges = List<_>()
        }
        let cfg = {
            methodBase = methodBase
            ilBytes = mb.GetILAsByteArray()
            sortedOffsets = List<_>()
            topologicalTimes = null
            graph = List<_>()
            reverseGraph = List<_>()
            clauses = []
            offsetsDemandingCall = List<_>()
        }
        interim, cfg

    let private add (data : cfgData) (s, d) =
        let sIndex = data.sortedOffsets.BinarySearch(s)
        let dIndex = data.sortedOffsets.BinarySearch(d)
        data.graph.[sIndex].Add(dIndex)
        data.reverseGraph.[dIndex].Add(sIndex)

    let private addVerticesAndEdges (cfgData : cfgData) (interimData : interimData) =
        List.init (interimData.verticesOffsets.Length) id
        |> List.iter (fun offset -> if interimData.verticesOffsets.[offset] then cfgData.sortedOffsets.Add offset
                                                                                 cfgData.graph.Add (List<_>())
                                                                                 cfgData.reverseGraph.Add (List<_>()))
        interimData.edges |> Seq.iter (add cfgData)
        cfgData.sortedOffsets |> Seq.iter (fun v ->
            let u = interimData.nextVertexOffset.[v]
            if u <> v then add cfgData (v, u)
        )
        cfgData

    let private markVertex (data : interimData) vOffset =
        if not data.verticesOffsets.[vOffset] then
            data.verticesOffsets.[vOffset] <- true

            let mutable up = data.previousVertexOffset.[vOffset]
            let mutable down = data.nextVertexOffset.[up]

            while down < vOffset do
                up <- down
                down <- data.nextVertexOffset.[up]

            data.nextVertexOffset.[up] <- vOffset
            data.previousVertexOffset.[vOffset] <- up
            data.nextVertexOffset.[vOffset] <- down
            data.previousVertexOffset.[down] <- vOffset

    let private dfs (cfg : cfgData) (data : interimData) (ilBytes : byte []) (v : offset) (lastOffset : offset) =
        let rec dfsHelper (v : offset)  =
            data.visitedOffsets.[v] <- true
            let opcode = Instruction.parseInstruction ilBytes v
            let nextTargets = Instruction.findNextInstructionOffsetAndEdges opcode ilBytes v
            match nextTargets with
            | Choice1Of2 ins when Instruction.shouldSeparateOpcode opcode ->
                cfg.offsetsDemandingCall.Add v
                markVertex data v
                markVertex data ins
                if not data.visitedOffsets.[ins] then
                    dfsHelper ins
                data.edges.Add(v, ins)
            | Choice1Of2 ins ->
                if ins > lastOffset
                then markVertex data v // marking leave.s going to next instruction as terminal
                else
                    let niV = data.previousVertexOffset.[v]
                    data.previousVertexOffset.[ins] <- niV
                    data.nextVertexOffset.[niV] <- ins
                    if not data.visitedOffsets.[ins] then
                        dfsHelper ins
            | Choice2Of2 restTargets ->
                // marking terminal vertices
                markVertex data v
                for x in restTargets do
                    if x <= lastOffset then
                        markVertex data x
                        if not data.visitedOffsets.[x] then
                            dfsHelper x
                        data.edges.Add(v, x)
        dfsHelper v
    let private dfsComponent cfg (data : interimData) (ilBytes : byte []) startOffset lastOffset =
        markVertex data startOffset
        dfs cfg data ilBytes startOffset lastOffset

    let private dfsExceptionHandlingClause cfg (data : interimData) (ilBytes : byte []) (ehc : ExceptionHandlingClause) =
        if ehc.Flags = ExceptionHandlingClauseOptions.Filter
        then dfsComponent cfg data ilBytes ehc.FilterOffset (ilBytes.Length - 1)
        dfsComponent cfg data ilBytes ehc.HandlerOffset (ilBytes.Length - 1) // some catch handlers may be nested
    let topDfs cnt (gr : graph) =
        let mutable t = cnt
        let topTm = Array.create cnt -1
        let rec helperH v =
            topTm.[v] <- 0
            gr.[v] |> Seq.iter (fun u -> if topTm.[u] = -1 then helperH u)
            topTm.[v] <- t
            t <- t - 1
        for i in [0 .. cnt - 1] do
            if topTm.[i] = -1 then helperH i
        topTm

    let build (methodBase : MethodBase) =
        let methodBody = methodBase.GetMethodBody()
        let interimData, cfgData = createData methodBase
        let ilBytes = methodBody.GetILAsByteArray()
        dfsComponent cfgData interimData ilBytes 0 (ilBytes.Length - 1)
        Seq.iter (dfsExceptionHandlingClause cfgData interimData ilBytes) methodBody.ExceptionHandlingClauses
        let cfgData = { cfgData with clauses = List.ofSeq methodBody.ExceptionHandlingClauses }
        let cfgData = addVerticesAndEdges cfgData interimData
        { cfgData with topologicalTimes = topDfs cfgData.sortedOffsets.Count cfgData.graph }
