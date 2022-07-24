namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open VSharp

type DotVisualizer(outputDirectory : DirectoryInfo) =
    let dotPath = Environment.GetEnvironmentVariable("DOT_PATH")
    let dotPath = if String.IsNullOrWhiteSpace dotPath then "dot" else dotPath
    let dotFile = Path.GetTempFileName()
    let outputDirectory = outputDirectory.CreateSubdirectory("visualization")
    let ids = Dictionary<codeLocation, string>()
    let mutable lastVertexId = 0
    let mutable lastPictureId = 0
    let stateMarkers = Dictionary<codeLocation, int>()
    let visitedVertices = HashSet<codeLocation>()
    let visitedEdges = HashSet<codeLocation * codeLocation>()
    let states = ResizeArray<IGraphTrackableState>()

    let loc2BB loc = {method = loc.method; offset = loc.method.CFG.ResolveBasicBlock loc.offset}

    let leave loc =
        stateMarkers.[loc] <- stateMarkers.[loc] - 1
    let visit loc =
        visitedVertices.Add loc |> ignore
        if stateMarkers.ContainsKey loc then stateMarkers.[loc] <- stateMarkers.[loc] + 1
        else stateMarkers.Add(loc, 1)
    let move fromLoc toLoc =
        visit toLoc
        visitedEdges.Add(fromLoc, toLoc) |> ignore

    let id loc = Dict.getValueOrUpdate ids loc (fun () -> lastVertexId <- lastVertexId + 1; $"v{lastVertexId}")
    let visitedColor = "#D3D3D3B4"
    let unvisitedColor = "#000000"
    let colorOfNode loc =
        let markers = Dict.tryGetValue stateMarkers loc 0
        if markers > 0 then
            let alpha = 256 - 128 / (1 <<< (min 7 markers))
            let r, g, b = 0xCC, 0x88, 0x99
            sprintf "#%x%x%x%x" r g b alpha, "filled"
        elif visitedVertices.Contains loc then visitedColor, "filled"
        else unvisitedColor, "\"\""
    let colorOfEdge fromLoc toLoc =
        if visitedEdges.Contains(fromLoc, toLoc) then visitedColor else unvisitedColor

    let node loc =
        let color, style = colorOfNode loc
        let text = (loc.method.BasicBlockToString loc.offset |> join "\\l") + "\\l"
        $"{id loc} [shape=box, label=\"{text}\", color=\"{color}\", style={style}]"
    let edge fromLoc toLoc =
        $"{id fromLoc} -> {id toLoc} [color=\"{colorOfEdge fromLoc toLoc}\"]"

    let methodToDot (m : Method) =
        let cfg = m.CFG
        seq{
            let name = m.FullName
            let id = m.Id
            yield $"subgraph cluster_{id} {{"
            yield $"label=%A{name}"
            for vertex in cfg.SortedOffsets do
                yield node {method = m; offset = vertex}
            for kvp in cfg.Edges do
                let from = {method = m; offset = kvp.Key}
                for toOffset in kvp.Value do
                    let toLoc = {method = m; offset = toOffset}
                    yield edge from toLoc
            yield "}"
        }

    let relevantMethods() =
        let result = ResizeArray<Method>()
        for state in states do
            for method in List.rev state.CallStack do
                if not <| result.Contains method then
                    result.Add method
        result

    member private x.Compile() =
        lastPictureId <- lastPictureId + 1
        let format = "0000000000"
        let output = $"{outputDirectory.FullName}{Path.DirectorySeparatorChar}Step{lastPictureId.ToString(format)}.svg"
        let startInfo = ProcessStartInfo()
        startInfo.FileName <- dotPath
        startInfo.Arguments <- $"-Tsvg {dotFile} -o {output}"
        Process.Start(startInfo) |> ignore

    interface IVisualizer with

        override x.AddState state =
            visit (loc2BB state.CodeLocation)
            states.Add state

        override x.TerminateState state =
            leave (loc2BB state.CodeLocation)
            states.Remove state |> ignore

        override x.VisualizeGraph () =
            let dot = seq {
               yield "digraph G"
               yield "{"
               for m in relevantMethods() do
                   yield! methodToDot m
               yield "}"
            }
            File.WriteAllLines(dotFile, dot)
            x.Compile()

        override x.VisualizeStep fromLoc toState newStates =
            let fromLoc = loc2BB fromLoc
            let toLoc = loc2BB toState.CodeLocation
            let transited = fromLoc <> toLoc
            if transited then
                leave fromLoc
                move fromLoc toLoc
            newStates |> Seq.iter (fun state ->
                move fromLoc state.CodeLocation
                states.Add state)
            if transited || not <| Seq.isEmpty newStates then
                (x :> IVisualizer).VisualizeGraph()
