namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open VSharp

type DotVisualizer(drawInterproceduralEdges: bool, outputDirectory : DirectoryInfo) =
    let dotPath = Environment.GetEnvironmentVariable("DOT_PATH")
    let dotPath = if String.IsNullOrWhiteSpace dotPath then "dot" else dotPath
    let dotFile = Path.GetTempFileName()
    let outputDirectory = outputDirectory.CreateSubdirectory("visualization")
    let ids = Dictionary<BasicBlock, string>()
    let mutable lastVertexId = 0
    let mutable lastPictureId = 0
    let stateMarkers = Dictionary<BasicBlock, int>()
    let visitedVertices = HashSet<BasicBlock>()
    let visitedEdges = HashSet<BasicBlock * BasicBlock>()
    let states = ResizeArray<IGraphTrackableState>()
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

    let node basicBlock =
        let color, style = colorOfNode basicBlock
        let shape = if basicBlock.IsGoal then "note" else "box"
        let text = (basicBlock.ToString() |> join "\\l") + "\\l"
        $"{id basicBlock} [shape={shape}, label=\"{text}\", color=\"{color}\", style={style}]"
    let edge fromLoc toLoc =
        $"{id fromLoc} -> {id toLoc} [color=\"{colorOfEdge fromLoc toLoc}\"]"

    let methodToDot (m : Method) =
        let cfg = m.CFG
        let edges = ResizeArray<_>()
        seq{
            let name = m.FullName
            let id = m.Id
            yield $"subgraph cluster_{id} {{"
            yield $"label=%A{name}"
            for basicBlock in cfg.SortedBasicBlocks do
                yield node basicBlock
                if drawInterproceduralEdges
                then 
                    for kvp in basicBlock.OutgoingEdges do
                        for _to in kvp.Value do            
                            edges.Add (edge basicBlock _to)
                else
                    for _to in basicBlock.OutgoingEdges.[CfgInfo.TerminalForCFGEdge] do            
                        edges.Add (edge basicBlock _to)
            yield "}"
            yield! edges
        }

    let relevantMethods() =
        let result = ResizeArray<Method>()
        for state in states do
            for method in List.rev state.CallStack do
                if not <| result.Contains method then
                    result.Add method
        result

    new (outputDirectory) = DotVisualizer(false, outputDirectory)
    member private x.Compile() =
        lastPictureId <- lastPictureId + 1
        let format = "0000000000"
        let output = $"{outputDirectory.FullName}{Path.DirectorySeparatorChar}Step{lastPictureId.ToString(format)}.svg"
        let startInfo = ProcessStartInfo()
        startInfo.FileName <- dotPath
        startInfo.Arguments <- $"-Tsvg {dotFile} -o {output}"
        Process.Start(startInfo) |> ignore

    interface IVisualizer with
        override x.DrawInterproceduralEdges = drawInterproceduralEdges
        override x.AddState state =
            visit state.CodeLocation.BasicBlock
            states.Add state

        override x.TerminateState state =
            leave state.CodeLocation.BasicBlock
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
            let fromLoc = fromLoc.BasicBlock
            let toLoc = toState.CodeLocation.BasicBlock
            let transited = fromLoc <> toLoc
            if transited then
                leave fromLoc
                move fromLoc toLoc
            newStates |> Seq.iter (fun state ->
                move fromLoc (state.CodeLocation.method.CFG.ResolveBasicBlock state.CodeLocation.offset)
                states.Add state)
            if transited || not <| Seq.isEmpty newStates then
                (x :> IVisualizer).VisualizeGraph()
