namespace VSharp

open System.Collections.Generic
open System.Reflection
open VSharp.GraphUtils

// TODO: ideally, we don't need CallGraph: it gets lazily bundled into application graph. Get rid of it when CFL reachability is ready.
module CallGraph =
    type callGraph =
        {
            graph : GraphUtils.graph<Method>
            reverseGraph : GraphUtils.graph<Method>
        }

    let graph = GraphUtils.graph<Method>()
    let reverseGraph = GraphUtils.graph<Method>()

    let callGraphs = Dictionary<Assembly, callGraph>()
    let callGraphDijkstras = Dictionary<Assembly, Dictionary<Method * Method, uint>>()
    let callGraphFloyds = Dictionary<Assembly, Dictionary<Method * Method, uint>>()
    let callGraphDistanceFrom = Dictionary<Assembly, GraphUtils.distanceCache<ICallGraphNode>>()
    let callGraphDistanceTo = Dictionary<Assembly, GraphUtils.distanceCache<IReversedCallGraphNode>>()

    let private fromCurrentAssembly assembly (current : Method) = current.Module.Assembly = assembly

    let private addToDict (dict : Dictionary<'a, HashSet<'b>> ) (k : 'a) (v : 'b) =
        let mutable refSet = ref null
        if not (dict.TryGetValue(k, refSet)) then
            refSet <- ref (HashSet<_>())
            dict.Add(k, refSet.Value)
        refSet.Value.Add(v) |> ignore

    let private addCall (methods : HashSet<Method>) methodsReachability caller callee =
        methods.Add callee |> ignore
        addToDict methodsReachability caller callee

    let private buildMethodsReachabilityForAssembly (assembly : Assembly) (entryMethod : Method) =
        let methods = HashSet<Method>()
        let methodsReachability = Dictionary<Method, HashSet<Method>>()
        let rec exit processedMethods = function
            | [] -> ()
            | m :: q' -> dfs processedMethods q' m
        and dfs (processedMethods : Method list) (methodsQueue : Method list) (current : Method) =
            methods.Add current |> ignore
            if List.contains current processedMethods || not (fromCurrentAssembly assembly current) then exit processedMethods methodsQueue
            else
                let processedMethods = current :: processedMethods
                if current.HasBody then
                    current.CFG.Calls |> Seq.iter (fun kvp ->
                        let m = kvp.Value.Callee
                        addCall methods methodsReachability current m)
                let newQ =
                    if methodsReachability.ContainsKey(current) then List.ofSeq methodsReachability.[current] @ methodsQueue
                    else methodsQueue
                exit processedMethods newQ
        let mq = List<Method>()
        dfs [] (List.ofSeq mq) entryMethod
        methods, methodsReachability

    let private buildCallGraph (assembly : Assembly) (entryMethod : Method) =
        let methods, methodsReachability = buildMethodsReachabilityForAssembly assembly entryMethod
        let graph = GraphUtils.graph<Method>()
        let reverseGraph = GraphUtils.graph<Method>()
        for i in methods do
            graph.Add (i, HashSet<_>())
            reverseGraph.Add (i, HashSet<_>())
        for i in methodsReachability do
            for j in i.Value do
                let added = graph.[i.Key].Add j in assert added
                let added = reverseGraph.[j].Add i.Key in assert added
        { graph = graph; reverseGraph = reverseGraph }

    let private findCallGraph (assembly : Assembly) (entryMethod : Method) =
        let callGraph = Dict.getValueOrUpdate callGraphs assembly (fun () -> buildCallGraph assembly entryMethod)
        if not <| callGraph.graph.ContainsKey entryMethod then
            let updateCallGraph = buildCallGraph assembly entryMethod
            for i in updateCallGraph.graph do
                callGraph.graph.TryAdd(i.Key, i.Value) |> ignore
            for i in updateCallGraph.reverseGraph do
                callGraph.reverseGraph.TryAdd(i.Key, i.Value) |> ignore
        callGraph

    let private buildCallGraphDistance (assembly : Assembly) (entryMethod : Method) =
        let methods, methodsReachability = buildMethodsReachabilityForAssembly assembly entryMethod
        let callGraph = GraphUtils.graph<Method>()
        for i in methods do
            callGraph.Add (i, HashSet<_>())
        for i in methodsReachability do
            for j in i.Value do
                let added = callGraph.[i.Key].Add j
                assert added
        GraphUtils.dijkstraAlgo methods callGraph

    let findCallGraphDistance (assembly : Assembly) (entryMethod : Method) =
        let callGraphDist = Dict.getValueOrUpdate callGraphDijkstras assembly (fun () -> buildCallGraphDistance assembly entryMethod)
        if not <| callGraphDist.ContainsKey (entryMethod, entryMethod) then
            for i in buildCallGraphDistance assembly entryMethod do
                callGraphDist.TryAdd(i.Key, i.Value) |> ignore
        callGraphDist

    let findCallGraphDistanceFrom (method : Method) =
        let assembly = method.Module.Assembly
        let callGraphDist = Dict.getValueOrUpdate callGraphDistanceFrom assembly (fun () -> Dictionary<_, _>())
        Dict.getValueOrUpdate callGraphDist method (fun () ->
        //let callGraph = findCallGraph assembly method
        let dist = GraphUtils.incrementalSourcedDijkstraAlgo (method :> ICallGraphNode) callGraphDist
        let distFromNode = Dictionary<ICallGraphNode, uint>()
        for i in dist do
            if i.Value <> GraphUtils.infinity then
                distFromNode.Add(i.Key, i.Value)
        distFromNode)

    let findCallGraphDistanceTo (method : Method) =
        let assembly = method.Module.Assembly
        let callGraphDist = Dict.getValueOrUpdate callGraphDistanceTo assembly (fun () -> Dictionary<_, _>())
        Dict.getValueOrUpdate callGraphDist method (fun () ->
        //let callGraph = findCallGraph assembly method
        let dist = GraphUtils.incrementalSourcedDijkstraAlgo (method:>IReversedCallGraphNode)  callGraphDist
        let distToNode = Dictionary<IReversedCallGraphNode, uint>()
        for i in dist do
            if i.Value <> GraphUtils.infinity then
                distToNode.Add(i.Key, i.Value)
        distToNode)

