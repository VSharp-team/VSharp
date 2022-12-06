namespace VSharp

open System.Collections.Generic
open System.Reflection
open VSharp.GraphUtils

// TODO: ideally, we don't need CallGraph: it gets lazily bundled into application graph. Get rid of it when CFL reachability is ready.
module CallGraph =
        
    let callGraphDistanceFrom = Dictionary<Assembly, GraphUtils.distanceCache<ICallGraphNode>>()
    let callGraphDistanceTo = Dictionary<Assembly, GraphUtils.distanceCache<IReversedCallGraphNode>>()
  
    let findCallGraphDistanceFrom (method : Method) =
        let assembly = method.Module.Assembly
        let callGraphDist = Dict.getValueOrUpdate callGraphDistanceFrom assembly (fun () -> Dictionary<_, _>())
        Dict.getValueOrUpdate callGraphDist method (fun () ->        
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
        let dist = GraphUtils.incrementalSourcedDijkstraAlgo (method:>IReversedCallGraphNode)  callGraphDist
        let distToNode = Dictionary<IReversedCallGraphNode, uint>()
        for i in dist do
            if i.Value <> GraphUtils.infinity then
                distToNode.Add(i.Key, i.Value)
        distToNode)

