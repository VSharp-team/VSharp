namespace VSharp

open System.Collections.Generic

module GraphUtils =
    type graph<'a> = Dictionary<'a, HashSet<'a>>
    type distanceCache<'a> = Dictionary<'a, Dictionary<'a, uint>>

    type IGraphNode<'t> =
        abstract OutgoingEdges : seq<'t> with get

    let infinity = System.UInt32.MaxValue

    let floydAlgo nodes (graph : graph<'a>) =
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

    let sourcedDijkstraAlgo source (graph : graph<'a>) =
        let dist = Dictionary<'a, uint>()
        dist.Add (source, 0u)
        let queue = Queue<_>()
        queue.Enqueue source
        while not <| Seq.isEmpty queue do
            let parent = queue.Dequeue()
            for children in graph.[parent] do
                if not <| dist.ContainsKey children then
                    dist.Add (children, dist.[parent] + 1u)
                    queue.Enqueue children
        dist

    let inline incrementalSourcedDijkstraAlgo<'t when 't :> IGraphNode<'t> and 't : equality> (source : 't) (allPairDist : Dictionary<'t, Dictionary<'t, uint>>) =
        let dist = Dictionary<'t, uint>()
        dist.Add (source, 0u)
        let queue = Queue<_>()
        queue.Enqueue source
        while not <| Seq.isEmpty queue do
            let parent = queue.Dequeue()
            if allPairDist.ContainsKey parent then
                for parentDist in allPairDist[parent] do
                    if not <| dist.ContainsKey parentDist.Key then
                        dist.Add (parentDist.Key, dist[parent] + parentDist.Value)
            else
                for children in parent.OutgoingEdges do
                    if dist.ContainsKey children && dist[parent] + 1u < dist[children] then
                        dist.Remove children |> ignore
                    if not <| dist.ContainsKey children then
                        dist.Add (children, dist[parent] + 1u)
                        queue.Enqueue children
        dist

    let dijkstraAlgo nodes (graph : graph<'a>) =
        let dist = Dictionary<'a * 'a, uint>()
        for i in nodes do
            let iDist = sourcedDijkstraAlgo i graph
            for kvpair in iDist do
                dist.Add ((i, kvpair.Key), kvpair.Value)
        dist
