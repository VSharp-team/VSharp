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

    let sourcedShortestDistanceBfs source (graph : graph<'a>) =
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

    /// <summary>
    /// Computes shortest distances from source vertex to all vertices in unweighted (!) graph
    /// </summary>
    /// <param name="source">Vertex to compute distances from</param>
    /// <param name="distanceCache"></param>
    let inline incrementalSourcedShortestDistanceBfs<'t when 't :> IGraphNode<'t> and 't : equality> (source : 't) (distanceCache : Dictionary<'t, Dictionary<'t, uint>>) =
        let currentDistances = Dictionary<'t, uint>()
        currentDistances.Add(source, 0u)
        let queue = Queue<_>()
        queue.Enqueue source
        while not <| Seq.isEmpty queue do
            let currentVertex = queue.Dequeue()
            let distanceToCurrent = currentDistances[currentVertex]
            if distanceCache.ContainsKey currentVertex then
                for distanceFromCurrent in distanceCache[currentVertex] do
                    let newDistance = distanceToCurrent + distanceFromCurrent.Value
                    if not <| currentDistances.ContainsKey distanceFromCurrent.Key || newDistance < currentDistances[distanceFromCurrent.Key] then
                        (*
                            1.
                            Why won't we break anything with such update?
                            Because even if newDistanceToTheVertex < currentDistanceToTheVertex holds, we haven't visited theVertex yet:

                            Suppose we have, then

                            currentDistanceToTheVertex <= distanceToCurrentVertex (because we're doing BFS)

                            newDistanceToTheVertex < currentDistanceToTheVertex ~
                            distanceToCurrentVertex + distanceFromCurrentToTheVertex < currentDistanceToTheVertex

                            distanceToCurrentVertex + distanceFromCurrentToTheVertex < distanceToCurrentVertex ?!
                        *)
                        currentDistances[distanceFromCurrent.Key] <- newDistance
            else
                for adjacent in currentVertex.OutgoingEdges do
                    let newDistanceToAdjacent = distanceToCurrent + 1u
                    if not <| currentDistances.ContainsKey adjacent || newDistanceToAdjacent < currentDistances[adjacent] then
                        currentDistances[adjacent] <- newDistanceToAdjacent
                        (*
                            2.
                            If the vertex was added to queue, then it will never be added again:
                    
                                If we write newDistanceToAdjacentVertex to dictionary here, newDistanceToAdjacentVertex >= currentDistanceToAdjacentVertex
                                will always hold because of BFS. It won't be broken by cache because currentDistanceToAdjacentVertex won't be rewritten from cache (see 1.)
                        *)
                        queue.Enqueue adjacent
        currentDistances

    let shortestDistanceBfs nodes (graph : graph<'a>) =
        let dist = Dictionary<'a * 'a, uint>()
        for i in nodes do
            let iDist = sourcedShortestDistanceBfs i graph
            for kvpair in iDist do
                dist.Add ((i, kvpair.Key), kvpair.Value)
        dist
