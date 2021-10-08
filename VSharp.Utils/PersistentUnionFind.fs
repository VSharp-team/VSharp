module VSharp.Utils.PersistentUnionFind

open VSharp

(*
    Union-find sets are implemented with dictionary cyclically mapping previous
    value to the next value wrapped with node. The last element (i. e. element before Tail)
    is considered as representative parent element of the set
*)
type private node<'a> =
    | Tail of 'a
    | Node of 'a

/// <summary>
/// Persistent union-find (disjoint-set) structure
/// </summary>
type public pUnionFind<'a> when 'a : equality =    
    private {elements : pdict<'a, node<'a>>}

module public PersistentUnionFind =
    
    let public empty<'a when 'a : equality> : pUnionFind<'a> =
        {elements = PersistentDict.empty}
    
    let public toSeq puf = PersistentDict.keys puf.elements
    
    /// <summary>
    /// Returns representative element of the set containing the given element or
    /// throws if the given element not found
    /// </summary>
    let rec public find a puf =
        PersistentDict.find puf.elements a
        |> function
            | Tail _ -> a
            | Node(next) -> find next puf
            
    /// <summary>
    /// Returns representative element of the set containing the given element or
    /// None if the given element not found
    /// </summary>
    let public tryFind a puf =
        try
            find a puf |> Some
        with
            _ -> None
            
    let private unwrapNode = function
        | Tail value -> value
        | Node value -> value
    
    /// <summary>
    /// Unions two sets containing the given elements
    /// </summary>
    let public union a b puf =
        let aParentOption = tryFind a puf
        let bParentOption = tryFind b puf
        match aParentOption, bParentOption with
        | Some(aParent), Some(bParent) when aParent <> bParent ->
            let aTail = PersistentDict.find puf.elements aParent |> unwrapNode
            let bTail = PersistentDict.find puf.elements bParent |> unwrapNode
            let mergedElements =
                puf.elements
                |> PersistentDict.add aParent (Tail(bTail))
                |> PersistentDict.add bParent (Node(aTail))
            {elements = mergedElements}
        | _ -> puf
    
    /// <summary>
    /// Adds a single-element set containing the given element 
    /// </summary>
    let public add puf a =
        match tryFind a puf with
        | Some _ -> puf
        | None -> {elements = PersistentDict.add a (Tail(a)) puf.elements}
        
    /// <summary>
    /// Returns a single-set union-find with the set containing the given element
    /// </summary>
    let public subset a puf =
        let rec traverse current acc =
            let next = PersistentDict.find puf.elements current
            let updatedDict = PersistentDict.add current next acc
            let unwrappedNext = unwrapNode next
            if (unwrappedNext <> a) then
                traverse unwrappedNext updatedDict
            else updatedDict
        let subsetElements = traverse a PersistentDict.empty
        {elements = subsetElements}