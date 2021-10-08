module VSharp.Utils.PersistentUnionFind

open VSharp

type private node<'a> =
    | Tail of 'a
    | Node of 'a
    
type public pUnionFind<'a> when 'a : equality =    
    private {elements : pdict<'a, node<'a>>}

module public PersistentUnionFind =
    
    let public empty<'a when 'a : equality> : pUnionFind<'a> =
        {elements = PersistentDict.empty}
    
    let public toSeq puf = PersistentDict.keys puf.elements
    
    let rec public find a puf =
        PersistentDict.find puf.elements a
        |> function
            | Tail _ -> a
            | Node(next) -> find next puf
            
    let public tryFind a puf =
        try
            find a puf |> Some
        with
            _ -> None
            
    let public union a b puf =
        let aParentOption = tryFind a puf
        let bParentOption = tryFind b puf
        match aParentOption, bParentOption with
        | Some(aParent), Some(bParent) when aParent <> bParent ->
            let unwrap = function
                | Tail value -> value
                | Node value -> value
            let aTail = PersistentDict.find puf.elements aParent |> unwrap
            let bTail = PersistentDict.find puf.elements bParent |> unwrap
            puf.elements
            |> PersistentDict.add aParent (Tail(bTail))
            |> PersistentDict.add bParent (Node(aTail))
            |> (fun pdict -> {elements = pdict})
        | _ -> puf
        
    let public add puf a =
        match tryFind a puf with
        | Some _ -> puf
        | None -> {elements = PersistentDict.add a (Tail(a)) puf.elements}