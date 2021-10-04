module VSharp.Utils.PersistentUnionFind

open VSharp

type public pUnionFind<'a> when 'a : equality =
    private {impl : pdict<'a, 'a>}

module public PersistentUnionFind =
    
    let public empty<'a when 'a : equality> : pUnionFind<'a> = {impl = PersistentDict.empty}
    
    let public toSeq puf = PersistentDict.keys puf.impl
    
    let rec public tryFind puf a =
        let tryFindInternal pdict a =
            try
                PersistentDict.find pdict a |> Some
            with
                _ -> None
        match tryFindInternal puf.impl a with
            | Some b when a = b -> Some a
            | Some b -> tryFind puf b
            | None -> None
            
    let public union a b puf =
        let aParentOption = tryFind puf a
        let bParentOption = tryFind puf b
        match aParentOption, bParentOption with
        | Some aParent, Some bParent when aParent <> bParent ->
            {impl = PersistentDict.add aParent bParent puf.impl}
        | _  -> puf
        
    let public add puf a =
        match tryFind puf a with
        | Some _ -> puf
        | None -> {impl = PersistentDict.add a a puf.impl}