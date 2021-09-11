module VSharp.Utils.PersistentUnionFind

open VSharp

type public puf<'a> when 'a : equality =
    private {impl : pdict<'a, 'a>}

module public PersistentUnionFind =
    
    let public empty<'a when 'a : equality> : puf<'a> = {impl = PersistentDict.empty}
    
    let rec public tryFind a puf =
        let tryFindInternal pdict a =
            try
                PersistentDict.find pdict a |> Some
            with
                _ -> None
        match tryFindInternal puf.impl a with
            | Some b when a = b -> Some a
            | Some b -> tryFind b puf
            | None -> None
            
    let public union a b puf =
        let aParentOption = tryFind a puf
        let bParentOption = tryFind b puf
        match aParentOption, bParentOption with
        | Some aParent, Some bParent when aParent <> bParent ->
            {impl = PersistentDict.add aParent bParent puf.impl}
        | _  -> puf
        
    let public add puf a =
        match tryFind a puf with
        | Some _ -> puf
        | None -> {impl = PersistentDict.add a a puf.impl}
        
        
    