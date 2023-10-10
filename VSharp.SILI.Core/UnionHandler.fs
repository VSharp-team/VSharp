namespace VSharp.Core

open VSharp

module internal UnionHandler =
    let mapUnion term lambda merge =
        match term.term with
        | Union gvs -> List.map (fun (_,v) -> lambda v) gvs |> merge
        | _ -> internalfailf "mapUnion: expected union, but got %O" term