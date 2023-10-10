namespace VSharp.Core

open VSharp

module TermsBranching =
    // мне кажется, тут странно, что в map только value меняется, guard нигде не меняется; но в choose могут меняться оба.
    // надо ли единообразно сделать, или пусть останется как есть?
    let mapUnion term mapper merge =
        match term.term with
        | Union gvs -> List.map (fun (g, v) -> (g, mapper v)) gvs |> merge
        | _ -> internalfailf "mapUnion: expected union, but got %O" term
        
    let chooseUnion term chooser merge =
        match term.term with
        | Union gvs -> List.choose chooser gvs |> merge
        | _ -> internalfailf "chooseUnion: expected union, but got %O" term