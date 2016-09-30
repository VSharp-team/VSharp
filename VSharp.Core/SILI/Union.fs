namespace VSharp.Core.Symbolic

open VSharp.Core.Symbolic.Propositional

module internal Unions =

    let internal make elements = Union elements

    let internal guardWith condition union =
        match union with
        | Union elements -> elements |> List.map (fun (g, v) -> (condition &&& g, v)) |> make
        | _ -> union
