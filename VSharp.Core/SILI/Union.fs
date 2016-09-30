namespace VSharp.Core.Symbolic

open VSharp.Core.Symbolic.Propositional

module internal Unions =

    let internal make elements = Union elements

    let internal guardOneWith condition (g, v) = (condition &&& g, v)

    let internal guardWith condition union =
        union |> List.map (fun (g, v) -> (condition &&& g, v))
