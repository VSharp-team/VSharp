namespace VSharp.Core.Symbolic

open VSharp.Core.Symbolic
open VSharp.Core.Symbolic.Propositional

module internal Unions =
    let internal make = Union
    let internal make2 guards values = Union (List.zip guards values)
    let internal makeIte c t e = Union [(c, t); (!!c, t)]

    let internal guardOneWith condition (g, v) = (condition &&& g, v)

    let internal guardWith condition union =
        union |> List.map (guardOneWith condition)
