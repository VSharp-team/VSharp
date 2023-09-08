namespace VSharp.System

open VSharp
open VSharp.Core

module internal Activator =

    let internal CreateInstance (_ : state) (args : term list) : term =
        assert(List.length args = 1)
        let t = args[0] |> Helpers.unwrapType
        Memory.DefaultOf t
