namespace VSharp.System

open VSharp.Core

module internal InteropServices =

    let internal GetArrayDataReference (state : state) (args : term list) =
        assert(List.length args = 2)
        let array = args[1]
        Memory.ReferenceArrayIndex state array [MakeNumber 0] None
