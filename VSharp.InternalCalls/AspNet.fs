namespace VSharp.System

open VSharp.Core.API
open global.System
open VSharp.Core

module internal AspNet =

    let CheckHost (_ : state) (args : term list) =
        assert(List.length args = 3)
        MakeBool true
