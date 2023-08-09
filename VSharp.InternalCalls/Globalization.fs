namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Globalization =

    let allocateCulture state =
        let cultureType = typeof<System.Globalization.CultureInfo>
        Memory.AllocateDefaultClass state cultureType

    let get_CurrentCulture (state : state) (args : term list) : term =
        assert(List.length args = 0)
        allocateCulture state

    let get_InvariantCulture (state : state) (args : term list) : term =
        assert(List.length args = 0)
        allocateCulture state

    let get_CompareInfo (state : state) (args : term list) : term =
        assert(List.length args = 1)
        let cultureType = typeof<System.Globalization.CompareInfo>
        Memory.AllocateDefaultClass state cultureType

    let get_Invariant (_ : state) (args : term list) : term =
        assert(List.length args = 0)
        True()
