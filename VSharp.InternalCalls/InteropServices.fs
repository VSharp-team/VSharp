namespace VSharp.System

open VSharp
open VSharp.Core

module internal InteropServices =

    let internal GetArrayDataReference (state : state) (args : term list) =
        assert(List.length args = 2)
        let array = args[1]
        Memory.ReferenceArrayIndex state array [MakeNumber 0] None

    let internal Free (_ : state) (args : term list) =
        assert(List.length args = 1)
        // TODO: add checks
        Nop()

    let internal AlignedAlloc (state : state) (args : term list) =
        assert(List.length args = 2)
        let count = Types.Cast args[0] typeof<int>
        let ref = Memory.AllocateVectorArray state count typeof<byte>
        match ref.term with
        | HeapRef(address, sightType) ->
            let pointerBase = HeapLocation(address, sightType)
            let t = typeof<byte>.MakePointerType()
            let offset = MakeNumber 0
            Ptr pointerBase t offset
        | _ -> internalfail $"AlignedAlloc: unexpected array reference {ref}"
