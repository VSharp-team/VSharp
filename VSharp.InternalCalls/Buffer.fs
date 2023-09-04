namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ System.Buffer --------------------------------

module Buffer =

    let private Memmove (state : state) dst src elemCount =
        let elemCount = Types.Cast elemCount typeof<int>
        let getArrayInfo ref =
            match ref.term with
            | Ref(ArrayIndex(address, indices, arrayType)) -> address, indices, arrayType
            | Ref(ClassField(address, field)) when field = Reflection.stringFirstCharField ->
                let address, stringArrayType = Memory.StringArrayInfo state address None
                address, [MakeNumber 0], stringArrayType
            | Ptr(HeapLocation(address, t), sightType, offset) ->
                match TryPtrToArrayInfo t sightType offset with
                | Some(index, arrayType) ->
                    let address =
                        if t = typeof<string> then Memory.StringArrayInfo state address None |> fst
                        else address
                    address, index, arrayType
                | None -> internalfail $"Memmove: unexpected pointer {ref}"
            | _ -> internalfail $"Memmove: unexpected reference {ref}"
        let dstAddr, dstIndices, dstArrayType = getArrayInfo dst
        let srcAddr, srcIndices, srcArrayType = getArrayInfo src
        let dstType = Types.ArrayTypeToSymbolicType dstArrayType
        let srcType = Types.ArrayTypeToSymbolicType srcArrayType
        let dstHeapRef = HeapRef dstAddr dstType
        let srcHeapRef = HeapRef srcAddr srcType
        let dstLinearIndex = Memory.LinearizeArrayIndex state dstAddr dstIndices dstArrayType
        let srcLinearIndex = Memory.LinearizeArrayIndex state srcAddr srcIndices srcArrayType
        Memory.CopyArray state srcHeapRef srcLinearIndex srcType dstHeapRef dstLinearIndex dstType elemCount

    let internal GenericMemmove (state : state) (args : term list) : term =
        let dst, src, elemCount = args[1], args[2], args[3]
        Memmove state dst src elemCount
        Nop()

    let internal ByteMemmove (state : state) (args : term list) : term =
        let dst, src, elemCount = args[0], args[1], args[2]
        Memmove state dst src elemCount
        Nop()

    let internal MemoryCopy (state : state) (args : term list) : term =
        assert(List.length args = 4)
        let dst, src, dstSize, count = args[0], args[1], args[2], args[3]
        // TODO: use 'dstSize' to check for exception
        Memmove state dst src count
        Nop()
