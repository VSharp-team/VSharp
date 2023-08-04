namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ System.Buffer --------------------------------

module Buffer =

    let internal Memmove (state : state) (args : term list) : term =
        let dst, src, elemCount = args[1], args[2], args[3]
        let elemCount = Types.Cast elemCount typeof<int>
        let getArrayInfo ref =
            match ref.term with
            | Ref(ArrayIndex(address, indices, arrayType)) -> address, indices, arrayType
            | Ref(ClassField(address, field)) when field = Reflection.stringFirstCharField ->
                let stringArrayType = (typeof<char>, 1, true)
                address, [MakeNumber 0], stringArrayType
            | Ptr(HeapLocation(address, t), sightType, offset) ->
                match TryPtrToArrayInfo t sightType offset with
                | Some(index, arrayType) -> address, index, arrayType
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
        Nop()
