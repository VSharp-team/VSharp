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
        let addr1, indices1, arrayType1 = getArrayInfo dst
        let addr2, indices2, arrayType2 = getArrayInfo src
        let typ1 = Types.ArrayTypeToSymbolicType arrayType1
        let typ2 = Types.ArrayTypeToSymbolicType arrayType2
        let heapRef1 = HeapRef addr1 typ1
        let heapRef2 = HeapRef addr2 typ2
        let linearIndex1 = Memory.LinearizeArrayIndex state addr1 indices1 arrayType1
        let linearIndex2 = Memory.LinearizeArrayIndex state addr2 indices2 arrayType2
        Memory.CopyArray state heapRef1 linearIndex1 typ1 heapRef2 linearIndex2 typ2 elemCount
        Nop
