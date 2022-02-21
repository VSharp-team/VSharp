namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ System.Buffer --------------------------------

module Buffer =

    let internal Memmove (state : state) (args : term list) : term =
        let dst, src, elemCount = args.[1], args.[2], args.[3]
        match dst.term, src.term with
        | Ref(ArrayIndex(addr1, indices1, arrayType1)), Ref(ArrayIndex(addr2, indices2, arrayType2)) ->
            let typ1 = Types.ArrayTypeToSymbolicType arrayType1
            let typ2 = Types.ArrayTypeToSymbolicType arrayType2
            let heapRef1 = HeapRef addr1 typ1
            let heapRef2 = HeapRef addr2 typ2
            let linearIndex1 = Memory.LinearizeArrayIndex state addr1 indices1 arrayType1
            let linearIndex2 = Memory.LinearizeArrayIndex state addr2 indices2 arrayType2
            Memory.CopyArray state heapRef1 linearIndex1 typ1 heapRef2 linearIndex2 typ2 elemCount
            Nop
        | _ -> internalfailf "Memmove is not implemented for src: %O, dst: %O" src dst
