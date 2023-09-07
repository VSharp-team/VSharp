namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ System.Buffer --------------------------------

module internal Buffer =

    let private getArrayInfo state ref =
        let zero = MakeNumber 0
        match ref.term with
        | HeapRef(address, _) ->
            let t = MostConcreteTypeOfRef state ref
            if TypeUtils.isArrayType t then
                let elemType = t.GetElementType()
                let indices, arrayType =
                    if t.IsSZArray then [zero], (elemType, 1, true)
                    else
                        let rank = t.GetArrayRank()
                        let indices = List.init rank (fun _ -> zero)
                        let arrayType = (elemType, rank, false)
                        indices, arrayType
                address, indices, arrayType
            elif t = typeof<string> then
                let address, stringArrayType = Memory.StringArrayInfo state address None
                address, [zero], stringArrayType
            else internalfail $"Memmove: unexpected HeapRef type {t}"
        | Ref(ArrayIndex(address, indices, arrayType)) -> address, indices, arrayType
        | Ref(ClassField(address, field)) when field = Reflection.stringFirstCharField ->
            let address, stringArrayType = Memory.StringArrayInfo state address None
            address, [zero], stringArrayType
        | Ptr(HeapLocation _ as pointerBase, sightType, offset) ->
            match TryPtrToRef state pointerBase sightType offset with
            | Some(ArrayIndex(address, index, arrayType)) -> address, index, arrayType
            | _ -> internalfail $"Memmove: unexpected pointer {ref}"
        | _ -> internalfail $"Memmove: unexpected reference {ref}"

    let CommonMemmove (state : state) dst dstIndex src srcIndex elemCount =
        let elemCount = Types.Cast elemCount typeof<int>
        let dstAddr, dstIndices, dstArrayType = getArrayInfo state dst
        let srcAddr, srcIndices, srcArrayType = getArrayInfo state src
        let dstType = Types.ArrayTypeToSymbolicType dstArrayType
        let srcType = Types.ArrayTypeToSymbolicType srcArrayType
        let dstHeapRef = HeapRef dstAddr dstType
        let srcHeapRef = HeapRef srcAddr srcType
        let dstLinearIndex = Memory.LinearizeArrayIndex state dstAddr dstIndices dstArrayType
        let dstLinearIndex =
            match dstIndex with
            | Some dstIndex -> API.Arithmetics.Add dstLinearIndex dstIndex
            | None -> dstLinearIndex
        let srcLinearIndex = Memory.LinearizeArrayIndex state srcAddr srcIndices srcArrayType
        let srcLinearIndex =
            match srcIndex with
            | Some srcIndex -> API.Arithmetics.Add srcLinearIndex srcIndex
            | None -> srcLinearIndex
        Memory.CopyArray state srcHeapRef srcLinearIndex srcType dstHeapRef dstLinearIndex dstType elemCount

    let Memmove (state : state) dst src elemCount =
        CommonMemmove state dst None src None elemCount

    let GenericMemmove (state : state) (args : term list) : term =
        assert(List.length args = 4)
        let dst, src, elemCount = args[1], args[2], args[3]
        Memmove state dst src elemCount
        Nop()

    let ByteMemmove (state : state) (args : term list) : term =
        assert(List.length args = 3)
        let dst, src, elemCount = args[0], args[1], args[2]
        Memmove state dst src elemCount
        Nop()

    let MemoryCopy (state : state) (args : term list) : term =
        assert(List.length args = 4)
        let dst, src, dstSize, count = args[0], args[1], args[2], args[3]
        // TODO: use 'dstSize' to check for exception
        Memmove state dst src count
        Nop()
