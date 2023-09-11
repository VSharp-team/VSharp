namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilStateOperations

// ------------------------------ System.Buffer --------------------------------

module internal Buffer =

    // TODO: return address option
    let private getArrayInfo cilState ref =
        let state = cilState.state
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
                (Some (address, indices, arrayType), cilState) |> List.singleton
            elif t = typeof<string> then
                let address, stringArrayType = Memory.StringArrayInfo state address None
                (Some (address, [zero], stringArrayType), cilState) |> List.singleton
            else internalfail $"Memmove: unexpected HeapRef type {t}"
        | Ref(ArrayIndex(address, indices, arrayType)) ->
            (Some (address, indices, arrayType), cilState) |> List.singleton
        | Ref(ClassField(address, field)) when field = Reflection.stringFirstCharField ->
            let address, stringArrayType = Memory.StringArrayInfo state address None
            (Some (address, [zero], stringArrayType), cilState) |> List.singleton
        | Ptr(HeapLocation _ as pointerBase, sightType, offset) ->
            let cases = PtrToRefFork state pointerBase sightType offset
            assert(List.isEmpty cases |> not)
            let createArrayRef (address, state) =
                let cilState = changeState cilState state
                match address with
                | Some(ArrayIndex(address, index, arrayType)) -> Some (address, index, arrayType), cilState
                | _ ->
                    let iie = createInsufficientInformation "Memmove: unknown pointer"
                    cilState.iie <- Some iie
                    None, cilState
            List.map createArrayRef cases

        | _ -> internalfail $"Memmove: unexpected reference {ref}"

    let private Copy dstAddr dstIndex dstIndices dstArrayType srcAddr srcIndex srcIndices srcArrayType state elemCount =
        let srcElemType = fst3 srcArrayType
        let dstElemType = fst3 dstArrayType
        let size = TypeUtils.internalSizeOf srcElemType
        assert(srcElemType = dstElemType || TypeUtils.internalSizeOf dstElemType = size)
        let elemCount = Arithmetics.Div elemCount (MakeNumber size)
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

    let CommonMemmove (cilState : cilState) dst dstIndex src srcIndex bytesCount =
        let state = cilState.state
        let bytesCount = Types.Cast bytesCount typeof<int>
        let checkDst (info, cilState) =
            match info with
            | Some(dstAddr, dstIndices, dstArrayType) ->
                let checkSrc (info, cilState) =
                    match info with
                    | Some(srcAddr, srcIndices, srcArrayType) ->
                        Copy dstAddr dstIndex dstIndices dstArrayType srcAddr srcIndex srcIndices srcArrayType state bytesCount
                        cilState
                    | None -> cilState
                getArrayInfo cilState src |> List.map checkSrc
            | None -> cilState |> List.singleton
        getArrayInfo cilState dst |> List.collect checkDst

    let Memmove (cilState : cilState) dst src bytesCount =
        CommonMemmove cilState dst None src None bytesCount

    let GenericMemmove (_ : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 4)
        let dst, src, bytesCount = args[1], args[2], args[3]
        Memmove cilState dst src bytesCount

    let ByteMemmove (_ : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 3)
        let dst, src, bytesCount = args[0], args[1], args[2]
        Memmove cilState dst src bytesCount

    let MemoryCopy (i : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 4)
        let dst, src, dstBytesCount, bytesCount = args[0], args[1], args[2], args[3]
        let memMove cilState k =
            Memmove cilState dst src bytesCount |> k
        let checkDst cilState k =
            i.NpeOrInvoke cilState dst memMove k
        let checkSrc cilState k =
            i.NpeOrInvoke cilState src checkDst k
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (Arithmetics.LessOrEqual bytesCount dstBytesCount, state))
            checkSrc
            (i.Raise i.ArgumentOutOfRangeException)
            id
