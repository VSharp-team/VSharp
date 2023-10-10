namespace VSharp.Core

open VSharp
open Memory
open VSharp.Core

module internal Copying =

    let private copyArrayConcrete state srcAddress srcIndex srcType srcLens srcLBs dstAddress dstIndex dstType dstLens dstLBs length =
        let dstElemType = fst3 dstType
        let offsets = List.init length id
        let copyOneElem offset =
            let srcIndex = add srcIndex (makeNumber offset)
            let srcIndices = delinearizeArrayIndex srcIndex srcLens srcLBs
            let srcElem = readArrayIndex state srcAddress srcIndices srcType
            let casted = TypeCasting.cast srcElem dstElemType
            let dstIndex = add dstIndex (makeNumber offset)
            let dstIndices = delinearizeArrayIndex dstIndex dstLens dstLBs
            // Saving source elements before writing in case of overlapping copy
            dstIndices, casted
        let toWrite = List.map copyOneElem offsets
        let writeCopied (dstIndices, casted) =
            // Writing saved source elements
            writeArrayIndex state dstAddress dstIndices dstType casted
        List.iter writeCopied toWrite

    let private copyArraySymbolic state srcAddress srcIndex srcType srcLens srcLBs dstAddress dstIndex dstType dstLens dstLBs length =
        let dstElemType = fst3 dstType
        let srcFromIndices = delinearizeArrayIndex srcIndex srcLens srcLBs
        let lenMinOne = sub length (makeNumber 1)
        let srcToIndices = delinearizeArrayIndex (add srcIndex lenMinOne) srcLens srcLBs
        let srcMemory = readArrayRange state srcAddress srcFromIndices srcToIndices srcType
        let casted = TypeCasting.cast srcMemory dstElemType
        let dstFromIndices = delinearizeArrayIndex dstIndex dstLens dstLBs
        let dstToIndices = delinearizeArrayIndex (add dstIndex lenMinOne) dstLens dstLBs
        writeArrayRange state dstAddress dstFromIndices dstToIndices dstType casted

    let private copyArrayCommon state srcAddress srcIndex (_, srcDim, _ as srcType) dstAddress dstIndex dstType length =
        let dstDim = snd3 dstType
        let srcLBs = List.init srcDim (fun dim -> readLowerBound state srcAddress (makeNumber dim) srcType)
        let srcLens = List.init srcDim (fun dim -> readLength state srcAddress (makeNumber dim) srcType)
        let dstLBs = List.init dstDim (fun dim -> readLowerBound state dstAddress (makeNumber dim) dstType)
        let dstLens = List.init dstDim (fun dim -> readLength state dstAddress (makeNumber dim) dstType)
        match length.term with
        | Concrete(l, _) ->
            let length = NumericUtils.ObjToInt l
            copyArrayConcrete state srcAddress srcIndex srcType srcLens srcLBs dstAddress dstIndex dstType dstLens dstLBs length
        | _ ->
            copyArraySymbolic state srcAddress srcIndex srcType srcLens srcLBs dstAddress dstIndex dstType dstLens dstLBs length

    let isSafeContextCopy srcArrayType dstArrayType =
        let srcElemType = fst3 srcArrayType
        let dstElemType = fst3 dstArrayType
        isSafeContextWrite srcElemType dstElemType

    let copyArray state srcAddress srcIndex srcType dstAddress dstIndex dstType length =
        assert(isSafeContextCopy srcType dstType)
        let cm = state.concreteMemory
        let concreteSrcIndex = tryTermToObj state srcIndex
        let concreteDstIndex = tryTermToObj state dstIndex
        let concreteLength = tryTermToObj state length
        match srcAddress.term, dstAddress.term, concreteSrcIndex, concreteDstIndex, concreteLength with
        | ConcreteHeapAddress src, ConcreteHeapAddress dst, Some srcI, Some dstI, Some l when cm.Contains src && cm.Contains dst ->
            let srcI = NumericUtils.ObjToLong srcI
            let dstI = NumericUtils.ObjToLong dstI
            let l = NumericUtils.ObjToLong l
            cm.CopyArray src dst srcI dstI l
        // TODO: if src array is in concrete memory, make more efficient case: get array data and write it to dst array
        | _ -> copyArrayCommon state srcAddress srcIndex srcType dstAddress dstIndex dstType length

    let copyCharArrayToStringSymbolic (state : state) arrayAddress stringConcreteAddress startIndex arrayLength =
        let stringAddress = ConcreteHeapAddress stringConcreteAddress
        let stringAddress, arrayType = stringArrayInfo state stringAddress (Some arrayLength)
        copyArray state arrayAddress startIndex arrayType stringAddress (makeNumber 0) arrayType arrayLength
        writeClassField state stringAddress Reflection.stringLengthField arrayLength

    let copyCharArrayToString (state : state) arrayAddress stringConcreteAddress startIndex length =
        let cm = state.concreteMemory
        match arrayAddress.term, startIndex.term, length with
        | ConcreteHeapAddress concreteAddress, _, _ when concreteAddress = VectorTime.zero ->
            if cm.Contains stringConcreteAddress then
                cm.Remove stringConcreteAddress
            cm.Allocate stringConcreteAddress ""
        | ConcreteHeapAddress concreteAddress, Concrete(i, _), Some {term = Concrete(len, _)} when cm.Contains concreteAddress ->
            if cm.Contains stringConcreteAddress |> not then
                // Allocating not empty string, because it should not be interned
                // Copying array will mutate whole string
                cm.Allocate stringConcreteAddress "\000"
            let len = TypeUtils.convert len typeof<int> :?> int
            let i = TypeUtils.convert i typeof<int> :?> int
            cm.CopyCharArrayToStringLen concreteAddress stringConcreteAddress i len
        | ConcreteHeapAddress concreteAddress, Concrete(i, _), None when cm.Contains concreteAddress ->
            if cm.Contains stringConcreteAddress |> not then
                // Allocating not empty string, because it should not be interned
                // Copying array will mutate whole string
                cm.Allocate stringConcreteAddress "\000"
            let i = TypeUtils.convert i typeof<int> :?> int
            cm.CopyCharArrayToString concreteAddress stringConcreteAddress i
        | _, _, Some length ->
            if cm.Contains stringConcreteAddress then
                cm.Remove stringConcreteAddress
            copyCharArrayToStringSymbolic state arrayAddress stringConcreteAddress startIndex length
        | _, _, None ->
            if cm.Contains stringConcreteAddress then
                cm.Remove stringConcreteAddress
            let arrayType = (typeof<char>, 1, true)
            let length = readLength state arrayAddress (makeNumber 0) arrayType
            copyCharArrayToStringSymbolic state arrayAddress stringConcreteAddress startIndex length

    let private fillArrayConcrete state arrayAddress arrayType startIndex length lbs lens castedValue =
        let offsets = List.init length id
        let copyOneElem offset =
            let linearIndex = add startIndex (makeNumber offset)
            let indices = delinearizeArrayIndex linearIndex lens lbs
            writeArrayIndex state arrayAddress indices arrayType castedValue
        List.iter copyOneElem offsets

    let private fillArraySymbolic state arrayAddress arrayType startIndex length lbs lens castedValue =
        let lastIndex = sub length (makeNumber 1)
        let lowerBounds = delinearizeArrayIndex startIndex lens lbs
        let upperBounds = delinearizeArrayIndex lastIndex lens lbs
        writeArrayRange state arrayAddress lowerBounds upperBounds arrayType castedValue

    let private fillArrayCommon state arrayAddress (elemType, dim, _ as arrayType) index length value =
        let castedValue = TypeCasting.cast value elemType
        let lbs = List.init dim (fun dim -> readLowerBound state arrayAddress (makeNumber dim) arrayType)
        let lens = List.init dim (fun dim -> readLength state arrayAddress (makeNumber dim) arrayType)
        match length.term with
        | Concrete(l, _) ->
            let length = NumericUtils.ObjToInt l
            fillArrayConcrete state arrayAddress arrayType index length lbs lens castedValue
        | _ -> fillArraySymbolic state arrayAddress arrayType index length lbs lens castedValue

    let fillArray state arrayAddress (elemType, _, _ as arrayType) index length value =
        let cm = state.concreteMemory
        let concreteIndex = tryTermToObj state index
        let concreteLength = tryTermToObj state length
        let castedValue = TypeCasting.cast value elemType
        let concreteValue = tryTermToObj state castedValue
        match arrayAddress.term, concreteIndex, concreteLength, concreteValue with
        | ConcreteHeapAddress address, Some i, Some l, Some v when cm.Contains address ->
            let i = NumericUtils.ObjToInt i
            let l = NumericUtils.ObjToInt l
            cm.FillArray address i l v
        | _ -> fillArrayCommon state arrayAddress arrayType index length value
