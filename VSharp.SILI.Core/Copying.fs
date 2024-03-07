namespace VSharp.Core

open VSharp
open Memory
open VSharp.Core

module internal Copying =

    let private copyArrayConcrete state srcAddress srcIndex srcType srcLens srcLBs dstAddress dstIndex dstType dstLens dstLBs length =
        let dstElemType = dstType.elemType
        let offsets = List.init length id
        let copyOneElem offset =
            let srcIndex = add srcIndex (makeNumber offset)
            let srcIndices = delinearizeArrayIndex srcIndex srcLens srcLBs
            let srcElem = state.memory.ReadArrayIndex srcAddress srcIndices srcType
            let casted = TypeCasting.cast srcElem dstElemType
            let dstIndex = add dstIndex (makeNumber offset)
            let dstIndices = delinearizeArrayIndex dstIndex dstLens dstLBs
            // Saving source elements before writing in case of overlapping copy
            dstIndices, casted
        let toWrite = List.map copyOneElem offsets
        let writeCopied (dstIndices, casted) =
            // Writing saved source elements
            state.memory.WriteArrayIndex dstAddress dstIndices dstType casted
        List.iter writeCopied toWrite

    let private copyArraySymbolic state srcAddress srcIndex srcType srcLens srcLBs dstAddress dstIndex dstType dstLens dstLBs length =
        let memory = state.memory
        let dstElemType = dstType.elemType
        let srcFromIndices = delinearizeArrayIndex srcIndex srcLens srcLBs
        let lenMinOne = sub length (makeNumber 1)
        let srcToIndices = delinearizeArrayIndex (add srcIndex lenMinOne) srcLens srcLBs
        let srcMemory = memory.ReadArrayRange srcAddress srcFromIndices srcToIndices srcType
        let casted = TypeCasting.cast srcMemory dstElemType
        let dstFromIndices = delinearizeArrayIndex dstIndex dstLens dstLBs
        let dstToIndices = delinearizeArrayIndex (add dstIndex lenMinOne) dstLens dstLBs
        memory.WriteArrayRange dstAddress dstFromIndices dstToIndices dstType casted

    let private copyArrayCommon state srcAddress srcIndex srcType dstAddress dstIndex dstType length =
        let memory = state.memory
        let srcDim = srcType.dimension
        let dstDim = dstType.dimension
        let srcLBs = List.init srcDim (fun dim -> memory.ReadLowerBound srcAddress (makeNumber dim) srcType)
        let srcLens = List.init srcDim (fun dim -> memory.ReadLength srcAddress (makeNumber dim) srcType)
        let dstLBs = List.init dstDim (fun dim -> memory.ReadLowerBound dstAddress (makeNumber dim) dstType)
        let dstLens = List.init dstDim (fun dim -> memory.ReadLength dstAddress (makeNumber dim) dstType)
        match length.term with
        | Concrete(l, _) ->
            let length = NumericUtils.ObjToInt l
            copyArrayConcrete state srcAddress srcIndex srcType srcLens srcLBs dstAddress dstIndex dstType dstLens dstLBs length
        | _ ->
            copyArraySymbolic state srcAddress srcIndex srcType srcLens srcLBs dstAddress dstIndex dstType dstLens dstLBs length

    let isSafeContextCopy srcArrayType dstArrayType =
        isSafeContextWrite srcArrayType.elemType dstArrayType.elemType

    let copyArray state srcAddress srcIndex srcType dstAddress dstIndex dstType length =
        assert(isSafeContextCopy srcType dstType)
        let memory = state.memory
        let cm = memory.ConcreteMemory
        let concreteSrcIndex = memory.TryTermToObj srcIndex
        let concreteDstIndex = memory.TryTermToObj dstIndex
        let concreteLength = memory.TryTermToObj length
        match srcAddress.term, dstAddress.term, concreteSrcIndex, concreteDstIndex, concreteLength with
        | ConcreteHeapAddress src, ConcreteHeapAddress dst, Some srcI, Some dstI, Some l when cm.Contains src && cm.Contains dst ->
            let srcI = NumericUtils.ObjToLong srcI
            let dstI = NumericUtils.ObjToLong dstI
            let l = NumericUtils.ObjToLong l
            cm.CopyArray src dst srcI dstI l
        // TODO: if src array is in concrete memory, make more efficient case: get array data and write it to dst array
        | _ -> copyArrayCommon state srcAddress srcIndex srcType dstAddress dstIndex dstType length

    let copyCharArrayToStringSymbolic (state : state) arrayAddress stringConcreteAddress startIndex arrayLength =
        let memory = state.memory
        let stringAddress = ConcreteHeapAddress stringConcreteAddress
        let stringAddress, arrayType = memory.StringArrayInfo stringAddress (Some arrayLength)
        copyArray state arrayAddress startIndex arrayType stringAddress (makeNumber 0) arrayType arrayLength
        memory.WriteClassField stringAddress Reflection.stringLengthField arrayLength

    let copyCharArrayToString (state : state) arrayAddress stringConcreteAddress startIndex length =
        let memory = state.memory
        let cm = memory.ConcreteMemory
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
            let arrayType = arrayType.CharVector
            let length = memory.ReadLength arrayAddress (makeNumber 0) arrayType
            copyCharArrayToStringSymbolic state arrayAddress stringConcreteAddress startIndex length

    let private fillArrayConcrete state arrayAddress arrayType startIndex length lbs lens castedValue =
        let offsets = List.init length id
        let copyOneElem offset =
            let linearIndex = add startIndex (makeNumber offset)
            let indices = delinearizeArrayIndex linearIndex lens lbs
            state.memory.WriteArrayIndex arrayAddress indices arrayType castedValue
        List.iter copyOneElem offsets

    let private fillArraySymbolic state arrayAddress arrayType startIndex length lbs lens castedValue =
        let lastIndex = sub length (makeNumber 1)
        let lowerBounds = delinearizeArrayIndex startIndex lens lbs
        let upperBounds = delinearizeArrayIndex lastIndex lens lbs
        state.memory.WriteArrayRange arrayAddress lowerBounds upperBounds arrayType castedValue

    let private fillArrayCommon state arrayAddress arrayType index length value =
        let memory = state.memory
        let dim = arrayType.dimension
        let castedValue = TypeCasting.cast value arrayType.elemType
        let lbs = List.init dim (fun dim -> memory.ReadLowerBound arrayAddress (makeNumber dim) arrayType)
        let lens = List.init dim (fun dim -> memory.ReadLength arrayAddress (makeNumber dim) arrayType)
        match length.term with
        | Concrete(l, _) ->
            let length = NumericUtils.ObjToInt l
            fillArrayConcrete state arrayAddress arrayType index length lbs lens castedValue
        | _ -> fillArraySymbolic state arrayAddress arrayType index length lbs lens castedValue

    let fillArray state arrayAddress arrayType index length value =
        let memory = state.memory
        let cm = memory.ConcreteMemory
        let concreteIndex = memory.TryTermToObj index
        let concreteLength = memory.TryTermToObj length
        let castedValue = TypeCasting.cast value arrayType.elemType
        let concreteValue = memory.TryTermToObj castedValue
        match arrayAddress.term, concreteIndex, concreteLength, concreteValue with
        | ConcreteHeapAddress address, Some i, Some l, Some v when cm.Contains address ->
            let i = NumericUtils.ObjToInt i
            let l = NumericUtils.ObjToInt l
            cm.FillArray address i l v
        | _ -> fillArrayCommon state arrayAddress arrayType index length value
