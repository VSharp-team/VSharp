namespace VSharp.Core

open VSharp
open Memory

module internal Copying =

// ------------------------------ Primitives -----------------------------

    [<NoEquality;NoComparison>]
    type private symbolicArrayIndexSource =
        { lowerBound : term; upperBound : term }
        interface INonComposableSymbolicConstantSource with
            override x.SubTerms = seq[] :> term seq
            override x.Time = VectorTime.zero

    let private makeArrayIndexConstant state lowerBound upperBound =
        let source = {lowerBound = lowerBound; upperBound = upperBound}
        let constant = Constant "i" source Types.Int32
        let leftBound = simplifyLessOrEqual lowerBound constant id
        let rightBound = simplifyLessOrEqual constant upperBound id
        let pcWithBounds = PC.add (PC.add state.pc leftBound) rightBound
        let stateWithPC = { state with pc = pcWithBounds }
        constant, stateWithPC

// ------------------------------- Copying -------------------------------

    let private delinearizeArrayIndex ind lens lbs =
        let detachOne (acc, lens) lb =
            let lensProd = List.fold mul (makeNumber 1) (List.tail lens)
            let curOffset = div acc lensProd
            let curIndex = add curOffset lb
            let rest = rem acc lensProd
            curIndex, (rest, List.tail lens)
        List.mapFold detachOne (ind, lens) lbs |> fst

    let private linearizeArrayIndex (lens : term list) (lbs : term list) (indices : term list) =
        let length = List.length indices
        let attachOne acc i =
            let relOffset = sub indices.[i] lbs.[i]
            let prod acc j = mul acc lens.[j]
            let lensProd = List.fold prod (makeNumber 1) [i .. length - 1]
            let absOffset = mul relOffset lensProd
            add acc absOffset
        List.fold attachOne (makeNumber 0) [0 .. length - 1]

    let private copyArrayConcrete state srcAddress srcIndex srcType srcLens srcLBs dstAddress dstIndex dstType dstLens dstLBs length =
        let dstElemType = fst3 dstType
        let offsets = List.init length id
        let copyOneElem state offset =
            let srcIndex = add srcIndex (makeNumber offset)
            let srcIndices = delinearizeArrayIndex srcIndex srcLens srcLBs
            let srcElem = readArrayIndex state srcAddress srcIndices srcType
            let casted = TypeCasting.cast srcElem dstElemType
            let dstIndex = add dstIndex (makeNumber offset)
            let dstIndices = delinearizeArrayIndex dstIndex dstLens dstLBs
            writeArrayIndex state dstAddress dstIndices dstType casted
        List.fold copyOneElem state offsets

    let private copyArraySymbolic state srcAddress srcIndex srcType srcLens srcLBs dstAddress dstIndex dstType dstLens dstLBs length =
        let dstElemType = fst3 dstType
        let constant, stateWithPC = makeArrayIndexConstant state (makeNumber 0) (sub length (makeNumber 1))
        let srcIndices = delinearizeArrayIndex (add srcIndex constant) srcLens srcLBs
        let srcElem = readArrayIndex stateWithPC srcAddress srcIndices srcType
        let casted = TypeCasting.cast srcElem dstElemType
        let dstIndices = delinearizeArrayIndex (add dstIndex constant) dstLens dstLBs
        writeArrayIndex stateWithPC dstAddress dstIndices dstType casted

    // TODO: consider the case of overlapping src and dest arrays
    let copyArray state srcAddress srcIndex (_, srcDim, _ as srcType) dstAddress dstIndex dstType length =
        let dstDim = snd3 dstType
        let srcLBs = List.init srcDim (fun dim -> readLowerBound state srcAddress (makeNumber dim) srcType)
        let srcLens = List.init srcDim (fun dim -> readLength state srcAddress (makeNumber dim) srcType)
        let dstLBs = List.init dstDim (fun dim -> readLowerBound state dstAddress (makeNumber dim) dstType)
        let dstLens = List.init dstDim (fun dim -> readLength state dstAddress (makeNumber dim) dstType)
        match length.term with
        | Concrete(length, _) ->
            let length = length :?> int
            copyArrayConcrete state srcAddress srcIndex srcType srcLens srcLBs dstAddress dstIndex dstType dstLens dstLBs length
        | _ -> copyArraySymbolic state srcAddress srcIndex srcType srcLens srcLBs dstAddress dstIndex dstType dstLens dstLBs length

    let copyCharArrayToString state arrayAddress dstAddress =
        let arrayType = (Types.Char, 1, true)
        let length = readLength state arrayAddress (makeNumber 0) arrayType
        let lengthPlus1 = add length (makeNumber 1)
        let dstAddress = ConcreteHeapAddress dstAddress
        let state = copyArray state arrayAddress (makeNumber 0) arrayType dstAddress (makeNumber 0) arrayType length
        let state = writeLength state dstAddress (makeNumber 0) arrayType lengthPlus1
        let state = writeArrayIndex state dstAddress [length] arrayType (Concrete '\000' Types.Char)
        let state = writeClassField state dstAddress Reflection.stringLengthField length
        state

    let private fillArrayConcrete state arrayAddress arrayType startIndex length lbs lens castedValue =
        let offsets = List.init length id
        let copyOneElem state offset =
            let linearIndex = add startIndex (makeNumber offset)
            let indices = delinearizeArrayIndex linearIndex lens lbs
            writeArrayIndex state arrayAddress indices arrayType castedValue
        List.fold copyOneElem state offsets

    let private fillArraySymbolic state arrayAddress arrayType startIndex length lbs lens castedValue =
        let constant, stateWithPC = makeArrayIndexConstant state (makeNumber 0) (sub length (makeNumber 1))
        let indices = delinearizeArrayIndex (add startIndex constant) lens lbs
        writeArrayIndex stateWithPC arrayAddress indices arrayType castedValue

    let fillArray state arrayAddress (elemType, dim, _ as arrayType) index length value =
        let lbs = List.init dim (fun dim -> readLowerBound state arrayAddress (makeNumber dim) arrayType)
        let lens = List.init dim (fun dim -> readLength state arrayAddress (makeNumber dim) arrayType)
        let castedValue = TypeCasting.cast value elemType
        match length.term with
        | Concrete(length, _) ->
            let length = length :?> int
            fillArrayConcrete state arrayAddress arrayType index length lbs lens castedValue
        | _ -> fillArraySymbolic state arrayAddress arrayType index length lbs lens castedValue
