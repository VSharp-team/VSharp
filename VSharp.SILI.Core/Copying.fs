namespace VSharp.Core

open VSharp
open Memory

module internal Copying =

// ------------------------------ Primitives -----------------------------

    [<NoEquality;NoComparison>]
    type private symbolicArrayIndexSource =
        { lowerBound : term; upperBound : term }
        interface INonComposableSymbolicConstantSource with
            override x.SubTerms = seq [] :> term seq
            override x.Time = VectorTime.zero
            override x.TypeOfLocation = typeof<int>

    let private makeArrayIndexConstant state lowerBound upperBound =
        let source = {lowerBound = lowerBound; upperBound = upperBound}
        let constant = Constant "i" source typeof<int32>
        let leftBound = simplifyLessOrEqual lowerBound constant id
        let rightBound = simplifyLessOrEqual constant upperBound id
        let pcWithBounds = PC.add (PC.add state.pc leftBound) rightBound
        state.pc <- pcWithBounds
        constant

// ------------------------------- Copying -------------------------------

    // TODO: add heuristic for concrete memory
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
            writeArrayIndex state dstAddress dstIndices dstType casted
        List.iter copyOneElem offsets

    let private copyArraySymbolic state srcAddress srcIndex srcType srcLens srcLBs dstAddress dstIndex dstType dstLens dstLBs length =
        let dstElemType = fst3 dstType
        let constant = makeArrayIndexConstant state (makeNumber 0) (sub length (makeNumber 1))
        let srcIndices = delinearizeArrayIndex (add srcIndex constant) srcLens srcLBs
        let srcElem = readArrayIndex state srcAddress srcIndices srcType
        let casted = TypeCasting.cast srcElem dstElemType
        let dstIndices = delinearizeArrayIndex (add dstIndex constant) dstLens dstLBs
        writeArrayIndex state dstAddress dstIndices dstType casted

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

    let copyCharArrayToString (state : state) arrayAddress stringConcreteAddress =
        let cm = state.concreteMemory
        match arrayAddress.term with
        | ConcreteHeapAddress concreteAddress when ConcreteMemory.contains cm concreteAddress ->
            ConcreteMemory.copyCharArrayToString state concreteAddress stringConcreteAddress
        | _ ->
            let arrayType = (typeof<char>, 1, true)
            let length = readLength state arrayAddress (makeNumber 0) arrayType
            let lengthPlus1 = add length (makeNumber 1)
            let stringAddress = ConcreteHeapAddress stringConcreteAddress
            copyArray state arrayAddress (makeNumber 0) arrayType stringAddress (makeNumber 0) arrayType length
            writeLengthSymbolic state stringAddress (makeNumber 0) arrayType lengthPlus1
            writeArrayIndex state stringAddress [length] arrayType (Concrete '\000' typeof<char>)
            writeClassField state stringAddress Reflection.stringLengthField length

    // TODO: add heuristic for concrete memory
    let private fillArrayConcrete state arrayAddress arrayType startIndex length lbs lens castedValue =
        let offsets = List.init length id
        let copyOneElem offset =
            let linearIndex = add startIndex (makeNumber offset)
            let indices = delinearizeArrayIndex linearIndex lens lbs
            writeArrayIndex state arrayAddress indices arrayType castedValue
        List.iter copyOneElem offsets

    let private fillArraySymbolic state arrayAddress arrayType startIndex length lbs lens castedValue =
        let constant = makeArrayIndexConstant state (makeNumber 0) (sub length (makeNumber 1))
        let indices = delinearizeArrayIndex (add startIndex constant) lens lbs
        writeArrayIndex state arrayAddress indices arrayType castedValue

    let fillArray state arrayAddress (elemType, dim, _ as arrayType) index length value =
        let lbs = List.init dim (fun dim -> readLowerBound state arrayAddress (makeNumber dim) arrayType)
        let lens = List.init dim (fun dim -> readLength state arrayAddress (makeNumber dim) arrayType)
        let castedValue = TypeCasting.cast value elemType
        match length.term with
        | Concrete(length, _) ->
            let length = length :?> int
            fillArrayConcrete state arrayAddress arrayType index length lbs lens castedValue
        | _ -> fillArraySymbolic state arrayAddress arrayType index length lbs lens castedValue
