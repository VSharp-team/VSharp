namespace VSharp.Core

open VSharp
open Memory

module internal Copying =

// ------------------------------ Primitives -----------------------------

    [<NoEquality;NoComparison>]
    type private symbolicArrayIndexSource () = // TODO: how to compare constants? need more info here? #do
        interface INonComposableSymbolicConstantSource with
            override x.SubTerms = seq[] :> term seq
            override x.Time = VectorTime.zero

    let makeArrayIndexConstant () =
        let source = symbolicArrayIndexSource() :> ISymbolicConstantSource
        Constant "i" source Types.Int32 // TODO: make better name of constant #do

// ------------------------------- Copying -------------------------------

    let private delinearizeArrayIndex ind lens lbs = // TODO: check #do
        let mapper (acc, lens) lb =
            let lensProd = List.fold mul (makeNumber 1) (List.tail lens)
            let curOffset = div acc lensProd
            let curIndex = add curOffset lb
            let rest = rem acc lensProd // TODO: (mul and sub) or rem
            curIndex, (rest, List.tail lens)
        List.mapFold mapper (ind, lens) lbs |> fst

    let private linearizeArrayIndex (lens : term list) (lbs : term list) (indices : term list) =
        let length = List.length indices
        let folder acc i =
            let a = indices.[i]
            let lb = lbs.[i]
            let offset = sub a lb
            let prod acc j =
                mul acc lens.[j]
            let lensProd = List.fold prod (makeNumber 1) [i .. length - 1]
            let kek = mul offset lensProd
            add acc kek
        List.fold folder (makeNumber 0) [0 .. length - 1]

    let copyArrayConcrete state srcAddress srcIndex srcType srcLens srcLBs dstAddress dstIndex dstElemType dstLens dstLBs length =
        let offsets = List.init length id
        let copyOneElem state offset =
            let srcIndex = add srcIndex (makeNumber offset)
            let srcIndices = delinearizeArrayIndex srcIndex srcLens srcLBs
            let srcElem = readArrayIndex state srcAddress srcIndices srcType
            let casted = TypeCasting.cast srcElem dstElemType
            let dstIndex = add dstIndex (makeNumber offset)
            let dstIndices = delinearizeArrayIndex dstIndex dstLens dstLBs
            writeArrayIndex state dstAddress dstIndices srcType casted
        List.fold copyOneElem state offsets

    let copyArraySymbolic state srcAddress srcIndex srcType srcLens srcLBs dstAddress dstIndex dstElemType dstLens dstLBs length =
        let constant = makeArrayIndexConstant ()
        let leftBound = simplifyLessOrEqual (makeNumber 0) constant id
        let rightBound = simplifyLessOrEqual constant (sub length (makeNumber 1)) id
        let pcWithBounds = PC.add (PC.add state.pc leftBound) rightBound
        let stateWithPC = { state with pc = pcWithBounds }
        let srcIndices = delinearizeArrayIndex (add srcIndex constant) srcLens srcLBs
        let srcElem = readArrayIndex stateWithPC srcAddress srcIndices srcType
        let casted = TypeCasting.cast srcElem dstElemType
        let dstIndices = delinearizeArrayIndex (add dstIndex constant) dstLens dstLBs
        writeArrayIndex stateWithPC dstAddress dstIndices srcType casted

    let copyArray state srcAddress srcIndex ((_, srcDim, _) as srcType) dstAddress dstIndex ((dstElemType, dstDim, _) as dstType) length =
        // TODO: consider the case of overlapping src and dest arrays. Done? #do
        let srcLBs = List.init srcDim (fun dim -> readLowerBound state srcAddress (makeNumber dim) srcType)
        let srcLens = List.init srcDim (fun dim -> readLength state srcAddress (makeNumber dim) srcType)
        let dstLBs = List.init dstDim (fun dim -> readLowerBound state dstAddress (makeNumber dim) dstType)
        let dstLens = List.init dstDim (fun dim -> readLength state dstAddress (makeNumber dim) dstType)
        match length.term with
        | Concrete(length, _) ->
            let length = length :?> int
            copyArrayConcrete state srcAddress srcIndex srcType srcLens srcLBs dstAddress dstIndex dstElemType dstLens dstLBs length
        | _ -> copyArraySymbolic state srcAddress srcIndex srcType srcLens srcLBs dstAddress dstIndex dstElemType dstLens dstLBs length

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
