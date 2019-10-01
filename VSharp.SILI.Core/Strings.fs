namespace VSharp.Core

open VSharp
open Arrays
open Types

module internal Strings =

    let strLength = "System.String.m_StringLength"
    let strArray = "System.String.m_FirstChar"

    let makeArrayFQL fql = addToOptionFQL fql <| BlockField(strArray, ArrayType(Char, Vector))
    let makeLengthFQL fql = addToOptionFQL fql <| BlockField(strLength, lengthType)

    let makeStringOfFields metadata length array arrayFQL fql =
        let lengthFQL = makeLengthFQL fql
        let fields = Heap.ofSeq (seq [ makeKey strLength lengthFQL lengthType, length;
                                       makeKey strArray arrayFQL (ArrayType(Char, Vector)), array])
        Class metadata fields

    let makeConcreteStringStruct metadata (str : string) fql =
        let length = Concrete metadata str.Length lengthType
        let arraySource = (str + "\000").ToCharArray()
        let valMaker i = makeNumber metadata arraySource.[i]
        let keyMaker mtd i = makeIndexArray metadata (fun _ -> makeIndex mtd i) 1
        let arrayFQL = makeArrayFQL fql
        let array = makeLinearConcreteArray metadata keyMaker valMaker (str.Length + 1) Char arrayFQL
        makeStringOfFields metadata length array arrayFQL fql

    let makeStringArray metadata length instor contents arrayFQL =
        let arrLength = makeNumber metadata 1 |> add metadata length
        let indexLength = makeIndexArray metadata (always length) 1
        let indexLengthKey = makePathKey arrayFQL (mkArrayIndex Char) indexLength Char
        let contentsWithZero = Heap.add indexLengthKey (makeNumber metadata '\000') contents
        makeArray metadata arrLength contentsWithZero instor arrayFQL

    let ctorOfCharArray metadata fql = Merging.guardedErroredApply (function
        | VectorT(length, instor, contents) ->
            let arrayFQL = makeArrayFQL fql
            let stringArray = makeStringArray metadata length instor contents arrayFQL
            makeStringOfFields metadata length stringArray arrayFQL fql
        | t -> internalfailf "expected char array, but got %O" t)

    let length = Merging.guardedErroredApply (term >> function
        | Class fields -> fields.[strLength]
        | t -> internalfailf "expected string struct, but got %O" t)

    let simplifyStructEq mtd x y =
        match x.term, y.term with
        | Class fieldsOfX, Class fieldsOfY ->
            let str1Len = fieldsOfX.[strLength]
            let str2Len = fieldsOfY.[strLength]
            let str1Arr = fieldsOfX.[strArray]
            let str2Arr = fieldsOfY.[strArray]
            simplifyEqual mtd str1Len str2Len (fun lengthEq ->
            simplifyAnd mtd lengthEq (Arrays.equals mtd str1Arr str2Arr) id)
        | _ -> internalfailf "expected string struct and string struct, but got %O and %O" x y

    let simplifyConcatenation mtd x y =
        // TODO: implement concatenation
        Terms.makeBinary OperationType.Add x y false String mtd

    let simplifyOperation mtd op x y =
        match op with
        | OperationType.Add -> simplifyConcatenation mtd x y
        | OperationType.Equal -> simplifyStructEq mtd x y
        | OperationType.NotEqual -> !! (simplifyStructEq mtd x y)
        | _ -> __notImplemented__()

    let isStringOperation op t1 t2 =
        isString t1 && isString t2 &&
        match op with
        | OperationType.Add
        | OperationType.Equal
        | OperationType.NotEqual -> true
        | _ -> false
