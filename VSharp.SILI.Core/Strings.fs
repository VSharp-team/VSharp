namespace VSharp.Core

open VSharp
open Arrays
open Types

module internal Strings =

    let strLength = "System.String.m_StringLength"
    let strArray = "System.String.m_FirstChar"

    let makeArrayFQL fql = addToOptionFQL fql <| StructField(strArray, ArrayType(Char, Vector))

    let makeStringOfFields metadata time length array arrayFQL fql =
        let lengthFQL = addToOptionFQL fql <| StructField(strLength, Arrays.lengthTermType)
        let fields = Heap.ofSeq (seq [ makeKey strLength lengthFQL, { value = length; created = time; modified = time };
                                       makeKey strArray arrayFQL, { value = array; created = time; modified = time } ])
        Struct metadata fields String

    let makeConcreteStringStruct metadata time (str : string) fql =
        let length = Concrete metadata str.Length lengthTermType
        let arraySource = (str + "\000").ToCharArray()
        let valMaker i = makeNumber arraySource.[i] metadata
        let keyMaker i mtd = makeIndexArray metadata (fun _ -> makeNumber i mtd) 1
        let arrayFQL = makeArrayFQL fql
        let array = makeLinearConcreteArray metadata keyMaker valMaker (str.Length + 1) Char arrayFQL
        makeStringOfFields metadata time length array arrayFQL fql

    let makeStringArray metadata time length instor contents elType arrayFQL =
        let arrLength = makeNumber 1 metadata |> add metadata length
        let indexLength = makeIndexArray metadata (always length) 1
        let indexLengthKey = makePathKey arrayFQL (mkArrayIndex Char) indexLength
        let contentsWithZero = Heap.add indexLengthKey { value = makeNumber '\000' metadata; created = time; modified = time } contents
        makeArray metadata arrLength contentsWithZero instor elType arrayFQL

    let simplifyStructEq mtd x y =
        match x.term, y.term with
        | Struct(fieldsOfX, StringType), Struct(fieldsOfY, StringType) ->
            let str1Len = fieldsOfX.[strLength].value
            let str2Len = fieldsOfY.[strLength].value
            let str1Arr = fieldsOfX.[strArray].value
            let str2Arr = fieldsOfY.[strArray].value
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
