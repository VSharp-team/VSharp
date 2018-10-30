namespace VSharp.Core

open VSharp
open Arrays
open Types

module internal Strings =

    let strLength = "System.String.m_StringLength"
    let strArray = "System.String.m_FirstChar"

    let makeString metadata time (str : string) fql =
        let fields =
            let stringTermLength = Concrete metadata str.Length lengthTermType
            let arraySource = (str + "\000").ToCharArray()
            let valMaker i = makeNumber arraySource.[i] metadata
            let keyMaker i mtd = makeIndexArray metadata (fun _ -> makeNumber i mtd) 1
            let arrayFQL = addToOptionFQL fql <| StructField(strArray, ArrayType(Char, Vector))
            let lengthFQL = addToOptionFQL fql <| StructField(strLength, Arrays.lengthTermType)
            let array = makeLinearConcreteArray metadata keyMaker valMaker (str.Length + 1) Char arrayFQL
            Heap.ofSeq (seq [ makeKey strLength lengthFQL, { value = stringTermLength; created = time; modified = time };
                              makeKey strArray arrayFQL, { value = array; created = time; modified = time } ])
        Struct metadata fields String

    let simplifyEquality mtd x y =
        match x.term, y.term with
        | Concrete(x, StringType), Concrete(y, StringType) -> makeBool ((x :?> string) = (y :?> string)) mtd
        | Struct(fieldsOfX, StringType), Struct(fieldsOfY, StringType) ->
            let str1Len = fieldsOfX.[strLength].value
            let str2Len = fieldsOfY.[strLength].value
            let str1Arr = fieldsOfX.[strArray].value
            let str2Arr = fieldsOfY.[strArray].value
            simplifyEqual mtd str1Len str2Len (fun lengthEq ->
            simplifyAnd mtd lengthEq (Arrays.equals mtd str1Arr str2Arr) id)
        | _ -> __notImplemented__()

    let simplifyConcatenation mtd x y =
        match x.term, y.term with
        | Concrete(xval, _), Concrete(yval, _) ->
            let mtd' = Metadata.combine3 mtd x.metadata y.metadata
            makeConcreteString (VSharp.CSharpUtils.Calculator.Add(xval, yval, typedefof<string>) :?> string) mtd'
        | _ -> Terms.makeBinary OperationType.Add x y false String mtd

    let simplifyOperation mtd op x y =
        match op with
        | OperationType.Add -> simplifyConcatenation mtd x y
        | OperationType.Equal -> simplifyEquality mtd x y
        | OperationType.NotEqual -> !! (simplifyEquality mtd x y)
        | _ -> __notImplemented__()

    let isStringOperation op t1 t2 =
        isString t1 && isString t2 &&
        match op with
        | OperationType.Add
        | OperationType.Equal
        | OperationType.NotEqual -> true
        | _ -> false
