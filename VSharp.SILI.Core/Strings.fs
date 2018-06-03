namespace VSharp.Core

open VSharp
open Arrays
open Types

module internal Strings =

    let makeString metadata time (str : string) =
        let fields =
            let stringTermLength = Concrete metadata str.Length lengthTermType
            let arraySource = (str + "\000").ToCharArray()
            let valMaker i = makeNumber arraySource.[i] metadata
            let keyMaker i mtd = makeIntegerArray metadata (fun _ -> makeNumber i mtd) 1
            let array = makeLinearConcreteArray metadata keyMaker valMaker (str.Length + 1) (Numeric typedefof<char>)
            Heap.ofSeq (seq [ makeStringKey "System.String.m_StringLength", { value = stringTermLength; created = time; modified = time; typ = lengthTermType };
                              makeStringKey "System.String.m_FirstChar", { value = array; created = time; modified = time; typ = String } ])
        Struct metadata fields String

    let simplifyEquality mtd x y =
        match x.term, y.term with
        | Concrete(x, StringType), Concrete(y, StringType) -> makeBool ((x :?> string) = (y :?> string)) mtd
        | Struct(fieldsOfX, StringType), Struct(fieldsOfY, StringType) ->
            let str1Len = fieldsOfX.[makeStringKey "System.String.m_StringLength"].value
            let str2Len = fieldsOfY.[makeStringKey "System.String.m_StringLength"].value
            let str1Arr = fieldsOfX.[makeStringKey "System.String.m_FirstChar"].value
            let str2Arr = fieldsOfY.[makeStringKey "System.String.m_FirstChar"].value
            simplifyEqual mtd str1Len str2Len (fun lengthEq ->
            simplifyAnd mtd lengthEq (Arrays.equalsArrayIndices mtd str1Arr str2Arr) id)
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
