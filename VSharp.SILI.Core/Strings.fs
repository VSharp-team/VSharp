namespace VSharp.Core

open VSharp
open Arrays
open Types

module internal Strings =

    let strLength = "System.String.m_StringLength"
    let strArray = "System.String.m_FirstChar"

    let makeArrayFQL fql = addToOptionFQL fql <| StructField(strArray, ArrayType(Char, Vector))

    let makeStringOfFields metadata time length array arrayFQL fql =
        let lengthFQL = addToOptionFQL fql <| StructField(strLength, lengthType)
        let fields = Heap.ofSeq (seq [ makeKey strLength lengthFQL, { value = length; created = time; modified = time };
                                       makeKey strArray arrayFQL, { value = array; created = time; modified = time } ])
        Struct metadata fields String

    let makeConcreteStringStruct metadata time (str : string) fql =
        let length = Concrete metadata str.Length lengthType
        let arraySource = (str + "\000").ToCharArray()
        let valMaker i = makeNumber metadata arraySource.[i]
        let keyMaker mtd i = makeIndexArray metadata (fun _ -> makeIndex mtd i) 1
        let arrayFQL = makeArrayFQL fql
        let array = makeLinearConcreteArray metadata keyMaker valMaker (str.Length + 1) Char arrayFQL
        makeStringOfFields metadata time length array arrayFQL fql

    let makeStringArray metadata time length instor contents elType arrayFQL =
        let arrLength = makeNumber metadata 1 |> add metadata length
        let indexLength = makeIndexArray metadata (always length) 1
        let indexLengthKey = makePathKey arrayFQL (mkArrayIndex Char) indexLength
        let contentsWithZero = Heap.add indexLengthKey { value = makeNumber metadata '\000'; created = time; modified = time } contents
        makeArray metadata arrLength contentsWithZero instor elType arrayFQL

    let ctorOfCharArray metadata time fql = Merging.guardedErroredApply (function
        | VectorT(length, instor, contents, elType) when elType = Numeric typedefof<char> ->
            let arrayFQL = makeArrayFQL fql
            let stringArray = makeStringArray metadata time length instor contents Char arrayFQL
            makeStringOfFields metadata time length stringArray arrayFQL fql
        | t -> internalfailf "expected char array, but got %O" t)

    let length = Merging.guardedErroredApply (term >> function
        | Struct(fields, StringType) -> fields.[strLength].value
        | t -> internalfailf "expected string struct, but got %O" t)

    let private contentIsConcrete contents =
        let contentSeq = Heap.toSeq contents
        Cps.Seq.foldlk (fun acc (key, cell) k ->
            match key.key, cell.value with
            | Index(ConcreteT(x, _)), ConcreteT(y, _) -> k <| (unbox x, unbox y) :: acc
            | _ -> None)
            List.empty contentSeq Some

    let private complementArray length xs =
        let value = System.Activator.CreateInstance(typedefof<char>) |> unbox
        let indices = List.except (List.map fst xs) [0 .. length - 1]
        xs @ List.map (withSnd value) indices

    let private contentArrayToString =
        List.sortBy fst >> List.discardLast >> List.map snd >> List.toArray >> System.String

    let (|ConcreteStringArray|_|) = function
        | VectorT(ConcreteT(length, _), _, contents, Char) ->
            let contentToString = complementArray (unbox length) >> contentArrayToString
            contents |> contentIsConcrete |> Option.map contentToString
        | _ -> None

    let private makeHashOfAddress metadata = Merging.guardedErroredApply (function
        // TODO: use specific symbolicConstantSource instead of address in case of symbolic string
        | ConcreteT(:? list<int> as x, _) ->
            let hash = List.last x
            makeNumber metadata hash
        | { term = Constant _ } as hash -> { hash with metadata = metadata }
        | t -> internalfailf "expected address, but got %O" t)

    let private transformString concreteCase symbolicCase = term >> function
        | Struct(fields, StringType) ->
            match fields.[strArray].value with
            | ConcreteStringArray(string) -> concreteCase string
            | { term = Array _ } -> symbolicCase ()
            | t -> internalfailf "expected char array, but got %O" t
        | t -> internalfailf "expected string struct, but got %O" t

    let getHashCode metadata addr =
        Merging.guardedErroredApply (transformString (hash >> makeNumber metadata) (fun () -> makeHashOfAddress metadata addr))

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
