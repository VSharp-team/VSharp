namespace VSharp.Core

open VSharp

module internal Strings =

    let makeString (length : int) str timestamp =
        let fields : symbolicHeap =
            Heap.ofSeq (seq [ makeStringKey "System.String.m_StringLength", { value = Concrete Metadata.empty length (Numeric typedefof<int>); created = timestamp; modified = timestamp };
            makeStringKey "System.String.m_FirstChar", { value = Concrete Metadata.empty str Core.String; created = timestamp; modified = timestamp }])
        Struct Metadata.empty fields Core.String

    let simplifyEquality mtd x y =
        match x.term, y.term with
        | Concrete(x, String), Concrete(y, String) -> makeBool ((x :?> string) = (y :?> string)) mtd
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
        Types.isString t1 && Types.isString t2 &&
        match op with
        | OperationType.Add
        | OperationType.Equal
        | OperationType.NotEqual -> true
        | _ -> false
