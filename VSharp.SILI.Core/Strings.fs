namespace VSharp.Core

open VSharp
open Types

module internal Strings =

    let makeString (length : int) str timestamp =
        let lengthTermType = Numeric typeof<int>
        let fields : symbolicHeap =
            Heap.ofSeq (seq [ makeStringKey "System.String.m_StringLength", { value = Concrete Metadata.empty length lengthTermType; created = timestamp; modified = timestamp; typ = lengthTermType };
            makeStringKey "System.String.m_FirstChar", { value = Concrete Metadata.empty str String; created = timestamp; modified = timestamp; typ = String }])
        Struct Metadata.empty fields String

    let simplifyEquality mtd x y =
        match x.term, y.term with
        | Concrete(x, StringType), Concrete(y, StringType) -> makeBool ((x :?> string) = (y :?> string)) mtd
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
