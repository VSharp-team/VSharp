namespace VSharp

open JetBrains.Decompiler.Ast

module internal Strings =

    let MakeString length str timestamp =
        let fields : Heap<Term,Term> =
            Heap.ofSeq (seq [ MakeStringKey "System.String.m_StringLength", (Concrete length (Numeric typedefof<int>) Metadata.empty, timestamp, timestamp);
            MakeStringKey "System.String.m_FirstChar", (Concrete str VSharp.String Metadata.empty, timestamp, timestamp) ])
        in
        Struct fields VSharp.String Metadata.empty

    let internal simplifyEquality mtd x y =
        match x.term, y.term with
        | Concrete(x, String), Concrete(y, String) -> MakeBool ((x :?> string) = (y :?> string)) mtd
        | _ -> __notImplemented__()

    let internal simplifyConcatenation mtd x y =
        match x.term, y.term with
        | Concrete(xval, _), Concrete(yval, _) ->
            let mtd' = Metadata.combine3 mtd x.metadata y.metadata in
            MakeConcreteString (VSharp.CSharpUtils.Calculator.Add(xval, yval, typedefof<string>) :?> string) mtd'
        | _ -> Terms.MakeBinary OperationType.Add x y false String mtd

    let internal simplifyOperation mtd op x y =
        match op with
        | OperationType.Add -> simplifyConcatenation mtd x y
        | OperationType.Equal -> simplifyEquality mtd x y
        | OperationType.NotEqual -> !! (simplifyEquality mtd x y)
        | OperationType.NullCoalescing
        | _ -> __notImplemented__()

    let internal isStringOperation op t1 t2 =
        Types.IsString t1 && Types.IsString t2 &&
        match op with
        | OperationType.Add
        | OperationType.Equal
        | OperationType.NotEqual
        | OperationType.NullCoalescing -> true
        | _ -> false
