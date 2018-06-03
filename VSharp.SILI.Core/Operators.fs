namespace VSharp.Core

open VSharp

[<AutoOpen>]
module internal Operators =

    let rec refToInt term =
        match term.term with
        | Error _ -> term
        | Concrete(null, _) -> Concrete term.metadata 0 Types.pointerType
        | HeapRef(((addr, _), _), _, _, _) -> addr
        | Union gvs -> Merging.guardedMap refToInt gvs
        | _ -> term

    let simplifyBinaryOperation mtd op isChecked state (t: System.Type) left right k =
        let t1 = Terms.typeOf left
        let t2 = Terms.typeOf right
        match op with
        | _ when Types.isBottom t1 -> k (left, state)
        | _ when Types.isBottom t2 -> k (right, state)
        | op when Propositional.isLogicalOperation op t1 t2 ->
            Propositional.simplifyBinaryConnective mtd op left right (withSnd state >> k)
        | op when Arithmetics.isArithmeticalOperation op t1 t2 ->
            Arithmetics.simplifyBinaryOperation mtd op state left right isChecked k
        | op when Strings.isStringOperation op t1 t2 ->
            Strings.simplifyOperation mtd op left right |> (withSnd state >> k)
        | op when Pointers.isPointerOperation op t1 t2 ->
            Pointers.simplifyBinaryOperation mtd op state left right t k
        | _ -> __notImplemented__()

    let ksimplifyEquality mtd x y k =
        simplifyBinaryOperation mtd OperationType.Equal false State.empty typeof<bool> x y (fst >> k)

    let simplifyEquality mtd x y =
        ksimplifyEquality mtd x y id

    let (===) x y = ksimplifyEquality Metadata.empty x y id
    let (!==) x y = ksimplifyEquality Metadata.empty x y (!!)

    let simplifyUnaryOperation mtd op isChecked state t arg k =
        match t with
        | Bool -> Propositional.simplifyUnaryConnective mtd op arg (withSnd state >> k)
        | Numeric t -> Arithmetics.simplifyUnaryOperation mtd op state arg isChecked t k
        | Types.StringType -> __notImplemented__()
        | _ -> __notImplemented__()

    let simplifyOperation mtd op isChecked t args k =
        let arity = Operations.operationArity op
        match arity with
        | 1 ->
            assert(List.length args = 1)
            simplifyUnaryOperation mtd op isChecked State.empty t (List.head args) (fst >> k)
        | 2 ->
            assert(List.length args >= 2)
            let dnt = Types.toDotNetType t
            Cps.List.reducek (fun x y k -> simplifyBinaryOperation mtd op isChecked State.empty dnt x y (fst >> k)) args k
        | _ -> internalfailf "unknown operation %O" op
