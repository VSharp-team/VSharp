namespace VSharp.Core

open VSharp

[<AutoOpen>]
module internal Operators =

    let simplifyBinaryOperation mtd op left right k =
        let t1 = Terms.typeOf left
        let t2 = Terms.typeOf right
        match op with
        | op when Propositional.isLogicalOperation op t1 t2 ->
            Propositional.simplifyBinaryConnective mtd op left right k
        | op when Arithmetics.isArithmeticalOperation op t1 t2 ->
            Arithmetics.simplifyBinaryOperation mtd op left right k
        | op when Pointers.isPointerOperation op t1 t2 ->
            Pointers.simplifyBinaryOperation mtd op left right k
        | _ -> internalfailf "simplifyBinary of: %O %O %O" left op right

    let ksimplifyEquality mtd x y k =
        simplifyBinaryOperation mtd OperationType.Equal x y k

    let simplifyEquality mtd x y =
        ksimplifyEquality mtd x y id

    let (===) x y = ksimplifyEquality Metadata.empty x y id
    let (!==) x y = ksimplifyEquality Metadata.empty x y (!!)

    let simplifyUnaryOperation mtd op t arg k =
        match t with
        | Bool -> Propositional.simplifyUnaryConnective mtd op arg k
        | Numeric(Id t) -> Arithmetics.simplifyUnaryOperation mtd op arg t k
        | Types.StringType -> __notImplemented__()
        | _ -> __notImplemented__()

    let simplifyOperation mtd op t args k =
        let arity = Operations.operationArity op
        match arity with
        | 1 ->
            assert(List.length args = 1)
            simplifyUnaryOperation mtd op t (List.head args) k
        | 2 ->
            assert(List.length args >= 2)
            Cps.List.reducek (fun x y k -> simplifyBinaryOperation mtd op x y k) args k
        | _ -> internalfailf "unknown operation %O" op
