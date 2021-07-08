namespace VSharp.Core

open VSharp

[<AutoOpen>]
module internal Operators =

    let simplifyBinaryOperation op left right k =
        let t1 = Terms.typeOf left
        let t2 = Terms.typeOf right
        match op with
        | op when Propositional.isLogicalOperation op t1 t2 ->
            Propositional.simplifyBinaryConnective op left right k
        | op when Arithmetics.isArithmeticalOperation op t1 t2 ->
            Arithmetics.simplifyBinaryOperation op left right k
        | op when Pointers.isPointerOperation op t1 t2 ->
            Pointers.simplifyBinaryOperation op left right k
        | _ -> internalfailf "simplifyBinary of: %O %O %O" left op right

    let ksimplifyEquality x y k =
        simplifyBinaryOperation OperationType.Equal x y k

    let simplifyEquality x y =
        ksimplifyEquality x y id

    let (===) x y = ksimplifyEquality x y id
    let (!==) x y = ksimplifyEquality x y (!!)

    let simplifyUnaryOperation op arg k =
        match typeOf arg with
        | Bool -> simplifyUnaryConnective op arg k
        | Numeric(Id t) -> simplifyUnaryOperation op arg t k
        | Types.StringType -> __notImplemented__()
        | _ -> __notImplemented__()

    let simplifyOperation op args k =
        let arity = Operations.operationArity op
        match arity with
        | 1 ->
            assert(List.length args = 1)
            simplifyUnaryOperation op (List.head args) k
        | 2 ->
            assert(List.length args >= 2)
            Cps.List.reducek (fun x y k -> simplifyBinaryOperation op x y k) args k
        | _ -> internalfailf "unknown operation %O" op
