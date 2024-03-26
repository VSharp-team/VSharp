namespace VSharp.Core

open VSharp
open TypeUtils

[<AutoOpen>]
module internal Operators =

    let simplifyBinaryOperation op left right k =
        let t1 = typeOf left
        let t2 = typeOf right
        match op with
        | op when Pointers.isPointerOperation op left right ->
            Pointers.simplifyBinaryOperation op left right k
        | op when isLogicalOperation op t1 t2 ->
            simplifyBinaryConnective op left right k
        | op when isArithmeticalOperation op t1 t2 ->
            simplifyBinaryOperation op left right k
        | _ -> internalfail $"simplifyBinary of: {left} {op} {right}"

    let ksimplifyEquality x y k =
        simplifyBinaryOperation OperationType.Equal x y k

    let simplifyEquality x y =
        ksimplifyEquality x y id

    let (===) x y = ksimplifyEquality x y id
    let (!==) x y = ksimplifyEquality x y (!!)

    let simplifyUnaryOperation op arg k =
        match typeOf arg with
        | Bool -> simplifyUnaryConnective op arg k
        | Numeric t -> simplifyUnaryOperation op arg t k
        | StringType -> __notImplemented__()
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
