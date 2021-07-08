namespace VSharp.Interpreter.IL
open VSharp
open VSharp.Core

type public 'a symbolicLambda = cilState -> (cilState list -> 'a) -> 'a

module internal Lambdas =
    let make methodWithThis typ = Concrete methodWithThis typ

    let private (|Lambda|_|) = function
        | Concrete(lambda, _) when (lambda :? 'a symbolicLambda) ->
            Some(Lambda(lambda :?> 'a symbolicLambda))
        | _ -> None

    let rec invokeDelegate (cilState : cilState) deleg k =
        let callDelegate (cilState : cilState) deleg k =
            match deleg.term with
            | Lambda(lambda) -> lambda cilState k
            | _ -> internalfailf "Invalid delegate term %O" deleg
        let deleg = Memory.ReadDelegate cilState.state deleg
        CilStateOperations.GuardedApplyCIL cilState deleg callDelegate k
