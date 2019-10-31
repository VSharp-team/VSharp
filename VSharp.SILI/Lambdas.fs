namespace VSharp.Interpreter.IL
open VSharp
open VSharp.Core

type public 'a symbolicLambda = locationBinding -> cilState -> term list symbolicValue -> ((term * cilState) list -> 'a) -> 'a

module internal Lambdas =
    let make (body : 'a symbolicLambda) typ = Concrete body typ

    let private (|Lambda|_|) = function
        | Concrete(lambda, _) when (lambda :? 'a symbolicLambda) ->
            Some(Lambda(lambda :?> 'a symbolicLambda))
        | _ -> None

    let rec invokeDelegate (caller : locationBinding) args (cilState : cilState) deleg k =
        let callDelegate (cilState : cilState) deleg k =
            match deleg.term with
            | Lambda(lambda) -> lambda caller cilState (Specified args) k
            | _ -> __unreachable__()
        let term, state = Memory.Dereference cilState.state deleg
        InstructionsSet.GuardedApply {cilState with state = state} term callDelegate k
