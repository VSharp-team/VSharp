namespace VSharp.Interpreter.IL
open VSharp
open VSharp.Core

type public 'a symbolicLambda = cilState -> term list symbolicValue -> (cilState list -> 'a) -> 'a

module internal Lambdas =
    let make (body : 'a symbolicLambda) typ (k : term -> 'a) = Concrete body typ |> k

    let private (|Lambda|_|) = function
        | Concrete(lambda, _) when (lambda :? 'a symbolicLambda) ->
            Some(Lambda(lambda :?> 'a symbolicLambda))
        | _ -> None

    let rec invokeDelegate args (cilState : cilState) deleg k =
        let callDelegate (cilState : cilState) deleg k =
            match deleg.term with
            | Lambda(lambda) -> lambda cilState (Specified args) k
            | _ -> internalfailf "Invalid delegate term %O" deleg
        let term = Memory.ReadDelegate cilState.state deleg
        InstructionsSet.GuardedApply cilState term callDelegate k
