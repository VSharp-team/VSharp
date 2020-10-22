namespace VSharp.Interpreter.IL
open VSharp
open VSharp.Core

type public 'a symbolicLambda = state -> (state list -> 'a) -> 'a

module internal Lambdas =
    let make (body : 'a symbolicLambda) typ (k : term -> 'a) = Concrete body typ |> k

    let private (|Lambda|_|) = function
        | Concrete(lambda, _) when (lambda :? 'a symbolicLambda) ->
            Some(Lambda(lambda :?> 'a symbolicLambda))
        | _ -> None

    let rec invokeDelegate (state : state) deleg k =
        let callDelegate (state : state) deleg k =
            match deleg.term with
            | Lambda(lambda) -> lambda state k
            | _ -> internalfailf "Invalid delegate term %O" deleg
        let deleg = Memory.ReadDelegate state deleg
        InstructionsSet.GuardedApplyForState state deleg callDelegate k
