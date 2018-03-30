namespace VSharp.Core

open VSharp

type public 'a symbolicLambda = locationBinding -> state -> term list symbolicValue -> (statementResult * state -> 'a) -> 'a

module internal Lambdas =

    let make mtd (body : 'a symbolicLambda) typ = Concrete mtd body typ

    let (|Lambda|_|) = function
        | Concrete(lambda, t) when Types.isFunction t && (lambda :? 'a symbolicLambda) ->
            Some(Lambda(lambda :?> 'a symbolicLambda))
        | _ -> None
