namespace VSharp.Core

open VSharp

type public SymbolicLambda<'a> = LocationBinding -> State -> Term list SymbolicValue -> (StatementResult * State -> 'a) -> 'a

module internal Lambdas =

    let make mtd (body : SymbolicLambda<'a>) typ = Concrete mtd body typ

    let (|Lambda|_|) = function
        | Concrete(lambda, t) when Types.IsFunction t && (lambda :? SymbolicLambda<'a>) ->
            Some(Lambda(lambda :?> SymbolicLambda<'a>))
        | _ -> None
