namespace VSharp

[<AutoOpen>]
module public Prelude =
    let public internalfail message = "Internal error: " + message |> failwith
    let public internalfailf format = Printf.ksprintf internalfail format
    let inline public __notImplemented__() = raise (new System.NotImplementedException())
    let inline public __unreachable__() = internalfail "unreachable branch hit!"

    let inline public toString x = x.ToString()
    let inline public join s (ss : seq<string>) = System.String.Join(s, ss)

    let inline public always x _ = x

    let inline public cons x xs = x :: xs
    let inline public optCons xs = function
        | Some x -> x::xs
        | None -> xs

    let inline public withFst x = fun y -> (x, y)
    let inline public withSnd y = fun x -> (x, y)
    let inline public makePair x y = (x, y)

    let inline fst3 (x, _, _) = x
    let inline snd3 (_, y, _) = y
    let inline thd3 (_, _, z) = z

    let inline public (|?) lhs rhs =
        if lhs = null then rhs else lhs
    let inline public (|??) lhs rhs =
        match lhs with
        | Some x -> x
        | None -> rhs
    let inline public (||??) (lhs : 'a option) (rhs : 'a Lazy) =
        match lhs with
        | Some x -> x
        | None -> rhs.Force()
