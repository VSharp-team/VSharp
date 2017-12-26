namespace VSharp

[<AutoOpen>]
module public Prelude =
    let public internalfail message = "Internal error: " + message |> failwith
    let public internalfailf format = Printf.ksprintf internalfail format
    let public __notImplemented__() = raise (new System.NotImplementedException())
    let public __unreachable__() = internalfail "unreachable branch hit!"

    let inline public toString x = x.ToString()
    let inline public join s (ss : seq<string>) = System.String.Join(s, ss)

    let public always x _ = x

    let inline public cons x xs = x :: xs

    let public withFst x = fun y -> (x, y)
    let public withSnd y = fun x -> (x, y)
    let inline public makePair x y = (x, y)

    let inline public fst3 (x, _, _) = x
    let inline public snd3 (_, y, _) = y
    let inline public thd3 (_, _, z) = z

    let inline public (|?) lhs rhs = if lhs = null then rhs else lhs
    let inline public (|??) lhs rhs = Option.defaultValue rhs lhs

    let inline (&&&&) a b = (&&&) a b
    let inline (||||) a b = (|||) a b