﻿namespace VSharp

[<AutoOpen>]
module public Prelude =
    let public __notImplemented__() = raise (new System.NotImplementedException())
    let public internalfail message = "Internal error: " + message |> failwith
    let public internalfailf format = Printf.ksprintf internalfail format

    let public toString x = x.ToString()
    let public apply f x = f x
    let public join s (ss : seq<string>) = System.String.Join(s, ss)

    let public id1 x _ = x
    let public id2 _ x = x
    let public always x = (fun _ -> x)

    let public cons x xs = x :: xs

    let public withFst x = fun y -> (x, y)
    let public withSnd y = fun x -> (x, y)
    let public makePair x y = (x, y)

    let fst3 (x, _, _) = x
    let snd3 (_, y, _) = y
    let thd3 (_, _, z) = z

    let public (|SeqNode|SeqEmpty|) s =
            if Seq.isEmpty s then SeqEmpty
            else SeqNode ((Seq.head s), Seq.tail s)
