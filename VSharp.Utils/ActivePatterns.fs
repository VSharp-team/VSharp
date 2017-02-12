namespace VSharp

[<AutoOpen>]
module public ActivePatterns =
    let public (|SeqNode|SeqEmpty|) s =
            if Seq.isEmpty s then SeqEmpty
            else SeqNode ((Seq.head s), Seq.skip 1 s)