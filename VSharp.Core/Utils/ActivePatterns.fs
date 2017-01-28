namespace VSharp.Core.Utils

[<AutoOpen>]
module ActivePatterns =
    let (|SeqNode|SeqEmpty|) s =
            if Seq.isEmpty s then SeqEmpty
            else SeqNode ((Seq.head s), Seq.skip 1 s)