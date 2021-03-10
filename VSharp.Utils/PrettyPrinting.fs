namespace VSharp.Utils

open System.Text
open VSharp

module PrettyPrinting =

    let appendLine (sb : StringBuilder) (str : string) =
        sb.Append(str).Append('\n')

    let dumpSection section (sb : StringBuilder) =
        sprintf "--------------- %s: ---------------" section |> appendLine sb

    let dumpDict section sort keyToString valueToString (sb : StringBuilder) d =
        if PersistentDict.isEmpty d then sb
        else
            let sb = dumpSection section sb
            PersistentDict.dump d sort keyToString valueToString |> appendLine sb
