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

    let private valueToString v = if box v = null then "null" else v.ToString()

    let private classToString obj (t : System.Type) =
        let fields = t.GetFields() |> Array.map (fun f -> f.Name + ": " + (f.GetValue(obj) |> valueToString)) |> join "; "
        $"{t} {{ {fields} }}"

    let printConcrete (obj : obj) =
        let t = TypeUtils.getTypeOfConcrete obj
        match obj with
        | null -> "null"
        | _ when t.IsValueType -> obj.ToString()
        | :? System.Array -> obj.ToString()
        | :? System.String -> "\"" + obj.ToString() + "\""
        | _ -> classToString obj t
