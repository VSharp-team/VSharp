namespace VSharp

open System

module NumericUtils =

    let ObjToInt (value : obj) =
        match value with
        | :? int as v -> v
        | :? int16 as v -> int v
        | :? uint16 as v -> int v
        | :? byte as v -> int v
        | :? sbyte as v -> int v
        | :? char as v -> int v
        | :? int64 as v when v <= int64 Int32.MaxValue -> int v
        | :? IntPtr as v when v <= IntPtr Int32.MaxValue -> int v
        | _ -> internalfail $"ObjToInt: object {value} can not be converted to int"

    let ObjToLong (value : obj) =
        match value with
        | :? int as v -> int64 v
        | :? int16 as v -> int64 v
        | :? uint16 as v -> int64 v
        | :? byte as v -> int64 v
        | :? sbyte as v -> int64 v
        | :? char as v -> int64 v
        | :? int64 as v -> v
        | :? IntPtr as v -> int64 v
        | _ -> internalfail $"ObjToInt: object {value} can not be converted to int"
