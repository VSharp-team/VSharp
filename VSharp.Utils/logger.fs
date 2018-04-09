namespace VSharp

module Logger =
    open System

    let Error = 1
    let Warning = 2
    let Info = 3
    let Trace = 4

    let mutable current_log_level = Trace

    let LevelToString = function
        | 1 -> "Error"
        | 2 -> "Warning"
        | 3 -> "Info"
        | 4 -> "Trace"
        | _ -> "Unknown"

    let public printLog vLevel format =
        Printf.ksprintf (fun s -> if current_log_level >= vLevel then printfn "[%s] [%A] %s" (LevelToString vLevel) System.DateTime.Now s) format
