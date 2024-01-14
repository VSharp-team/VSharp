module internal VSharp.Fuzzer.Utils

open System.Runtime.InteropServices


let inline isNull value = obj.ReferenceEquals(value, null)
let inline failIfNull value message = if isNull value then VSharp.Prelude.internalfail message

type IdGenerator(initValue : int) =
    let mutable currentValue = initValue
    member this.NextId() =
        currentValue <- currentValue + 1
        currentValue

let withExceptionLogging (t: System.Threading.Tasks.Task) =
    task {
        try
            do! t
        with e ->
            VSharp.Logger.error $"{e.Message}\n{e.StackTrace}"
    } :> System.Threading.Tasks.Task
