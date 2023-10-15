module internal VSharp.Fuzzer.Utils

open System.Runtime.InteropServices


let inline isNull value = obj.ReferenceEquals(value, null)
let inline failIfNull value message = if isNull value then VSharp.Prelude.internalfail message

type Platform =
    | Windows
    | Linux
    | MacOs

let getPlatform () =
    if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then Windows
    elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then Linux
    elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then MacOs
    else VSharp.Prelude.__notImplemented__()

let getLibExtension () =
    match getPlatform () with
    | Windows -> ".dll"
    | Linux -> ".so"
    | MacOs -> ".dylib"

type IdGenerator(initValue: int) =
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
