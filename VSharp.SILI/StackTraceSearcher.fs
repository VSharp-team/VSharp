namespace VSharp.Interpreter.IL

open System.Diagnostics
open VSharp

type StackTraceSearcher(error : System.Exception) =
    let stackTrace = StackTrace(error)
    interface INewSearcher with
        override  x.ChooseAction(_,_,_) =
            __notImplemented__()
        override x.Reset () = __notImplemented__()
        override x.Init (_,_) = __notImplemented__()
        override x.PriorityQueue _ = __notImplemented__()

