namespace VSharp.Interpreter.IL

open System.Reflection
open System.Collections.Generic
open VSharp
open VSharp.Core

type BidirectionalSearcherForMethods() =
    let starts = HashSet<MethodBase>()
    let mutable mainMethod = null
    interface INewSearcher with
        override x.CanReach(_,_,_) = true
        override x.TotalNumber = 0u
        override x.Init (m,_) =
            mainMethod <- m
        override x.Reset () = mainMethod <- null
        override x.ChooseAction (qFront, qBack, _) =
            match qBack, qFront.StatesForPropagation() with
            | Seq.Cons(ps, _), _ -> GoBackward ps
            | _, Seq.Empty when starts.Contains(mainMethod) -> Stop
            | _, Seq.Empty -> Start(Instruction(0, mainMethod))
            | _, Seq.Cons(s, _) ->
                let method = ipOperations.methodOf (CilStateOperations.currentIp s)
                if starts.Contains(method) then GoForward s
                else Start(Instruction(0, method))




