namespace VSharp.Interpreter.IL

open System.Reflection
open System.Collections.Generic
open VSharp
open VSharp.Core

type BidirectionalSearcherForMethods() =
    let starts = HashSet<MethodBase>()
    interface INewSearcher with
        override x.CanReach(_,_,_) = true
        override x.TotalNumber = 0u
        override x.Init (_,_) = ()
        override x.Reset () = ()
        override x.ChooseAction (qFront, qBack, _, main) =
            match qBack, qFront.StatesForPropagation() with
            | Seq.Cons(ps, _), _ -> GoBackward ps
            | _, Seq.Empty when starts.Contains(main) -> Stop
            | _, Seq.Empty -> Start(Instruction(0, main))
            | _, Seq.Cons(s, _) ->
                let method = ipOperations.methodOf (CilStateOperations.currentIp s)
                if starts.Contains(method) then GoForward s
                else Start(Instruction(0, method))




