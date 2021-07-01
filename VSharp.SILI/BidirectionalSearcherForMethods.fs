namespace VSharp.Interpreter.IL

open System.Reflection
open System.Collections.Generic
open VSharp
open VSharp.Core

type BidirectionalSearcherForMethods() =
    let starts = HashSet<MethodBase>()
    static let mutable totalNumber = 0u
    let mutable mainMethod = null
    let mutable stepsNumber = 0u

    let canStartFrom (m : MethodBase) =
        (not (starts.Contains(m))) && m.GetMethodBody() <> null && starts.Count < 4
    let startFrom (m : MethodBase) =
        let added = starts.Add(m) in assert(added)
//        Logger.warning "Starting for method = %s" (Reflection.getFullMethodName m)
        Start(Instruction(0x00, m))
    interface INewSearcher with
        override x.CanReach(_,_,_) = true
        override x.TotalNumber = totalNumber
        override x.Init (m,_) =
            mainMethod <- m
        override x.Reset () =
            Logger.warning "steps number done by %O = %d" (x.GetType()) stepsNumber
            totalNumber <- totalNumber + stepsNumber
            stepsNumber <- 0u
            mainMethod <- null
            starts.Clear()
        override x.ChooseAction (qFront, qBack, _) =
            match qBack, qFront.ExtractMin() with
            | Seq.Cons(ps, _), _ -> GoBackward ps
            | _, None when not <| canStartFrom(mainMethod) -> Stop
            | _, None -> startFrom mainMethod
            | _, Some s ->
                let method = ipOperations.methodOf (CilStateOperations.currentIp s)
                if canStartFrom method then startFrom method
                else
                    stepsNumber <- stepsNumber + 1u
//                    Logger.warning "GoForward for method = %s" (Reflection.getFullMethodName method)
                    GoForward s

