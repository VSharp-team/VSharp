namespace VSharp.Interpreter.IL

open System.Reflection
open System.Collections.Generic
open VSharp
open VSharp.Core
open VSharp.Utils

type BidirectionalSearcherForMethods() =
    let starts = Queue<MethodBase>()
    let usedStarts = HashSet<MethodBase>()
    static let mutable totalNumber = 0u
    let mutable mainMethod = null
    let mutable stepsNumber = 0u
    let mutable inverseReachability : Dictionary<MethodBase, HashSet<MethodBase>> = null

    let rememberStart (m : MethodBase) =
        if usedStarts.Contains(m) then ()
        else
            usedStarts.Add(m) |> ignore
            starts.Enqueue(m)
    let startFrom (m : MethodBase) =
        assert(usedStarts.Contains(m))
//        Logger.warning "Starting for method = %s" (Reflection.getFullMethodName m)
        Start(Instruction(0x00, m))
    let getInverse (ip : ip) =
        let m = CilStateOperations.methodOf ip
        if inverseReachability.ContainsKey(m) then inverseReachability.[m]
        else HashSet<_>()

    interface INewSearcher with
        override x.CanReach(_,_,_) = true
        override x.TotalNumber = totalNumber
        override x.Init (m, locs) =
            mainMethod <- m
            let _, inverseReachability' = CFG.buildReachability m
            inverseReachability <- inverseReachability'
            Seq.iter (fun loc -> rememberStart loc.method) locs
        override x.PriorityQueue _ = ComparerPriorityQueue<cilState>(IpStackComparer()) :> IPriorityQueue<cilState>

        override x.Reset () =
            Logger.warning "steps number done by %O = %d" (x.GetType()) stepsNumber
            totalNumber <- totalNumber + stepsNumber
            stepsNumber <- 0u
            mainMethod <- null
            starts.Clear()
        override x.ChooseAction (qFront, qBack, _) =
            stepsNumber <- stepsNumber + 1u
            match qBack, qFront.ExtractMin() with
            | Seq.Cons((p,s), _), _ ->
                Seq.iter rememberStart (getInverse(s.startingIP))
                GoBackward (p, s)
            |  _, _ when starts.Count > 0 ->
                let m = starts.Dequeue()
                startFrom m
            |  _, None  -> Stop
            | _, Some s -> GoForward s

