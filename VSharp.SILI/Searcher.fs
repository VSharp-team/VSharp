namespace VSharp.Interpreter.IL

open System.Collections.Generic
open VSharp
open CilStateOperations
open VSharp.Core

type IndexedQueue() =
    let q = List<cilState>()
//    let isRecursiveEffect (s : cilState) =
//        let isEffect = (Seq.last s.state.frames).isEffect
//        if isEffect then
//            let effectsMethod = (Seq.last s.state.frames).func.Method
//            match currentIp s with
//            | {label = Instruction offset; method = m} when Instruction.isDemandingCallOpCode (Instruction.parseInstruction m offset)->
//                let callSite = Instruction.parseCallSite m offset
//                callSite.calledMethod.Equals(effectsMethod)
//            | _ -> false
//        else false
    member x.Add (s : cilState) =
        if List.length s.ipStack <> List.length s.state.frames then __unreachable__() // TODO: change to assert; this falls in factAgain #do
        q.Add s

    member x.Remove s =
        let removed = q.Remove s
        if not removed then Logger.trace "CilState was not removed from IndexedQueue:\n%O" s
    member x.GetStates () = List.ofSeq q

[<AbstractClass>]
type ISearcher() = // TODO: max bound is needed, when we are in recursion, but when we go to one method many time -- it's okay #do
    let maxBound = 50u // 10u is caused by number of iterations for tests: Always18, FirstEvenGreaterThen7
    abstract member PickNext : IndexedQueue -> cilState option

    member x.Used (cilState : cilState) =
        let ip = currentIp cilState
        PersistentDict.contains ip cilState.level && PersistentDict.find cilState.level ip >= maxBound

    member x.GetResults initialState (q : IndexedQueue) =
        let (|CilStateWithIIE|_|) (cilState : cilState) = cilState.iie
        let isStartingDescender (s : cilState) =
            let lastFrame = List.last s.state.frames
            s.startingIP = initialState.startingIP && not lastFrame.isEffect
        let allStates = List.filter isStartingDescender (q.GetStates())
        let iieStates, nonIIEstates = List.partition isIIEState allStates
        let isFinished (s : cilState) = s.ipStack = [{label = Exit; method = initialState.startingIP.method}]
        let finishedStates = List.filter isFinished nonIIEstates
        let printInfoForDebug () =
            let allStatesInQueue = q.GetStates()
            Logger.info "No states were obtained. Most likely such a situation is a bug. Check it!"
            Logger.info "Indexed queue size = %d\n" (List.length allStatesInQueue)
            List.iteri (fun i -> dump >> Logger.info "Queue.[%d]:\n%s\n" i) allStatesInQueue
            true
        match iieStates with // TODO: write error states? #do
        | CilStateWithIIE iie :: _ -> raise iie
        | _ :: _ -> __unreachable__()
//        | _, _ :: _ -> internalfailf "exception handling is not implemented yet"
        | _ when finishedStates = [] ->
            assert(printInfoForDebug())
            internalfailf "No states were obtained. Most likely such a situation is a bug. Check it!"
        | _ -> finishedStates

type DummySearcher() =
    inherit ISearcher() with
        override x.PickNext q =
            let canBePropagated (s : cilState) =
                not (isIIEState s || isUnhandledError s) && isExecutable s && not <| x.Used s
            let states = (q.GetStates()) |> List.filter canBePropagated
            match states with
            | x :: _ -> Some x
            | [] -> None

