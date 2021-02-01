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
        if List.length s.ipStack <> List.length s.state.frames then __unreachable__() //TODO: change to assert
        q.Add s

    member x.Remove s =
        let removed = q.Remove s
        if not removed then Logger.trace "CilState was not removed from IndexedQueue:\n%O" s
    member x.GetStates () = List.ofSeq q

[<AbstractClass>]
type ISearcher() =
    let maxBound = 10u // 10u is caused by number of iterations for tests: Always18, FirstEvenGreaterThen7
    abstract member PickNext : IndexedQueue -> cilState option

    member x.Used (cilState : cilState) =
        let ip = currentIp cilState
        PersistentDict.contains ip cilState.level && PersistentDict.find cilState.level ip >= maxBound

    member x.GetResults initialState (q : IndexedQueue) =
        let (|CilStateWithIIE|_|) (cilState : cilState) = cilState.iie

        let isResult (s : cilState) =
            let lastFrame = List.last s.state.frames
            let method = initialState.startingIP.method
            s.startingIP = initialState.startingIP && not lastFrame.isEffect && s.ipStack = [{label = Exit; method = method}]

        let allStates = q.GetStates() |> List.filter isResult
        let iieStates, nonIIEstates = List.partition isIIEState allStates

        let printInfoForDebug () =
            let allStatesInQueue = q.GetStates()
            Logger.info "No states were obtained. Most likely such a situation is a bug. Check it!"
            Logger.info "Indexed queue size = %d\n" (List.length allStatesInQueue)
            List.iteri (fun i -> dump >> Logger.info "Queue.[%d]:\n%s\n" i) allStatesInQueue
            true

        match iieStates with
        | CilStateWithIIE iie :: _ -> raise iie
        | _ :: _ -> __unreachable__()
//        | _, _ :: _ -> internalfailf "exception handling is not implemented yet"
        | _ when nonIIEstates = [] ->
            assert(printInfoForDebug())
            internalfailf "No states were obtained. Most likely such a situation is a bug. Check it!"
        | _ -> nonIIEstates


type DummySearcher() =
    inherit ISearcher() with
        override x.PickNext q =
            let canBePropagated (s : cilState) =
                not (isIIEState s || isUnhandledError s) && isExecutable s && not <| x.Used s
            let states = (q.GetStates()) |> List.filter canBePropagated
            match states with
            | x :: _ -> Some x
            | [] -> None

