namespace VSharp.Explorer

open System

open VSharp
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilStateOperations
open VSharp.Interpreter.IL.IpOperations

type ShortestDistanceWeighter(target : codeLocation) =
    let infinity = UInt32.MaxValue
    let handleInfinity n = if n = infinity then None else Some n
    let logarithmicScale weight =
        if weight = 0u then 0u
        elif weight = 1u then 1u
        else double weight |> Math.Log2 |> Math.Ceiling |> uint

    let callGraphDistanceToTarget = target.method.CallGraphDistanceToMe

    // Returns the number proportional to distance from the offset in frameOffset of frameMethod to target. Uses both
    // call graph for interprocedural and CFG for intraprocedural distance approximation.
    let frameWeight (frameMethod : Method) frameOffset frameNumber =
        let frameMethodCFG = frameMethod.CFG
        let frameDist = frameMethodCFG.DistancesFrom frameOffset
        let checkDist() = Dict.tryGetValue frameDist target.BasicBlock infinity <> infinity
        let callWeight callMethod =
            let callGraphDistance = Dict.tryGetValue callGraphDistanceToTarget callMethod infinity
            if callGraphDistance = infinity then infinity
            else 2u * (callGraphDistance + 1u) + frameNumber

        match () with
        | _ when frameMethod = target.method && checkDist () -> frameNumber
        | _ when Seq.isEmpty frameMethodCFG.Calls -> infinity
        | _ ->
            frameMethodCFG.Calls |> Seq.map (fun kvp ->
            if Dict.tryGetValue frameDist kvp.Key infinity = infinity then infinity
            else callWeight kvp.Value.Callee)
         |> Seq.min

    let calculateCallWeight (state : cilState) =
        let suitable ip =
            match offsetOf ip with
            | Some offset -> Some (forceMethodOf ip, offset)
            | None -> None
        let calculateWeight frameNumber (method, offset) =
            // TODO: do not execute this for frames with frameNumber > current minimum
            let frameNumber = uint frameNumber
            frameWeight method offset frameNumber, frameNumber
        let frameWeights =
            state.ipStack
            |> Seq.choose suitable
            |> Seq.mapi calculateWeight

        if Seq.isEmpty frameWeights then None
        else
            let w, n = Seq.minBy fst frameWeights
            if w = infinity then None else Some (w, n)

    // Returns the number proportional to distance from loc to target in CFG.
    let localWeight loc (targets : codeLocation seq) =
        let localCFG = loc.method.CFG
        let dist = localCFG.DistancesFrom loc.offset
        targets
        |> Seq.fold (fun m l -> min m (Dict.tryGetValue dist l.BasicBlock infinity)) infinity
        |> handleInfinity
        |> Option.map logarithmicScale

    let targetWeight currLoc =
        localWeight currLoc [target]

    // Returns the number proportional to distance from loc to relevant calls in this method
    let preTargetWeight currLoc =
        let localCFG = currLoc.method.CFG
        let targets =
            localCFG.Calls
            |> Seq.filter (fun kv -> callGraphDistanceToTarget.ContainsKey kv.Value.Callee)
            |> Seq.map (fun kv -> { offset = kv.Key.StartOffset; method = currLoc.method })
        match localWeight currLoc targets with
        | Some w -> Some(w + 32u)
        | None -> Some 63u

    // Returns the number proportional to distance from loc to return of this method
    let postTargetWeight currLoc =
        let localCFG = currLoc.method.CFG
        let targets = localCFG.Sinks |> Seq.map (fun basicBlock -> { offset = basicBlock.StartOffset; method = currLoc.method })
        match localWeight currLoc targets with
        | Some w -> Some(w + 32u)
        | None -> Some 63u

    interface IWeighter with
        override x.Weight(state) =
            option {
                match tryCurrentLoc state with
                | Some currLoc ->
                    let! callWeight = calculateCallWeight state
                    let! weight =
                        match callWeight with
                        | 0u, _ -> targetWeight currLoc
                        | _, 0u -> preTargetWeight currLoc
                        | _ -> postTargetWeight currLoc
                    return weight * logarithmicScale state.stepsNumber
                | None -> return 1u
            }

type IntraproceduralShortestDistanceToUncoveredWeighter(statistics : SVMStatistics) =

    let minDistance (method : Method) fromLoc =
        let infinity = UInt32.MaxValue
        method.CFG.DistancesFrom fromLoc
        |> Seq.fold (fun min kvp ->
            let loc = { offset = kvp.Key.Offset; method = method }
            let distance = kvp.Value
            if distance < min && distance <> 0u && not <| statistics.IsCovered loc then distance
            else min) infinity

    interface IWeighter with
        override x.Weight(state) =
            let calculateWeight ip =
                match ip2codeLocation ip, ip with
                | Some loc, _ when loc.method.InCoverageZone -> minDistance loc.method loc.offset |> Some
                | Some _, _-> None
                | None, SearchingForHandler(_, _, toObserve, _) ->
                    List.length toObserve |> uint |> Some
                | _ -> Some 1u
            state.ipStack |> Seq.tryPick calculateWeight
