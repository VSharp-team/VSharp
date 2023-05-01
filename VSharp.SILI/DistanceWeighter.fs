namespace VSharp.Interpreter.IL

open System

open VSharp
open VSharp.Interpreter.IL.CilStateOperations
open VSharp.Interpreter.IL.ipOperations

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
        let frameMethodCFG = frameMethod.ForceCFG
        let frameDist = frameMethodCFG.DistancesFrom frameOffset
        let checkDist() = Dict.tryGetValue frameDist target.ForceBasicBlock infinity <> infinity
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
    let localWeight loc (tagets : codeLocation seq) =
        let localCFG = loc.method.ForceCFG
        let dist = localCFG.DistancesFrom loc.offset
        tagets
        |> Seq.fold (fun m l -> min m (Dict.tryGetValue dist l.ForceBasicBlock infinity)) infinity
        |> handleInfinity
        |> Option.map logarithmicScale

    let targetWeight currLoc =
        localWeight currLoc [target]

    // Returns the number proportional to distance from loc to relevant calls in this method
    let preTargetWeight currLoc =
        let localCFG = currLoc.method.ForceCFG
        let targets =
            localCFG.Calls
            |> Seq.filter (fun kv -> callGraphDistanceToTarget.ContainsKey kv.Value.Callee)
            |> Seq.map (fun kv -> { offset = kv.Key.StartOffset; method = currLoc.method })
        localWeight currLoc targets |> Option.map ((+) 32u)

    // Returns the number proportional to distance from loc to return of this method
    let postTargetWeight currLoc =
        let localCFG = currLoc.method.ForceCFG
        let targets = localCFG.Sinks |> Seq.map (fun basicBlock -> { offset = basicBlock.StartOffset; method = currLoc.method })
        localWeight currLoc targets |> Option.map ((+) 32u)

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
        override x.Next() = 0u

type IntraproceduralShortestDistanceToUncoveredWeighter(statistics : SILIStatistics) =

    let minDistance (method : Method) fromLoc =
        let infinity = UInt32.MaxValue
        match method.CFG with
        | Some cfg ->
            let minDistance =
                cfg.DistancesFrom fromLoc
                |> Seq.fold (fun min kvp ->
                    let loc = { offset = kvp.Key.Offset; method = method }
                    let distance = kvp.Value
                    if distance < min && distance <> 0u && not <| statistics.IsCovered loc then distance
                    else min) infinity
            Some minDistance
        | None -> None

    interface IWeighter with
        override x.Weight(state) =
            state.ipStack |> Seq.tryPick (fun ip ->
                match ipOperations.ip2codeLocation ip with
                | Some loc when loc.method.InCoverageZone -> minDistance loc.method loc.offset
                | Some _ -> None
                | None -> Some 1u)

        override x.Next() = 0u
