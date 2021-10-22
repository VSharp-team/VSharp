namespace VSharp.Interpreter.IL

open System

open VSharp
open VSharp.Core
open VSharp.Interpreter.IL.CilStateOperations

type ShortestDistancetWeighter(target : codeLocation) =
    let infinity = UInt32.MaxValue
    let handleInfinity n = if n = infinity then None else Some n
    let logarithmicScale weight =
        if weight = 0u then 0u
        elif weight = 1u then 1u
        else double weight |> Math.Log2 |> Math.Ceiling |> uint

    let callGraphDistanceToTarget = CFG.findCallGraphDistanceTo target.method
    let calculateCallWeight (state : cilState) =
        let frameWeight frameMethod frameNumber =
            let callGraphDistance = Dict.tryGetValue callGraphDistanceToTarget frameMethod infinity
            if callGraphDistance = infinity then infinity
            else 2u * callGraphDistance + frameNumber
        state.ipStack
     |> List.map methodOf
     |> List.mapi (fun number method -> frameWeight method (uint number), uint number)
     |> List.minBy fst
     |> fun (w, n) -> if w = infinity then None else Some (w, n)

    let localWeight loc (tagets : codeLocation list) =
        let localCFG = CFG.findCfg loc.method
        let dist = CFG.findDistanceFrom localCFG loc.offset
        tagets
     |> List.fold (fun m l -> min m (Dict.tryGetValue dist l.offset infinity)) infinity
     |> handleInfinity
     |> Option.map logarithmicScale

    let targetWeight state =
        let currLoc = currentLoc state
        localWeight currLoc [target]

    let preTargetWeight state =
        let currLoc = currentLoc state
        let localCFG = CFG.findCfg currLoc.method
        let targets =
            localCFG.offsetsDemandingCall
         |> Seq.filter (fun kv -> callGraphDistanceToTarget.ContainsKey (snd kv.Value))
         |> Seq.map (fun kv -> { offset = kv.Key; method = currLoc.method })
         |> List.ofSeq
        localWeight currLoc targets

    let postTargetWeight state =
        let currLoc = currentLoc state
        let localCFG = CFG.findCfg currLoc.method
        let targets =
            localCFG.retOffsets
         |> Seq.map (fun offset -> { offset = offset; method = currLoc.method })
         |> List.ofSeq
        localWeight currLoc targets

    interface IWeighter with
        override x.Weight(state) =
            let callWeight = calculateCallWeight state
            match callWeight with
            | Some (0u, _) -> targetWeight state
            | Some (_, 0u) -> preTargetWeight state
            | Some _ -> postTargetWeight state
            | None -> None
        override x.Next() = 0u
