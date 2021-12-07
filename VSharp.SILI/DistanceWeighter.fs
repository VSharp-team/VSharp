namespace VSharp.Interpreter.IL

open System

open VSharp
open VSharp.Core
open VSharp.Interpreter.IL.CilStateOperations

type ShortestDistanceWeighter(target : codeLocation) =
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
     |> Seq.map methodOf
     |> Seq.mapi (fun number method -> frameWeight method (uint number), uint number)
     |> Seq.minBy fst
     |> fun (w, n) -> if w = infinity then None else Some (w, n)

    let localWeight loc (tagets : codeLocation seq) =
        option {
            let localCFG = CFG.findCfg loc.method
            let! vertexOffset = CFG.vertexOf loc.method loc.offset
            let dist = CFG.findDistanceFrom localCFG vertexOffset
            return!
                tagets
             |> Seq.fold (fun m l -> min m (Dict.tryGetValue dist l.offset infinity)) infinity
             |> handleInfinity
             |> Option.map logarithmicScale
        }

    let targetWeight currLoc =
        localWeight currLoc [target]

    let preTargetWeight currLoc =
        let localCFG = CFG.findCfg currLoc.method
        let targets =
            localCFG.offsetsDemandingCall
         |> Seq.filter (fun kv -> callGraphDistanceToTarget.ContainsKey (snd kv.Value))
         |> Seq.choose (fun kv -> CFG.vertexOf currLoc.method kv.Key)
         |> Seq.map (fun vertex -> { offset = vertex; method = currLoc.method })
        localWeight currLoc targets |> Option.map ((+) 32u)

    let postTargetWeight currLoc =
        let localCFG = CFG.findCfg currLoc.method
        let targets =
            localCFG.retOffsets |> Seq.choose (CFG.vertexOf currLoc.method)
         |> Seq.map (fun offset -> { offset = offset; method = currLoc.method })
        localWeight currLoc targets |> Option.map ((+) 32u)

    interface IWeighter with
        override x.Weight(state) =
            option {
                let! currLoc = tryCurrentLoc state
                let! callWeight = calculateCallWeight state
                let! weight =
                    match callWeight with
                    | 0u, _ -> targetWeight currLoc
                    | _, 0u -> preTargetWeight currLoc
                    | _ -> postTargetWeight currLoc
                return weight * logarithmicScale state.stepsNumber
            }
        override x.Next() = 0u
