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
    let frameWeight frameMethod frameOffset frameNumber =
        let frameMethodCFG = CFG.findCfg frameMethod
        let vertexFrameOffset = CFG.vertexOf frameMethod frameOffset |> Option.get
        let frameDist = CFG.findDistanceFrom frameMethodCFG vertexFrameOffset
        let checkDist () = Dict.tryGetValue frameDist target.offset infinity <> infinity
        let callWeight callMethod =
            let callGraphDistance = Dict.tryGetValue callGraphDistanceToTarget callMethod infinity
            if callGraphDistance = infinity then infinity
            else 2u * (callGraphDistance + 1u) + frameNumber

        match () with
        | _ when frameMethod = target.method && checkDist () -> frameNumber
        | _ when Seq.isEmpty frameMethodCFG.offsetsDemandingCall -> infinity
        | _ ->
            frameMethodCFG.offsetsDemandingCall |> Seq.map (fun kvp ->
            if Dict.tryGetValue frameDist kvp.Key infinity = infinity then infinity
            else callWeight (snd kvp.Value))
         |> Seq.min

    let calculateCallWeight (state : cilState) =
        let frameWeights =
            state.ipStack
         |> Seq.choose (fun ip ->
            let optOffset = offsetOf ip
            if Option.isSome optOffset
                then Some (methodOf ip, optOffset |> Option.get)
                else None)
         |> Seq.mapi (fun number (method, offset) -> frameWeight method offset (uint number), uint number)

        if Seq.isEmpty frameWeights then None
        else
            let w, n = Seq.minBy fst frameWeights
            if w = infinity then None else Some (w, n)

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
