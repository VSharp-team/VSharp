namespace VSharp.Core

open System.Collections.Generic
open VSharp.Logger

module internal Database =
    let private exploredResults = new Dictionary<IFunctionIdentifier, statementResult>()
    let private exploredExceptionGuards = new Dictionary<IFunctionIdentifier, term>()
    let private exploredExceptions = new Dictionary<IFunctionIdentifier, term>()
    let private exploredStates = new Dictionary<IFunctionIdentifier, state>()
    let private dependenciesOfResults = new Dictionary<IFunctionIdentifier, term seq>()
    let private dependenciesOfStates = new Dictionary<IFunctionIdentifier, term seq>()

    let report id (result, state) =
        printLog Info "For %O got %O\n%O!" id (ControlFlow.resultToTerm result) (State.dumpMemory state)
        exploredResults.Add(id, result) |> ignore
        exploredStates.Add(id, state) |> ignore
        let thrown, _ = ControlFlow.pickOutExceptions result
        let depsOfResult = result |> ControlFlow.resultToTerm |> discoverConstants
//        let depsOfState = State.fold (fun (acc : HashSet<term>) term -> acc.UnionWith(discoverConstants term); acc) (new HashSet<term>()) state
        dependenciesOfResults.Add(id, depsOfResult)
//        dependenciesOfStates.Add(id, depsOfState)
        match thrown with
        | Some(g, e) ->
            exploredExceptionGuards.Add(id, g)
            exploredExceptions.Add(id, e)
        | None -> ()

    let query id =
        assert(exploredResults.ContainsKey id = exploredStates.ContainsKey id)
        if exploredResults.ContainsKey id then Some(exploredResults.[id], exploredStates.[id]) else None

    let queryDependenciesOfResult id =
        assert(dependenciesOfResults.ContainsKey id)
        dependenciesOfResults.[id]

    let queryDependenciesOfState id =
        assert(dependenciesOfStates.ContainsKey id)
        dependenciesOfStates.[id]

    let queryState id =
        if exploredStates.ContainsKey id then Some exploredStates.[id] else None

    let queryExceptionsGuard id =
        if exploredExceptionGuards.ContainsKey id then Some exploredExceptionGuards.[id] else None

    let queryExceptions id =
        if exploredExceptions.ContainsKey id then Some exploredExceptions.[id] else None
