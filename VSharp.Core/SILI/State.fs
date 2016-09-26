namespace VSharp.Core.Symbolic

module internal State =
    type private environment = Map<string, Term>
    type private path = Term list
    type private assertions = Term list
    type internal state = environment * path * assertions

    let private addToEnv (env : environment) name term = env.Add(name, term)

    let private mergeEnv (env1 : environment) (env2 : environment) =
        let smaller = if env1.Count < env2.Count then env1 else env2
        let bigger = if env1.Count < env2.Count then env2 else env1
        smaller |> Map.fold addToEnv bigger

    let internal empty : state = (Map.empty, [], [])

    let internal addTerm ((e, p, a) : state) name term : state = (addToEnv e name term, p, a)

    let internal appendPath ((e, p, a) : state) cond : state = (e, cond :: p, a)

    let internal addAssertion ((e, p, a) : state) cond : state = if List.contains cond a then (e, p, a) else (e, p, cond :: a)

    let internal eval ((e, _, _) : state) id = e.[id]
