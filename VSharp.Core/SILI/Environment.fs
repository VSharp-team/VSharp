namespace VSharp.Core.Symbolic

type Env = Map<string, Term>

module internal Environment =

    let internal empty = Map.empty

    let internal addTerm (env : Env) name term = env.Add(name, term)

    let internal merge (env1 : Env) (env2 : Env) =
        let smaller = if env1.Count < env2.Count then env1 else env2
        let bigger = if env1.Count < env2.Count then env2 else env1
        smaller |> Map.fold addTerm bigger
