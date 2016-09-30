namespace VSharp.Core.Symbolic

module internal State =
    type private environment = Map<string, Term>
    type private path = Term list
    type private assertions = Term list
    type internal state = environment * path * assertions

    let private addToEnv (env : environment) name term = env.Add(name, term)

    let internal empty : state = (Map.empty, List.empty, List.empty)

    let internal addTerm ((e, p, a) : state) name term : state = (addToEnv e name term, p, a)
    let internal eval ((e, _, _) : state) id = e.[id]
    let internal map mapping ((e, p, a) : state) = (e |> Map.map mapping, p, a)

    let internal path ((_, p, _) : state) = p
    let internal appendPath ((e, p, a) : state) cond : state = (e, cond :: p, a)

    let internal assertions ((_, _, a) : state) = a
    let internal addAssertion ((e, p, a) : state) cond : state = if List.contains cond a then (e, p, a) else (e, p, cond :: a)
    let internal withAssertions assertions ((e, p, _) : state) = (e, p, assertions)
    let internal uniteAssertions (a1 : assertions) (a2 : assertions) : assertions =
        // TODO: assertions should be Set (but Term should override comparison then)!
        let shorter = if List.length a1 < List.length a2 then a1 else a2
        let longer = if List.length a1 < List.length a2 then a2 else a1
        shorter |> List.foldBack (fun x xs -> if List.contains x xs then xs else x::xs) longer
