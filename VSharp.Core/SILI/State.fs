namespace VSharp.Core.Symbolic

open VSharp.Core.Utils

module internal State =
    type private environment = Map<string, Stack.stack<Term>>
    type private frames = Stack.stack<string list>
    type private path = Term list
    type private assertions = Term list
    type internal state = environment * frames * path * assertions

    let internal empty : state = (Map.empty, Stack.empty, List.empty, List.empty)

    let internal push ((e, f, p, a) : state) frame : state =
        let pushOne (map : environment) (name, value) =
            let current = if map.ContainsKey(name) then map.[name] else Stack.empty in
            (name, map.Add(name, Stack.push current value))
        let names, newEnv = frame |> List.mapFold pushOne e in
        (newEnv, Stack.push f names, p, a)
    let internal pop ((e, f, p, a) : state) : state =
        let popOne (map : environment) name =
            let vals = Stack.pop map.[name]
            if Stack.isEmpty vals then map.Remove(name) else map.Add(name, vals)
        let names = Stack.peak f in
        (List.fold popOne e names, Stack.pop f, p, a)
    let internal update ((e, f, p, a) : state) name term : state =
        assert (e.ContainsKey(name))
        (e.Add(name, Stack.push (Stack.pop e.[name]) term), f, p, a)
    let internal introduce ((e, f, p, a) : state) name term : state =
        let existing = if e.ContainsKey(name) then e.[name] else Stack.empty in
        (e.Add(name, Stack.push existing term), Stack.push (Stack.pop f) (name::(Stack.peak f)), p, a)

    let internal eval ((e, _, _, _) : state) id = e.[id] |> Stack.peak
    let internal mapKeys mapping ((e, f, p, a) : state) : state =
        let updateOne id vals =
            (Stack.push (Stack.pop vals) (mapping id))
        (e |> Map.map updateOne, f, p, a)

    let internal path ((_, _, p, _) : state) = p
    let internal appendPath ((e, f, p, a) : state) cond : state = (e, f, cond :: p, a)

    let internal assertions ((_, _, _, a) : state) = a
    let internal addAssertion ((e, f, p, a) : state) cond : state =
        if Terms.IsTrue cond || List.contains cond a then (e, f, p, a) else (e, f, p, cond :: a)
    let internal withAssertions assertions ((e, f, p, _) : state) : state = (e, f, p, assertions)
    let internal uniteAssertions (a1 : assertions) (a2 : assertions) : assertions =
        // TODO: assertions should be Set (but Term should override comparison then)!
        let shorter = if List.length a1 < List.length a2 then a1 else a2
        let longer = if List.length a1 < List.length a2 then a2 else a1
        shorter |> List.foldBack (fun x xs -> if List.contains x xs then xs else x::xs) longer
