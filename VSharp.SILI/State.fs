namespace VSharp

module internal State =
    [<StructuralEquality;StructuralComparison>]
    type internal HeapKey =
        | ConcreteAddress of int
        | SymbolicAddress of string
        override this.ToString() =
            match this with
            | ConcreteAddress a -> a.ToString()
            | SymbolicAddress a -> a

    type internal environment = Map<string, Stack.stack<Term>>
    type internal heap = Map<HeapKey, Term>
    type internal frames = Stack.stack<string list>
    type internal path = Term list
    type internal assertions = Term list
    type internal state = environment * heap * frames * path * assertions

    let internal empty : state = (Map.empty, Map.empty, Stack.empty, List.empty, List.empty)

    let internal push ((e, h, f, p, a) : state) frame : state =
        let pushOne (map : environment) (name, value) =
            let current = if map.ContainsKey(name) then map.[name] else Stack.empty in
            (name, map.Add(name, Stack.push current value))
        let names, newEnv = frame |> List.mapFold pushOne e in
        (newEnv, h, Stack.push f names, p, a)
    let internal pop ((e, h, f, p, a) : state) : state =
        let popOne (map : environment) name =
            let vals = Stack.pop map.[name]
            if Stack.isEmpty vals then map.Remove(name) else map.Add(name, vals)
        let names = Stack.peak f in
        (List.fold popOne e names, h, Stack.pop f, p, a)

    let internal environment ((e, _, _, _, _) : state) = e
    let internal path ((_, _, _, p, _) : state) = p
    let internal frames ((_, _, f, _, _) : state) = f
    let internal appendPath ((e, h, f, p, a) : state) cond : state = (e, h, f, cond::p, a)

    let internal assertions ((_, _, _, _, a) : state) = a
    let internal addAssertion ((e, h, f, p, a) : state) cond : state =
        if Terms.IsTrue cond || List.contains cond a then (e, h, f, p, a) else (e, h, f, p, cond :: a)
    let private uniteAssertions (a1 : assertions) (a2 : assertions) : assertions =
        // TODO: assertions should be Set (but Term should override comparison then)!
        let shorter = if List.length a1 < List.length a2 then a1 else a2
        let longer = if List.length a1 < List.length a2 then a2 else a1
        shorter |> List.foldBack (fun x xs -> if List.contains x xs then xs else x::xs) longer

    let private mergeMaps (map1 : Map<'a, 'b>) (map2 : Map<'a, 'b>) take update resolve =
        let resolveIfShould (map : Map<'a, 'b>) key value =
            if map.ContainsKey(key) then
                let oldValue = take (map.[key]) in
                let newValue = take value in
                if oldValue = newValue then map
                else
                    map.Add(key, update value (resolve oldValue newValue))
            else
                map.Add(key, value)
        Map.fold resolveIfShould map1 map2

    let internal merge ((e1, h1, f1, p1, a1) : state) ((e2, h2, f2, p2, a2) : state) resolve : state =
        assert(p1 = p2)
        assert(f1 = f2)
        (mergeMaps e1 e2 Stack.peak Stack.updateHead resolve, mergeMaps h1 h2 id id2 resolve, f1, p1, uniteAssertions a1 a2)
