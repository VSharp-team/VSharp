namespace VSharp

module internal State =
    [<StructuralEquality;StructuralComparison>]
    type internal HeapKey =
        | ConcreteAddress of int
        | StaticAddress of string
        | SymbolicAddress of string
        override this.ToString() =
            match this with
            | ConcreteAddress a -> a.ToString()
            | StaticAddress a -> System.Type.GetType(a).FullName
            | SymbolicAddress a -> a

    type internal environment = Map<string, Stack.stack<Term>>
    type internal heap = Map<HeapKey, Term>
    type internal frames = Stack.stack<string list>
    type internal path = Term list
    type internal state = environment * heap * frames * path

    let internal empty : state = (Map.empty, Map.empty, Stack.empty, List.empty)

    let internal push ((e, h, f, p) : state) frame : state =
        let pushOne (map : environment) (name, value) =
            let current = if map.ContainsKey(name) then map.[name] else Stack.empty in
            (name, map.Add(name, Stack.push current value))
        let names, newEnv = frame |> List.mapFold pushOne e in
        (newEnv, h, Stack.push f names, p)
    let internal pop ((e, h, f, p) : state) : state =
        let popOne (map : environment) name =
            let vals = Stack.pop map.[name]
            if Stack.isEmpty vals then map.Remove(name) else map.Add(name, vals)
        let names = Stack.peak f in
        (List.fold popOne e names, h, Stack.pop f, p)

    let internal environment ((e, _, _, _) : state) = e
    let internal path ((_, _, _, p) : state) = p
    let internal frames ((_, _, f, _) : state) = f
    let internal appendPath ((e, h, f, p) : state) cond : state = (e, h, f, cond::p)

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

    let internal merge ((e1, h1, f1, p1) : state) ((e2, h2, f2, p2) : state) resolve : state =
        assert(p1 = p2)
        assert(f1 = f2)
        (mergeMaps e1 e2 Stack.peak Stack.updateHead resolve, mergeMaps h1 h2 id id2 resolve, f1, p1)

    let internal staticMembersInitialized ((_, h, _, _) : state) typeName =
        h.ContainsKey(StaticAddress typeName)

    let internal dumpHeap ((_, h, _, _) : state) =
        let elements = h |> Map.toList |> List.map (fun (key, value) ->
            key.ToString() + " ==> " + value.ToString())
        in
        List.sort elements |> join "\n"
