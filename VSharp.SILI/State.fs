namespace VSharp

module internal State =
    [<StructuralEquality;CustomComparison>]
    type internal HeapKey =
        | ConcreteAddress of int
        | StaticAddress of string
        | SymbolicAddress of string * SymbolicConstantSource
        with
        interface System.IComparable with
            member x.CompareTo y =
                match y with
                | :? HeapKey as k ->
                    match x, k with
                    | ConcreteAddress a1, ConcreteAddress a2 -> compare a1 a2
                    | ConcreteAddress _, _ -> -1
                    | StaticAddress _, ConcreteAddress _ -> 1
                    | StaticAddress a1, StaticAddress a2 -> compare a1 a2
                    | StaticAddress _, _ -> -1
                    | SymbolicAddress(a1, _), SymbolicAddress(a2, _) -> compare a1 a2
                    | SymbolicAddress _, _ -> 1
                | t -> internalfail (sprintf "comparing heap key with %s" (toString t))
        override this.ToString() =
            match this with
            | ConcreteAddress a -> a.ToString()
            | StaticAddress a -> System.Type.GetType(a).FullName
            | SymbolicAddress(a, _) -> a

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
    let internal pathCondition ((_, _, _, p) : state) = p
    let internal frames ((_, _, f, _) : state) = f

    let internal withPathCondition ((e, h, f, p) : state) cond : state = (e, h, f, cond::p)
    let internal popPathCondition ((e, h, f, p) : state) : state =
        match p with
        | [] -> internalfail "cannot pop empty path condition"
        | _::p' -> (e, h, f, p')

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
