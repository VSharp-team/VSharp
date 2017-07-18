namespace VSharp
open VSharp.Utils

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

    type internal StackKey = string * string // name and token

    type internal stack = MappedStack.stack<StackKey, Term>
    type internal heap = Map<HeapKey, Term>
    type internal frames = Stack.stack<StackKey list>
    type internal path = Term list
    type internal state = stack * heap * frames * path

    let internal empty : state = (MappedStack.empty, Map.empty, Stack.empty, List.empty)

    let internal derefStack (s, _, _, _) key = MappedStack.find key s
    let internal pushToStack (s, _, _, _) key value = MappedStack.push key value s
    let internal mutateStack (s, _, _, _) key value = MappedStack.add key value s
    let internal isAllocatedOnStack (s, _, _, _) key = MappedStack.containsKey key s
    let internal stackMap f (s, _, _, _) = MappedStack.map f s
    let internal compareStacks = MappedStack.compare

    let internal push ((s, h, f, p) : state) frame : state =
        let pushOne (map : stack) (key, value) = (key, MappedStack.push key value map)
        let names, newStack = frame |> List.mapFold pushOne s in
        (newStack, h, Stack.push f names, p)
    let internal pop ((s, h, f, p) : state) : state =
        let popOne (map : stack) name = MappedStack.remove map name
        let names = Stack.peak f in
        (List.fold popOne s names, h, Stack.pop f, p)

    let internal stack ((s, _, _, _) : state) = s
    let internal pathCondition ((_, _, _, p) : state) = p
    let internal frames ((_, _, f, _) : state) = f

    let internal withPathCondition ((s, h, f, p) : state) cond : state = (s, h, f, cond::p)
    let internal popPathCondition ((s, h, f, p) : state) : state =
        match p with
        | [] -> internalfail "cannot pop empty path condition"
        | _::p' -> (s, h, f, p')

    let private mergeMaps map1 map2 find add fold contains resolve =
        let resolveIfShould map key value =
            if contains key map then
                let oldValue = find key map in
                let newValue = value in
                if oldValue = newValue then map
                else
                    add key (resolve oldValue newValue) map
            else
                add key value map
        fold resolveIfShould map1 map2

    let internal merge ((s1, h1, f1, p1) : state) ((s2, h2, f2, p2) : state) resolve : state =
        assert(p1 = p2)
        assert(f1 = f2)
        let mergedStack = mergeMaps s1 s2 MappedStack.find MappedStack.add MappedStack.fold MappedStack.containsKey resolve in
        let mergedHeap = mergeMaps h1 h2 Map.find Map.add Map.fold Map.containsKey resolve in
        (mergedStack, mergedHeap, f1, p1)

    let internal staticMembersInitialized ((_, h, _, _) : state) typeName =
        h.ContainsKey(StaticAddress typeName)

    let internal dumpHeap ((_, h, _, _) : state) =
        let elements = h |> Map.toList |> List.map (fun (key, value) ->
            key.ToString() + " ==> " + value.ToString())
        in
        List.sort elements |> join "\n"
