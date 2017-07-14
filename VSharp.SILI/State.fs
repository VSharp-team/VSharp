namespace VSharp
open VSharp.Utils

module internal State =
    module SymbolicHeap = Heap

    type internal stack = MappedStack.stack<StackKey, Term>
    type internal heap = SymbolicHeap
    type internal staticMemory = SymbolicHeap
    type internal frames = Stack.stack<StackKey list>
    type internal pathCondition = Term list
    type internal state = stack * heap * staticMemory * frames * pathCondition

    let internal empty : state = (MappedStack.empty, SymbolicHeap.empty, SymbolicHeap.empty, Stack.empty, List.empty)

    let internal readStackLocation ((s, _, _, _, _) : state) key = MappedStack.find key s
    let internal newStackFrame ((s, h, m, f, p) : state) frame : state =
        let pushOne (map : stack) (key, value) = (key, MappedStack.push key value map)
        let names, newStack = frame |> List.mapFold pushOne s in
        (newStack, h, m, Stack.push f names, p)
    let internal popStack ((s, h, m, f, p) : state) : state =
        let popOne (map : stack) name = MappedStack.remove map name
        let names = Stack.peak f in
        (List.fold popOne s names, h, m, Stack.pop f, p)
    let internal pushToCurrentStackFrame ((s, _, _, _, _) : state) key value = MappedStack.push key value s
    let internal mutateStack ((s, _, _, _, _) : state) key value = MappedStack.add key value s
    let internal isAllocatedOnStack ((s, _, _, _, _) : state) key = MappedStack.containsKey key s
    let internal compareStacks s1 s2 = MappedStack.compare (fun key value -> StackRef(key, [], PointerType (Terms.TypeOf value))) s1 s2

    let internal readHeapLocation ((_, h, _, _, _) : state) key = h.[key]
    let internal readStaticLocation ((_, _, m, _, _) : state) key = m.[key]
    let internal staticMembersInitialized ((_, _, m, _, _) : state) typeName =
        SymbolicHeap.contains (Concrete(typeName, String)) m

    let internal withPathCondition ((s, h, m, f, p) : state) cond : state = (s, h, m, f, cond::p)
    let internal popPathCondition ((s, h, m, f, p) : state) : state =
        match p with
        | [] -> internalfail "cannot pop empty path condition"
        | _::p' -> (s, h, m, f, p')

    let private stackOf ((s, _, _, _, _) : state) = s
    let private heapOf ((_, h, _, _, _) : state) = h
    let private staticsOf ((_, _, m, _, _) : state) = m
    let private framesOf ((_, _, _, f, _) : state) = f
    let internal pathConditionOf ((_, _, _, _, p) : state) = p

    let internal merge ((s1, h1, m1, f1, p1) : state) ((s2, h2, m2, f2, p2) : state) resolve : state =
        assert(p1 = p2)
        assert(f1 = f2)
        let mergedStack = MappedStack.merge2 s1 s2 resolve in
        let mergedHeap = Heap.merge2 h1 h2 resolve in
        let mergedStatics = Heap.merge2 m1 m2 resolve in
        (mergedStack, mergedHeap, mergedStatics, f1, p1)

    let internal mergeMany guards states resolve : state =
        assert(List.length states > 0)
        let first = List.head states in
        let frames = framesOf first in
        let path = pathConditionOf first in
        assert(List.forall (fun s -> framesOf s = frames) states)
        assert(List.forall (fun s -> pathConditionOf s = path) states)
        let mergedStack = MappedStack.merge guards (List.map stackOf states) resolve in
        let mergedHeap = Heap.merge guards (List.map heapOf states) resolve in
        let mergedStatics = Heap.merge guards (List.map staticsOf states) resolve in
        (mergedStack, mergedHeap, mergedStatics, frames, path)

    let private staticKeyToString = function
        | Concrete(typeName, String) -> System.Type.GetType(typeName :?> string).FullName
        | t -> toString t

    let internal dumpMemory ((_, h, m, _, _) : state) =
        let sh = Heap.dump h toString in
        let mh = Heap.dump m staticKeyToString in
        let separator = if System.String.IsNullOrWhiteSpace(sh) then "" else "\n"
        sh + separator + mh
