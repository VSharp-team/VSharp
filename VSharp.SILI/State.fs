namespace VSharp
open VSharp.Utils

module internal State =
    module SymbolicHeap = Heap

    let internal pointerType = Numeric typedefof<int> in

    type internal stack = MappedStack.stack<StackKey, MemoryCell<Term>>
    type internal heap = SymbolicHeap
    type internal staticMemory = SymbolicHeap
    type internal frames = Stack.stack<(StackKey * TermType) list * Timestamp>
    type internal pathCondition = Term list
    type internal state = stack * heap * staticMemory * frames * pathCondition

// ------------------------------- Primitives -------------------------------

    let internal empty : state = (MappedStack.empty, SymbolicHeap.empty, SymbolicHeap.empty, Stack.empty, List.empty)

    type internal 'a SymbolicValue =
        | Specified of 'a
        | Unspecified

    let private nameOfLocation = function
        | HeapRef((_, (x, _)::xs), _) -> toString x
        | HeapRef(((_, t), _), _) -> toString t
        | StackRef((name, _), x::_) -> sprintf "%s.%O" name x
        | StackRef((name, _), _) -> name
        | StaticRef(name, x::_) -> sprintf "%s.%O" name x
        | StaticRef(name, _) -> name
        | l -> "requested name of an unexpected location " + (toString l) |> internalfail

    let internal readStackLocation ((s, _, _, _, _) : state) key = MappedStack.find key s
    let internal readHeapLocation ((_, h, _, _, _) : state) key = h.[key] |> fst3
    let internal readStaticLocation ((_, _, m, _, _) : state) key = m.[key] |> fst3

    let internal isAllocatedOnStack ((s, _, _, _, _) : state) key = MappedStack.containsKey key s
    let internal staticMembersInitialized ((_, _, m, _, _) : state) typeName =
        SymbolicHeap.contains (Concrete(typeName, String)) m

    let internal newStackFrame time ((s, h, m, f, p) : state) frame : state =
        let pushOne (map : stack) (key, value, typ) =
            match value with
            | Specified term -> ((key, typ), MappedStack.push key (term, time, time) map)
            | Unspecified -> ((key, typ), MappedStack.reserve key map)
        in
        let locations, newStack = frame |> List.mapFold pushOne s in
        (newStack, h, m, Stack.push f (locations, time), p)
    let internal pushToCurrentStackFrame ((s, _, _, _, _) : state) key value = MappedStack.push key value s
    let internal popStack ((s, h, m, f, p) : state) : state =
        let popOne (map : stack) (name, _) = MappedStack.remove map name
        let locations, _ = Stack.peak f in
        (List.fold popOne s locations, h, m, Stack.pop f, p)

    let internal writeStackLocation ((s, h, m, f, p) : state) key value : state =
        (MappedStack.add key value s, h, m, f, p)

    let internal frameTime ((_, _, _, f, _) : state) key =
        match List.tryFind (fst >> List.exists (fst >> ((=) key))) f with
        | Some(_, t) -> t
        | None -> internalfailf "stack does not contain key %O!" key

    let internal typeOfStackLocation ((_, _, _, f, _) : state) key =
        match List.tryPick (fst >> List.tryPick (fun (l, t) -> if l = key then Some t else None)) f with
        | Some t -> t
        | None -> internalfailf "stack does not contain key %O!" key

    let internal compareStacks s1 s2 = MappedStack.compare (fun key value -> StackRef(key, [])) fst3 s1 s2

    let internal withPathCondition ((s, h, m, f, p) : state) cond : state = (s, h, m, f, cond::p)
    let internal popPathCondition ((s, h, m, f, p) : state) : state =
        match p with
        | [] -> internalfail "cannot pop empty path condition"
        | _::p' -> (s, h, m, f, p')

    let private stackOf ((s, _, _, _, _) : state) = s
    let internal heapOf ((_, h, _, _, _) : state) = h
    let internal staticsOf ((_, _, m, _, _) : state) = m
    let private framesOf ((_, _, _, f, _) : state) = f
    let internal pathConditionOf ((_, _, _, _, p) : state) = p

    let internal withHeap ((s, h, m, f, p) : state) h' = (s, h', m, f, p)
    let internal withStatics ((s, h, m, f, p) : state) m' = (s, h, m', f, p)

    let private staticKeyToString = function
        | Concrete(typeName, String) -> System.Type.GetType(typeName :?> string).FullName
        | t -> toString t

// ------------------------------- Memory level -------------------------------

    [<AllowNullLiteral>]
    type ActivatorInterface =
        abstract member CreateInstance : System.Type -> Term list -> state -> (Term * state)
    let mutable activator : ActivatorInterface = null

    let rec internal defaultOf time = function
        | Bool -> Terms.MakeFalse
        | Numeric t when t.IsEnum -> Terms.MakeConcrete (System.Activator.CreateInstance(t)) t
        | Numeric t -> Terms.MakeConcrete 0 t
        | String -> Concrete(null, String)
        | PointerType t -> Concrete(null, t)
        | ClassType _ as t -> Concrete(null, t)
        | ArrayType _ as t -> Concrete(null, t)
        | SubType(dotNetType, _, _,  _) as t when dotNetType.IsValueType -> Struct(Heap.empty, t)
        | SubType _ as t -> Concrete(null, t)
        | Func _ -> Concrete(null, SubType(typedefof<System.Delegate>, [], [], "func"))
        | StructType(dotNetType, _, _) as t ->
            let fields = Types.GetFieldsOf dotNetType false in
            Struct(Seq.map (fun (k, v) -> (Terms.MakeConcreteString k, (defaultOf time v, time, time))) (Map.toSeq fields) |> Heap.ofSeq, t)
        | _ -> __notImplemented__()

    let internal arrayLengthType = typedefof<int>
    let internal arrayLengthTermType = Numeric arrayLengthType in

    let internal mkArrayZeroLowerBound rank =
        Array.init rank (always(Concrete(0, arrayLengthTermType)))

    let internal mkArraySymbolicLowerBound array arrayName rank =
        match Options.SymbolicArrayLowerBoundStrategy() with
        | Options.AlwaysZero -> mkArrayZeroLowerBound rank
        | Options.AlwaysSymbolic ->
            Array.init rank (fun i ->
                let idOfBound = sprintf "lower bound of %s" arrayName |> IdGenerator.startingWith in
                Constant(idOfBound, SymbolicArrayLength(array, i, false), arrayLengthTermType))

    let internal makeSymbolicArray source rank typ name =
        let idOfLength = IdGenerator.startingWith (sprintf "|%s|" name) in
        let constant = Constant(name, source, typ) in
        let lengths = Array.init rank (fun i -> Constant(idOfLength, SymbolicArrayLength(constant, i, true), Numeric arrayLengthType)) in
        Array(mkArraySymbolicLowerBound constant name rank, Some constant, Heap.empty, lengths, typ)

    let internal makeSymbolicInstance time source name = function
        | PointerType t ->
            let constant = Constant(name, source, pointerType) in
            HeapRef(((constant, t), []), time)
        | t when Types.IsPrimitive t || Types.IsFunction t -> Constant(name, source, t)
        | StructType _
        | SubType _
        | ClassType _ as t -> Struct(Heap.empty, t)
        | ArrayType(e, d) as t -> makeSymbolicArray source d t name
        | _ -> __notImplemented__()

    let internal genericLazyInstantiator time fullyQualifiedLocation typ () =
        makeSymbolicInstance time (LazyInstantiation fullyQualifiedLocation) (nameOfLocation fullyQualifiedLocation) typ

    let private stackLazyInstantiator state time key =
        let fql = StackRef(key, []) in
        let t = typeOfStackLocation state key in
        let time = frameTime state key in
        (genericLazyInstantiator time fql t (), time, time)

    let internal dumpMemory ((_, h, m, _, _) : state) =
        let sh = Heap.dump h toString in
        let mh = Heap.dump m staticKeyToString in
        let separator = if System.String.IsNullOrWhiteSpace(sh) then "" else "\n"
        sh + separator + mh

// ------------------------------- Merging -------------------------------

    let internal merge2 ((s1, h1, m1, f1, p1) as state : state) ((s2, h2, m2, f2, p2) : state) resolve : state =
        assert(p1 = p2)
        assert(f1 = f2)
        let mergedStack = MappedStack.merge2 s1 s2 resolve (stackLazyInstantiator state) in
        let mergedHeap = Heap.merge2 h1 h2 resolve in
        let mergedStatics = Heap.merge2 m1 m2 resolve in
        (mergedStack, mergedHeap, mergedStatics, f1, p1)

    let internal merge guards states resolve : state =
        assert(List.length states > 0)
        let first = List.head states in
        let frames = framesOf first in
        let path = pathConditionOf first in
        assert(List.forall (fun s -> framesOf s = frames) states)
        assert(List.forall (fun s -> pathConditionOf s = path) states)
        let mergedStack = MappedStack.merge guards (List.map stackOf states) resolve (stackLazyInstantiator first) in
        let mergedHeap = Heap.merge guards (List.map heapOf states) resolve in
        let mergedStatics = Heap.merge guards (List.map staticsOf states) resolve in
        (mergedStack, mergedHeap, mergedStatics, frames, path)
