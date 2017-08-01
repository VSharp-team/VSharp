namespace VSharp

open VSharp.State

module internal Memory =

// ------------------------------- Primitives -------------------------------

    let private pointerType = Numeric typedefof<int> in

    let rec internal defaultOf = function
        | Bool -> Terms.MakeFalse
        | Numeric t when t.IsEnum -> Terms.MakeConcrete (System.Activator.CreateInstance(t)) t
        | Numeric t -> Terms.MakeConcrete 0 t
        | String -> Concrete(null, String)
        | ClassType _ as t -> Concrete(null, t)
        | ArrayType _ as t -> Concrete(null, t)
        | Object name as t -> Concrete(name, t)
        | SubType(name, _) as t -> Concrete(name, t)
        | Func _ -> Concrete(null, Object "func")
        | StructType dotNetType as t ->
            let fields = Types.GetFieldsOf dotNetType false in
            Struct(Seq.map (fun (k, v) -> (Terms.MakeConcreteString k, defaultOf v)) (Map.toSeq fields) |> Heap.ofSeq, t)
        | _ -> __notImplemented__()

    let rec internal makeSymbolicStruct isStatic source t dotNetType =
        let updateSource field =
            match source with
            | Symbolization(HeapRef((loc, path), t)) ->
                Symbolization(HeapRef((loc, List.append path [Terms.MakeConcreteString field]), t))
            | Symbolization(StackRef(loc, path, t)) ->
                Symbolization(StackRef(loc, List.append path [Terms.MakeConcreteString field], t))
            | _ -> FieldAccess(field, source)
        let fields = Types.GetFieldsOf dotNetType isStatic
                        |> Map.toSeq
                        |> Seq.map (fun (name, typ) -> (Terms.MakeConcreteString name, makeSymbolicInstance false (updateSource name) name typ))
                        |> Heap.ofSeq
        in Struct(fields, t)

    and internal makeSymbolicInstance isStatic source name = function
        | t when Types.IsPrimitive t || Types.IsFunction t -> Constant(name, source, t)
        | Object _ as t -> makeSymbolicStruct isStatic source t typedefof<obj>
        | StructType dotNetType as t -> makeSymbolicStruct isStatic source t dotNetType
        | ClassType dotNetType as t  -> makeSymbolicStruct isStatic source t dotNetType
        | SubType(dotNetType, name) as t -> makeSymbolicStruct isStatic source t dotNetType
        | ArrayType(e, 0) as t -> Array.makeSymbolic source 0 t name
        | ArrayType(e, d) as t -> Array.makeSymbolic source d t name
        | PointerType termType as t ->
            match termType with
            | ClassType _
            | ArrayType _ ->
                let address = IdGenerator.startingWith("addr") in
                HeapRef((Constant(address, source, pointerType), []), t)
            | StructType _ -> internalfail "symbolization of PointerType of StructType"
            | _ -> __notImplemented__()
        | _ -> __notImplemented__()

// ------------------------------- Dereferencing -------------------------------

    // TODO: Get rid of these 2
    let private stackKeyToTerm (key, value) =
        StackRef(key, [], Terms.TypeOf value |> PointerType)
    let private heapKeyToTerm (key, value) =
        HeapRef((key, []), Terms.TypeOf value |> PointerType)

    let private isStaticLocation = function
        | StaticRef _ -> true
        | _ -> false

    let private nameOfLocation = function
        | HeapRef((_, x::xs), _) -> toString x
        | HeapRef(_, t) -> toString t
        | StackRef((name, _), x::_, _) -> sprintf "%s.%O" name x
        | StackRef((name, _), _, _) -> name
        | StaticRef(name, x::_, _) -> sprintf "%s.%O" name x
        | StaticRef(name, _, _) -> name
        | l -> "requested name of an unexpected location " + (toString l) |> internalfail

    let internal npe state = State.activator.CreateInstance typeof<System.NullReferenceException> [] state

    let rec private refToInt = function
        | Error _ as e -> e
        | Concrete(null, _) -> Concrete(0, pointerType)
        | HeapRef((addr, _), t) -> addr
        | Union gvs -> Merging.guardedMap refToInt gvs
        | t -> t

    let rec internal referenceEqual p1 p2 =
        let addr1 = refToInt p1 in
        let addr2 = refToInt p2 in
        if not(Terms.IsInteger addr1 || Terms.IsInteger addr2) then
            internalfail "reference comparing non-reference types"
        Arithmetics.simplifyEqual addr1 addr2 id

    let rec internal isNull = function
        | Error _ as e -> e
        | Concrete(null, _) -> Terms.MakeTrue
        | HeapRef((addr, _), t) when Terms.IsInteger addr ->
            Arithmetics.simplifyEqual addr (Concrete(0, pointerType)) id
        | Union gvs -> Merging.guardedMap isNull gvs
        | _ -> Terms.MakeFalse

    let rec internal npeIfNull state = function
        | Error _ as e -> (e, state)
        | Concrete(null, _) -> npe state
        | HeapRef((addr, _), t) as reference ->
            let isNull = Arithmetics.simplifyEqual addr (Concrete(0, pointerType)) id in
            let term, state' = npe state in
            Merging.merge2Terms isNull !!isNull term reference, Merging.merge2States isNull !!isNull state' state
        | Union gvs -> Merging.guardedStateMap (npeIfNull state) gvs state
        | t -> (t, state)

    let rec private heapDeref defaultValue h key =
        if Heap.contains key h then Heap.find key h
        else
            // TODO: Lazy instantiations, symbolic dereferencing
            defaultValue ()

    let private structLazyInstantiator t field () =
        let fullyQualifiedLocation = field in // TODO
        let fieldType = Void in // TODO
        makeSymbolicInstance false (Symbolization fullyQualifiedLocation) (toString field) fieldType

    let private arrayElementLazyInstantiator location t = function
        | None -> fun _ -> defaultOf t
        | Some constant -> fun _ ->
            let id = sprintf "%s[%s]" (toString constant) (toString location) |> IdGenerator.startingWith in
            let fullyQualifiedLocation = location in // TODO
            makeSymbolicInstance false (Symbolization fullyQualifiedLocation) id t

    let rec private termDeref path term =
        match path with
        | [] -> term
        | key::path' ->
            match term with
            | Error _ -> term
            | Struct(fields, t) ->
                termDeref path' (heapDeref (structLazyInstantiator t key) fields key)
            | Array(lower, constant, contents, lengths, t) ->
                termDeref path' (heapDeref (arrayElementLazyInstantiator key t constant) contents key)
            | Union gvs ->
                internalfail "unexpected union of complex types!"
            | t ->
                internalfail ("expected complex type, but got " + toString t)

    let rec internal deref ((_, h, _, _, _) as state) = function
        | Error _ as e -> (e, state)
        | StackRef(location, path, _) -> (termDeref path (readStackLocation state location), state)
        | StaticRef(location, path, _) -> (termDeref path (readStaticLocation state (Terms.MakeConcreteString location)), state)
        | HeapRef((addr, path), _) ->
            let isNull = Arithmetics.simplifyEqual addr (Concrete(0, pointerType)) id in
            match isNull with
            | Terms.False -> (termDeref path (heapDeref __notImplemented__ h addr), state)
            | Terms.True -> npe state
            | _ ->
                let derefed = termDeref path (heapDeref __notImplemented__ h addr) in
                let exn, state' = npe state in
                (Merging.merge2Terms isNull !!isNull (Error exn) derefed, Merging.merge2States isNull !!isNull state' state)
        | Union gvs -> Merging.guardedStateMap (deref state) gvs state
        | t -> internalfail ("deref expected reference, but got " + toString t)

    let internal fieldOf term name = termDeref [Terms.MakeConcreteString name] term

// ------------------------------- Referencing -------------------------------

    let rec private referenceSubLocation location = function
        | Error _ as e -> e
        | StackRef(var, path, t) -> StackRef(var, List.append path [location], t)
        | StaticRef(var, path, t) -> StaticRef(var, List.append path [location], t)
        | HeapRef((addr, path), t) -> HeapRef((addr, List.append path [location]), t)
        | Terms.GuardedValues(gs, vs) -> vs |> List.map (referenceSubLocation location) |> List.zip gs |> Union
        | t -> internalfail ("expected reference, but got " + toString t)

    let rec private referenceTerm state name followHeapRefs = function
        | Error _ as e -> e
        | StackRef _ as r -> r
        | StaticRef _ as r -> r
        | HeapRef _ as r when followHeapRefs -> r
        | Union gvs -> Merging.guardedMap (referenceTerm state name followHeapRefs) gvs
        | term -> StackRef(name, [], Terms.TypeOf term)
        | term -> StackRef(name, [], Terms.TypeOf term)

    let internal referenceLocalVariable state location followHeapRefs =
        referenceTerm state location followHeapRefs (readStackLocation state location)

    let rec private referenceFieldOf state field parentRef = function
        | Error _ as e -> e
        | HeapRef((addr, path), t) ->
            assert(List.isEmpty path) // TODO: will this really be always empty?
            HeapRef((addr, [field]), t)
        | Struct _ -> referenceSubLocation field parentRef
        | Union gvs -> Merging.guardedMap (referenceFieldOf state field parentRef) gvs
        | t -> internalfail ("expected reference or struct, but got " + toString t)

    let rec private followOrReturnReference state reference =
        let term, state = deref state reference in
        match term with
        | Error _ as e -> e, state
        | StackRef _ as r -> r, state
        | HeapRef _ as r -> r, state
        | Union gvs -> Merging.guardedStateMap (followOrReturnReference state) gvs state
        | term -> reference, state

    let internal referenceField state followHeapRefs name parentRef =
        let term, state = deref state parentRef in
        let reference = referenceFieldOf state (Terms.MakeConcreteString name) parentRef term in
        if followHeapRefs then followOrReturnReference state reference
        else (reference, state)

    let internal referenceStaticField state followHeapRefs fieldName typeName =
        let reference = StaticRef(typeName, [Terms.MakeConcreteString fieldName], String) in
        if followHeapRefs then followOrReturnReference state reference
        else (reference, state)

    let internal referenceArrayIndex state arrayRef indices =
        // TODO: what about followHeapRefs?
        let array, state = deref state arrayRef in
        match array with
        | Error _ as e -> (e, state)
        | Array(lowerBounds, _, _, dimensions, _) ->
            let inBounds = Array.checkIndices lowerBounds dimensions indices in
            let physicalIndex = Array.physicalIndex lowerBounds dimensions indices in
            let reference = referenceSubLocation physicalIndex arrayRef in
            match inBounds with
            | Terms.True -> (reference, state)
            | _ ->
                let exn, state' = State.activator.CreateInstance typeof<System.IndexOutOfRangeException> [] state in
                Merging.merge2Terms inBounds !!inBounds reference (Error exn), Merging.merge2States inBounds !!inBounds state state'
        | t -> internalfail ("accessing index of non-array term " + toString t)

    let internal derefLocalVariable state id =
        referenceLocalVariable state id false |> deref state

// ------------------------------- Allocation -------------------------------

    let private pointer = ref 0
    let private timestamp = ref 0
    let private freshAddress () =
        pointer := !pointer + 1
        !pointer
    let private tick () =
        timestamp := !timestamp + 1
        !timestamp
    let public reset () =
        pointer := 0
        timestamp := 0

    let internal allocateOnStack ((s, h, m, f, p) as state : state) key term : state =
        (pushToCurrentStackFrame state key term, h, m, Stack.updateHead f (key::(Stack.peak f)), p)

    let internal allocateInHeap ((s, h, m, f, p) : state) term : Term * state =
        let address = Concrete(freshAddress(), pointerType) in
        let pointer = HeapRef((address, []), PointerType (Terms.TypeOf term)) in
        (pointer, (s, h.Add(address, term), m, f, p))

    let internal allocateInStaticMemory ((s, h, m, f, p) : state) typeName term =
        let address = Terms.MakeConcreteString typeName in
        (s, h, m.Add(address, term), f, p)

    let internal allocateSymbolicInstance isStatic source name state t =
        let value = makeSymbolicInstance isStatic source name t in
        allocateInHeap state value

// ------------------------------- Mutation -------------------------------

    let private mutateStack ((s, h, m, f, p) as state : state) key term : state =
        assert (isAllocatedOnStack state key)
        (mutateStack state key term, h, m, f, p)

    let private mutateHeap (h : SymbolicHeap) addr term =
        // TODO: symbolic cases!
        // assert (h.ContainsKey(addr))
        h.Add(addr, term)

    let private mutateHeapLocation ((s, h, m, f, p) : state) addr term : state =
        (s, mutateHeap h addr term, m, f, p)

    let private mutateStaticLocation ((s, h, m, f, p) : state) addr term : state =
        (s, h, mutateHeap m addr term, f, p)

    let rec private mutateField state path update term =
        match path with
        | [] -> update term
        | addr::path' ->
            match term with
            | Error _ as e -> e
            | Struct(fields, t) ->
                let oldField = heapDeref (structLazyInstantiator t addr) fields addr in
                let newField = mutateField state path' update oldField in
                let mutatedFields = mutateHeap fields addr newField in
                Struct(mutatedFields, t)
            | Array(lower, constant, contents, dimensions, t) ->
                let oldField = heapDeref (arrayElementLazyInstantiator addr t constant) contents addr in
                let newField = mutateField state path' update oldField in
                let mutatedContents = mutateHeap contents addr newField in
                Array(lower, constant, mutatedContents, dimensions, t)
            | Union gvs -> Merging.guardedMap (mutateField state path update) gvs
            | t -> internalfail ("expected struct, but got " + toString t)

    let rec private errorOr term = function
        | Error _ as e -> e
        | Union gvs -> Merging.guardedMap (errorOr term) gvs
        | _ -> term

    let private mutatePath read commit state path key update result =
        let originalValue = read state key in
        let mutatedValue = mutateField state path update originalValue in
        (errorOr result mutatedValue, commit state key mutatedValue)

    let private mutateStackPath = mutatePath readStackLocation mutateStack
    let private mutateHeapPath = mutatePath readHeapLocation mutateHeapLocation
    let private mutateStaticPath = mutatePath readStaticLocation mutateStaticLocation

    let internal mutate state reference value =
        match reference with
        | Error _ as e -> (e, state)
        | StackRef(name, path, _) -> mutateStackPath state path name (always value) value
        | HeapRef((addr, path), _) -> mutateHeapPath state path addr (always value) value
        | StaticRef(name, path, _) -> mutateStaticPath state path (Terms.MakeConcreteString name) (always value) value
        | Union gvs ->
            let mutateOneGuarded state (g, v) =
                match v with
                | Error _ -> (v, state)
                | StackRef(name, path, _) -> mutateStackPath state path name (Merging.merge2Terms g !!g value) value
                | HeapRef((addr, path), _) -> mutateHeapPath state path addr (Merging.merge2Terms g !!g value) value
                | t -> internalfail ("expected union of references, but got " + toString t)
            in
            let results, state = List.mapFold mutateOneGuarded state gvs in
            let gs = List.unzip gvs |> fst in
            (List.zip gs results |> Merging.merge, state)
        | t -> internalfail ("expected reference, but got " + toString t)

    let symbolizeState ((s, h, m, f, p) : state) =
        let rec symbolizeValue name location v =
           makeSymbolicInstance (isStaticLocation location) (Symbolization location) name (Terms.TypeOf v)
        in
        let s' = s |> Utils.MappedStack.map (fun key value -> symbolizeValue (fst key) (stackKeyToTerm (key, value)) value) in
        let h' = h |> Heap.map (fun key value -> symbolizeValue (toString key) (heapKeyToTerm (key, value)) value) in
        let m' = m |> Heap.map (fun key value -> symbolizeValue (toString key) (heapKeyToTerm (key, value)) value) in
        (s', h', m', f, p)

    type internal StateDiff =
        | Mutation of Term * Term
        | Allocation of Term * Term

    let symbolizeLocations ((_, h, _, _, _) as state) sourceRef locations =
        let rec symbolizeValue state = function
            | Mutation(location, _) ->
                let v, state = deref state location in
                let name = nameOfLocation location |> IdGenerator.startingWith in
                let result = makeSymbolicInstance (isStaticLocation location) (UnboundedRecursion (TermRef sourceRef)) name (Terms.TypeOf v) in
                mutate state location result
            | Allocation(location, value) ->
                match location with
                | StaticRef _ -> Nop, state
                | _ ->
                    let name = nameOfLocation location |> IdGenerator.startingWith in
                    allocateSymbolicInstance false (UnboundedRecursion (TermRef sourceRef)) name state (Terms.TypeOf value)
        in
        let terms, state = List.mapFold symbolizeValue state locations in
        terms |> List.filter (Terms.IsVoid >> not), state

// ------------------------------- Comparison -------------------------------

    let private compareHeaps h1 h2 =
        assert(Heap.size h1 <= Heap.size h2)
        let oldValues, newValues = Heap.partition (fun (k, _) -> Heap.contains k h1) h2 in
        let changedValues = List.filter (fun (k, v) -> h1.[k] <> v) oldValues in
        changedValues, newValues

    let rec private addrLess addr1 addr2 =
        match addr1, addr2 with
        | Concrete(a1, t1), Concrete(a2, t2) ->
            match t1, t2 with
            | _ when t1 = t2 && t1 = pointerType -> compare (a1 :?> int) (a2 :?> int)
            | String, String -> compare (a1 :?> string) (a2 :?> string)
            | _, String when t1 = pointerType -> -1
            | String, _ when t2 = pointerType -> 1
            | _ -> __notImplemented__()
        | Constant(name1, _, _), Constant(name2, _, _) -> compare name1 name2
        | Concrete _, _ -> -1
        | _, Concrete _ -> 1
        | _ -> __notImplemented__()

    let rec internal compareRefs ref1 ref2 =
        match ref1, ref2 with
        | _ when ref1 = ref2 -> 0
        | HeapRef((addr1, path1), _), HeapRef((addr2, path2), _) ->
            let h = addrLess addr1 addr2 in
            if h = 0 then List.compareWith addrLess path1 path2 else h
        | StackRef(name1, path1, _), StackRef(name2, path2, _) ->
            let h = compare name1 name2 in
            if h = 0 then List.compareWith addrLess path1 path2 else h
        | StaticRef(name1, path1, _), StaticRef(name2, path2, _) ->
            let h = compare name1 name2 in
            if h = 0 then List.compareWith addrLess path1 path2 else h
        | StackRef _, _ -> -1
        | _, StackRef _ -> 1
        | HeapRef _, _ -> -1
        | _, HeapRef _ -> 1
        | _ -> internalfail "compareRefs called with non-reference terms"

    let private sortMap mapper = List.sortWith (fun (k1, _) (k2, _) -> addrLess k1 k2) >> List.map mapper

    let rec private structDiff key val1 val2 =
        match val1, val2 with
        | Struct(fields1, _), Struct(fields2, _) ->
            let mutatedFields, addedFields = compareHeaps fields1 fields2 in
            let innerMutatedFields, innerAddedFields =
                mutatedFields |> sortMap (fun (k, v) -> structDiff (referenceSubLocation k key) fields1.[k] v) |> List.unzip
            in
            let overalMutatedFields = List.concat innerMutatedFields in
            let overalAddedFields =
                List.append
                    (sortMap (fun (k, v) -> (referenceSubLocation k key, v)) addedFields)
                    (List.concat innerAddedFields)
            in
            (overalMutatedFields, overalAddedFields)
        | _ ->
            [(key, val2)], []

    let internal diff ((s1, h1, m1, _, _) as state : state) ((s2, h2, m2, _, _) : state) =
        let mutatedStack = compareStacks s1 s2 in
        let mutatedHeap, newHeap = compareHeaps h1 h2 in
        let mutatedStatics, newStatics = compareHeaps m1 m2 in
        let mapper (key, newValue) =
            let oldValue, _ = deref state key in
            structDiff key oldValue newValue
        let mutatedStackFieldss,  newStackFieldss   = mutatedStack   |> sortMap mapper |> List.unzip in
        let mutatedHeapFieldss,   newHeapFieldss    = mutatedHeap    |> sortMap mapper |> List.unzip in
        let mutatedStaticFieldss, newStaticFieldss  = mutatedStatics |> sortMap mapper |> List.unzip in
        let overalMutatedValues =
            List.append3
                (List.concat mutatedStackFieldss)
                (List.concat mutatedHeapFieldss)
                (List.concat mutatedStaticFieldss)
        in
        let allocatedValues = (sortMap (fun (k, v) -> (heapKeyToTerm (k, v), v)) newHeap)::newHeapFieldss |> List.concat in
        List.append
            (List.map Mutation overalMutatedValues)
            (List.map Allocation allocatedValues)
