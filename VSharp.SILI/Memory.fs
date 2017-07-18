namespace VSharp

open VSharp.State

module internal Memory =

// ------------------------------- Dereferencing -------------------------------

    let private pointerType = Numeric typedefof<int> in

    let private extractHeapAddress = function
        | Concrete(address, t) when Types.IsNumeric t -> ConcreteAddress (address :?> int)
        | Concrete(typeName, t) when Types.IsString t -> StaticAddress (typeName :?> string)
        | Constant(name, source, _) -> SymbolicAddress(name, source)
        | term -> internalfail ("expected primitive heap address " + (toString term))

    let private stackKeyToTerm (key, value) =
        StackRef(key, [], Terms.TypeOf value |> PointerType)

    let private heapKeyToTerm (key, value) =
        let t = Terms.TypeOf value |> PointerType in
        match key with
        | ConcreteAddress addr ->
            HeapRef(Concrete(addr, pointerType), [], t)
        | StaticAddress addr ->
            HeapRef(Concrete(addr, String), [], t)
        | SymbolicAddress(addr, source) ->
            HeapRef(Constant(addr, source, pointerType), [], t)

    let private isStaticLocation = function
        | HeapRef(Concrete(typeName, String), _, _) -> true
        | _ -> false

    let private nameOfLocation = function
        | HeapRef(_, x::xs, _) -> x
        | HeapRef(_, _, t) -> toString t
        | StackRef((name, _), x::xs, _) -> sprintf "%s.%s" name x
        | StackRef((name, _), _, _) -> name
        | l -> "requested name of an unexpected location " + (toString l) |> internalfail

    let private stackDeref ((s, _, _, _) as state : state) key = derefStack state key
    let private heapDeref ((_, h, _, _) : state) addr = h.[addr]

    let internal npe () = Terms.MakeError(new System.NullReferenceException()) in

    let rec private refToInt = function
        | Error _ as e -> e
        | Concrete(null, _) -> Concrete(0, pointerType)
        | HeapRef(addr, _, t) -> addr
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
        | HeapRef(addr, _, t) when Terms.IsInteger addr ->
            Arithmetics.simplifyEqual addr (Concrete(0, pointerType)) id
        | Union gvs -> Merging.guardedMap isNull gvs
        | _ -> Terms.MakeFalse

    let rec internal npeIfNull = function
        | Error _ as e -> e
        | Concrete(null, _) -> npe()
        | HeapRef(addr, _, t) as reference ->
            let isNull = Arithmetics.simplifyEqual addr (Concrete(0, pointerType)) id in
            Merging.merge2Terms isNull !!isNull (npe()) reference
        | Union gvs -> Merging.guardedMap npeIfNull gvs
        | t -> t

    let rec private structDeref path term =
        match path with
        | [] -> term
        | name::path' ->
            match term with
            | Error _ -> term
            | Struct(fields, _) as s ->
                if not (fields.ContainsKey(name)) then
                    internalfail (format2 "{0} does not contain field {1}" s name)
                structDeref path' fields.[name]
            | Union gvs -> Merging.guardedMap (structDeref path) gvs
            | t -> internalfail ("expected struct, but got " + (toString t))

    let rec internal deref state = function
        | Error _ as e -> e
        | StackRef(name, path, _) -> structDeref (List.rev path) (stackDeref state name)
        | HeapRef(Concrete(typeName, t), path, _) when Types.IsString t -> structDeref (List.rev path) (heapDeref state (StaticAddress (typeName :?> string)))
        | HeapRef(addr, path, _) ->
            let isNull = Arithmetics.simplifyEqual addr (Concrete(0, pointerType)) id in
            match isNull with
            | Terms.False -> structDeref (List.rev path) (heapDeref state (extractHeapAddress addr))
            | Terms.True -> npe()
            | _ ->
                let derefed = structDeref (List.rev path) (heapDeref state (extractHeapAddress addr)) in
                Merging.merge2Terms isNull !!isNull (npe()) derefed
        | Union gvs -> Merging.guardedMap (deref state) gvs
        | t -> internalfail ("deref expected reference, but got " + (toString t))

    let internal valueOf = stackDeref
    let internal fieldOf term name = structDeref [name] term

// ------------------------------- Referencing -------------------------------

    let rec private addFieldToPath name = function
        | Error _ as e -> e
        | StackRef(var, path, t) -> StackRef(var, name::path, t)
        | HeapRef(addr, path, t) -> HeapRef(addr, name::path, t)
        | Terms.GuardedValues(gs, vs) -> vs |> List.map (addFieldToPath name) |> List.zip gs |> Union
        | t -> internalfail ("expected reference, but got " + (toString t))

    let rec private referenceTerm state name followHeapRefs = function
        | Error _ as e -> e
        | StackRef _ as r -> r
        | HeapRef _ as r when followHeapRefs -> r
        | Union gvs -> Merging.guardedMap (referenceTerm state name followHeapRefs) gvs
        | term -> StackRef(name, [], Terms.TypeOf term)

    let internal referenceToVariable state name followHeapRefs =
        referenceTerm state name followHeapRefs (stackDeref state name)

    let rec private referenceToFieldOf state name parentRef = function
        | Error _ as e -> e
        | HeapRef(addr, path, t) ->
            assert(List.isEmpty path) // TODO: will this really be always empty?
            HeapRef(addr, name::path, t)
        | Struct _ -> addFieldToPath name parentRef
        | Union gvs -> Merging.guardedMap (referenceToFieldOf state name parentRef) gvs
        | t -> internalfail ("expected reference or struct, but got " + (toString t))

    let rec private followOrReturnReference state reference =
        match deref state reference with
        | Error _ as e -> e
        | StackRef _ as r -> r
        | HeapRef _ as r -> r
        | Union gvs -> Merging.guardedMap (followOrReturnReference state) gvs
        | term -> reference

    let internal referenceToField state followHeapRefs name parentRef =
        let reference = referenceToFieldOf state name parentRef (deref state parentRef) in
        if followHeapRefs then followOrReturnReference state reference
        else reference

    let rec internal referenceToStaticField state followHeapRefs fieldName typeName =
        let reference = HeapRef(Concrete(typeName, String), [fieldName], String) in
        if followHeapRefs then followOrReturnReference state reference
        else reference

// ------------------------------- Allocation -------------------------------

    let private pointer = ref 0
    let private freshAddress () =
        pointer := !pointer + 1
        !pointer
    let public resetHeap () =
        pointer := 0

    let internal allocateOnStack ((s, h, f, p) as state : state) key term : state =
        (pushToStack state key term, h, Stack.updateHead f (key::(Stack.peak f)), p)

    let internal allocateInHeap ((s, h, f, p) : state) term clusterSource : Term * state =
        let pointer, address =
            match clusterSource with
            | Some source ->
                let address = IdGenerator.startingWith("cluster") in
                HeapRef(Constant(address, source, pointerType), [], Terms.TypeOf term), SymbolicAddress(address, source)
            | None ->
                let address = freshAddress()
                HeapRef (Concrete(address, pointerType), [], Terms.TypeOf term), ConcreteAddress address
        (pointer, (s, h.Add(address, term), f, p))

    let internal allocateInStaticMemory ((s, h, f, p) : state) typeName term =
        let address = StaticAddress typeName in
        (s, h.Add(address, term), f, p)

    let rec defaultOf = function
        | Bool -> Terms.MakeFalse
        | Numeric t when t.IsEnum -> Terms.MakeConcrete (System.Activator.CreateInstance(t)) t
        | Numeric t -> Terms.MakeConcrete 0 t
        | String -> Concrete(null, String)
        | ClassType _ as t -> Concrete(null, t)
        | ArrayType _ as t -> Concrete(null, t)
        | Object name as t -> Concrete(name, t)
        | SubType (name, _) as t -> Concrete(name, t)
        | Func _ -> Concrete(null, Object "func")
        | StructType dotNetType as t ->
            let fields = Types.GetFieldsOf dotNetType false in
            Struct(Map.map (fun _ -> defaultOf) fields, t)
        | _ -> __notImplemented__()
        
    let rec internal makeSymbolicStruct isStatic source t dotNetType =
        let updateSource field =
            match source with
            | Symbolization(HeapRef(loc, path, t)) -> Symbolization(HeapRef(loc, field::path, t))
            | Symbolization(StackRef(loc, path, t)) -> Symbolization(StackRef(loc, field::path, t))
            | _ -> FieldAccess(field, source)
        let fields = Types.GetFieldsOf dotNetType isStatic |> Map.map (fun name -> makeSymbolicInstance false (updateSource name) name) in
        Struct(fields, t)
    
    and internal makeSymbolicInstance isStatic source name = function
        | t when Types.IsPrimitive t || Types.IsObject t || Types.IsFunction t -> Constant(name, source, t)
        | StructType dotNetType as t -> makeSymbolicStruct isStatic source t dotNetType
        | ClassType dotNetType as t  -> makeSymbolicStruct isStatic source t dotNetType
        | SubType (dotNetType, name) as t -> makeSymbolicStruct isStatic source t dotNetType
        | ArrayType(e, d) as t -> Array.makeSymbolic source d t name
        | PointerType termType as t -> 
            match termType with
            | ClassType _
            | ArrayType _ ->
                let address = IdGenerator.startingWith("addr") in
                HeapRef (Constant(address, source, pointerType), [], t)
            | StructType _ -> internalfail "symbolization of PointerType of StructType"
            | _ -> __notImplemented__()
        | _ -> __notImplemented__()
        
        
    and internal allocateSymbolicInstance isStatic source name state t =
        let value = makeSymbolicInstance isStatic source name t in
        allocateInHeap state value None
        
// ------------------------------- Mutation -------------------------------

    let private mutateStack ((s, h, f, p) as state : state) key term : state =
        assert (isAllocatedOnStack state key)
        (mutateStack state key term, h, f, p)

    let private mutateHeap ((s, h, f, p) : state) addr term : state =
        assert (h.ContainsKey(addr))
        (s, h.Add(addr, term), f, p)

    let rec private mutateField state path update term =
        match path with
        | [] -> update term
        | name::path' ->
            match term with
            | Error _ as e -> e
            | Struct(fields, t) ->
                let newField = mutateField state path' update fields.[name] in
                Struct(fields.Add(name, newField), t)
            | Union gvs -> Merging.guardedMap (mutateField state path update) gvs
            | t -> internalfail ("expected struct, but got " + (toString t))

    let rec private errorOr term = function
        | Error _ as e -> e
        | Union gvs -> Merging.guardedMap (errorOr term) gvs
        | _ -> term

    let private mutateStackPath state path key update result =
        let originalValue = stackDeref state key in
        let mutatedValue = mutateField state (List.rev path) update originalValue in
        (errorOr result mutatedValue, mutateStack state key mutatedValue)

    let private mutateHeapPath state path addr update result =
        let heapKey = extractHeapAddress addr in
        let originalValue = heapDeref state heapKey in
        let mutatedValue = mutateField state (List.rev path) update originalValue in
        (errorOr result mutatedValue, mutateHeap state heapKey mutatedValue)

    let internal mutate state reference value =
        match reference with
        | Error _ as e -> (e, state)
        | StackRef(name, path, _) -> mutateStackPath state path name (always value) value
        | HeapRef(addr, path, _) -> mutateHeapPath state path addr (always value) value
        | Union gvs ->
            let mutateOneGuarded state (g, v) =
                match v with
                | Error _ -> (v, state)
                | StackRef(name, path, _) -> mutateStackPath state path name (Merging.merge2Terms g !!g value) value
                | HeapRef(addr, path, _) -> mutateHeapPath state path addr (Merging.merge2Terms g !!g value) value
                | t -> internalfail ("expected union of references, but got " + (toString t))
            in
            let results, state = List.mapFold mutateOneGuarded state gvs in
            let gs = List.unzip gvs |> fst in
            (List.zip gs results |> Merging.merge, state)
        | t -> internalfail ("expected reference, but got " + (toString t))

    let internal mutateArray state reference indices value =
        let originalArray = deref state reference in
        let mutatedArray = Array.write originalArray indices value in
        let rec refine = function
            | Error _ -> originalArray
            | Union gvs -> Merging.guardedMap refine gvs
            | t -> t
        let resultingArray = refine mutatedArray in
        let _, state = mutate state reference resultingArray in
        (mutatedArray, state)

    let symbolizeState ((s, h, f, p) as state : state) : state =
        let rec symbolizeValue name location v =
           makeSymbolicInstance (isStaticLocation location) (Symbolization location) name (Terms.TypeOf v)
        in
        let s' = state |> stackMap (fun key value -> symbolizeValue (fst key) (stackKeyToTerm (key, value)) value) in
        let h' = h |> Map.map (fun key value -> symbolizeValue (toString key) (heapKeyToTerm (key, value)) value) in
        (s', h', f, p)

    type internal StateDiff =
        | Mutation of Term * Term
        | Allocation of Term * Term

    let symbolizeLocations state sourceRef locations =
        let rec symbolizeValue state = function
            | Mutation(location, _) ->
                let v = deref state location in
                let name = nameOfLocation location |> IdGenerator.startingWith in
                let result = makeSymbolicInstance (isStaticLocation location) (UnboundedRecursion (TermRef sourceRef)) name (Terms.TypeOf v) in
                mutate state location result
            | Allocation(location, value) ->
                match location, state with
                | HeapRef(Concrete(addr, String), [], _), (_, h, _, _) when h.ContainsKey(StaticAddress(addr :?> string)) ->
                    Nop, state
                | _ ->
                    let name = nameOfLocation location |> IdGenerator.startingWith in
                    allocateSymbolicInstance false (UnboundedRecursion (TermRef sourceRef)) name state (Terms.TypeOf value)
        in
        let terms, state = List.mapFold symbolizeValue state locations in
        terms |> List.filter (Terms.IsVoid >> not), state

// ------------------------------- Comparison -------------------------------

    let private compareMaps (m1 : Map<_, _>) (m2 : Map<_, _>) =
        assert(m1.Count <= m2.Count)
        let oldValues, newValues = Map.partition (fun k _ -> Map.containsKey k m1) m2 in
        let _, changedValues = Map.partition (fun k v -> m1.[k] = v) oldValues in
        changedValues, newValues

    let private sortMap map mapper =
        map |> Map.toList |> List.sortBy fst |> List.map mapper

    let rec private structDiff key val1 val2 =
        match val1, val2 with
        | Struct(fields1, _), Struct(fields2, _) ->
            let mutatedFields, addedFields = compareMaps fields1 fields2 in
            let addFieldToKey key field =
                match key with
                | HeapRef(a, p, t) -> HeapRef(a, field::p, t)
                | StackRef(a, p, t) -> StackRef(a, field::p, t)
                | _ -> __notImplemented__()
            in
            let innerMutatedFields, innerAddedFields =
                sortMap mutatedFields (fun (k, v) -> structDiff (addFieldToKey key k) fields1.[k] v) |> List.unzip
            in
            let overalMutatedFields = List.concat innerMutatedFields in
            let overalAddedFields =
                List.append
                    (sortMap addedFields (fun (k, v) -> (addFieldToKey key k, v)))
                    (List.concat innerAddedFields)
            in
            (overalMutatedFields, overalAddedFields)
        | _ ->
            [(key, val2)], []

    let internal diff ((s1, h1, _, _) as state : state) ((s2, h2, _, _) : state) =
        let mutatedStack = compareStacks s1 s2 in
        let mutatedHeap,  newHeap  = compareMaps h1 h2 in
        let stackKvpToTerms (key, value) =
            let oldTerm = derefStack state key in
            let newTerm = value in
            let reference = stackKeyToTerm (key, value) in
            structDiff reference oldTerm newTerm
        in
        let heapKvpToTerms (key, value) =
            let oldValue = h1.[key] in
            let reference = heapKeyToTerm (key, value) in
            structDiff reference oldValue value
        in
        let mutatedStackFieldss, newStackFieldss = sortMap mutatedStack stackKvpToTerms |> List.unzip in
        let mutatedHeapFieldss,  newHeapFieldss  = sortMap mutatedHeap  heapKvpToTerms  |> List.unzip in
        let overalMutatedValues =
            List.append
                (List.concat mutatedStackFieldss)
                (List.concat mutatedHeapFieldss)
        in
        let allocatedValues = (sortMap newHeap  (fun (k, v) -> (heapKeyToTerm  (k, v), v)))::newHeapFieldss |> List.concat in
        List.append
            (List.map Mutation overalMutatedValues)
            (List.map Allocation allocatedValues)

    let internal compareRefs ref1 ref2 =
        match ref1, ref2 with
        | _ when ref1 = ref2 -> 0
        | StackRef _, HeapRef _ -> -1
        | HeapRef _, StackRef _ -> 1
        | StackRef(name1, path1, _), StackRef(name2, path2, _) ->
            if name1 < name2 || name1 = name2 && path1 < path2 then -1 else 1
        | HeapRef(addr1, path1, _), HeapRef(addr2, path2, _) ->
            let haddr1 = extractHeapAddress addr1 in
            let haddr2 = extractHeapAddress addr2 in
            if haddr1 < haddr2 || haddr1 = haddr2 && path1 < path2 then -1 else 1
        | _ -> internalfail "compareRefs called with non-reference terms"
