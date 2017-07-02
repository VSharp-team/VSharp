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

    let private stackKeyToTerm (name, vals) =
        StackRef(name, (Stack.size vals) - 1, [], Terms.TypeOf (Stack.peak vals))

    let private heapKeyToTerm (key, value) =
        match key with
        | ConcreteAddress addr ->
            HeapRef(Concrete(addr, pointerType), [], Terms.TypeOf value)
        | StaticAddress addr ->
            HeapRef(Concrete(addr, String), [], Terms.TypeOf value)
        | SymbolicAddress(addr, source) ->
            HeapRef(Constant(addr, source, pointerType), [], Terms.TypeOf value)

    let private isStaticLocation = function
        | HeapRef(Concrete(typeName, t), _, _) when Types.IsString t -> true
        | _ -> false

    let private stackValue ((e, _, _, _) : state) name = e.[name] |> Stack.peak
    let private stackDeref ((e, _, _, _) : state) name idx = e.[name] |> Stack.middle idx
    let private heapDeref ((_, h, _, _) : state) addr = h.[addr]

    let internal npe () = Terms.MakeError(new System.NullReferenceException()) in

    let rec private refToInt = function
        | Error _ as e -> e
        | Concrete(null, _) -> Concrete(0, pointerType)
        | HeapRef(addr, _, t) -> addr
        | Terms.GuardedValues(gs, vs) -> vs |> List.map refToInt |> List.zip gs |> Merging.merge
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
        | Terms.GuardedValues(gs, vs) -> vs |> List.map isNull |> List.zip gs |> Merging.merge
        | _ -> Terms.MakeFalse

    let rec internal npeIfNull = function
        | Error _ as e -> e
        | Concrete(null, _) -> npe()
        | HeapRef(addr, _, t) as reference ->
            let isNull = Arithmetics.simplifyEqual addr (Concrete(0, pointerType)) id in
            Merging.merge2Terms isNull !!isNull (npe()) reference
        | Terms.GuardedValues(gs, vs) -> vs |> List.map npeIfNull |> List.zip gs |> Merging.merge
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
            | Terms.GuardedValues(gs, vs) ->
                vs |> List.map (structDeref path) |> List.zip gs |> Merging.merge
            | t -> internalfail ("expected struct, but got " + (toString t))

    let rec internal deref state = function
        | Error _ as e -> e
        | StackRef(name, idx, path, _) -> structDeref (List.rev path) (stackDeref state name idx)
        | HeapRef(Concrete(typeName, t), path, _) when Types.IsString t -> structDeref (List.rev path) (heapDeref state (StaticAddress (typeName :?> string)))
        | HeapRef(addr, path, _) ->
            let isNull = Arithmetics.simplifyEqual addr (Concrete(0, pointerType)) id in
            match isNull with
            | Terms.False -> structDeref (List.rev path) (heapDeref state (extractHeapAddress addr))
            | Terms.True -> npe()
            | _ ->
                let derefed = structDeref (List.rev path) (heapDeref state (extractHeapAddress addr)) in
                Merging.merge2Terms isNull !!isNull (npe()) derefed
        | Terms.GuardedValues(gs, vs) -> vs |> List.map (deref state) |> List.zip gs |> Merging.merge
        | t -> internalfail ("deref expected reference, but got " + (toString t))

    let internal valueOf = stackValue
    let internal fieldOf term name = structDeref [name] term

// ------------------------------- Referencing -------------------------------

    let rec private addFieldToPath name = function
        | Error _ as e -> e
        | StackRef(var, idx, path, t) -> StackRef(var, idx, name::path, t)
        | HeapRef(addr, path, t) -> HeapRef(addr, name::path, t)
        | Terms.GuardedValues(gs, vs) -> vs |> List.map (addFieldToPath name) |> List.zip gs |> Union
        | t -> internalfail ("expected reference, but got " + (toString t))

    let rec private referenceTerm state name followHeapRefs = function
        | Error _ as e -> e
        | StackRef _ as r -> r
        | HeapRef _ as r when followHeapRefs -> r
        | Terms.GuardedValues(gs, vs) -> List.map (referenceTerm state name followHeapRefs) vs |> List.zip gs |> Merging.merge
        | term -> StackRef(name, (Stack.size (environment state).[name]) - 1, [], Terms.TypeOf term)

    let internal referenceToVariable state name followHeapRefs =
        referenceTerm state name followHeapRefs (stackValue state name)

    let rec private referenceToFieldOf state name parentRef = function
        | Error _ as e -> e
        | HeapRef(addr, path, t) ->
            assert(List.isEmpty path) // TODO: will this really be always empty?
            HeapRef(addr, name::path, t)
        | Struct _ -> addFieldToPath name parentRef
        | Terms.GuardedValues(gs, vs) ->
            vs |> List.map (referenceToFieldOf state name parentRef) |> List.zip gs |> Merging.merge
        | t -> internalfail ("expected reference or struct, but got " + (toString t))

    let rec private followOrReturnReference state reference =
        match deref state reference with
        | Error _ as e -> e
        | StackRef _ as r -> r
        | HeapRef _ as r -> r
        | Terms.GuardedValues(gs, vs) -> List.map (followOrReturnReference state) vs |> List.zip gs |> Merging.merge
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

    let internal allocateOnStack ((e, h, f, p) : state) name term : state =
        let existing = if e.ContainsKey(name) then e.[name] else Stack.empty in
        (e.Add(name, Stack.push existing term), h, Stack.updateHead f (name::(Stack.peak f)), p)

    let internal allocateInHeap ((e, h, f, p) : state) term clusterSource : Term * state =
        let pointer, address =
            match clusterSource with
            | Some source ->
                let address = IdGenerator.startingWith("cluster") in
                HeapRef(Constant(address, source, pointerType), [], Terms.TypeOf term), SymbolicAddress(address, source)
            | None ->
                let address = freshAddress()
                HeapRef (Concrete(address, pointerType), [], Terms.TypeOf term), ConcreteAddress address
        (pointer, (e, h.Add(address, term), f, p))

    let internal allocateInStaticMemory ((e, h, f, p) : state) typeName term =
        let address = StaticAddress typeName in
        (e, h.Add(address, term), f, p)

    let rec defaultOf = function
        | Bool -> Terms.MakeFalse
        | Numeric t when t.IsEnum -> Terms.MakeConcrete (System.Activator.CreateInstance(t)) t
        | Numeric t -> Terms.MakeConcrete 0 t
        | String -> Concrete(null, String)
        | ClassType _ as t -> Concrete(null, t)
        | ArrayType _ as t -> Concrete(null, t)
        | Object -> Concrete(null, Object)
        | Func _ -> Concrete(null, Object)
        | StructType dotNetType as t ->
            let fields = Types.GetFieldsOf dotNetType false in
            Struct(Map.map (fun _ -> defaultOf) fields, t)
        | _ -> __notImplemented__()
        
    // GZ: Drop state
    let rec internal makeSymbolicStruct isStatic source state t dotNetType =
        let fields, state = Types.GetFieldsOf dotNetType isStatic |> mapFoldMap (fun name state -> makeSymbolicInstance false (FieldAccess(name, source)) name state) state in
        (Struct(fields, t), state)
    
    and internal makeSymbolicInstance isStatic source name state = function
        | t when Types.IsPrimitive t || Types.IsObject t || Types.IsFunction t -> (Constant(name, source, t), state)
        | StructType dotNetType as t -> makeSymbolicStruct isStatic source state t dotNetType
        | ClassType dotNetType as t  -> makeSymbolicStruct isStatic source state t dotNetType
        | ArrayType(e, d) as t -> (Array.makeSymbolic source d t name, state)
        | PointerType termType as t -> 
            match termType with
            | ClassType _
            | ArrayType _ ->
                let address = IdGenerator.startingWith("addr") in
                HeapRef (Constant(address, source, pointerType), [], t), state
            | StructType _ -> internalfail "symbolization of PointerType of StructType"
            | _ -> __notImplemented__()
        | _ -> __notImplemented__()
        
        
    and internal allocateSymbolicInstance isStatic source name state t =
        let value, state = makeSymbolicInstance isStatic source name state t in
        allocateInHeap state value None
        
// ------------------------------- Mutation -------------------------------

    let private mutateTop ((e, h, f, p) : state) name term : state =
        assert (e.ContainsKey(name))
        (e.Add(name, Stack.updateHead e.[name] term), h, f, p)

    let private mutateMiddle ((e, h, f, p) : state) name idx term : state =
        assert (e.ContainsKey(name))
        (e.Add(name, Stack.updateMiddle e.[name] idx term), h, f, p)

    let private mutateHeap ((e, h, f, p) : state) addr term : state =
        assert (h.ContainsKey(addr))
        (e, h.Add(addr, term), f, p)

    let rec private mutateField state path update term =
        match path with
        | [] -> update term
        | name::path' ->
            match term with
            | Error _ as e -> e
            | Struct(fields, t) ->
                let newField = mutateField state path' update fields.[name] in
                Struct(fields.Add(name, newField), t)
            | Terms.GuardedValues(gs, vs) ->
                vs |> List.map (mutateField state path update) |> List.zip gs |> Merging.merge
            | t -> internalfail ("expected struct, but got " + (toString t))

    let rec private errorOr term = function
        | Error _ as e -> e
        | Terms.GuardedValues(gs, vs) ->
            vs |> List.map (errorOr term) |> List.zip gs |> Merging.merge
        | _ -> term

    let private mutateStackPath state path name idx update result =
        let originalValue = stackDeref state name idx in
        let mutatedValue = mutateField state (List.rev path) update originalValue in
        (errorOr result mutatedValue, mutateMiddle state name idx mutatedValue)

    let private mutateHeapPath state path addr update result =
        let heapKey = extractHeapAddress addr in
        let originalValue = heapDeref state heapKey in
        let mutatedValue = mutateField state (List.rev path) update originalValue in
        (errorOr result mutatedValue, mutateHeap state heapKey mutatedValue)

    let internal mutate state reference value =
        match reference with
        | Error _ as e -> (e, state)
        | StackRef(name, idx, path, _) -> mutateStackPath state path name idx (always value) value
        | HeapRef(addr, path, _) -> mutateHeapPath state path addr (always value) value
        | Union gvs ->
            let mutateOneGuarded state (g, v) =
                match v with
                | Error _ -> (v, state)
                | StackRef(name, idx, path, _) -> mutateStackPath state path name idx (Merging.merge2Terms g !!g value) value
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
            | Terms.GuardedValues(gs, vs) ->
                vs |> List.map refine |> List.zip gs |> Merging.merge
            | t -> t
        let resultingArray = refine mutatedArray in
        let _, state = mutate state reference resultingArray in
        (mutatedArray, state)

    let symbolizeState ((e, h, f, p) : state) : state =
        let rec symbolizeValue name location v =
           makeSymbolicInstance (isStaticLocation location) (Symbolization location) name (e, h, f, p) (Terms.TypeOf v) |> fst
        in
        let e' = e |> Map.map (fun key values -> Stack.updateHead values (symbolizeValue key (stackKeyToTerm (key, values)) (Stack.peak values))) in
        let h' = h |> Map.map (fun key value -> symbolizeValue (toString key) (heapKeyToTerm (key, value)) value) in
        (e', h', f, p)

    type internal StateDiff =
        | Mutation of Term * Term
        | Allocation of Term * Term

    let symbolizeLocations state locations =
        let rec symbolizeValue state = function
            | Mutation(location, _) ->
                let v = deref state location in
                let name = IdGenerator.startingWith (toString location) in
                let result, state = makeSymbolicInstance (isStaticLocation location) (UnboundedRecursion (ref Nop)) name state (Terms.TypeOf v) in
                mutate state location result // TODO: raw mutate here after refactoring!
            | Allocation(location, value) ->
                let name = IdGenerator.startingWith (toString location) in
                allocateSymbolicInstance false (UnboundedRecursion (ref Nop)) name state (Terms.TypeOf value)
        in
        List.mapFold symbolizeValue state locations

// ------------------------------- Comparison -------------------------------

    let private compareMaps m1 m2 =
        assert(Map.count m1 <= Map.count m2)
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
                | StackRef(a, n, p, t) -> StackRef(a, n, field::p, t)
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

    let internal diff ((e1, h1, _, _) : state) ((e2, h2, _, _) : state) =
        let mutatedStack, newStack = compareMaps e1 e2 in
        let mutatedHeap,  newHeap  = compareMaps h1 h2 in
        let stackKvpToTerms (name, vals) =
            let oldTerm = Stack.peak e1.[name] in
            let newTerm = Stack.peak vals in
            let reference = stackKeyToTerm (name, vals) in
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
        let overalAllocatedValues =
            List.append
                ((sortMap newStack (fun (k, v) -> (stackKeyToTerm (k, v), Stack.peak v)))::newStackFieldss |> List.concat)
                ((sortMap newHeap  (fun (k, v) -> (heapKeyToTerm  (k, v), v)))::newHeapFieldss |> List.concat)
        in
        List.append
            (List.map Mutation overalMutatedValues)
            (List.map Allocation overalAllocatedValues)

    let internal compareRefs ref1 ref2 =
        match ref1, ref2 with
        | _ when ref1 = ref2 -> 0
        | StackRef _, HeapRef _ -> -1
        | HeapRef _, StackRef _ -> 1
        | StackRef(name1, n1, _, _), StackRef(name2, n2, _, _) -> if name1 = name2 && n1 < n2 || name1 < name2 then -1 else 1
        | HeapRef _, HeapRef _ -> if extractHeapAddress ref1 < extractHeapAddress ref2 then -1 else 1
        | _ -> internalfail "compareRefs called with non-reference terms"
