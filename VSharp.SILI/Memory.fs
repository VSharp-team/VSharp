namespace VSharp

open VSharp.State

module internal Memory =

// ------------------------------- Dereferencing -------------------------------

    let private pointerType = Numeric typedefof<int> in

    let private extractHeapAddress = function
        | Concrete(address, t) when Types.IsNumeric t -> ConcreteAddress (address :?> int)
        | Concrete(typeName, t) when Types.IsString t -> StaticAddress (typeName :?> string)
        | Constant(name, _) -> SymbolicAddress name
        | term -> failwith ("Internal error: expected primitive heap address " + (toString term))

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
            failwith "Internal error: reference comparing non-reference types"
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
                if not (fields.ContainsKey(name)) then failwith (format2 "Internal error: {0} does not contain field {1}" s name)
                structDeref path' fields.[name]
            | Terms.GuardedValues(gs, vs) ->
                vs |> List.map (structDeref path) |> List.zip gs |> Merging.merge
            | t -> failwith ("Internal error: expected struct, but got " + (toString t))

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
        | t -> failwith ("Internal error: deref expected reference, but got " + (toString t))

    let internal valueOf = stackValue
    let internal fieldOf term name = structDeref [name] term

// ------------------------------- Referencing -------------------------------

    let rec private addFieldToPath name = function
        | Error _ as e -> e
        | StackRef(var, idx, path, t) -> StackRef(var, idx, name::path, t)
        | HeapRef(addr, path, t) -> HeapRef(addr, name::path, t)
        | Terms.GuardedValues(gs, vs) -> vs |> List.map (addFieldToPath name) |> List.zip gs |> Union
        | t -> failwith ("Internal error: expected reference, but got " + (toString t))

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
        | t -> failwith ("Internal error: expected reference or struct, but got " + (toString t))

    let internal referenceToField state name parentRef =
        referenceToFieldOf state name parentRef (deref state parentRef)

    let rec internal referenceToStaticField state fieldName typeName =
        HeapRef(Concrete(typeName, String), [fieldName], String)

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

    let internal allocateInHeap ((e, h, f, p) : state) term isSymbolic : Term * state =
        let pointer, address =
            match isSymbolic with
            | true ->
                let address = IdGenerator.startingWith("addr") in
                HeapRef (Constant(address, pointerType), [], pointerType), SymbolicAddress address
            | false ->
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

    let rec internal allocateSymbolicStruct isStatic state t dotNetType =
        let fields, state = Types.GetFieldsOf dotNetType isStatic |> mapFoldMap (allocateSymbolicInstance false) state in
        (Struct(fields, t), state)

    and internal allocateSymbolicInstance isStatic name state = function
        | t when Types.IsPrimitive t -> (Constant(name, t), state)
        | StructType dotNetType as t -> allocateSymbolicStruct isStatic state t dotNetType
        | ClassType dotNetType as t ->
            let value, state = allocateSymbolicStruct isStatic state t dotNetType in
            allocateInHeap state value false
        | ArrayType _ as t ->
            // TODO!!!
            (Concrete(null, t), state)
        | _ -> __notImplemented__()

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
            | t -> failwith ("Internal error: expected struct, but got " + (toString t))

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

    let internal mutate state reference term =
        match reference with
        | Error _ as e -> (e, state)
        | StackRef(name, idx, path, _) -> mutateStackPath state path name idx (always term) term
        | HeapRef(addr, path, _) -> mutateHeapPath state path addr (always term) term
        | Union gvs ->
            let mutateOneGuarded state (g, v) =
                match v with
                | Error _ -> (v, state)
                | StackRef(name, idx, path, _) -> mutateStackPath state path name idx (Merging.merge2Terms g !!g term) term
                | HeapRef(addr, path, _) -> mutateHeapPath state path addr (Merging.merge2Terms g !!g term) term
                | t -> failwith ("Internal error: expected union of references, but got " + (toString t))
            in
            let results, state = List.mapFold mutateOneGuarded state gvs in
            let gs = List.unzip gvs |> fst in
            (List.zip gs results |> Merging.merge, state)
        | t -> failwith ("Internal error: expected reference, but got " + (toString t))
