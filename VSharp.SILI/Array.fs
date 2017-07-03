namespace VSharp

open FSharpx.Collections

module Array =
    let internal lengthType = typedefof<int>
    let internal lengthTermType = Numeric lengthType in

// ------------------------------- Creation -------------------------------

    let internal zeroLowerBound rank =
        Array.init rank (fun _ -> Concrete(0, lengthTermType))

    let internal symbolicLowerBound array arrayName rank =
        match Options.SymbolicArrayLowerBoundStrategy() with
        | Options.AlwaysZero -> zeroLowerBound rank
        | Options.AlwaysSymbolic ->
            Array.init rank (fun i ->
                let idOfBound = sprintf "lower bound of %s" arrayName |> IdGenerator.startingWith in
                Constant(idOfBound, SymbolicArrayLength(array, i, false), lengthTermType))

    let internal dimensionsToLength =
        Array.fold ( *** ) (Concrete(1, lengthTermType))

    let rec internal length = function
        | Error _ as e -> e
        | Array(_, _, _, ds, _) -> dimensionsToLength ds
        | Terms.GuardedValues(gs, vs) ->
            vs |> List.map length |> List.zip gs |> Merging.merge
        | _ -> internalfail "computing length of non-array object"

    let rec private guardsProduct = function
        | [] -> [(Terms.MakeTrue, [])]
        | d::ds ->
            let current =
                match d with
                | Union gvs -> gvs
                | _ -> [(Terms.MakeTrue, d)]
            in
            let rest = guardsProduct ds in
            FSharpx.Collections.List.lift2 (fun (g1, v1) (g2, v2) -> (g1 &&& g2, v1::v2)) current rest

    let rec internal makeDefault defaultOf dimensions typ lowerBounds =
        let unguardedDimensions = guardsProduct dimensions in
        let makeArray dimensions =
            let length = dimensionsToLength dimensions in
            match length with
            | Terms.GuardedValues(gs, vs) -> internalfail "unexpected union in array length!"
            | _ ->
                Array(lowerBounds, None, List.empty, dimensions, typ)
        in
        unguardedDimensions |> List.map (fun (g, ds) -> (g, makeArray (Array.ofList ds))) |> Merging.merge

    let internal makeSymbolic source rank typ name =
        let idOfLength = IdGenerator.startingWith (sprintf "|%s|" name) in
        let constant = Constant(name, source, typ) in
        let lengths = Array.init rank (fun i -> Constant(idOfLength, SymbolicArrayLength(constant, i, true), Numeric lengthType)) in
        Array(symbolicLowerBound constant name rank, Some constant, List.empty, lengths, typ)

    let rec internal fromInitializer rank typ initializer =
        let rec flatten depth = function
            | Concrete(terms, _) when (terms :? Term list) ->
                let terms = terms :?> Term list in
                let children, dims = terms |> List.map (flatten (depth - 1)) |> List.unzip in
                match dims with
                | d::ds when not (List.forall ((=) d) ds) ->
                    failwith "Unexpected jugged array in multidimesional initializer!"
                | d::ds ->
                    List.concat children, (List.length children)::d
                | [] -> [], List.init depth (fun _ -> 0)
            | term -> [term], []
        let linearContent, dimensions = flatten rank initializer in
        let len = List.length linearContent in
        assert(len = List.fold (*) 1 dimensions)
        let intToTerm i = Concrete(i, lengthTermType) in
        let dimensionTerms = dimensions |> List.map intToTerm |> Array.ofList in
        let indices = List.init len intToTerm in
        let contents = List.zip indices linearContent in
        Array(zeroLowerBound rank, None, contents, dimensionTerms, typ)

// ------------------------------- Reading/writing -------------------------------

    let private unguardedAccess doJob returnValue merge2 array indices =
        match array with
        | Array(lowerBounds, constant, contents, dimensions, ArrayType(elementType, _)) ->
            assert(List.length indices = Array.length dimensions)
            let bounds =
                Seq.map3
                    (fun idx low len ->
                        let up = low +++ len in
                        let bigEnough = Arithmetics.simplifyGreaterOrEqual idx low id in
                        let smallEnough = Arithmetics.simplifyLess idx up id in
                        bigEnough &&& smallEnough)
                    indices lowerBounds dimensions
            in
            let inBounds = Seq.fold (&&&) Terms.MakeTrue bounds in
            let normalizedIndices = List.map2 (---) indices (List.ofArray lowerBounds) in
            let facticalAddress = List.fold ( *** ) (Concrete(1, lengthTermType)) normalizedIndices in
            match facticalAddress with
            | Error _ -> returnValue facticalAddress
            | _ ->
                let exn = Terms.MakeError(new System.IndexOutOfRangeException()) in
                let result = doJob lowerBounds constant contents dimensions elementType facticalAddress
                in merge2 inBounds !!inBounds result exn
        | t -> internalfail (sprintf "expected array, but %s got!" (toString t))

    let private accessUnguardedArray doJob merge merge2 returnValue guards indices array =
        indices |> List.map (unguardedAccess doJob returnValue merge2 array) |> merge guards

    let rec private accessArrayAt array indices returnValue merge merge2 doJob =
        let guards, unguardedIndices = guardsProduct indices |> List.unzip in
        match array with
        | Error _ -> returnValue array
        | Terms.GuardedValues(gs, vs) ->
            vs |> List.map (accessUnguardedArray doJob merge merge2 returnValue guards unguardedIndices) |> merge gs
        | _ -> accessUnguardedArray doJob merge merge2 returnValue guards unguardedIndices array

    let rec internal read defaultOf createSymbolic state array indices =
        accessArrayAt array indices
            (fun v -> (v, array, state))
            (fun guards triples ->
                let values, arrays, states = List.unzip3 triples in
                let mergedValues = Merging.merge (List.zip guards values) in
                let mergedArrays = Merging.merge (List.zip guards arrays) in
                let mergedStates = Merging.mergeStates guards states in
                (mergedValues, mergedArrays, mergedStates))
            (fun u1 u2 (v, a, s) w -> (Merging.merge2Terms u1 u2 v w, a, s))
            (fun lowerBounds maybeConstant contents dimensions elementType address ->
            let readSymbolicIndex idx =
                match maybeConstant with
                | None -> (defaultOf elementType, array, state)
                | Some constant ->
                    let id = sprintf "%s[%s]" (toString constant) (toString idx) |> IdGenerator.startingWith in
                    let value, state = createSymbolic (ArrayAccess(constant, idx)) id state elementType in
                    let newContents = List.append contents [(idx, value)] in
                    let typ = ArrayType(elementType, dimensions.Length) in
                    let newArray = Array(lowerBounds, maybeConstant, newContents, dimensions, typ) in
                    (value, newArray, state)
            in
            if Terms.IsConcrete address && List.forall (fst >> Terms.IsConcrete) contents
            then
                match List.tryFind (fst >> ((=) address)) contents with
                | None -> readSymbolicIndex address
                | Some(_, value) -> (value, array, state)
            else
                let defaultValue, newArray, newState = readSymbolicIndex address in
                let matchedContents, g =
                    contents |> (Terms.MakeTrue |> List.mapFold (fun g (idx, value) ->
                        let isIdx = Arithmetics.simplifyEqual address idx id in
                        (g &&& isIdx, value), g &&& !!isIdx))
                in
                (List.append matchedContents [(g, defaultValue)] |> Merging.merge, newArray, newState))

    let rec internal write array indices value =
        accessArrayAt array indices id (fun gs vs -> List.zip gs vs |> Merging.merge) Merging.merge2Terms
            (fun lowerBounds constant contents dimensions elementType address ->
                let filteredContents = contents |> List.filter (fst >> ((=) address) >> not) in
                let typ = ArrayType(elementType, dimensions.Length) in
                Array(lowerBounds, constant, (address, value)::filteredContents, dimensions, typ))
