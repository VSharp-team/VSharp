namespace VSharp

module Array =
    let internal lengthType = typedefof<int>
    let internal lengthTermType = Numeric lengthType in

    let internal zeroLowerBound rank =
        Array.init rank (always(Concrete(0, lengthTermType)))

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
        | Union gvs -> Merging.guardedMap length gvs
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
            [for i in current do for j in rest do yield (fun (g1, v1) (g2, v2) -> (g1 &&& g2, v1::v2)) i j]

    let rec internal makeDefault defaultOf dimensions typ lowerBounds =
        let unguardedDimensions = guardsProduct dimensions in
        let makeArray dimensions =
            let length = dimensionsToLength dimensions in
            match length with
            | Terms.GuardedValues(gs, vs) -> internalfail "unexpected union in array length!"
            | _ ->
                Array(lowerBounds, None, Heap.empty, dimensions, typ)
        in
        unguardedDimensions |> List.map (fun (g, ds) -> (g, makeArray (Array.ofList ds))) |> Merging.merge

    let internal makeSymbolic source rank typ name =
        let idOfLength = IdGenerator.startingWith (sprintf "|%s|" name) in
        let constant = Constant(name, source, typ) in
        let lengths = Array.init rank (fun i -> Constant(idOfLength, SymbolicArrayLength(constant, i, true), Numeric lengthType)) in
        Array(symbolicLowerBound constant name rank, Some constant, Heap.empty, lengths, typ)

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
                | [] -> [], List.init depth (always 0)
            | term -> [term], []
        let linearContent, dimensions = flatten rank initializer in
        let len = List.length linearContent in
        assert(len = List.fold (*) 1 dimensions)
        let intToTerm i = Concrete(i, lengthTermType) in
        let dimensionTerms = dimensions |> List.map intToTerm |> Array.ofList in
        let indices = List.init len intToTerm in
        let contents = Seq.zip indices linearContent |> Heap.ofSeq in
        Array(zeroLowerBound rank, None, contents, dimensionTerms, typ)

    let internal checkIndices lowerBounds dimensions indices =
        assert(List.length indices = Array.length dimensions)
        let bounds =
            Seq.map3
                (fun idx low len ->
                    let up = low +++ len in
                    let bigEnough = Arithmetics.simplifyGreaterOrEqual idx low id in
                    let smallEnough = Arithmetics.simplifyLess idx up id in
                    bigEnough &&& smallEnough)
                indices lowerBounds dimensions
        in conjunction bounds

    let physicalIndex lowerBounds dimensions indices =
        let normalizedIndices = List.map2 (---) indices (List.ofArray lowerBounds) in
        List.fold ( *** ) (Concrete(1, lengthTermType)) normalizedIndices
