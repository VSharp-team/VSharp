namespace VSharp

open FSharpx.Collections

module Array =
    let private lengthType = typedefof<int>
    let private lengthTermType = Numeric lengthType in

    let internal zeroLowerBound rank =
        Array.init rank (fun _ -> Concrete(0, lengthTermType))

    let internal dimensionsToLength =
        Array.fold ( *** ) (Concrete(1, lengthTermType))

    let rec internal length = function
        | Array(_, _, _, ds, _) -> dimensionsToLength ds
        | Terms.GuardedValues(gs, vs) ->
            vs |> List.map length |> List.zip gs |> Merging.merge
        | _ -> failwith "Internal error: computing length of non-array object"

    let internal makeSymbolic dimensions typ name lowerBounds =
        let length = dimensionsToLength dimensions in
        Array(lowerBounds, Some(Constant(name, typ)), PersistentHashMap<Term, Term>.Empty(), dimensions, typ)

    let rec internal makeDefault defaultOf dimensions typ lowerBounds =
        let elementType = Types.elementType typ in
        let rec decomposeDimensions = function
            | [] -> [(Terms.MakeTrue, [])]
            | d::ds ->
                let current =
                    match d with
                    | Union gvs -> gvs
                    | _ -> [(Terms.MakeTrue, d)]
                in
                let rest = decomposeDimensions ds in
                FSharpx.Collections.List.lift2 (fun (g1, v1) (g2, v2) -> (g1 &&& g2, v1::v2)) current rest
        in
        let unguardedDimensions = decomposeDimensions dimensions in
        let makeArray dimensions =
            let length = dimensionsToLength dimensions in
            match length with
            | Terms.GuardedValues(gs, vs) -> failwith "Internal error: unexpected union in array length!"
            | _ ->
                Array(lowerBounds, None, PersistentHashMap<Term, Term>.Empty(), dimensions, typ)
        in
        unguardedDimensions |> List.map (fun (g, ds) -> (g, makeArray (Array.ofList ds))) |> Union

    let internal fresh rank typ name =
        let idOfLength = IdGenerator.startingWith (sprintf "|%s|" name) in
        let lengths = Array.init rank (fun _ -> Constant(idOfLength, Numeric lengthType))
        makeSymbolic lengths typ name (zeroLowerBound rank)

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
        let indeces = Seq.init len intToTerm in
        let contents = Seq.zip indeces linearContent |> PersistentHashMap.ofSeq in
        Array(zeroLowerBound rank, None, contents, dimensionTerms, typ)
