namespace VSharp

module Array =
    let internal lengthTermType = State.arrayLengthTermType in

    let internal zeroLowerBound = State.mkArrayZeroLowerBound

    let internal dimensionsToLength mtd =
        FSharp.Collections.Array.fold (mul mtd) (Concrete 1 lengthTermType mtd)

    let rec internal length mtd term =
        match term.term with
        | Error _ -> term
        | Array(_, _, _, ds, _) -> dimensionsToLength mtd ds
        | Union gvs -> Merging.guardedMap (length mtd) gvs
        | _ -> internalfail "computing length of non-array object"

    let rec private guardsProduct mtd = function
        | [] -> [(Terms.MakeTrue mtd, [])]
        | d::ds ->
            let current =
                match d.term with
                | Union gvs -> gvs
                | _ -> [(Terms.MakeTrue mtd, d)]
            in
            let rest = guardsProduct mtd ds in
            FSharpx.Collections.List.lift2 (fun (g1, v1) (g2, v2) -> (g1 &&& g2, v1::v2)) current rest

    let rec internal makeDefault mtd dimensions typ lowerBounds =
        let unguardedDimensions = guardsProduct mtd dimensions in
        let makeArray dimensions =
            let length = dimensionsToLength mtd dimensions in
            match length.term with
            | Union _ -> internalfail "unexpected union in array length!"
            | _ ->
                Array lowerBounds None Heap.empty dimensions typ mtd
        in
        unguardedDimensions |> List.map (fun (g, ds) -> (g, makeArray (FSharp.Collections.Array.ofList ds))) |> Merging.merge

    let rec internal fromInitializer mtd time rank typ initializer =
        let rec flatten depth term =
            match term.term with
            | Concrete(terms, _) when (terms :? Term list) ->
                let terms = terms :?> Term list in
                let children, dims = terms |> List.map (flatten (depth - 1)) |> List.unzip in
                match dims with
                | d::ds when not (List.forall ((=) d) ds) ->
                    failwith "Unexpected jugged array in multidimesional initializer!"
                | d::ds ->
                    List.concat children, (List.length children)::d
                | [] -> [], List.init depth (always 0)
            | _ -> [(term, time, time)], []
        let linearContent, dimensions = flatten rank initializer in
        let len = List.length linearContent in
        assert(len = List.fold (*) 1 dimensions)
        let intToTerm i = Concrete i lengthTermType mtd in
        let dimensionTerms = dimensions |> List.map intToTerm |> FSharp.Collections.Array.ofList in
        let indices = List.init len intToTerm in
        let contents = Seq.zip indices linearContent |> Heap.ofSeq in
        Array (zeroLowerBound mtd rank) None contents dimensionTerms typ mtd

    let internal checkIndices mtd lowerBounds dimensions indices =
        assert(List.length indices = FSharp.Collections.Array.length dimensions)
        let bounds =
            Seq.map3
                (fun idx low len ->
                    let up = add mtd low len in
                    let bigEnough = Arithmetics.simplifyGreaterOrEqual mtd idx low id in
                    let smallEnough = Arithmetics.simplifyLess mtd idx up id in
                    bigEnough &&& smallEnough)
                indices lowerBounds dimensions
        in conjunction mtd bounds

    let physicalIndex mtd lowerBounds dimensions indices =
        let normalizedIndices = List.map2 (sub mtd) indices (List.ofArray lowerBounds) in
        List.fold (mul mtd) (Concrete 1 lengthTermType mtd) normalizedIndices
