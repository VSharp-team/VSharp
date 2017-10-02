namespace VSharp

module internal Arrays =
    type private SymbolicArrayBound(array : Term, index : Term, upper : bool) =
        inherit SymbolicConstantSource()

    type private SymbolicArrayDimension(array : Term) =
        inherit SymbolicConstantSource()

    type private SymbolicArrayIndex(indices : Term) =
        inherit SymbolicConstantSource()

    type internal ArrayIndicesType =
        | Contetns
        | LowerBounds
        | Lengths

    let internal lengthType = typedefof<int>
    let internal lengthTermType = Numeric lengthType

    let internal makeIntegerArray mtd maker length =
        let contents =
            Seq.init length maker |>
            Seq.foldi (fun h i v -> Heap.add (MakeNumber i mtd) (v, State.zeroTime, State.zeroTime) h) Heap.empty
        in
        let zero = MakeZeroAddress mtd
        let lowerBound = Heap.add zero (zero, State.zeroTime, State.zeroTime) Heap.empty in
        let instantiator = [Terms.True, DefaultInstantiator lengthTermType] in
        let typ = ArrayType(lengthTermType, Some length) in
        let lengths = Heap.add zero (MakeNumber length mtd, State.zeroTime, State.zeroTime) Heap.empty in
        Array (MakeNumber 1 mtd) lowerBound instantiator contents lengths typ mtd

    let internal makeSymbolicIntegerArray mtd length symbolicValue =
        let zero = MakeZeroAddress mtd
        let lowerBound = Heap.add zero (zero, State.zeroTime, State.zeroTime) Heap.empty in
        let instantiator = [Terms.True, LazyInstantiator (symbolicValue, lengthTermType)] in
        let typ = ArrayType(lengthTermType, None) in
        let lengths = Heap.add zero (length, State.zeroTime, State.zeroTime) Heap.empty in
        Array (MakeNumber 1 mtd) lowerBound instantiator Heap.empty lengths typ mtd in

    let internal equalsArrayIndices mtd addr1 addr2 =
        let equalsHeap h1 h2 resolve1 resolve2 =
            let keysSet = System.Collections.Generic.HashSet(Heap.locations h1)
            keysSet.UnionWith(System.Collections.Generic.HashSet(Heap.locations h2))
            let keys = seq(keysSet)
            Seq.mapFold (fun (h1, h2) key ->
                let fill val1 val2 h1 h2 =
                    let (value1, _, _) = val1 in
                    let (value2, _, _) = val2 in
                    Arithmetics.eq mtd value1 value2, (h1, h2)
                in
//                match Heap.contains key h1, Heap.contains key h2 with
//                | true, false ->
//                    let val2 = resolve2 key
//                    fill h1.[key] val2 h1 (Heap.add key val2 h2)
//                | false, true ->
//                    let val1 = resolve1 key
//                    fill val1 h2.[key] (Heap.add key val1 h1) h2 
//                | true, true -> fill h1.[key] h2.[key] h1 h2
//                | false, false -> __unreachable__()) (h1, h2) keys
                match Heap.contains key h1, Heap.contains key h2 with
                | true, false ->
                    let val2 = resolve2 key
                    match val2 with
                    | Some val2 -> fill h1.[key] val2 h1 (Heap.add key val2 h2)
                    | None -> False, (h1, h2)
                | false, true ->
                    let val1 = resolve1 key
                    match val1 with
                    | Some val1 -> fill val1 h2.[key] (Heap.add key val1 h1) h2
                    | None -> False, (h1, h2)
                | true, true -> fill h1.[key] h2.[key] h1 h2
                | false, false -> __unreachable__()) (h1, h2) keys
        in
        match addr1.term, addr2.term with
        | Array(d1, lb1, init1, indices1, l1, t1), Array(d2, lb2, init2, indices2, l2, t2) ->
            assert(List.length init1 = 1 && List.length init2 = 1)
            let initTerm1, initTerm2 = List.head init1 |> snd, List.head init2 |> snd in
            let makeConstant mtd constant i =
                let id = sprintf "%s[%s]" (toString constant) (toString i) |> IdGenerator.startingWith in
                Some (Constant id (SymbolicArrayIndex(constant)) lengthTermType mtd, State.zeroTime, State.zeroTime)
            in
            let makeGvs h1 h2 val1 val2 = 
                let zero = MakeZeroAddress mtd in
                let (len1, _ , _) , (len2, _, _) = Heap.find zero l1, Heap.find zero l2 in
                let isSameLen = Arithmetics.eq mtd len1 len2 in
                match isSameLen with
                | False -> isSameLen, (addr1, addr2)
                | _ ->
                    let gvs, (h1, h2) = equalsHeap h1 h2 val1 val2
                    let arr1 = Array d1 lb1 init1 h1 l1 t1 addr1.metadata
                    let arr2 = Array d2 lb2 init2 h2 l2 t2 addr2.metadata
                    isSameLen &&& Propositional.conjunction mtd gvs, (arr1, arr2)
            in
            match initTerm1, initTerm2 with
            | LazyInstantiator(constant1, _), LazyInstantiator(constant2, _) -> Arithmetics.eq mtd constant1 constant2, (addr1, addr2)
            | LazyInstantiator(constant1, _), DefaultInstantiator _ -> makeGvs indices1 indices2 (makeConstant mtd constant1) (always None)
            | DefaultInstantiator _, LazyInstantiator(constant2, _) -> makeGvs indices1 indices2 (always None) (makeConstant mtd constant2)
            | DefaultInstantiator _, DefaultInstantiator _ -> makeGvs indices1 indices2 (always None) (always None)
        | _ -> __unreachable__()

    let private makeSymbolicDimensionLengths metadata arrayConstant arrayName =
        let dim = IdGenerator.startingWith (sprintf "|dimensions of %s|" arrayName)
        Constant dim (SymbolicArrayDimension(arrayConstant)) lengthTermType metadata in

    let private makeSymbolicDimension metadata arrayConstant arrayName =
        let idOfDimension = IdGenerator.startingWith (sprintf "dimension of %s" arrayName) in
        Constant idOfDimension (SymbolicArrayDimension(arrayConstant)) lengthTermType metadata

    let internal zeroLowerBound metadata dimension =
        let bound = Concrete 0 lengthTermType metadata, State.zeroTime, State.zeroTime in
        Seq.fold (fun h l -> Heap.add l bound h) Heap.empty (Seq.init dimension (fun i -> MakeNumber i metadata))

    let rec internal length mtd term =
        match term.term with
        | Error _ -> term
        | Array(_, _, _, _, ls, _) -> ls |> Heap.values |> Seq.reduce (mul mtd)
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

    let rec internal makeDefault mtd lengthList typ =
        let elemTyp =
            match typ with
            | ArrayType(e, _) -> e
            | _ -> internalfail "unexpected type of array!"
        let unguardedLengths = guardsProduct mtd lengthList in
        let makeArray (lengthList : Term list) =
            let dim = List.length lengthList in
            let lowerBounds = zeroLowerBound mtd dim in
            let lengths = Seq.foldi (fun h i l -> Heap.add (MakeNumber i mtd) (l, State.zeroTime, State.zeroTime) h) Heap.empty lengthList in
            Array (MakeNumber dim mtd) lowerBounds [Terms.True, DefaultInstantiator elemTyp] Heap.empty lengths typ mtd
        in
        unguardedLengths |> List.map (fun (g, ls) -> (g, makeArray ls)) |> Merging.merge

    and internal makeSymbolicLowerBound metadata arrayConstant arrayName dimension =
        match Options.SymbolicArrayLowerBoundStrategy() with
        | Options.AlwaysZero -> zeroLowerBound metadata dimension
        | Options.AlwaysSymbolic ->
            let idOfBound i = sprintf "%s.GetLowerBound(%i)" arrayName i |> IdGenerator.startingWith in
            let mkLowerBound i = Constant (idOfBound i) (SymbolicArrayBound(arrayConstant, MakeNumber i metadata, false)) lengthTermType metadata in
            Seq.foldi (fun h i l -> Heap.add (MakeNumber i metadata) (l, State.zeroTime, State.zeroTime) h) Heap.empty (Seq.init dimension mkLowerBound)

    and internal makeSymbolicLengths metadata arrayConstant arrayName dimension =
        let idOfLength i = sprintf "%s.GetLength(%i)" arrayName i |> IdGenerator.startingWith in
        let mkLength i = Constant (idOfLength i) (SymbolicArrayBound(arrayConstant, MakeNumber i metadata, true)) lengthTermType metadata in
        Seq.foldi (fun h i l -> Heap.add (MakeNumber i metadata) (l, State.zeroTime, State.zeroTime) h) Heap.empty (Seq.init dimension mkLength)

    and internal makeSymbolic metadata source (dimension : int option) elemTyp typ arrayName =
        let arrayConstant = Constant arrayName source typ metadata in
        let instantiator = [Terms.True , LazyInstantiator(arrayConstant, elemTyp)] in
        let lowerBound, arrayLengths, dim =
            match dimension with
            | Some d ->
                let lb = makeSymbolicLowerBound metadata arrayConstant arrayName d in
                let al = makeSymbolicLengths metadata arrayConstant arrayName d in
                lb, al, MakeNumber d metadata
            | None -> Heap.empty, Heap.empty, makeSymbolicDimensionLengths metadata arrayConstant arrayName
        in
        Array dim lowerBound instantiator Heap.empty arrayLengths typ metadata

    let rec internal fromInitializer mtd time rank typ initializer =
        let elemTyp =
            match typ with
            | ArrayType(e, _) -> e
            | _ -> internalfail "unexpected type of array!"
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
        assert(len = List.reduce (*) dimensions)
        let intToTerm i = Concrete i lengthTermType mtd in
        let dimensionList = dimensions |> List.map intToTerm
        let lengths = Seq.foldi (fun h i l -> Heap.add (MakeNumber i mtd) (l, State.zeroTime, State.zeroTime) h) Heap.empty dimensionList in
        let indices =
            List.foldBack (fun i s ->
                let indicesInDim = Seq.init i intToTerm
                let res = Seq.map (fun x -> Seq.map (fun xs -> x :: xs) s) indicesInDim
                res |> Seq.concat) dimensions (Seq.init 1 (always List.empty))
            |> Seq.map (fun index -> makeIntegerArray mtd (fun i -> index.[i]) index.Length)
        in
        let contents = Seq.zip indices linearContent |> Heap.ofSeq in
        Array (MakeNumber rank mtd) (zeroLowerBound mtd rank) [Terms.True, DefaultInstantiator elemTyp] contents lengths typ mtd