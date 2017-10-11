namespace VSharp
open System.Collections.Generic

module internal Arrays =
    type private SymbolicArrayBound(array : Term, index : Term, upper : bool) =
        inherit SymbolicConstantSource()

    type private SymbolicArrayDimensionNumber(array : Term) =
        inherit SymbolicConstantSource()

    type private SymbolicArrayIndex(indices : Term) =
        inherit SymbolicConstantSource()

    type private SymbolicArrayLength(array : Term) =
        inherit SymbolicConstantSource()

    type internal ArrayIndicesType =
        | Contents
        | LowerBounds
        | Lengths

    let internal lengthType = typedefof<int>
    let internal lengthTermType = Numeric lengthType

    let internal makeArray mtd length contents instantiator =
        let zero = MakeZeroAddress mtd in
        let lowerBound = Heap.add zero (zero, State.zeroTime, State.zeroTime) Heap.empty in
        let typ = ArrayType(lengthTermType, ConcreteDimension 1) in
        let lengths = Heap.add zero (length, State.zeroTime, State.zeroTime) Heap.empty  in
        Array (MakeNumber 1 mtd) length lowerBound instantiator contents lengths typ mtd

    let internal makeIntegerArray mtd maker length =
        let contents =
            Seq.init length maker |>
            Seq.foldi (fun h i v -> Heap.add (MakeNumber i mtd) (v, State.zeroTime, State.zeroTime) h) Heap.empty
        in
        let length = MakeNumber length mtd in
        let instantiator = [Terms.True, DefaultInstantiator lengthTermType] in
        makeArray mtd length contents instantiator

    let internal makeSymbolicIntegerArray mtd length symbolicValue =
        let instantiator = [Terms.True, LazyInstantiator (symbolicValue, lengthTermType)] in
        makeArray mtd length Heap.empty instantiator

    let internal equalsArrayIndices mtd addr1 addr2 =
        let equalsHeap h1 h2 resolve1 resolve2 =
            let keysSet = HashSet(Heap.locations h1) in
            keysSet.UnionWith(HashSet(Heap.locations h2))
            let keys = seq(keysSet) in
            Seq.mapFold (fun (h1, h2) key ->
                let fill val1 val2 h1 h2 =
                    Arithmetics.eq mtd (fst3 val1) (fst3 val2), (h1, h2)
                in
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
        | Array(d1, len1, lb1, init1, indices1, l1, t1), Array(d2, len2, lb2, init2, indices2, l2, t2) ->
            assert(List.length init1 = 1 && List.length init2 = 1)
            let initTerm1, initTerm2 = List.head init1 |> snd, List.head init2 |> snd in
            let makeConstant mtd constant i =
                let id = sprintf "%s[%s]" (toString constant) (toString i) |> IdGenerator.startingWith in
                Some (Constant id (SymbolicArrayIndex(constant)) lengthTermType mtd, State.zeroTime, State.zeroTime)
            in
            let makeGvs h1 h2 val1 val2 = 
                let zero = MakeZeroAddress mtd in
                let isSameLen = Arithmetics.eq mtd len1 len2 in
                match isSameLen with
                | False -> isSameLen, (addr1, addr2)
                | _ ->
                    let gvs, (h1, h2) = equalsHeap h1 h2 val1 val2
                    let arr1 = Array d1 len1 lb1 init1 h1 l1 t1 addr1.metadata
                    let arr2 = Array d2 len2 lb2 init2 h2 l2 t2 addr2.metadata
                    isSameLen &&& Propositional.conjunction mtd gvs, (arr1, arr2)
            in
            match initTerm1, initTerm2 with
            | LazyInstantiator(constant1, _), LazyInstantiator(constant2, _) -> Arithmetics.eq mtd constant1 constant2, (addr1, addr2)
            | LazyInstantiator(constant1, _), DefaultInstantiator _ -> makeGvs indices1 indices2 (makeConstant mtd constant1) (always None)
            | DefaultInstantiator _, LazyInstantiator(constant2, _) -> makeGvs indices1 indices2 (always None) (makeConstant mtd constant2)
            | DefaultInstantiator _, DefaultInstantiator _ -> makeGvs indices1 indices2 (always None) (always None)
        | _ -> __unreachable__()

    let private makeSymbolicDimensionsNumber metadata arrayConstant arrayName =
        let dim = sprintf "|dimensions of %s|" arrayName in
        Constant dim (SymbolicArrayDimensionNumber(arrayConstant)) lengthTermType metadata in

    let private makeSymbolicLength metadata arrayConstant arrayName =
        let idOfDimension = sprintf "|%s|" arrayName in
        Constant idOfDimension (SymbolicArrayLength(arrayConstant)) lengthTermType metadata

    let internal zeroLowerBound metadata dimension =
        let bound = Concrete 0 lengthTermType metadata, State.zeroTime, State.zeroTime in
        Seq.fold (fun h l -> Heap.add l bound h) Heap.empty (Seq.init dimension (fun i -> MakeNumber i metadata))

    let rec internal length mtd term =
        match term.term with
        | Error _ -> term
        | Array(_, len, _, _, _, ls, _) -> len
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
        in
        let unguardedLengths = guardsProduct mtd lengthList in
        let makeArray (lengthList : Term list) =
            let dim = List.length lengthList in
            let lowerBounds = zeroLowerBound mtd dim in
            let length = List.reduce (mul mtd) lengthList in
            let lengths = Seq.foldi (fun h i l -> Heap.add (MakeNumber i mtd) (l, State.zeroTime, State.zeroTime) h) Heap.empty lengthList in
            Array (MakeNumber dim mtd) length lowerBounds [Terms.True, DefaultInstantiator elemTyp] Heap.empty lengths typ mtd
        in
        unguardedLengths |> List.map (fun (g, ls) -> (g, makeArray ls)) |> Merging.merge

    and internal makeSymbolicLowerBound metadata arrayConstant arrayName dimension =
        match Options.SymbolicArrayLowerBoundStrategy() with
        | Options.AlwaysZero -> zeroLowerBound metadata dimension
        | Options.AlwaysSymbolic ->
            let idOfBound i = sprintf "%s.GetLowerBound(%i)" arrayName i in
            let mkLowerBound i = Constant (idOfBound i) (SymbolicArrayBound(arrayConstant, MakeNumber i metadata, false)) lengthTermType metadata in
            Seq.foldi (fun h i l -> Heap.add (MakeNumber i metadata) (l, State.zeroTime, State.zeroTime) h) Heap.empty (Seq.init dimension mkLowerBound)

    and internal makeSymbolicLengths metadata arrayConstant arrayName dimension =
        let idOfLength i = sprintf "%s.GetLength(%i)" arrayName i in
        let mkLength i = Constant (idOfLength i) (SymbolicArrayBound(arrayConstant, MakeNumber i metadata, true)) lengthTermType metadata in
        let lengths = Seq.init dimension mkLength in
        let length = Seq.reduce (mul metadata) lengths in
        Seq.foldi (fun h i l -> Heap.add (MakeNumber i metadata) (l, State.zeroTime, State.zeroTime) h) Heap.empty lengths, length

    and internal makeSymbolic metadata source (dimension : ArrayDimensionType) elemTyp typ arrayName =
        let arrayConstant = Constant arrayName source typ metadata in
        let instantiator = [Terms.True , LazyInstantiator(arrayConstant, elemTyp)] in
        let lowerBound, arrayLengths, arrayLength, dim =
            match dimension with
            | ConcreteDimension d ->
                let lb = makeSymbolicLowerBound metadata arrayConstant arrayName d in
                let al, length = makeSymbolicLengths metadata arrayConstant arrayName d in
                lb, al, length, MakeNumber d metadata
            | SymbolicDimension _ ->
                let length = makeSymbolicLength metadata arrayConstant arrayName in
                Heap.empty, Heap.empty, length, makeSymbolicDimensionsNumber metadata arrayConstant arrayName
        in
        Array dim arrayLength lowerBound instantiator Heap.empty arrayLengths typ metadata

    let rec internal fromInitializer mtd time rank typ initializer =
        let elemTyp =
            match typ with
            | ArrayType(e, _) -> e
            | _ -> internalfail "unexpected type of array!"
        in
        let rec flatten depth term =
            match term.term with
            | Concrete(:? (Term list) as terms, _) ->
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
        let dimensionList = dimensions |> List.map intToTerm in
        let length = MakeNumber len mtd in
        let lengths = Seq.foldi (fun h i l -> Heap.add (MakeNumber i mtd) (l, State.zeroTime, State.zeroTime) h) Heap.empty dimensionList in
        let indices =
            List.foldBack (fun i s ->
                let indicesInDim = Seq.init i intToTerm in
                let res = Seq.map (fun x -> Seq.map (cons x) s) indicesInDim in
                res |> Seq.concat) dimensions (Seq.init 1 (always List.empty))
            |> Seq.map (fun index -> makeIntegerArray mtd (fun i -> index.[i]) index.Length)
        in
        let contents = Seq.zip indices linearContent |> Heap.ofSeq in
        Array (MakeNumber rank mtd) length (zeroLowerBound mtd rank) [Terms.True, DefaultInstantiator elemTyp] contents lengths typ mtd
