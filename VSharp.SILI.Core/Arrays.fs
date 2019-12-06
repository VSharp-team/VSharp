namespace VSharp.Core

open VSharp

module internal Arrays =
    [<StructuralEquality;NoComparison>]
    type private DefaultArray =
        struct end
        interface INonComposableSymbolicConstantSource with
            override x.SubTerms = Seq.empty

    let mkArrayIndex typ idx = ArrayIndex(idx, typ)

    let makeArray mtd length contents instantiator fql =
        let zero = makeNumber mtd 0
        let zeroKey = makePathIndexKey mtd ArrayLowerBound 0 fql Types.lengthType
        let lowerBound = Heap.add zeroKey zero Heap.empty
        let lengths = Heap.add zeroKey length Heap.empty
        Array mtd (makeNumber mtd 1) length lowerBound instantiator contents lengths

    let makeLinearConcreteArray mtd keyMaker valMaker length elemTyp fql =
        let pathKeyMaker i = makePathKey fql (mkArrayIndex elemTyp) (keyMaker mtd i) elemTyp
        let contents =
            valMaker
            |> Seq.init length
            |> Seq.foldi (fun h i v -> Heap.add (pathKeyMaker i) v h) Heap.empty
        let length = makeNumber mtd length
        let instantiator = [makeTrue mtd, DefaultInstantiator elemTyp]
        makeArray mtd length contents instantiator fql

    let makeIndexArray mtd maker length =
        makeLinearConcreteArray mtd makeIndex maker length Types.indexType None

    let private getTypeOfArrayInstor = function
        | DefaultInstantiator typ
        | LazyInstantiator typ -> typ

    let simplifyArraysEquality mtd x y indecesEq eq =
        let simplifyHeapPointwiseEquality h1 h2 eq =
            let unifier acc =
                let resolve v1 v2 = acc &&& eq mtd v1 v2
                Merging.keysResolver2 false x y (State.readTerm mtd) getFQLOfKey resolve
            // TODO: make comparison finish when acc is false
            Heap.unify2 (makeTrue mtd) h1 h2 unifier
        let simplifyGInstantiatorEquality gInstor1 gInstor2 =
            let instorEq mtd x y =
                let typ1 = getTypeOfArrayInstor x
                let typ2 = getTypeOfArrayInstor y
                makeBool mtd (typ1 = typ2)
            List.fold (fun acc (g1, instor1) ->
                simplifyOr mtd acc (List.fold (fun acc (g2, instor2) ->
                    simplifyAnd mtd g1 g2 (fun guardsEq ->
                    let instantiatorEq = instorEq mtd instor1 instor2
                    simplifyOr mtd acc (simplifyAnd mtd guardsEq instantiatorEq id) id)) (makeFalse mtd) gInstor2) id)
                (makeFalse mtd)
                gInstor1
        match x.term, y.term with
        | Array(dim1, len1, lb1, instor1, content1, l1), Array(dim2, len2, lb2, instor2, content2, l2) ->
            Propositional.lazyConjunction mtd <|
                seq[
                    fun() -> Arithmetics.simplifyEqual mtd dim1 dim2 id;
                    fun() -> Arithmetics.simplifyEqual mtd len1 len2 id;
                    fun() -> simplifyHeapPointwiseEquality lb1 lb2 eq;
                    fun() -> simplifyGInstantiatorEquality instor1 instor2;
                    fun() -> simplifyHeapPointwiseEquality content1 content2 indecesEq;
                    fun() -> simplifyHeapPointwiseEquality l1 l2 eq
                ]
        | term1, term2 -> internalfailf "expected array and array but %O and %O got!" term1 term2

    let equalsIndicesArrays mtd addr1 addr2 =
        simplifyArraysEquality mtd addr1 addr2
            (fun mtd x y -> Arithmetics.simplifyEqual mtd x y id)
            (fun mtd x y -> Arithmetics.simplifyEqual mtd x y id)

    let equalsArrayIndices mtd addr1 addr2 =
        match addr1.term, addr2.term with
        | _ when isNumeric addr1 && isNumeric addr2 -> fastNumericCompare mtd addr1 addr2
        | Array _, Array _ -> equalsIndicesArrays mtd addr1 addr2
        | _ -> __notImplemented__()

    let equals mtd addr1 addr2 =
        simplifyArraysEquality mtd addr1 addr2
            equalsArrayIndices
            (fun mtd x y -> Arithmetics.simplifyEqual mtd x y id)

    let zeroLowerBounds metadata dimension fql =
        let bound = Concrete metadata 0 Types.lengthType
        Seq.fold (fun h l -> Heap.add l bound h) Heap.empty (Seq.init dimension (fun i -> makePathIndexKey metadata ArrayLowerBound i fql Types.lengthType))

    let length = Merging.guardedErroredApply (function
        | {term = Array(_, l, _, _, _, _)} -> l
        | t -> internalfailf "extracting length of non-array object %O" t)

    let rank = Merging.guardedErroredApply (function
        | {term = Array(d, _, _, _, _, _)} -> d
        | t -> internalfailf "extracting rank of non-array object %O" t)

    let rec private guardsProduct mtd = function
        | [] -> [(makeTrue mtd, [])]
        | d::ds ->
            let current =
                match d.term with
                | Union gvs -> gvs
                | _ -> [(makeTrue mtd, d)]
            let rest = guardsProduct mtd ds
            FSharpx.Collections.List.lift2 (fun (g1, v1) (g2, v2) -> (g1 &&& g2, v1::v2)) current rest

    let rec makeDefault mtd lengthList typ fql =
        let elemTyp = Types.elementType typ
        let unguardedLengths = guardsProduct mtd lengthList
        let makeArray (lengthList : term list) =
            let dim = List.length lengthList
            let lowerBounds = zeroLowerBounds mtd dim fql
            let length = List.reduce (mul mtd) lengthList
            let lengths = Seq.foldi (fun h i l -> Heap.add (makePathIndexKey mtd ArrayLength i fql Types.lengthType) l h) Heap.empty lengthList
            Array mtd (makeNumber mtd dim) length lowerBounds [Terms.True, DefaultInstantiator elemTyp] Heap.empty lengths
        unguardedLengths |> List.map (fun (g, ls) -> (g, makeArray ls)) |> Merging.merge

    let rec fromInitializer mtd rank typ initializer fql =
        let elemTyp = Types.elementType typ
        let rec flatten depth term =
            match term.term with
            | Concrete(:? (term list) as terms, _) ->
                let children, dims = terms |> List.map (flatten (depth - 1)) |> List.unzip
                match dims with
                | d::ds when not (List.forall ((=) d) ds) ->
                    failwith "Unexpected jugged array in multidimesional initializer!"
                | d::_ ->
                    List.concat children, (List.length children)::d
                | [] -> [], List.init depth (always 0)
            | _ -> [term], []
        let linearContent, dimensions = flatten rank initializer
        let len = List.length linearContent
        assert(len = List.reduce (*) dimensions)
        let intToTerm i = Concrete mtd i Types.lengthType
        let dimensionList = dimensions |> List.map intToTerm
        let length = makeNumber mtd len
        let lengths = Seq.foldi (fun h i l -> Heap.add (makePathIndexKey mtd ArrayLength i fql Types.lengthType) l h) Heap.empty dimensionList
        let mkIndex = makeIndex mtd
        let indices =
            List.foldBack (fun i s ->
                let indicesInDim = Seq.init i mkIndex
                Seq.collect (fun x -> Seq.map (cons x) s) indicesInDim
                ) dimensions (Seq.init 1 (always List.empty))
            |> Seq.map (fun index -> makePathKey fql (mkArrayIndex elemTyp) (makeIndexArray mtd (fun i -> index.[i]) index.Length) elemTyp)
        let contents = Seq.zip indices linearContent |> Heap.ofSeq
        Array mtd (makeNumber mtd rank) length (zeroLowerBounds mtd rank fql) [Terms.True, DefaultInstantiator elemTyp] contents lengths

    let fromMkContents mtd state rank typ dimensions mkContents fql =
        let elemTyp = Types.elementType typ
        let len = List.reduce (*) dimensions
        let intToTerm i = Concrete mtd i Types.lengthType
        let dimensionList = dimensions |> List.map intToTerm
        let length = makeNumber mtd len
        let lengths = Seq.foldi (fun h i l -> Heap.add (makePathIndexKey mtd ArrayLength i fql Types.lengthType) l h) Heap.empty dimensionList
        let mkIndex = makeIndex mtd
        let indices =
            List.foldBack (fun i s ->
                let indicesInDim = Seq.init i id
                Seq.collect (fun x -> Seq.map (cons x) s) indicesInDim
                ) dimensions (Seq.init 1 (always List.empty))
        let contents, state =
            Seq.fold (fun (h, s) index ->
                let termIndex = List.map mkIndex index
                let key = makePathKey fql (mkArrayIndex elemTyp) (makeIndexArray mtd (fun i -> termIndex.[i]) termIndex.Length) elemTyp
                let term, state = mkContents s index
                Heap.add key term h, state
            ) (Heap.empty, state) indices
        Array mtd (makeNumber mtd rank) length (zeroLowerBounds mtd rank fql) [Terms.True, DefaultInstantiator elemTyp] contents lengths, state

    let (|VectorT|_|) = term >> function
        | Array(ConcreteT(one, _), length, lower, instor, contents, _)
            when one :?> int = 1 && lower = zeroLowerBounds Metadata.empty 1 None -> Some(VectorT (length, instor, contents))
        | _ -> None

    type LengthExtractor() =
        inherit TermExtractor()
        override x.Extract t = length t
    type RankExtractor() =
        inherit TermExtractor()
        override x.Extract t = rank t
