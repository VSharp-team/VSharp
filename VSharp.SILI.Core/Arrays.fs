namespace VSharp.Core

open VSharp
open System.Collections.Generic

module internal Arrays =
    [<StructuralEquality;NoComparison>]
    type private DefaultArray =
        struct end
        interface INonComposableSymbolicConstantSource with
            override x.SubTerms = Seq.empty

    let private defaultArrayName = "<defaultArray>"
    let lengthType = typedefof<int>
    let lengthTermType = Numeric lengthType

    let makeArray mtd length contents instantiator elemTyp =
        let zero = makeZeroAddress mtd
        let lowerBound = Heap.add zero { value = zero; created = Timestamp.zero; modified = Timestamp.zero; typ = elemTyp } Heap.empty
        let typ = ArrayType(elemTyp, ConcreteDimension 1)
        let lengths = Heap.add zero { value = length; created = Timestamp.zero; modified = Timestamp.zero; typ = lengthTermType } Heap.empty
        Array mtd (makeNumber 1 mtd) length lowerBound instantiator contents lengths typ

    let makeLinearConcreteArray mtd keyMaker valMaker length elemTyp =
        let contents =
            valMaker
            |> Seq.init length
            |> Seq.foldi (fun h i v -> Heap.add (keyMaker i mtd) { value = v; created = Timestamp.zero; modified = Timestamp.zero; typ = elemTyp } h) Heap.empty
        let length = makeNumber length mtd
        let constant = Constant mtd defaultArrayName (DefaultArray()) (ArrayType(lengthTermType, ConcreteDimension 1))
        let instantiator = [makeTrue mtd, DefaultInstantiator(constant, elemTyp)]
        makeArray mtd length contents instantiator elemTyp

    let makeIntegerArray mtd maker length =
        makeLinearConcreteArray mtd makeNumber maker length lengthTermType

    let makeLinearSymbolicArray mtd length symbolicValue elemType =
        let instantiator = [Terms.True, LazyInstantiator (symbolicValue, elemType)]
        makeArray mtd length Heap.empty instantiator elemType

    let makeSymbolicIntegerArray mtd length symbolicValue =
        makeLinearSymbolicArray mtd length symbolicValue lengthTermType


    let simplifyHeapPointwiseEquality mtd h1 h2 eq instantiate1 instantiate2 =
        // TODO: make comparison finish when acc is false
        Heap.unify (makeTrue mtd) h1 h2
            (fun acc _ v1 v2 -> acc &&& eq mtd (v1 ||?? lazy(instantiate1())).value (v2 ||?? lazy(instantiate2())).value)

    let simplifyArraysEquality mtd x y eq instantiate1 instantiate2 =
        let simplifyGInstantiatorEquality mtd gInstor1 gInstor2 =
            let instorEq mtd x y =
                match x, y with
                | DefaultInstantiator(_, typ1), DefaultInstantiator(_, typ2) -> makeBool (typ1 = typ2) mtd
                | LazyInstantiator(term1, typ1), LazyInstantiator(term2, typ2)
                | DefaultInstantiator(term1, typ1), LazyInstantiator(term2, typ2)
                | LazyInstantiator(term1, typ1), DefaultInstantiator(term2, typ2) ->
                    if typ1 = typ2 then eq mtd term1 term2 else False
            List.fold (fun acc (g1, instor1) ->
                simplifyOr mtd acc (List.fold (fun acc (g2, instor2) ->
                    simplifyAnd mtd g1 g2 (fun guardsEq ->
                    let instantiatorEq = instorEq mtd instor1 instor2
                    simplifyOr mtd acc (simplifyAnd mtd guardsEq instantiatorEq id) id)) (makeFalse mtd) gInstor2) id)
                (makeFalse mtd)
                gInstor1
        match x.term, y.term with
        | Array(dim1, len1, lb1, instor1, content1, l1, _), Array(dim2, len2, lb2, instor2, content2, l2, _) ->
            Propositional.lazyConjunction mtd <|
                seq[
                    fun() -> Arithmetics.simplifyEqual mtd dim1 dim2 id;
                    fun() -> Arithmetics.simplifyEqual mtd len1 len2 id;
                    fun() -> simplifyHeapPointwiseEquality mtd lb1 lb2 eq (instantiate1 ArrayLowerBounds) (instantiate2 ArrayLowerBounds);
                    fun() -> simplifyGInstantiatorEquality mtd instor1 instor2;
                    fun() -> simplifyHeapPointwiseEquality mtd content1 content2 eq (instantiate1 ArrayContents) (instantiate2 ArrayContents);
                    fun() -> simplifyHeapPointwiseEquality mtd l1 l2 eq (instantiate1 ArrayLengths) (instantiate2 ArrayLengths)
                ]
        | term1, term2 -> internalfailf "expected array and array but %O and %O got!" term1 term2

    let equalsArrayIndices mtd addr1 addr2 =
        simplifyArraysEquality mtd addr1 addr2
            (fun mtd x y -> Arithmetics.simplifyEqual mtd x y id)
            (fun _ _ -> __notImplemented__())
            (fun _ _ -> __notImplemented__())

    let zeroLowerBound metadata dimension =
        let bound = { value = Concrete metadata 0 lengthTermType; created = Timestamp.zero; modified = Timestamp.zero; typ = lengthTermType }
        Seq.fold (fun h l -> Heap.add l bound h) Heap.empty (Seq.init dimension (fun i -> makeNumber i metadata))

    let length = Merging.map (function
        | {term = Array(_, l, _, _, _, _, _)} -> l
        | t -> internalfail "extracting length of non-array object %O" t)

    let rank = Merging.map (function
        | {term = Array(d, _, _, _, _, _, _)} -> d
        | t -> internalfail "extracting rank of non-array object %O" t)

    let rec private guardsProduct mtd = function
        | [] -> [(makeTrue mtd, [])]
        | d::ds ->
            let current =
                match d.term with
                | Union gvs -> gvs
                | _ -> [(makeTrue mtd, d)]
            let rest = guardsProduct mtd ds
            FSharpx.Collections.List.lift2 (fun (g1, v1) (g2, v2) -> (g1 &&& g2, v1::v2)) current rest

    let rec makeDefault mtd lengthList typ =
        let elemTyp =
            match typ with
            | ArrayType(e, _) -> e
            | _ -> internalfail "unexpected type of array!"
        let unguardedLengths = guardsProduct mtd lengthList
        let makeArray (lengthList : term list) =
            let dim = List.length lengthList
            let lowerBounds = zeroLowerBound mtd dim
            let length = List.reduce (mul mtd) lengthList
            let constant = Constant mtd defaultArrayName (DefaultArray()) typ
            let lengths = Seq.foldi (fun h i l -> Heap.add (makeNumber i mtd) { value = l; created = Timestamp.zero; modified = Timestamp.zero; typ = elemTyp } h) Heap.empty lengthList
            Array mtd (makeNumber dim mtd) length lowerBounds [Terms.True, DefaultInstantiator(constant, elemTyp)] Heap.empty lengths typ
        unguardedLengths |> List.map (fun (g, ls) -> (g, makeArray ls)) |> Merging.merge

    let rec fromInitializer mtd time rank typ initializer =
        let elemTyp =
            match typ with
            | ArrayType(e, _) -> e
            | _ -> internalfail "unexpected type of array!"
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
            | _ -> [{ value = term; created = time; modified = time; typ = elemTyp }], []
        let linearContent, dimensions = flatten rank initializer
        let len = List.length linearContent
        assert(len = List.reduce (*) dimensions)
        let intToTerm i = Concrete mtd i lengthTermType
        let dimensionList = dimensions |> List.map intToTerm
        let length = makeNumber len mtd
        let lengths = Seq.foldi (fun h i l -> Heap.add (makeNumber i mtd) { value = l; created = Timestamp.zero; modified = Timestamp.zero; typ = lengthTermType } h) Heap.empty dimensionList
        let indices =
            List.foldBack (fun i s ->
                let indicesInDim = Seq.init i intToTerm
                let res = Seq.map (fun x -> Seq.map (cons x) s) indicesInDim
                res |> Seq.concat) dimensions (Seq.init 1 (always List.empty))
            |> Seq.map (fun index -> makeIntegerArray mtd (fun i -> index.[i]) index.Length)
        let contents = Seq.zip indices linearContent |> Heap.ofSeq
        let constant = Constant mtd defaultArrayName (DefaultArray()) typ
        Array mtd (makeNumber rank mtd) length (zeroLowerBound mtd rank) [Terms.True, DefaultInstantiator(constant, elemTyp)] contents lengths typ

    let (|VectorT|_|) = term >> function
        | Array(ConcreteT(one, _), length, lower, instor, contents, lengths, ArrayType (elemTyp, ConcreteDimension 1))
            when one :?> int = 1 -> Some(VectorT (length, lower, instor, contents, lengths, elemTyp))
        | _ -> None

    let (|Index|_|) = function
        | VectorT(ConcreteT(length, _), lower, [_, DefaultInstantiator _], contents, _, _)
            when length :?> int = 1 && lower = zeroLowerBound Metadata.empty 1 -> Some(contents.[makeZeroAddress Metadata.empty])
        | _ -> None

    type LengthExtractor() =
        inherit TermExtractor()
        override x.Extract t = length t
    type RankExtractor() =
        inherit TermExtractor()
        override x.Extract t = rank t
