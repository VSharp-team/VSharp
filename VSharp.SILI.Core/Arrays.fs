namespace VSharp.Core

open VSharp
open System.Collections.Generic

module internal Arrays =
    [<StructuralEquality;NoComparison>]
    type private DefaultArray =
        struct end
        interface ISymbolicConstantSource with
            override x.SubTerms = Seq.empty

    [<StructuralEquality;NoComparison>]
    type private SymbolicArrayBound =
        {array : term; index : term; upper : bool}
        interface ISymbolicConstantSource with
            override x.SubTerms = Seq.singleton x.index

    [<StructuralEquality;NoComparison>]
    type private SymbolicArrayDimensionNumber =
        {array : term}
        interface ISymbolicConstantSource with
            override x.SubTerms = Seq.empty

    [<StructuralEquality;NoComparison>]
    type private SymbolicArrayIndex =
        {indices : term}
        interface ISymbolicConstantSource with
            override x.SubTerms = Seq.empty

    [<StructuralEquality;NoComparison>]
    type private SymbolicArrayLength =
        {array : term}
        interface ISymbolicConstantSource with
            override x.SubTerms = Seq.empty

    type ArrayIndicesType =
        | Contents
        | LowerBounds
        | Lengths

    let private defaultArrayName = "<defaultArray>"
    let lengthType = typedefof<int>
    let lengthTermType = Numeric lengthType

    let makeArray mtd length contents instantiator elemTyp =
        let zero = makeZeroAddress mtd
        let lowerBound = Heap.add zero { value = zero; created = Timestamp.zero; modified = Timestamp.zero } Heap.empty
        let typ = ArrayType(elemTyp, ConcreteDimension 1)
        let lengths = Heap.add zero { value = length; created = Timestamp.zero; modified = Timestamp.zero } Heap.empty
        Array mtd (makeNumber 1 mtd) length lowerBound instantiator contents lengths typ

    let makeLinearConreteArray mtd keyMaker valMaker length elemTyp =
        let contents =
            valMaker
            |> Seq.init length
            |> Seq.foldi (fun h i v -> Heap.add (keyMaker i mtd) { value = v; created = Timestamp.zero; modified = Timestamp.zero } h) Heap.empty
        let length = makeNumber length mtd
        let constant = Constant mtd defaultArrayName (DefaultArray()) (ArrayType(lengthTermType, ConcreteDimension 1))
        let instantiator = [makeTrue mtd, DefaultInstantiator(constant, elemTyp)]
        makeArray mtd length contents instantiator elemTyp

    let makeIntegerArray mtd maker length =
        makeLinearConreteArray mtd makeNumber maker length lengthTermType

    let makeLinearSymbolicArray mtd length symbolicValue elemType =
        let instantiator = [Terms.True, LazyInstantiator (symbolicValue, elemType)]
        makeArray mtd length Heap.empty instantiator elemType

    let makeSymbolicIntegerArray mtd length symbolicValue =
        makeLinearSymbolicArray mtd length symbolicValue lengthTermType

    let equalsArrayIndices mtd addr1 addr2 =
        let equalsHeap h1 h2 resolve1 resolve2 =
            let keysSet = HashSet(Heap.locations h1)
            keysSet.UnionWith(HashSet(Heap.locations h2))
            let keys = seq(keysSet)
            Seq.mapFold (fun (h1, h2) key ->
                let fill val1 val2 h1 h2 =
                    Arithmetics.eq mtd val1.value val2.value, (h1, h2)
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
        match addr1.term, addr2.term with
        | Array(d1, len1, lb1, init1, indices1, l1, t1), Array(d2, len2, lb2, init2, indices2, l2, t2) ->
            assert(List.length init1 = 1 && List.length init2 = 1)
            let initTerm1, initTerm2 = List.head init1 |> snd, List.head init2 |> snd
            let makeConstant mtd constant i =
                let id = sprintf "%s[%s]" (toString constant) (toString i) |> IdGenerator.startingWith
                Some { value = Constant mtd id ({indices = constant} : SymbolicArrayIndex) lengthTermType; created = Timestamp.zero; modified = Timestamp.zero }
            let makeGvs h1 h2 val1 val2 =
                let isSameLen = Arithmetics.eq mtd len1 len2
                match isSameLen with
                | False -> isSameLen, (addr1, addr2)
                | _ ->
                    let gvs, (h1, h2) = equalsHeap h1 h2 val1 val2
                    let arr1 = Array addr1.metadata d1 len1 lb1 init1 h1 l1 t1
                    let arr2 = Array addr2.metadata d2 len2 lb2 init2 h2 l2 t2
                    isSameLen &&& Propositional.conjunction mtd gvs, (arr1, arr2)
            match initTerm1, initTerm2 with
            | LazyInstantiator(constant1, _), LazyInstantiator(constant2, _) -> Arithmetics.eq mtd constant1 constant2, (addr1, addr2)
            | LazyInstantiator(constant1, _), DefaultInstantiator _ -> makeGvs indices1 indices2 (makeConstant mtd constant1) (always None)
            | DefaultInstantiator _, LazyInstantiator(constant2, _) -> makeGvs indices1 indices2 (always None) (makeConstant mtd constant2)
            | DefaultInstantiator _, DefaultInstantiator _ -> makeGvs indices1 indices2 (always None) (always None)
        | _ -> __unreachable__()

    let private makeSymbolicDimensionsNumber metadata arrayConstant arrayName =
        let dim = sprintf "|dimensions of %s|" arrayName
        Constant metadata dim ({array = arrayConstant} : SymbolicArrayDimensionNumber) lengthTermType

    let private makeSymbolicLength metadata arrayConstant arrayName =
        let idOfDimension = sprintf "|%s|" arrayName
        Constant metadata idOfDimension ({array = arrayConstant} : SymbolicArrayLength) lengthTermType

    let zeroLowerBound metadata dimension =
        let bound = { value = Concrete metadata 0 lengthTermType; created = Timestamp.zero; modified = Timestamp.zero }
        Seq.fold (fun h l -> Heap.add l bound h) Heap.empty (Seq.init dimension (fun i -> makeNumber i metadata))

    let rec length mtd term =
        match term.term with
        | Error _ -> term
        | Array(_, len, _, _, _, _, _) -> len
        | Union gvs -> Merging.guardedMap (length mtd) gvs
        | _ -> internalfail "computing length of non-array object"

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
            let lengths = Seq.foldi (fun h i l -> Heap.add (makeNumber i mtd) { value = l; created = Timestamp.zero; modified = Timestamp.zero} h) Heap.empty lengthList
            Array mtd (makeNumber dim mtd) length lowerBounds [Terms.True, DefaultInstantiator(constant, elemTyp)] Heap.empty lengths typ
        unguardedLengths |> List.map (fun (g, ls) -> (g, makeArray ls)) |> Merging.merge

    and makeSymbolicLowerBound metadata arrayConstant arrayName dimension =
        match Options.ExplorationMode() with
        | TrustConventions -> zeroLowerBound metadata dimension
        | CompleteExploration ->
            let idOfBound i = sprintf "%s.GetLowerBound(%i)" arrayName i
            let mkLowerBound i = Constant metadata (idOfBound i) ({array=arrayConstant; index = makeNumber i metadata; upper = false} : SymbolicArrayBound) lengthTermType
            Seq.foldi (fun h i l -> Heap.add (makeNumber i metadata) { value = l; created = Timestamp.zero; modified = Timestamp.zero } h) Heap.empty (Seq.init dimension mkLowerBound)

    and makeSymbolicLengths metadata arrayConstant arrayName dimension =
        let idOfLength i = sprintf "%s.GetLength(%i)" arrayName i
        let mkLength i = Constant metadata (idOfLength i) ({array=arrayConstant; index = makeNumber i metadata; upper = true} : SymbolicArrayBound) lengthTermType
        let lengths = Seq.init dimension mkLength
        let length = Seq.reduce (mul metadata) lengths
        Seq.foldi (fun h i l -> Heap.add (makeNumber i metadata) { value = l; created = Timestamp.zero; modified = Timestamp.zero } h) Heap.empty lengths, length

    and makeSymbolic metadata source (dimension : arrayDimensionType) elemTyp typ arrayName =
        let arrayConstant = Constant metadata arrayName source typ
        let instantiator = [Terms.True , LazyInstantiator(arrayConstant, elemTyp)]
        let lowerBound, arrayLengths, arrayLength, dim =
            let makeConcrete d =
                let lb = makeSymbolicLowerBound metadata arrayConstant arrayName d
                let al, length = makeSymbolicLengths metadata arrayConstant arrayName d
                lb, al, length, makeNumber d metadata
            match dimension with
            | Vector -> makeConcrete 1
            | ConcreteDimension d -> makeConcrete d
            | SymbolicDimension _ ->
                let length = makeSymbolicLength metadata arrayConstant arrayName
                Heap.empty, Heap.empty, length, makeSymbolicDimensionsNumber metadata arrayConstant arrayName
        Array metadata dim arrayLength lowerBound instantiator Heap.empty arrayLengths typ

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
            | _ -> [{ value = term; created = time; modified = time }], []
        let linearContent, dimensions = flatten rank initializer
        let len = List.length linearContent
        assert(len = List.reduce (*) dimensions)
        let intToTerm i = Concrete mtd i lengthTermType
        let dimensionList = dimensions |> List.map intToTerm
        let length = makeNumber len mtd
        let lengths = Seq.foldi (fun h i l -> Heap.add (makeNumber i mtd) { value = l; created = Timestamp.zero; modified = Timestamp.zero} h) Heap.empty dimensionList
        let indices =
            List.foldBack (fun i s ->
                let indicesInDim = Seq.init i intToTerm
                let res = Seq.map (fun x -> Seq.map (cons x) s) indicesInDim
                res |> Seq.concat) dimensions (Seq.init 1 (always List.empty))
            |> Seq.map (fun index -> makeIntegerArray mtd (fun i -> index.[i]) index.Length)
        let contents = Seq.zip indices linearContent |> Heap.ofSeq
        let constant = Constant mtd defaultArrayName (DefaultArray()) typ
        Array mtd (makeNumber rank mtd) length (zeroLowerBound mtd rank) [Terms.True, DefaultInstantiator(constant, elemTyp)] contents lengths typ
