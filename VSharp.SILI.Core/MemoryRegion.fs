namespace VSharp.Core

open System
open System.Text
open Microsoft.FSharp.Core
open VSharp
open VSharp.CSharpUtils
open TypeUtils
open VSharp.Core

type IMemoryKey<'a, 'reg when 'reg :> IRegion<'reg>> =
    abstract Region : 'reg
    abstract Map : (term -> term) -> (Type -> Type) -> (vectorTime -> vectorTime) -> 'reg -> 'reg * 'a
    abstract IsUnion : bool
    abstract Unguard : (term * 'a) list
    abstract InRegionCondition : 'reg -> term
    abstract IntersectionCondition : 'a -> term
    abstract MatchCondition : 'a -> 'reg -> term
    abstract IsExplicit: vectorTime pset -> bool
    abstract IsRange : bool

type regionSort =
    | HeapFieldSort of fieldId
    | StaticFieldSort of fieldId
    | ArrayIndexSort of arrayType
    | ArrayLengthSort of arrayType
    | ArrayLowerBoundSort of arrayType
    | StackBufferSort of stackKey
    | BoxedSort of Type

    member x.TypeOfLocation =
        match x with
        | HeapFieldSort field
        | StaticFieldSort field -> field.typ
        | ArrayIndexSort arrayType -> arrayType.elemType
        | ArrayLengthSort _
        | ArrayLowerBoundSort _ -> typeof<int32>
        | StackBufferSort _ -> typeof<int8>
        | BoxedSort t -> t

    override x.ToString() =
        match x with
        | HeapFieldSort field -> $"HeapField {field.FullName}"
        | StaticFieldSort field -> $"StaticField {field.FullName}"
        | ArrayIndexSort {elemType = elementType; dimension = dim; isVector = isVector} ->
            if isVector then
                $"VectorIndex to {elementType}[{dim}]"
            else $"ArrayIndex to {elementType}[{dim}]"
        | ArrayLengthSort {elemType = elementType; dimension = dim; isVector = isVector} ->
            if isVector then
                $"VectorLength to {elementType}[{dim}]"
            else $"ArrayLength to {elementType}[{dim}]"
        | ArrayLowerBoundSort {elemType = elementType; dimension = dim; isVector = isVector} ->
            if isVector then
                $"VectorLowerBound to {elementType}[{dim}]"
            else $"ArrayLowerBound to {elementType}[{dim}]"
        | StackBufferSort stackKey -> $"StackBuffer of {stackKey}"
        | BoxedSort t -> $"Boxed of {t}"

module private MemoryKeyUtils =

    let regionOfHeapAddress = function
        | {term = ConcreteHeapAddress addr} -> intervals<vectorTime>.Singleton addr
        | addr -> intervals<vectorTime>.Closed VectorTime.minfty (timeOf addr)

    let private extractInt = function
        | {term = Concrete(:? int as value, typ)} when typ = lengthType -> Some value
        | {term = Concrete(:? uint as value, typ)} when typ = typeof<UInt32> -> Some (int value)
        | _ -> None

    let regionOfIntegerTerm = extractInt >> function
        | Some value -> points<int>.Singleton value
        | None -> points<int>.Universe

    let regionsOfIntegerTerms = List.map regionOfIntegerTerm >> listProductRegion<points<int>>.OfSeq

    let regionOfIntegerRange lowerBound upperBound =
        match extractInt lowerBound, extractInt upperBound with
        | Some lb, Some ub -> points<int>.Range lb ub
        | _ -> points<int>.Universe

    let regionsOfIntegerRanges lowerBounds upperBounds = List.map2 regionOfIntegerRange lowerBounds upperBounds |> listProductRegion<points<int>>.OfSeq

    let heapAddressInVectorTimeIntervals (key : heapAddress) (region : vectorTime intervals) =
        let vt2Term time = Concrete time addressType
        let isLeft point =
            match point.sort with
            | endpointSort.ClosedLeft
            | endpointSort.OpenLeft -> true
            | _ -> false
        let left, right = List.partition isLeft region.points
        let processOne left right =
            match left.sort, right.sort with
            | endpointSort.ClosedLeft, endpointSort.ClosedRight ->
                (simplifyLessOrEqual (vt2Term left.elem) key id) &&& (simplifyLessOrEqual key (vt2Term right.elem) id)
            | endpointSort.OpenLeft, endpointSort.ClosedRight ->
                (simplifyLess (vt2Term left.elem) key id) &&& (simplifyLessOrEqual key (vt2Term right.elem) id)
            | endpointSort.ClosedLeft, endpointSort.OpenRight ->
                (simplifyLessOrEqual (vt2Term left.elem) key id) &&& (simplifyLess key (vt2Term right.elem) id)
            | endpointSort.OpenLeft, endpointSort.OpenRight ->
                (simplifyLess (vt2Term left.elem) key id) &&& (simplifyLess key (vt2Term right.elem) id)
            | _ -> __unreachable__()
        (False(), List.zip left right) ||> List.fold (fun acc (l,r) -> acc ||| (processOne l r))

    let keyInIntPoints key (region : int points) =
        let points, negateIfNeed =
            match region with
            | {points = points; thrown = true} -> points, (!!)
            | {points = points; thrown = false} -> points, id
        let point2Term point = Concrete point indexType
        let keyInPoints point = key === (point2Term point)
        PersistentSet.fold (fun acc p ->  acc ||| (keyInPoints p)) (False()) points |> negateIfNeed

    let keyInProductRegion keyInFst keyInSnd (region : productRegion<'a,'b>) =
        let checkInOne acc (first, second) = acc &&& (keyInFst first) &&& (keyInSnd second)
        List.fold checkInOne (True()) region.products

    let rec keysInListProductRegion (keys : term list) (region : int points listProductRegion) =
        match region, keys with
        | NilRegion, [] -> True()
        | ConsRegion products, curr :: rest ->
            let keyInPoints = keyInIntPoints curr
            let keyInProduct = keysInListProductRegion rest
            keyInProductRegion keyInPoints keyInProduct products
        | _ -> __unreachable__()

[<CustomEquality;CustomComparison>]
type heapAddressKey =
    {address : heapAddress}
    interface IHeapAddressKey with
        override x.Region = MemoryKeyUtils.regionOfHeapAddress x.address
        override x.Map mapTerm _ mapTime reg =
            let symbolicReg = intervals<vectorTime>.Closed VectorTime.minfty VectorTime.zero
            let newReg =
                if (reg :> IRegion<vectorTime intervals>).CompareTo symbolicReg = Includes then
                    let rightBound = mapTime []
                    assert(not <| VectorTime.isEmpty rightBound)
                    let reg' = (reg :> IRegion<vectorTime intervals>).Subtract symbolicReg
                    let mappedZeroInterval = intervals<vectorTime>.Closed VectorTime.minfty rightBound
                    mappedZeroInterval.Union(reg'.Map mapTime)
                else
                    reg.Map mapTime
            newReg, {address = mapTerm x.address}
        override x.IsUnion = isUnion x.address
        override x.Unguard = Merging.unguardGvs x.address |> List.map (fun (g, a) -> (g, {address = a}))
        override x.InRegionCondition region =
            MemoryKeyUtils.heapAddressInVectorTimeIntervals x.address region
        override x.IntersectionCondition key =
            x.address === key.address
        override x.MatchCondition key keyIndexingRegion =
            let addressesAreEqual = (x :> IHeapAddressKey).IntersectionCondition key
            let xInKeyRegion = (x :> IHeapAddressKey).InRegionCondition keyIndexingRegion
            addressesAreEqual &&& xInKeyRegion
        override x.IsExplicit explicitAddresses =
            match x.address.term with
            | ConcreteHeapAddress addr -> PersistentSet.contains addr explicitAddresses
            | _ -> false
        override x.IsRange = false
    interface IComparable with
        override x.CompareTo y =
            match y with
            | :? heapAddressKey as y -> compareTerms x.address y.address
            | _ -> -1
    override x.ToString() = x.address.ToString()
    override x.Equals(other : obj) =
        match other with
        | :? heapAddressKey as other ->
            x.address = other.address
        | _ -> false
    override x.GetHashCode() = x.address.GetHashCode()

and IHeapAddressKey = IMemoryKey<heapAddressKey, vectorTime intervals>

[<CustomEquality;CustomComparison>]
type heapArrayKey =
    | OneArrayIndexKey of heapAddress * term list
    | RangeArrayIndexKey of heapAddress * term list * term list

    member x.Address =
        match x with
        | OneArrayIndexKey(a, _) -> a
        | RangeArrayIndexKey(a, _, _) -> a

    member x.Rank =
        match x with
        | OneArrayIndexKey(_, indices) -> List.length indices
        | RangeArrayIndexKey(_, fromIndices, toIndices) ->
            let toIndicesLength = List.length toIndices
            assert(List.length fromIndices = toIndicesLength)
            toIndicesLength

    member x.IsOneIndexKey =
        match x with
        | OneArrayIndexKey _ -> true
        | _ -> false
    
    member x.Specialize writeKey srcA srcF srcT  =
        match x, writeKey with
        | OneArrayIndexKey(_, i), OneArrayIndexKey(_, dstI)
        | OneArrayIndexKey(_, i), RangeArrayIndexKey(_, dstI, _) ->
            let newI = List.map3 (fun i dstI srcF -> add (sub i dstI) srcF) i dstI srcF
            OneArrayIndexKey(srcA, newI)
        | RangeArrayIndexKey(_, fI, tI), OneArrayIndexKey(_, dstI)
        | RangeArrayIndexKey(_, fI, tI), RangeArrayIndexKey(_, dstI, _) ->
            let delta = List.map3 (fun fI tI dstI -> (sub fI dstI), (sub tI dstI)) fI tI dstI
            let fromIndices = List.map2 (fun (fD, _) srcF -> add fD srcF) delta srcF
            let toIndices = List.map2 (fun (_, tD) srcT -> add tD srcT) delta srcT
            RangeArrayIndexKey(srcA, fromIndices, toIndices)

    member x.Includes key =
        match x, key with
        | _ when key = x -> true
        | RangeArrayIndexKey(a1, lbs1, ubs1), RangeArrayIndexKey(a2, lbs2, ubs2) when a1 = a2 ->
            let lbsConcrete1 = tryIntListFromTermList lbs1
            let lbsConcrete2 = tryIntListFromTermList lbs2
            let ubsConcrete1 = tryIntListFromTermList ubs1
            let ubsConcrete2 = tryIntListFromTermList ubs2
            match lbsConcrete1, lbsConcrete2, ubsConcrete1, ubsConcrete2 with
            | Some lbs1, Some lbs2, Some ubs1, Some ubs2 ->
                List.zip lbs1 ubs1
                |> List.map3 (fun lb2 ub2 (lb1, ub1) -> lb1 <= lb2 && ub2 <= ub1) lbs2 ubs2
                |> List.forall id
            | _ -> x = key
        | RangeArrayIndexKey(a1, lbs, ubs), OneArrayIndexKey(a2, i) when a1 = a2 ->
            let lbsConcrete = tryIntListFromTermList lbs
            let ubsConcrete = tryIntListFromTermList ubs
            let iConcrete = tryIntListFromTermList i
            match lbsConcrete, ubsConcrete, iConcrete with
            | Some lbs, Some ubs, Some i ->
                List.map3 (fun lb ub i -> lb <= i && i <= ub) lbs ubs i |> List.forall id
            | _ ->
                List.map3 (fun lb ub i -> lb = i || i = ub) lbs ubs i |> List.forall id
        | OneArrayIndexKey(a1, i), RangeArrayIndexKey(a2, lbs, ubs) when a1 = a2 ->
            List.map3 (fun i lb ub -> i = lb && i = ub) i lbs ubs |> List.forall id
        | OneArrayIndexKey _, OneArrayIndexKey _ -> x = key
        | _ -> false

    interface IHeapArrayKey with
        override x.Region =
            let address, indicesRegion =
                match x with
                | OneArrayIndexKey(address, indices) -> address, MemoryKeyUtils.regionsOfIntegerTerms indices
                | RangeArrayIndexKey(address, lowerBounds, upperBounds) -> address, MemoryKeyUtils.regionsOfIntegerRanges lowerBounds upperBounds
            productRegion<vectorTime intervals, int points listProductRegion>.ProductOf (MemoryKeyUtils.regionOfHeapAddress address) indicesRegion
        override x.Map mapTerm _ mapTime reg =
            match x with
            | OneArrayIndexKey(address, indices) ->
                reg.Map (fun x -> x.Map mapTime) id, OneArrayIndexKey(mapTerm address, List.map mapTerm indices)
            | RangeArrayIndexKey(address, lowerBounds, upperBounds) ->
                let reg' = reg.Map (fun x -> x.Map mapTime) id
                let address' = mapTerm address
                let lowerBounds' = List.map mapTerm lowerBounds
                let upperBounds' = List.map mapTerm upperBounds
                let y = if lowerBounds' = upperBounds' then OneArrayIndexKey(address', lowerBounds')
                        else RangeArrayIndexKey(address', lowerBounds', upperBounds')
                reg', y
        override x.IsUnion = isUnion x.Address
        override x.Unguard =
            match x with
            | OneArrayIndexKey(address, indices) ->
                (address::indices, [True(), []]) ||> List.foldBack (fun index accTerms ->
                    list {
                        let! gt, terms = accTerms
                        let! gi, i = Merging.unguardGvs index
                        return (gi &&& gt, i::terms)
                    })
                |> List.map (fun (g, address::indices) -> (g, OneArrayIndexKey(address, indices)))
            | RangeArrayIndexKey(address, lowerBounds, upperBounds) ->
                let boundsPairs = (lowerBounds, upperBounds, [True(), [], []]) |||> List.foldBack2 (fun lb ub acc ->
                    list {
                        let! g, accLbs, accUbs = acc
                        let! gl, l = Merging.unguardGvs lb
                        let! gu, u = Merging.unguardGvs ub
                        return (g &&& gl &&& gu, l::accLbs, u::accUbs)
                    })
                let addresses = Merging.unguardGvs address
                list {
                    let! gAddr, addr = addresses
                    let! gIndices, lbs, ubs = boundsPairs
                    return (gAddr &&& gIndices, RangeArrayIndexKey(addr, lbs, ubs))
                }

        override x.InRegionCondition region =
            match x with
            | OneArrayIndexKey(address, indices) ->
                let addressInVtIntervals = MemoryKeyUtils.heapAddressInVectorTimeIntervals address
                let indicesInListProd = MemoryKeyUtils.keysInListProductRegion indices
                MemoryKeyUtils.keyInProductRegion addressInVtIntervals indicesInListProd region
            | RangeArrayIndexKey _ -> internalfail $"InRegionCondition for RangeArrayIndexKey {x} is not implemented"
        override x.IntersectionCondition key =
            match x, key with
            | OneArrayIndexKey(address, indices), OneArrayIndexKey(keyAddress, keyIndices)->
                let addressesAreEqual = address === keyAddress
                List.fold2 (fun acc i j -> acc &&& (i === j)) addressesAreEqual indices keyIndices
            | OneArrayIndexKey(address, indices), RangeArrayIndexKey(keyAddress, keyLbs, keyUbs) ->
                let addressesAreEqual = address === keyAddress
                let inBounds acc i (l,u) = acc &&& (simplifyLessOrEqual l i id &&& simplifyLessOrEqual i u id)
                (indices, List.zip keyLbs keyUbs) ||> List.fold2 inBounds addressesAreEqual
            | RangeArrayIndexKey(address, lbs, ubs), OneArrayIndexKey(keyAddress, keyIndices) ->
                let addressesAreEqual = address === keyAddress
                let inBounds acc i (l, u) = acc &&& (simplifyLessOrEqual l i id &&& simplifyLessOrEqual i u id)
                (addressesAreEqual, keyIndices, List.zip lbs ubs) |||> List.fold2 inBounds
            | RangeArrayIndexKey _, RangeArrayIndexKey _ ->
                internalfail $"IntersectionCondition for RangeArrayIndexKey is not implemented: {x} {key}"
        override x.MatchCondition key keyIndexingRegion =
            let keysAreEqual = (x :> IHeapArrayKey).IntersectionCondition key
            match x, key with
            | OneArrayIndexKey _, RangeArrayIndexKey _ ->
                // key address and x indices are in key's region
                // NOTE: checking x indices, because key is range of indices and we can not encode it as Expr[]
                let xInKeyRegion = (x :> IHeapArrayKey).InRegionCondition keyIndexingRegion
                keysAreEqual &&& xInKeyRegion
            | _ -> keysAreEqual &&& (key :> IHeapArrayKey).InRegionCondition keyIndexingRegion
        override x.IsExplicit explicitAddresses =
            match x with
            | OneArrayIndexKey(a, _)
            | RangeArrayIndexKey(a, _, _) ->
                match a.term with
                | ConcreteHeapAddress addr -> PersistentSet.contains addr explicitAddresses
                | _ -> false
        override x.IsRange =
            match x with
            | OneArrayIndexKey _ -> false
            | RangeArrayIndexKey _ -> true

    interface IComparable with
        override x.CompareTo y =
            match y with
            | :? heapArrayKey as y ->
                match x, y with
                | OneArrayIndexKey _, RangeArrayIndexKey _ -> -1
                | RangeArrayIndexKey _, OneArrayIndexKey _ -> 1
                | OneArrayIndexKey(address, indices), OneArrayIndexKey(address', indices') ->
                    let cmp = compareTerms address address'
                    if cmp = 0 then List.compareWith compareTerms indices indices'
                    else cmp
                | RangeArrayIndexKey(address, lowerBounds, upperBounds), RangeArrayIndexKey(address', lowerBounds', upperBounds') ->
                    let cmp = compareTerms address address'
                    if cmp = 0 then
                        let cmp = List.compareWith compareTerms lowerBounds lowerBounds'
                        if cmp = 0 then List.compareWith compareTerms upperBounds upperBounds'
                        else cmp
                    else cmp
            | _ -> -1
    override x.ToString() =
        match x with
        | OneArrayIndexKey(address, indices) ->
            sprintf "%O[%O]" address (indices |> List.map toString |> join ", ")
        | RangeArrayIndexKey(address, lowerBounds, upperBounds) ->
            sprintf "%O[%O]" address (List.map2 (sprintf "%O..%O") lowerBounds upperBounds |> join ", ")
    override x.Equals(other : obj) =
        match other with
        | :? heapArrayKey as other ->
            match x, other with
            | OneArrayIndexKey(address1, indices1), OneArrayIndexKey(address2, indices2) ->
                address1 = address2 && indices1 = indices2
            | RangeArrayIndexKey(address1, lbs1, ubs1), RangeArrayIndexKey(address2, lbs2, ubs2) ->
                address1 = address2 && lbs1 = lbs2 && ubs1 = ubs2
            | _ -> false
        | _ -> false
    override x.GetHashCode() =
        match x with
        | OneArrayIndexKey(address, indices) -> HashCode.Combine(1, address, indices)
        | RangeArrayIndexKey(address, lbs, ubs) -> HashCode.Combine(2, address, lbs, ubs)

and IHeapArrayKey = IMemoryKey<heapArrayKey, productRegion<vectorTime intervals, int points listProductRegion>>

[<CustomEquality;CustomComparison>]
type heapVectorIndexKey =
    {address : heapAddress; index : term}
    interface IHeapVectorIndexKey with
        override x.Region =
            productRegion<vectorTime intervals, int points>.ProductOf (MemoryKeyUtils.regionOfHeapAddress x.address) (MemoryKeyUtils.regionOfIntegerTerm x.index)
        override x.Map mapTerm _ mapTime reg =
            reg.Map (fun x -> x.Map mapTime) id, {address = mapTerm x.address; index = mapTerm x.index}
        override x.IsUnion = isUnion x.address
        override x.Unguard =
            // TODO: if x.index is the union of concrete values, then unguard index as well
            Merging.unguardGvs x.address |> List.map (fun (g, a) -> (g, {address = a; index = x.index}))
        override x.InRegionCondition region =
            let addressInVtIntervals = MemoryKeyUtils.heapAddressInVectorTimeIntervals x.address
            let indexInPoints = MemoryKeyUtils.keyInIntPoints x.index
            MemoryKeyUtils.keyInProductRegion addressInVtIntervals indexInPoints region
        override x.IntersectionCondition key =
             (x.address === key.address) &&& (x.index === key.index)
        override x.MatchCondition key keyIndexingRegion =
            let keysAreEqual = (x :> IHeapVectorIndexKey).IntersectionCondition key
            let xInKeyRegion = (x :> IHeapVectorIndexKey).InRegionCondition keyIndexingRegion
            keysAreEqual &&& xInKeyRegion
        override x.IsExplicit explicitAddresses =
            match x.address.term with
            | ConcreteHeapAddress addr -> PersistentSet.contains addr explicitAddresses
            | _ -> false
        override x.IsRange = false
    interface IComparable with
        override x.CompareTo y =
            match y with
            | :? heapVectorIndexKey as y ->
                let cmp = compareTerms x.address y.address
                if cmp = 0 then compareTerms x.index y.index
                else cmp
            | _ -> -1
    override x.ToString() = $"{x.address}[{x.index}]"
    override x.Equals(other : obj) =
        match other with
        | :? heapVectorIndexKey as other ->
            x.address = other.address && x.index = other.index
        | _ -> false
    override x.GetHashCode() = HashCode.Combine(x.address, x.index)

and IHeapVectorIndexKey = IMemoryKey<heapVectorIndexKey, productRegion<vectorTime intervals, int points>>

[<CustomEquality;CustomComparison>]
type stackBufferIndexKey =
    {index : term}
    interface IStackBufferIndexKey with
//        override x.IsAllocated = true
        override x.Region = MemoryKeyUtils.regionOfIntegerTerm x.index
        override x.Map mapTerm _ _ reg =
            reg, {index = mapTerm x.index}
        override x.IsUnion =
            match x.index.term with
            | Ite {branches = branches; elseValue = e} -> List.forall (fst >> isConcrete) branches
            | _ -> false
        override x.Unguard =
            match x.index.term with
            | Ite iteType when List.forall (fst >> isConcrete) iteType.branches -> iteType |> IteAsGvs |> List.map (fun (g, idx) -> (g, {index = idx}))
            | _ -> [(True(), x)]
        override x.InRegionCondition region =
            MemoryKeyUtils.keyInIntPoints x.index region
        override x.IntersectionCondition key =
            x.index === key.index
        override x.MatchCondition key keyIndexingRegion =
            let keysAreEqual = (x :> IStackBufferIndexKey).IntersectionCondition key
            let xInKeyRegion = (x :> IStackBufferIndexKey).InRegionCondition keyIndexingRegion
            keysAreEqual &&& xInKeyRegion
        override x.IsExplicit explicitAddresses =
            match x.index.term with
            | ConcreteHeapAddress addr -> PersistentSet.contains addr explicitAddresses
            | _ -> false
        override x.IsRange = false
    interface IComparable with
        override x.CompareTo y =
            match y with
            | :? stackBufferIndexKey as y -> compareTerms x.index y.index
            | _ -> -1
    override x.ToString() = x.index.ToString()
    override x.Equals(other : obj) =
        match other with
        | :? stackBufferIndexKey as other ->
            x.index = other.index
        | _ -> false
    override x.GetHashCode() = x.index.GetHashCode()

and IStackBufferIndexKey = IMemoryKey<stackBufferIndexKey, int points>

[<CustomEquality;CustomComparison>]
type typeWrapper = {t : Type}
with
    interface IAtomicRegion<typeWrapper> with
        override x.Intersect y = structuralInfimum x.t y.t |> Option.map (fun t -> {t = t})
    override x.GetHashCode() = x.t.GetDeterministicHashCode()
    override x.Equals(o : obj) =
        match o with
        | :? typeWrapper as y -> x.t.TypeHandle = y.t.TypeHandle
        | _ -> false
    interface IComparable with
        override x.CompareTo(other) =
            match other with
            | :? typeWrapper as y -> compare x.t.TypeHandle.Value y.t.TypeHandle.Value
            | _ -> -1

[<CustomEquality;CustomComparison>]
type symbolicTypeKey =
    {typ : Type}
    interface ISymbolicTypeKey with
    // TODO: when statics are allocated? always or never? depends on our exploration strategy
//      override x.IsAllocated = false
        override x.Region = freeRegion<typeWrapper>.Singleton {t = x.typ}
        override x.Map _ mapper _ reg =
            reg.Map (fun t -> {t = mapper t.t}), {typ = mapper x.typ}
        override x.IsUnion = false
        override x.Unguard = [(True(), x)]
        override x.InRegionCondition _ =
            __unreachable__()
        override x.IntersectionCondition key =
            Concrete (x.typ = key.typ) typeof<bool>
        override x.MatchCondition key _ =
            Concrete (x.typ = key.typ) typeof<bool>
        override x.IsExplicit _ = __unreachable__()
        override x.IsRange = false
    override x.ToString() = x.typ.ToString()
    override x.GetHashCode() = x.typ.GetDeterministicHashCode()
    override x.Equals(o : obj) =
        match o with
        | :? symbolicTypeKey as y -> x.typ.TypeHandle.Value = y.typ.TypeHandle.Value
        | _ -> false
    interface IComparable with
        override x.CompareTo(other) =
            match other with
            | :? symbolicTypeKey as y -> compare x.typ.TypeHandle.Value y.typ.TypeHandle.Value
            | _ -> -1

and ISymbolicTypeKey = IMemoryKey<symbolicTypeKey, freeRegion<typeWrapper>>

type updateTreeKey<'key, 'value when 'key : equality> =
    {key : 'key; value : 'value; guard : term option}
    interface IRegionTreeKey<updateTreeKey<'key, 'value>> with
        override x.Hides y = x.key = y.key && x.hasTrueGuard
    member x.termGuard = Option.defaultValue (True()) x.guard
    member x.hasTrueGuard =
        match x.guard with
        | Some g when isTrue g |> not -> false
        | _ -> true
    member x.hasFalseGuard =
        match x.guard with
        | Some g when isFalse g -> true
        | _ -> false
    override x.ToString() = (x.key, x.value, x.guard).ToString()

type updateTree<'key, 'value, 'reg when 'key :> IMemoryKey<'key, 'reg> and 'reg :> IRegion<'reg> and 'key : equality and 'value : equality and 'reg : equality> =
    regionTree<updateTreeKey<'key, 'value>, 'reg>

module private UpdateTree =
    let empty<'key, 'value, 'reg when 'key :> IMemoryKey<'key, 'reg> and 'reg :> IRegion<'reg> and 'key : equality and 'value : equality and 'reg : equality> =
        RegionTree.empty<updateTreeKey<'key, 'value>, 'reg>

    let isEmpty tree = RegionTree.isEmpty tree

    let rec private termIsAddress term =
        match term.term with
        | HeapRef _
        | Struct _
        | Concrete(:? concreteHeapAddress, _) -> true
        | Ite iteType -> iteType.exists termIsAddress
        | _ -> false

    let private splitInvariantTree readKey (Node d) predicate specializedReading =
        let mutable matchingWrites = List.empty
        let mutable symbolicStack = List.empty
        let mutable foundExactKey = false
        let rec recSplit (Node d) =
            // records ordered from newest to oldest
            let records = PersistentDict.toSeq d |> Seq.rev // may be inefficient?
            let enumerator = records.GetEnumerator()
            while not foundExactKey && enumerator.MoveNext() do
                let reg, (utKey, subtree) = enumerator.Current
                let readCondition = (readKey :> IMemoryKey<_,_>).IntersectionCondition utKey.key
                let finalGuard = readCondition &&& utKey.termGuard
                // found exactly match with [readKey]
                if isTrue finalGuard then
                    foundExactKey <- true
                    if predicate utKey then
                        matchingWrites <- (finalGuard, utKey.value)::matchingWrites
                    else
                        symbolicStack <- (reg, (utKey, subtree))::symbolicStack
                elif predicate utKey then
                    if isFalse finalGuard |> not then
                        let value =
                            if (utKey.key :> IMemoryKey<_,_>).IsRange then specializedReading readKey utKey else utKey.value
                        matchingWrites <- (finalGuard, value)::matchingWrites
                    recSplit subtree
                else do
                    symbolicStack <- (reg, (utKey, subtree))::symbolicStack
        recSplit (Node d)
        let symbolicTree = List.fold (fun tree (r, (k, t)) -> PersistentDict.add r (k, t) tree) PersistentDict.empty symbolicStack
        List.rev matchingWrites, symbolicTree

    let private splitRead d key explicitAddresses specializedReading predicate isDefault makeSymbolic makeDefault =
        let matchingWrites, symbolicTree = splitInvariantTree key (Node d) predicate specializedReading
        if PersistentDict.isEmpty symbolicTree && key.IsExplicit explicitAddresses then
            match matchingWrites with
            | [(_, v)] -> v
            | _ -> iteType.FromGvs matchingWrites |> Merging.merge
        elif PersistentDict.isEmpty symbolicTree && isDefault key then
            assert not (key.IsExplicit explicitAddresses)
            let defaultCase = makeDefault()
            {branches = matchingWrites; elseValue = defaultCase} |> Merging.merge
        else
            let symbolicCase = makeSymbolic (Node symbolicTree)
            {branches = matchingWrites; elseValue = symbolicCase} |> Merging.merge

    let read (key : 'key) (tree : updateTree<'key, term, 'reg>) explicitAddresses specializedReading predicate isDefault makeSymbolic makeDefault =
        assert(not key.IsUnion)
        let reg = key.Region
        let d = RegionTree.localize reg tree
        if PersistentDict.isEmpty d then
            if isDefault key then makeDefault() else makeSymbolic (Node d)
        else
            // do not change the order of pattern matching
            match PersistentDict.last d with
            | Some(_, ({key = key'; value = value'} as utKey, _)) when utKey.hasTrueGuard && key = key' -> value' // exact match
            | _ when key.IsRange -> makeSymbolic (Node d) // range key reading
            | Some(_, ({key = key'} as utKey, _))
                when utKey.hasTrueGuard && key.IntersectionCondition key' |> isTrue && key'.IsRange -> // specialized reading
                specializedReading key utKey
            | _ when makeDefault() |> termIsAddress -> // reading from addresses region
                splitRead d key explicitAddresses specializedReading predicate isDefault makeSymbolic makeDefault
            | _ -> // reading from non addresses region
                makeSymbolic (Node d)

    let memset (keyAndValues : seq<'key * 'value>) (tree : updateTree<'key, 'value, 'reg>) =
        let keyAndRegions = keyAndValues |> Seq.map (fun (key, value) -> key.Region, {key=key; value=value; guard = None})
        RegionTree.memset keyAndRegions tree

    let rec private placeRecordUnderInvariantSubtree region utKey (Node d) predicate =
        let mutable restRegion = region
        let guardFromOverwriting treeKey =
            !!((utKey.key :> IMemoryKey<_,_>).IntersectionCondition treeKey.key &&& utKey.termGuard) &&& treeKey.termGuard
        let placeRecordUnderBranch acc stReg (stUtKey, st) =
            restRegion <- (restRegion :> IRegion<_>).Subtract stReg
            if predicate utKey then
                let modifiedSubtree = placeRecordUnderInvariantSubtree stReg utKey st predicate
                let updatedGuard = guardFromOverwriting stUtKey
                if isFalse updatedGuard then PersistentDict.append acc modifiedSubtree
                else
                    let guardedStUtKey = {stUtKey with guard = Some updatedGuard}
                    PersistentDict.add stReg (guardedStUtKey, Node modifiedSubtree) acc
            else
                let subtree = PersistentDict.add stReg (stUtKey, st) PersistentDict.empty
                PersistentDict.add stReg (utKey, Node subtree) acc 
        let modifiedTree = PersistentDict.fold placeRecordUnderBranch PersistentDict.empty d
        if restRegion.IsEmpty then
            modifiedTree
        else
            PersistentDict.add restRegion (utKey, Node PersistentDict.empty) modifiedTree

    let write region (utKey : updateTreeKey<_,_>) tree predicate =
        assert(not (utKey.key :> IMemoryKey<_,_>).IsUnion)
        assert(not utKey.hasFalseGuard)
        if termIsAddress utKey.value |> not then
            RegionTree.write utKey.key.Region utKey tree
        elif predicate utKey then
            // invariant will be preserved, do usual write
            RegionTree.write utKey.key.Region utKey tree
        else
            // need to preserve the invariant that is concrete values are always at the top of the tree
            let included, disjoint = RegionTree.localizeFilterHidden region utKey tree
            let modifiedTree = placeRecordUnderInvariantSubtree region utKey (Node included) predicate
            PersistentDict.append disjoint modifiedTree |> Node
 
    let map (mapKey : 'reg -> 'key -> 'reg * 'key) mapValue (tree : updateTree<'key, 'value, 'reg>) =
        let mapper reg {key = k; value = v; guard = g} =
            let reg', k' = mapKey reg k
            let g' = Option.map mapValue g
            match g' with
            | Some False -> None
            | _ -> Some(reg', k'.Region, {key = k'; value = mapValue v; guard = g'})
        RegionTree.choose mapper tree RegionTree.write

    let compose (earlier : updateTree<'key, 'value, 'reg>) (later : updateTree<'key, 'value, 'reg>) =
        let writeOneKey reg utKey accTree =
            assert(not (utKey.key :> IMemoryKey<_,_>).IsUnion)
            RegionTree.write reg utKey accTree
        RegionTree.foldr writeOneKey earlier later

    let deterministicCompose earlier later = RegionTree.append earlier later

    let print indent valuePrint tree =
        let window = 50
        let incIndent = indent + "    "
        let lines = tree |> RegionTree.flatten |> List.map (fun (_, {key=k; value=v}) -> $"{k} <- {valuePrint v}")
        if lines |> List.sumBy (fun s -> s.Length) > window then
            sprintf "{\n%s\n%s}" (lines |> List.map (fun s -> incIndent + s) |> join "\n") indent
        else sprintf "{%s}" (join "; " lines)

    let localize reg tree = RegionTree.localize reg tree |> Node

    let forall predicate tree = RegionTree.foldl (fun acc _ k -> acc && predicate k) true tree

[<CustomEquality; NoComparison>]
type memoryRegion<'key, 'reg when 'key : equality and 'key :> IMemoryKey<'key, 'reg> and 'reg : equality and 'reg :> IRegion<'reg>> =
    {typ : Type; updates : updateTree<'key, term, 'reg>; defaultValue : term option; explicitAddresses : vectorTime pset}
    with
    override x.Equals(other : obj) =
        match other with
        | :? memoryRegion<'key, 'reg> as other ->
            let compareDefault =
                lazy(
                    match x.defaultValue, other.defaultValue with
                    | Some v1, Some v2 -> v1 = v2
                    | None, None -> true
                    | _ -> false
                )
            x.typ = other.typ && x.updates = other.updates && compareDefault.Value
        | _ -> false
    override x.GetHashCode() =
        let defaultValue =
            match x.defaultValue with
            | Some v -> Some v
            | None -> None
        HashCode.Combine(x.typ, x.updates, defaultValue)

module MemoryRegion =
    let empty typ = {typ = typ; updates = UpdateTree.empty; defaultValue = None; explicitAddresses =  PersistentSet.empty}
    let emptyWithExplicit typ addr = {typ = typ; updates = UpdateTree.empty; defaultValue = None; explicitAddresses = PersistentSet.add PersistentSet.empty addr}
    let addExplicitAddress addr mr = {mr with explicitAddresses = PersistentSet.add mr.explicitAddresses addr}

    let rec private isConcreteHeapAddress term =
        match term.term with
        | Concrete(:? concreteHeapAddress, _) -> true
        | HeapRef(address, _) when isConcreteHeapAddress address -> true
        | Struct(s, _) ->
            PersistentDict.exists (fun (_, v) -> isConcreteHeapAddress v) s
        | Ite iteType -> iteType.exists isConcreteHeapAddress
        | _ -> false
    let private valueIsConcreteHeapAddress utKey =
        if (utKey.key :> IMemoryKey<_,_>).IsRange then true
        else isConcreteHeapAddress utKey.value

    let fillRegion defaultValue (region : memoryRegion<_,_>) =
        { region with defaultValue = Some defaultValue }

    let maxTime (tree : updateTree<'a, heapAddress, 'b>) startingTime =
        RegionTree.foldl (fun m _ {key=_; value=v} -> VectorTime.max m (timeOf v)) startingTime tree
    let read mr key isDefault instantiate specializedReading =
        let makeSymbolic tree = instantiate mr.typ {mr with updates = tree}
        let makeDefault() =
            match mr.defaultValue with
            | Some d -> d
            | _ -> makeDefaultValue mr.typ
        let unguardedKey = (key :> IMemoryKey<_,_>).Unguard
        let read k = UpdateTree.read k mr.updates mr.explicitAddresses specializedReading valueIsConcreteHeapAddress isDefault makeSymbolic makeDefault
        match unguardedKey with
        | [(_, k)] -> read k
        | _ -> List.map (mapsnd read) unguardedKey |> iteType.FromGvs |> Merging.merge

    let validateWrite value cellType =
        let typ = typeOf value
        canCastImplicitly typ cellType || isPointer typ && isIntegral cellType

    let memset mr keysAndValues =
        {typ = mr.typ; updates = UpdateTree.memset keysAndValues mr.updates; defaultValue = mr.defaultValue;
         explicitAddresses =  mr.explicitAddresses}

    let write mr guard key value =
        assert(validateWrite value mr.typ)
        assert(not (key :> IMemoryKey<_,_>).IsUnion)
        let utKey = {key = key; value = value; guard = guard}
        {mr with updates = UpdateTree.write (key :> IMemoryKey<_,_>).Region utKey mr.updates valueIsConcreteHeapAddress}

    let map (mapTerm : term -> term) (mapType : Type -> Type) (mapTime : vectorTime -> vectorTime) mr =
        let typ = mapType mr.typ
        let updates = UpdateTree.map (fun reg k -> k.Map mapTerm mapType mapTime reg) mapTerm mr.updates
        let defaultValue = Option.map mapTerm mr.defaultValue
        {typ = typ; updates = updates; defaultValue = defaultValue; explicitAddresses = mr.explicitAddresses }

    let mapKeys<'reg, 'key when 'key : equality and 'key :> IMemoryKey<'key, 'reg> and 'reg : equality and 'reg :> IRegion<'reg>> (mapKey : 'reg -> 'key -> 'reg * 'key) mr =
        {mr with updates = UpdateTree.map mapKey id mr.updates }

    let deterministicCompose earlier later =
        assert later.defaultValue.IsNone
        if earlier.typ = later.typ then
            if UpdateTree.isEmpty earlier.updates then { later with defaultValue = earlier.defaultValue }
            else {typ = earlier.typ
                  updates = UpdateTree.deterministicCompose earlier.updates later.updates
                  defaultValue = earlier.defaultValue
                  explicitAddresses = PersistentDict.append earlier.explicitAddresses later.explicitAddresses}
        else internalfail "Composing two incomparable memory objects!"

    let compose earlier later =
        // TODO write under guards
        assert later.defaultValue.IsNone
        if later.updates |> UpdateTree.forall (fun k -> not k.key.IsUnion) then
            deterministicCompose earlier later
        elif earlier.typ = later.typ then
            let composedTree = UpdateTree.compose earlier.updates later.updates
            {earlier with updates = composedTree}
        else internalfail "Composing two incomparable memory objects!"

    let toString indent mr = UpdateTree.print indent toString mr.updates

    let flatten mr =
        RegionTree.foldr (fun reg k acc -> (reg, k)::acc) [] mr.updates

    let localizeArray address dimension mr =
        let anyIndexRegion = List.replicate dimension points<int>.Universe |> listProductRegion<points<int>>.OfSeq
        let reg = productRegion<vectorTime intervals, int points listProductRegion>.ProductOf (MemoryKeyUtils.regionOfHeapAddress address) anyIndexRegion
        {mr with updates = UpdateTree.localize reg mr.updates}

    // TODO: merging!

type memoryRegion<'key, 'reg when 'key : equality and 'key :> IMemoryKey<'key, 'reg> and 'reg : equality and 'reg :> IRegion<'reg>> with
    override x.ToString() = MemoryRegion.toString "" x

type setKeyWrapper<'key when 'key : equality> =
    {key : 'key}
    interface IRegionTreeKey<setKeyWrapper<'key>> with
        override x.Hides _ = false
    override x.ToString() = x.key.ToString()
type symbolicSet<'key, 'reg when 'key : equality and 'key :> IMemoryKey<'key, 'reg> and 'reg : equality and 'reg :> IRegion<'reg>> = regionTree<setKeyWrapper<'key>, 'reg>

module SymbolicSet =
    let empty<'key, 'reg when 'key : equality and 'key :> IMemoryKey<'key, 'reg> and 'reg : equality and 'reg :> IRegion<'reg>> =
        RegionTree.empty<setKeyWrapper<'key>, 'reg>
    let isEmpty s = RegionTree.isEmpty s
    let matchingElements x s =
        s |> RegionTree.localize (x :> IMemoryKey<'key, 'reg>).Region |> Node |> RegionTree.foldl (fun acc _ k -> k.key::acc) []
    let add x s =
        let matching = matchingElements x s
        match matching with
        | [x'] when x' = x -> s
        | _ ->
            RegionTree.write x.Region {key=x} s
    let ofSeq s = Seq.fold (fun s x -> add x s) empty s
    let map (mapTerm : term -> term) (mapType : Type -> Type) (mapTime : vectorTime -> vectorTime) (s : symbolicSet<'key, 'reg>) =
        s |> RegionTree.map (fun reg k -> let reg, k' = k.key.Map mapTerm mapType mapTime reg in (reg, k'.Region, {key=k'}))
    let union x y = RegionTree.foldr (fun _ k acc -> add k.key acc) x y
    let print s =
        let sb = s |> RegionTree.foldl (fun (sb : StringBuilder) _ -> toString >> sprintf "%s, " >> sb.Append) (StringBuilder().Append "{ ")
        (if sb.Length > 2 then sb.Remove(sb.Length - 2, 2) else sb).Append(" }").ToString()

type heapRegion = memoryRegion<heapAddressKey, vectorTime intervals>
type arrayRegion = memoryRegion<heapArrayKey, productRegion<vectorTime intervals, int points listProductRegion>>
type vectorRegion = memoryRegion<heapVectorIndexKey, productRegion<vectorTime intervals, int points>>
type stackBufferRegion = memoryRegion<stackBufferIndexKey, int points>
type staticsRegion = memoryRegion<symbolicTypeKey, freeRegion<typeWrapper>>
type symbolicTypeSet = symbolicSet<symbolicTypeKey, freeRegion<typeWrapper>>
