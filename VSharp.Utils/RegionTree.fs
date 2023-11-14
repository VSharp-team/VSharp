namespace VSharp

type IRegionTreeKey<'a> =
    abstract Hides : 'a -> bool

 /// Implementation of region tree. Region tree is a tree indexed by IRegion's and having the following invariants:
/// - all regions of one node are disjoint;
/// - the parent region includes all child regions.
type regionTree<'key, 'reg when 'reg :> IRegion<'reg> and 'key : equality and 'reg : equality and 'key :> IRegionTreeKey<'key>> =
    | Node of pdict<'reg, 'key * regionTree<'key, 'reg>>

module RegionTree =

    let private shouldFilter = true

    let empty<'key, 'reg when 'reg :> IRegion<'reg> and 'key : equality and 'reg : equality and 'key :> IRegionTreeKey<'key>> : regionTree<'key, 'reg> = Node PersistentDict.empty

    let isEmpty (Node d) = PersistentDict.isEmpty d

    type private filterResult = FilteredOut | Preserved of RegionComparisonResult

    // Returns subtree of d completely covered by reg
    let rec private splitNode filterOut (reg : 'a when 'a :> IRegion<'a>) (Node d) =
        if PersistentDict.isEmpty d then PersistentDict.empty, PersistentDict.empty
        else
            match PersistentDict.tryFind d reg with
            | Some x ->
                let key, Node child = x
                let included =
                    if shouldFilter && filterOut key then child
                    else PersistentDict.add reg x PersistentDict.empty
                let disjoint = PersistentDict.remove reg d
                included, disjoint
            | None ->
                let makeGroup (reg', (key, _)) =
                    if shouldFilter && filterOut key then FilteredOut
                    else reg.CompareTo reg' |> Preserved
                let groups = PersistentDict.groupBy makeGroup d
                let filtered =
                    PersistentDict.tryFind groups FilteredOut
                    |> Option.defaultValue Seq.empty
                    |> Seq.map (fun (_, (_, Node child)) -> PersistentDict.toSeq child)
                    |> Seq.concat
                let included =
                    PersistentDict.tryFind groups (Preserved Includes)
                    |> Option.defaultValue Seq.empty
                    |> Seq.append filtered
                    |> PersistentDict.ofSeq
                let disjoint =
                    PersistentDict.tryFind groups (Preserved Disjoint)
                    |> Option.defaultValue Seq.empty
                    |> PersistentDict.ofSeq
                let intersected =
                    PersistentDict.tryFind groups (Preserved Intersects)
                    |> Option.defaultValue Seq.empty
                let splitChild (included, disjoint) (reg' : 'a, (k, child)) =
                    let childIncluded, childDisjoint = splitNode filterOut reg child
                    let included = PersistentDict.add (reg'.Intersect reg) (k, Node childIncluded) included
                    let disjoint = PersistentDict.add (reg'.Subtract reg) (k, Node childDisjoint) disjoint
                    included, disjoint
                Seq.fold splitChild (included, disjoint) intersected

    let localize reg tree = splitNode (always false) reg tree |> fst

    // NOTE: [ATTENTION] must be used only if 'reg' were not added earlier and keys are disjoint
    // NOTE: used for fast initialization of new array
    let memset regionsAndKeys (Node tree) =
        regionsAndKeys |> Seq.fold (fun acc (reg, k) -> PersistentDict.add reg (k, Node PersistentDict.empty) acc) tree |> Node

    let write reg key tree =
        let included, disjoint = splitNode (key :> IRegionTreeKey<_>).Hides reg tree
        Node(PersistentDict.add reg (key, Node included) disjoint)

    let rec foldl folder acc (Node d) =
        PersistentDict.fold (fun acc reg (k, t) -> let acc = folder acc reg k in foldl folder acc t) acc d

    let rec foldr folder acc (Node d) =
        PersistentDict.fold (fun acc reg (k, t) -> let acc = foldr folder acc t in folder reg k acc) acc d

    let rec private filterRec reg predicate (Node d as tree) =
        let mutable modified = false
        let mutable result = d
        for reg', (k, t) in PersistentDict.toSeq d do
            if reg'.CompareTo reg <> Disjoint then
                if predicate k then
                    match filterRec reg predicate t with
                    | true, t' ->
                        modified <- true
                        result <- PersistentDict.add reg' (k, t') result
                    | _ -> ()
                else result <- PersistentDict.remove reg' result
        let tree = if modified then Node result else tree
        modified, tree

    let filter reg predicate tree =
        filterRec reg predicate tree |> snd

    let rec map (mapper : 'a -> 'key -> 'a * 'a * 'key when 'a :> IRegion<'a>) tree =
        let folder reg k acc =
            let reg, reg', k' = mapper reg k
            if reg'.CompareTo reg = Disjoint then acc
            else write (reg.Intersect reg') k' acc
        foldr folder empty tree

    let rec append baseTree appendix =
        foldr write baseTree appendix

    let rec flatten tree =
        foldr (fun reg' k acc -> (reg', k)::acc) [] tree

    let private checkInvariantParent (parentReg : IRegion<'a> option) (Node d) =
        match parentReg with
        | Some parentReg -> PersistentDict.forall (fun (reg, _) -> parentReg.CompareTo reg = Includes) d
        | None -> true

    let rec private checkInvariantRec (parentReg : IRegion<'a> option) (Node d as tree) =
        let checkRec (reg, (_, subtree)) =
            checkInvariantRec (Some (reg :> IRegion<_>)) subtree
            && PersistentDict.forall (fun (reg', _) -> reg = reg' || reg.CompareTo reg' = Disjoint) d
        checkInvariantParent parentReg tree
        && PersistentDict.forall checkRec d

    let checkInvariant tree =
        if not <| checkInvariantRec None tree then
            internalfailf "The invariant of region tree is violated! Tree: {0}" tree
