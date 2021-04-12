namespace VSharp

type RegionComparisonResult =
    | Includes   // This region strictly includes other
    | Intersects // This region intersects with other, but not includes it
    | Disjoint

type IRegion<'a> =
    abstract IsEmpty : bool
    abstract CompareTo : 'a -> RegionComparisonResult
    abstract Subtract : 'a -> 'a
    abstract Intersect : 'a -> 'a

// -------------------- Intervals region: the union of open and closed intervals  --------------------
// -------------------------------- Example: [1, 2) U [3,3] U [9, 10] --------------------------------

type endpointSort = // TODO: need eneum? #do
    | OpenRight
    | ClosedLeft
    | ClosedRight
    | OpenLeft

[<StructuralEquality;StructuralComparison>]
type endpoint<'a when 'a : comparison> =
    { elem : 'a; sort : endpointSort }

module private Intervals =
    let private flip x =
        let sort =
            match x.sort with
                | ClosedLeft -> OpenRight
                | ClosedRight -> OpenLeft
                | OpenLeft -> ClosedRight
                | OpenRight -> ClosedLeft
        {x with sort = sort}

    let private isLeft = function
        | ClosedLeft
        | OpenLeft -> true
        | _ -> false

    let combine (p1 : 'a endpoint seq) (p2 : 'a endpoint seq) left1 right1 left2 right2 leftBoth rightBoth =
        let e1 = p1.GetEnumerator()
        let e2 = p2.GetEnumerator()
        let mutable atEnd1 = not <| e1.MoveNext()
        let mutable atEnd2 = not <| e2.MoveNext()
        let mutable c1 = 0
        let mutable c2 = 0
        seq {
            while not atEnd1 || not atEnd2 do
                let cmp = if atEnd1 then 1 elif atEnd2 then -1 else compare e1.Current e2.Current
                let res =
                    let x = if cmp <= 0 then e1.Current else e2.Current
                    if cmp <= 0 then
                        atEnd1 <- not <| e1.MoveNext()
                        if isLeft x.sort then c1 <- c1 + 1 else c1 <- c1 - 1
                    if cmp >= 0 then
                        atEnd2 <- not <| e2.MoveNext()
                        if isLeft x.sort then c2 <- c2 + 1 else c2 <- c2 - 1
                    if cmp = 0 then
                        if isLeft x.sort then leftBoth x (c1 > 1) (c2 > 1) else rightBoth x (c1 > 0) (c2 > 0)
                    elif cmp < 0 then
                        if isLeft x.sort then left1 x (c1 > 1) (c2 > 0) else right1 x (c1 > 0) (c2 > 0)
                    else
                        if isLeft x.sort then left2 x (c1 > 0) (c2 > 1) else right2 x (c1 > 0) (c2 > 0)
                match res with
                | Some a -> yield a
                | None -> ()
        } |> List.ofSeq

    let union (p1 : 'a endpoint seq) (p2 : 'a endpoint seq) =
        let visit x inside1 inside2 = if inside1 || inside2 then None else Some x
        combine p1 p2 visit visit visit visit visit visit

    let intersect (p1 : 'a endpoint seq) (p2 : 'a endpoint seq) =
        let visit1 x inside1 inside2 = if not inside1 && inside2 then Some x else None
        let visit2 x inside1 inside2 = if not inside2 && inside1 then Some x else None
        let visitBoth x inside1 inside2 = if not inside1 && not inside2 then Some x else None
        combine p1 p2 visit1 visit1 visit2 visit2 visitBoth visitBoth

    let subtract (p1 : 'a endpoint seq) (p2 : 'a endpoint seq) =
        let visitLeft1 x _ inside2 = if inside2 then None else Some x
        let visitRight1 x _ inside2 =  if inside2 then None else Some x
        let visitLeft2 x inside1 _ = if inside1 then Some (flip x) else None
        let visitRight2 x inside1 _ = if inside1 then Some (flip x) else None
        combine p1 p2 visitLeft1 visitRight1 visitLeft2 visitRight2 visitLeft2 visitRight2

    let compare (p1 : 'a endpoint seq) (p2 : 'a endpoint seq) =
        let mutable includes = true
        let mutable disjoint = true
        let visitLeft1 _ _ inside2 = disjoint <- disjoint && not inside2; None
        let visitRight1 _ _ inside2 = includes <- includes && not inside2; disjoint <- disjoint && not inside2; None
        let visitLeft2 _ inside1 _ = includes <- includes && inside1; disjoint <- disjoint && not inside1; None
        let visitRight2 _ inside1 _ = disjoint <- disjoint && not inside1; None
        let visitBoth _ _ _ = disjoint <- false; None
        combine p1 p2 visitLeft1 visitRight1 visitLeft2 visitRight2 visitBoth visitBoth |> ignore
        if includes then Includes
        elif disjoint then Disjoint
        else Intersects

[<StructuralEquality;NoComparison>]
type intervals<'a when 'a : comparison> =
    {points : 'a endpoint list}
    static member Closed x y = {points = [{elem = x; sort = ClosedLeft}; {elem = y; sort = ClosedRight}]}
    static member Singleton x = intervals<'a>.Closed x x
    member this.Map (mapper : 'a -> 'a) = {points = this.points |> List.map (fun e -> {e with elem = mapper e.elem}) }
    member this.Union other = { points = Intervals.union this.points other.points }
    interface IRegion<'a intervals> with
        override this.IsEmpty = Seq.isEmpty this.points
        override this.CompareTo other = Intervals.compare this.points other.points
        override this.Subtract other = { points = Intervals.subtract this.points other.points }
        override this.Intersect other = { points = Intervals.intersect this.points other.points }

// -------------------- Points region: the finite and co-finite sets of points  --------------------
// ------------------------------------ Example: Z \ {1, 2, 5} -------------------------------------

module private Points =
    let compare (p1 : pset<'a>) (p2 : pset<'a>) =
        let mutable includes = true
        let mutable disjoint = true
        for x in PersistentSet.toSeq p2 do
            if PersistentSet.contains x p1 then disjoint <- false else includes <- false
        includes, disjoint

    let flip (x, y) = (not x, not y)

    let toResult (includes, disjoint) =
        if includes then Includes
        elif disjoint then Disjoint
        else Intersects

type points<'a when 'a : equality> =
    {points : 'a pset; thrown : bool}
    static member Singleton x = {points = PersistentSet.add PersistentSet.empty<'a> x; thrown = false}
    static member Universe = {points = PersistentSet.empty<'a>; thrown = true}
    member this.Map (mapper : 'a -> 'a) = {this with points = this.points |> PersistentSet.map mapper }
    interface IRegion<'a points> with
        override this.IsEmpty = not this.thrown && PersistentSet.isEmpty this.points
        override this.CompareTo other =
            match this.thrown, other.thrown with
            | false, false -> Points.compare this.points other.points |> Points.toResult
            | true, false -> Points.compare this.points other.points |> Points.flip |> Points.toResult
            | false, true -> if Points.compare other.points this.points |> fst then Disjoint else Intersects
            | true, true -> if Points.compare other.points this.points |> fst then Includes else Intersects
        override this.Subtract other =
            match this.thrown, other.thrown with
            | false, false -> {points = PersistentSet.subtract this.points other.points; thrown = false}
            | true, false -> {points = PersistentSet.union this.points other.points; thrown = true}
            | false, true -> {points = PersistentSet.intersect other.points this.points; thrown = false}
            | true, true -> {points = PersistentSet.subtract other.points this.points; thrown = false}
        override this.Intersect other =
            match this.thrown, other.thrown with
            | false, false -> {points = PersistentSet.intersect this.points other.points; thrown = false}
            | true, false -> {points = PersistentSet.subtract other.points this.points; thrown = false}
            | false, true -> {points = PersistentSet.subtract this.points other.points; thrown = false}
            | true, true -> {points = PersistentSet.union other.points this.points; thrown = true}
    override x.ToString() = x.points |> PersistentSet.toSeq |> Seq.map toString |> join ", " |> sprintf (if x.thrown then "Z \ {%s}" else "{%s}")

// -------------------- Cartesian product of regions  --------------------

module private CartesianRegions =
    let isEmpty ((a, b) : ('a * 'b) when 'a :> IRegion<'a> and 'b :> IRegion<'b>) = a.IsEmpty || b.IsEmpty

    let intersect (r1 : ('a * 'b) seq when 'a :> IRegion<'a> and 'b :> IRegion<'b>) (r2 : ('a * 'b) seq) =
        seq {
            for (a, b) in r1 do
                for ((c, d) as rect) in r2 do
                    match a.CompareTo c, b.CompareTo d with
                    | Disjoint, _
                    | _, Disjoint -> ()
                    | Includes, Includes -> yield rect
                    | _ -> yield (a.Intersect c, b.Intersect d)
        } |> List.ofSeq

    let subtractRect (r : ('a * 'b) list when 'a :> IRegion<'a> and 'b :> IRegion<'b>) ((c, d) as rect2 : ('a * 'b)) =
        if isEmpty rect2 then (r, true)
        else
            let mutable unchanged = true
            seq {
                for ((a, b) as rect1) in r do
                    match c.CompareTo a, d.CompareTo b with
                    | Disjoint, _
                    | _, Disjoint -> yield rect1
                    | c1, c2 ->
                        unchanged <- false;
                        if c1 <> Includes then yield (a.Subtract c, b)
                        if c2 <> Includes then yield (a, b.Subtract d)
            } |> Seq.filter (not << isEmpty) |> List.ofSeq, unchanged

    let subtract (r1 : ('a * 'b) list when 'a :> IRegion<'a> and 'b :> IRegion<'b>) (r2 : ('a * 'b) list) =
        List.fold (fun (r, c) rect -> let r', c' = subtractRect r rect in if c' then r, c else r', false) (r1, true) r2

    let compare (r1 : ('a * 'b) list when 'a :> IRegion<'a> and 'b :> IRegion<'b>) (r2 : ('a * 'b) list) =
        let diff, unchanged = subtract r2 r1
        if unchanged then Disjoint
        elif List.isEmpty diff then Includes
        else Intersects

[<StructuralEquality;NoComparison>]
type productRegion<'a, 'b when 'a :> IRegion<'a> and 'b :> IRegion<'b>> =
    {products : ('a * 'b) list}
    static member ProductOf x y = {products = List.singleton (x, y)}
    member this.Map (mapper1 : 'a -> 'a) (mapper2 : 'b -> 'b) = {products = List.map (fun (a, b) -> mapper1 a, mapper2 b) this.products}
    interface IRegion<productRegion<'a, 'b>> with
        override this.IsEmpty = Seq.forall CartesianRegions.isEmpty this.products
        override this.CompareTo other = CartesianRegions.compare this.products other.products
        override this.Subtract other = { products = CartesianRegions.subtract this.products other.products |> fst }
        override this.Intersect other = { products = CartesianRegions.intersect this.products other.products }

[<StructuralEquality;NoComparison>]
type listProductRegion<'a when 'a :> IRegion<'a>> =
    | NilRegion
    | ConsRegion of productRegion<'a, listProductRegion<'a>>
    static member OfSeq xs =
        if Seq.isEmpty xs then NilRegion
        else
            let reg : 'a = Seq.head xs
            productRegion<'a, listProductRegion<'a>>.ProductOf reg (listProductRegion<'a>.OfSeq (Seq.tail xs)) |> ConsRegion
    static member private LengthMismatchException() = internalfail "Lengths of list product regions mismatch!"
    member this.Map (mapper : 'a -> 'a) =
        match this with
        | NilRegion -> NilRegion
        | ConsRegion p -> p.Map mapper (fun x -> x.Map mapper) |> ConsRegion
    interface IRegion<listProductRegion<'a>> with
        override this.IsEmpty =
            match this with
            | NilRegion -> false
            | ConsRegion p -> (p :> IRegion<productRegion<'a, listProductRegion<'a>>>).IsEmpty
        override this.CompareTo other =
            match this, other with
            | NilRegion, NilRegion -> Includes
            | ConsRegion p1, ConsRegion p2 -> (p1 :> IRegion<productRegion<'a, listProductRegion<'a>>>).CompareTo p2
            | _ -> listProductRegion<'a>.LengthMismatchException()
        override this.Subtract other =
            match this, other with
            | NilRegion, NilRegion -> this
            | ConsRegion p1, ConsRegion p2 -> (p1 :> IRegion<productRegion<'a, listProductRegion<'a>>>).Subtract p2 |> ConsRegion
            | _ -> listProductRegion<'a>.LengthMismatchException()
        override this.Intersect other =
            match this, other with
            | NilRegion, NilRegion -> this
            | ConsRegion p1, ConsRegion p2 -> (p1 :> IRegion<productRegion<'a, listProductRegion<'a>>>).Intersect p2 |> ConsRegion
            | _ -> listProductRegion<'a>.LengthMismatchException()

// -------------------- Free region --------------------

type IAtomicRegion<'a> =
    // Returns None if this does not intersect to argument or Some reg if reg is the intersection of this and argument.
    abstract Intersect : 'a -> 'a option

// Region built from atomic regions. Intuitively, atomic regions are never empty and cannot be covered by a finite set
// of strictly included regions.
// Invariants:
// FreeUnion xs: all x in xs are non-empty and disjoint; xs are all FreeDifferences
// FreeDifference(x, y): x strictly includes y
type freeRegion<'a when 'a :> IAtomicRegion<'a> and 'a : equality> =
    | FreeUnion of freeRegion<'a> list
    | FreeDifference of 'a * freeRegion<'a>
    static member Singleton reg = FreeDifference(reg, FreeUnion [])
    static member Empty = FreeUnion []
    member private this.Union other =
        match this, other with
        | _ when (this :> IRegion<freeRegion<'a>>).IsEmpty -> other
        | _ when (other :> IRegion<freeRegion<'a>>).IsEmpty -> this
        | FreeUnion xs, _ ->
            xs |> List.fold (fun acc x -> x.Union acc) other
        | _, FreeUnion xs ->
            xs |> List.fold (fun acc x -> x.Union acc) this
        | FreeDifference(x, y), FreeDifference(x', y') ->
            let z = x.Intersect x'
            match z with
            | None -> FreeUnion [this; other]
            | Some z when z = x' -> FreeDifference(x, (y' :> IRegion<freeRegion<'a>>).Subtract this)
            | Some z when z = x -> FreeDifference(x', (y :> IRegion<freeRegion<'a>>).Subtract other)
            | Some z ->
                let z = freeRegion<'a>.Singleton z
                FreeUnion [FreeDifference(x, y.Union z); FreeDifference(x', y'.Union z)]
    member this.Map (mapper : 'a -> 'a) =
        match this with
        | FreeUnion xs -> xs |> List.fold (fun (acc : freeRegion<'a>) x -> acc.Union (x.Map mapper)) freeRegion<'a>.Empty
        | FreeDifference(x, y) -> (freeRegion<'a>.Singleton(mapper x) :> IRegion<freeRegion<'a>>).Subtract(y.Map mapper)
    interface IRegion<freeRegion<'a>> with
        override this.IsEmpty =
            match this with
            | FreeUnion xs -> List.isEmpty xs
            | FreeDifference _ ->  false // Never empty because x strictly includes y
        override this.CompareTo other =
            // Do NOT change the order of pattern-matching clauses!
            match this, other with
            | FreeDifference(x, y), FreeDifference(x', y') ->
                let z = x.Intersect x'
                match z with
                | None -> Disjoint
                | Some z when z = x' ->
                    match (y :> IRegion<freeRegion<'a>>).CompareTo other with
                    | Includes -> Disjoint
                    | Disjoint -> Includes
                    | _ -> Intersects
                | Some z ->
                    match (y :> IRegion<freeRegion<'a>>).CompareTo ((freeRegion<'a>.Singleton z :> IRegion<freeRegion<'a>>).Subtract y') with
                    | Includes -> Disjoint
                    | _ -> Intersects
            | _, FreeUnion xs ->
                let cmps = xs |> List.map (this :> IRegion<freeRegion<'a>>).CompareTo
                if List.forall ((=) Includes) cmps then Includes
                elif List.forall ((=) Disjoint) cmps then Disjoint
                else Intersects
            | FreeUnion xs, FreeDifference _ ->
                let cmps = xs |> List.map (other :> IRegion<freeRegion<'a>>).CompareTo
                if List.exists ((=) Includes) cmps then Includes
                elif List.forall ((=) Disjoint) cmps then Disjoint
                else Intersects
        override this.Subtract other =
            match this, other with
            | FreeUnion xs, _ ->
                xs |> List.map (fun x -> (x :> IRegion<freeRegion<'a>>).Subtract other) |> List.filter (fun x -> not (x :> IRegion<freeRegion<'a>>).IsEmpty) |> FreeUnion
            | FreeDifference(x, y), FreeDifference(x', y') ->
                let z = x.Intersect x'
                match z with
                | None -> this
                | Some z when z = x -> (y' :> IRegion<freeRegion<'a>>).Subtract y
                | Some z -> FreeDifference(x, y.Union ((freeRegion<'a>.Singleton z :> IRegion<freeRegion<'a>>).Subtract y'))
            | FreeDifference _, FreeUnion zs ->
                zs |> List.fold (fun acc z -> (acc :> IRegion<freeRegion<'a>>).Subtract z) this
        override this.Intersect other =
            match this, other with
            | FreeDifference(x, y), FreeDifference(x', y') ->
                let z = x.Intersect x'
                match z with
                | None -> freeRegion<'a>.Empty
                | Some z when z = x' ->
                    match (y :> IRegion<freeRegion<'a>>).CompareTo other with
                    | Includes -> freeRegion<'a>.Empty
                    | Disjoint -> other
                    | Intersects -> FreeDifference(x', y.Union y')
                | Some z -> (other :> IRegion<freeRegion<'a>>).Intersect ((freeRegion<'a>.Singleton z :> IRegion<freeRegion<'a>>).Subtract y)
            | FreeUnion xs, _ ->
                xs |> List.fold (fun acc x -> (acc :> IRegion<freeRegion<'a>>).Intersect x) other
            | _, FreeUnion xs ->
                xs |> List.fold (fun acc x -> (acc :> IRegion<freeRegion<'a>>).Intersect x) this
