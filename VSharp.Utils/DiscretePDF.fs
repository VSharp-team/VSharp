namespace VSharp.Utils

open System.Collections.Generic

open VSharp

[<ReferenceEquality>]
type private node<'a when 'a : equality> =
    { mutable Left : node<'a> option
      mutable Right : node<'a> option
      mutable Key : 'a
      mutable Height : uint
      mutable Weight : uint
      mutable SumWeight : uint
    }

    member x.SetSum() =
        let leftWeight = x.Left |> Option.map (fun n -> n.SumWeight) |> Option.defaultValue 0u
        let rightWeight = x.Right |> Option.map (fun n -> n.SumWeight) |> Option.defaultValue 0u
        x.SumWeight <- x.Weight + leftWeight + rightWeight

type public DiscretePDF<'a when 'a : equality>(comparer : IComparer<'a>) =
    let less a b = comparer.Compare(a, b) = -1
    let mutable root = None : node<'a> option
    let mutable maxWeight = 0u

    let mkNode (key : 'a) weight =
        { Left = None
          Right = None
          Key = key
          Height = 1u
          Weight = weight
          SumWeight = 0u
        }

    let height nOpt =
        match nOpt with
        | Some n -> n.Height
        | None -> 0u

    let sumWeight nOpt =
        match nOpt with
        | Some n -> n.SumWeight
        | None -> 0u
 
    let update n =
        n.Height <- max (height n.Left) (height n.Right) + 1u
        n.SumWeight <- n.Weight + sumWeight n.Left + sumWeight n.Right

    let heightBalance nOpt =
        match nOpt with
        | Some n -> int (height n.Left) - int (height n.Right)
        | None -> 0

    let rec lookup' (nOpt : node<'a> option) item action k =
        option {
            let! n = nOpt
            let action' t =
                action t
                action n
            if less item n.Key then
                return! lookup' n.Left item action' k
            elif less n.Key item then
                return! lookup' n.Right item action' k
            else
                return k n action
        }

    let lookupAndUpdate nOpt item k =
        lookup' nOpt item update k

    let rec lookup (nOpt : node<'a> option) item =
        option {
            let! n = nOpt
            if less item n.Key then
                return! lookup n.Left item
            elif less n.Key item then
                return! lookup n.Right item
            else
                return n
        }

    let leftRotate (x : node<'a>) =
        option {
            let! y = x.Right
            let t = y.Left
            y.Left <- Some x
            x.Right <- t
            update x
            update y
            return y
        }

    let rightRotate (y : node<'a>) =
        option {
            let! x = y.Left
            let t = x.Right
            x.Right <- Some y
            y.Left <- t
            update y
            update x
            return x
        }

    let leftRightRotate (x : node<'a>) =
        option {
            let! left = x.Left
            x.Left <- leftRotate left
            return! rightRotate x
        }

    let rightLeftRotate (y : node<'a>) =
        option {
            let! right = y.Right
            y.Right <- rightRotate right
            return! leftRotate y
        }

    let balance n =
        let hBalance = heightBalance <| Some n
        let hLeftBalance = heightBalance n.Left
        let hRightBalance = heightBalance n.Right
        match () with
        | _ when hBalance > 1 && hLeftBalance >= 0 ->
            rightRotate n
        | _ when hBalance > 1 && hLeftBalance < 0 ->
            leftRightRotate n
        | _ when hBalance < -1 && hRightBalance <= 0 ->
            leftRotate n
        | _ when hBalance < -1 && hRightBalance > 0 ->
            rightLeftRotate n
        | _ -> Some n


    let rec insert nOpt item weight =
        maxWeight <- max maxWeight weight
        match nOpt with
        | Some n when n.Key = item -> nOpt
        | Some n ->
            if less item n.Key then
                n.Left <- insert n.Left item weight
            elif less n.Key item then
                n.Right <- insert n.Right item weight
            update n
            balance n
        | None -> Some <| mkNode item weight

    let rec minValueNode nOpt =
        option {
            let! n = nOpt
            let min = minValueNode n.Left
            return min |?? n
        }

    let rec maxValueNode nOpt =
        option {
            let! n = nOpt
            let max = maxValueNode n.Right
            return max |?? n
        }

    let rec remove nOpt item =
        let mutable root = nOpt
        option {
            let! n = root
            if less item n.Key then
                n.Left <- remove n.Left item
            elif less n.Key item then
                n.Right <- remove n.Right item
            else
                match n.Left, n.Right with
                | Some tmp, None
                | None, Some tmp -> root <- Some tmp
                | None, None -> root <- None
                | _ ->
                    let! tmp = minValueNode n.Right
                    n.Key <- tmp.Key
                    n.Weight <- tmp.Weight
                    n.Right <- remove n.Right tmp.Key
        } |> ignore
        match root with
        | Some n -> balance n
        | None -> None

    let rec choose (nOpt : node<'a> option) weight =
        option {
            let! n = nOpt
            match n.Left with
            | Some left when weight < left.SumWeight ->
                return! choose n.Left weight
            | Some left when weight >= left.SumWeight && weight - left.SumWeight >= n.Weight ->
                return! choose n.Right (weight - left.SumWeight - n.Weight)
            | None when weight >= n.Weight ->
                return! choose n.Right (weight - n.Weight)
            | _ -> return n.Key
        }

    member x.Empty() = root = None

    member private x.Lookup(item : 'a) = lookup root item

    member x.Insert(item : 'a, weight) =
        root <- insert root item weight

    member x.Remove(item : 'a) =
        root <- remove root item
        option {
            let! n = maxValueNode root
            maxWeight <- n.Weight
        } |> ignore

    member x.Contains(item : 'a) =
        lookup root item |> Option.isSome

    member x.GetWeight(item : 'a) =
        option {
            let! n = lookup root item
            return n.Weight
        }

    member x.MaxWeight() = maxWeight

    member x.Update(item : 'a, weight) =
        option {
            do! lookupAndUpdate root item (fun p act ->
            p.Weight <- weight
            act p)
            maxWeight <- max maxWeight weight
        } |> ignore

    member x.Choose(weight) =
        let wOpt = if weight >= maxWeight then None else Some weight
        option {
            let! r = root
            let! w = wOpt
            let scaledW =
                (double w / double maxWeight) * double r.SumWeight
             |> round
             |> uint
            return! choose root scaledW
        }

module public DiscretePDF =
    let insert (dpdf: DiscretePDF<'a>) item weight = dpdf.Insert(item, weight)
    let remove (dpdf: DiscretePDF<'a>) item = dpdf.Remove(item)
    let update (dpdf: DiscretePDF<'a>) item weight = dpdf.Update(item, weight)
    let choose (dpdf: DiscretePDF<'a>) weight = dpdf.Choose weight
    let maxWeight (dpdf: DiscretePDF<'a>) = dpdf.MaxWeight()
    let contains (dpdf: DiscretePDF<'a>) item = dpdf.Contains(item)
