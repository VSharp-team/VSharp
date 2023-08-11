namespace VSharp.Utils

open System
open System.Collections.Generic
open Checked

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

type public DiscretePDF<'a when 'a : equality>(comparer : IComparer<'a>, randomSeed : int option) =
    let less a b = comparer.Compare(a, b) = -1
    let mutable root = None : node<'a> option
    let mutable maxWeight = 0u
    let mutable count = 0u
    let random =
        match randomSeed with
        | Some seed -> Random(seed)
        | None -> Random()

    let mkNode (key : 'a) weight =
        { Left = None
          Right = None
          Key = key
          Height = 1u
          Weight = weight
          SumWeight = weight
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

    let rec maxWeightOf nOpt =
        option {
            let! n = nOpt
            let lMax = maxWeightOf n.Left
            let rMax = maxWeightOf n.Right
            return
                match lMax, rMax with
                | None, None -> n.Weight
                | Some l, None -> max n.Weight l
                | None, Some r-> max n.Weight r
                | Some l, Some r -> max n.Weight (max l r)
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

    let choose selector =
        let rec chooseInternal (nOpt : node<'a> option) (k : unit -> 'a option) =
            let selectSegment3 w1 w2 w3 on1 on2 on3 k =
                let weight = random.Next(0, w1 + w2 + w3)
                if weight < w1 then on1 k
                elif weight < w1 + w2 then on2 k
                else on3 k

            let selectSegment2 w1 w2 on1 on2 k =
                let weight = random.Next(0, w1 + w2)
                if weight < w1 then on1 (fun _ -> on2 k) else on2 (fun _ -> on1 k)

            let unwrapSumWeight (n : node<'a> option) =
                match n with
                | Some n -> int n.SumWeight
                | _ -> 0

            match nOpt with
            | None -> k()
            | Some n ->
                let goToLeft k = chooseInternal n.Left k
                let goToRight k = chooseInternal n.Right k
                let tryMiddle k = if selector n.Key then Some n.Key else k()
                let leftWeight = unwrapSumWeight n.Left
                let rightWeight = unwrapSumWeight n.Right
                let onLeft k = goToLeft (fun _ -> selectSegment2 (int n.Weight) rightWeight tryMiddle goToRight k)
                let onMiddle k = tryMiddle (fun _ -> selectSegment2 leftWeight rightWeight goToLeft goToRight k)
                let onRight k = goToRight (fun _ -> selectSegment2 leftWeight (int n.Weight) goToLeft tryMiddle k)
                selectSegment3 leftWeight (int n.Weight) rightWeight onLeft onMiddle onRight k
        chooseInternal root (always None)

    let rec enumerate optNode =
        seq {
            match optNode with
            | Some node ->
                yield node.Key
                yield! enumerate node.Left
                yield! enumerate node.Right
            | None -> ()
        }

    member x.Empty() = root = None

    member private x.Lookup(item : 'a) = lookup root item

    member x.Insert(item : 'a, weight) =
        root <- insert root item weight
        count <- count + 1u

    member x.Remove(item : 'a) =
        root <- remove root item
        option {
            let! w = maxWeightOf root
            maxWeight <- w
        } |> ignore
        count <- count - 1u

    member x.Contains(item : 'a) =
        lookup root item |> Option.isSome

    member x.TryGetWeight(item : 'a) =
        option {
            let! n = lookup root item
            return n.Weight
        }

    member x.MaxWeight = maxWeight

    member x.Update(item : 'a, weight) =
        let res = option {
            do! lookupAndUpdate root item (fun p act ->
            p.Weight <- weight
            act p)
            maxWeight <- max maxWeight weight
            return ()
        }
        assert (Option.isSome res)

    member x.Choose(selector) = choose selector

    member x.Clear() =
        root <- None
        maxWeight <- 0u
        count <- 0u

    member x.ToSeq = enumerate root
    member x.Count = count

    interface IPriorityCollection<'a> with
        override x.Insert item priority = x.Insert(item, priority)
        override x.Remove item = x.Remove(item)
        override x.Update item priority = x.Update(item, priority)
        override x.Choose() = x.Choose(always true)
        override x.Choose(selector) = x.Choose(selector)
        override x.Contains item = x.Contains(item)
        override x.TryGetPriority item = x.TryGetWeight(item)
        override x.Clear() = x.Clear()
        override x.Count = x.Count
        override x.ToSeq = x.ToSeq

module public DiscretePDF =
    let insert (dpdf: DiscretePDF<'a>) item weight = dpdf.Insert(item, weight)
    let remove (dpdf: DiscretePDF<'a>) item = dpdf.Remove(item)
    let update (dpdf: DiscretePDF<'a>) item weight = dpdf.Update(item, weight)
    let choose (dpdf: DiscretePDF<'a>) = (dpdf :> IPriorityCollection<'a>).Choose()
    let chooseWithSelector (dpdf: DiscretePDF<'a>) selector = (dpdf :> IPriorityCollection<'a>).Choose(selector)
    let contains (dpdf: DiscretePDF<'a>) item = dpdf.Contains(item)
    let tryGetWeight (dpdf: DiscretePDF<'a>) item = dpdf.TryGetWeight item
    let toSeq (dpdf: DiscretePDF<'a>) = dpdf.ToSeq
