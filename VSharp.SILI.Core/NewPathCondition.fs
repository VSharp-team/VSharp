namespace VSharp.Core
open VSharp
open VSharp.Utils
open System.Collections.Generic

module internal PC2 =

    type private node =
        | Tail of term * term pset
        | Node of term
        | Empty

    let private unwrapNode =
        function
        | Tail(term, _) -> term
        | Node(term) -> term
        | Empty -> invalidOp "Cannot unwrap empty node"
    
    type PathCondition private(
        constants : Dictionary<term, node>,
        constraints : HashSet<term>,
        isTrivialFalse : bool
    ) =
        let mutable isTrivialFalse = isTrivialFalse

        let nextNode term =
            let mutable found = Empty
            constants.TryGetValue(term, &found) |> ignore
            found
        
        let rec findPrevious term =            
            match nextNode term with
            | Tail(_, _) -> Some(term)
            | Node nextTerm -> findPrevious nextTerm
            | Empty -> None

        let rec find term = 
            match nextNode term with
            | Tail(representative, constraints) -> Some(representative, constraints)
            | Node nextTerm -> find nextTerm
            | Empty -> None

        let union oneConstant anotherConstant =
            match (findPrevious oneConstant), (findPrevious anotherConstant) with
            | Some(onePrevious), Some(anotherPrevious) ->
                match (nextNode onePrevious), (nextNode anotherPrevious) with
                | Tail(oneRepresentative, oneConstraints), Tail(anotherRepresentative, anotherConstraints) when
                    oneRepresentative <> anotherRepresentative ->
                        let constraintsUnion = PersistentSet.union oneConstraints anotherConstraints
                        constants.[onePrevious] <- Tail(anotherRepresentative, constraintsUnion)
                        constants.[anotherPrevious] <- Node(oneRepresentative)
                | _ -> __unreachable__()
            | _ -> invalidOp "Constant not found in dictionary"

        let subset constantInSubset =
            seq {
                let rec inner currentConstant =
                    seq {
                        if currentConstant <> constantInSubset then
                            yield currentConstant
                            match nextNode currentConstant with
                            | Empty -> __unreachable__()
                            | node -> yield! inner (unwrapNode node)
                    }
                match nextNode constantInSubset with
                | Empty -> invalidOp "Constant not found in dictionary"
                | node ->
                    yield constantInSubset
                    yield! inner (unwrapNode node)
            }

        let becomeTrivialFalse() =
            constants.Clear()
            constraints.Clear()
            isTrivialFalse <- true

        let addSubset (constants : Dictionary<term, node>) constantsToAdd constraintsToAdd =
            let firstConstant = constantsToAdd |> Seq.head
            constantsToAdd 
            |> Seq.pairwise 
            |> Seq.iteri (fun i (previous, next) ->
                    if (i <> Seq.length constantsToAdd - 2) then
                        constants.[previous] <- Node(next)
                    else
                        constants.[previous] <- Tail(next, constraintsToAdd)
                        constants.[next] <- Node(firstConstant)
                )

        let constSourcesIndependent =
            function
                | ConstantT(_, oneSrc, _), ConstantT(_, anotherSrc, _) -> oneSrc.IndependentWith anotherSrc
                | _ -> true

        let addNewConstraintWithMerge newConstraint = 
            let constraintConstants = discoverConstants [newConstraint]
            let (oldConstants, newConstants) = 
                constraintConstants
                    |> Seq.splitBy (fun c -> constants.ContainsKey(c))

            addSubset constants newConstants (PersistentSet.add PersistentSet.empty newConstraint)
            constraints.Add(newConstraint) |> ignore

            Seq.allPairs newConstants constants.Keys
                |> Seq.filter (constSourcesIndependent >> not)
                |> Seq.iter (fun (oneConst, anotherConst) -> union oneConst anotherConst)

            match (Seq.tryHead oldConstants) with
            | Some(someOldConstant) -> 
                Seq.iter (fun constant -> union someOldConstant constant) oldConstants
                match (Seq.tryHead newConstants) with
                | Some(someNewConstant) -> union someNewConstant someOldConstant
                | _ -> ()
            | _ -> ()

        let copy() = 
            PathCondition(Dictionary(constants), HashSet(constraints), isTrivialFalse)

        new() =
            PathCondition(Dictionary<term, node>(), HashSet<term>(), false)

        override this.ToString() =
            Seq.map toString constraints |> Seq.sort |> join " /\ "

        member this.IsTrivialFalse = isTrivialFalse

        member this.IsEmpty = constraints.Count = 0

        member this.ToSeq() = seq constraints

        member this.Add newConstraint =
            match newConstraint with
            | True -> ()
            | False -> becomeTrivialFalse()
            | _ when isTrivialFalse -> ()
            | _ when constraints.Contains(newConstraint) -> ()
            // what if constraint is not equal to newConstraint structurally, but is equal logically?
            | _ when constraints.Contains(!!newConstraint) -> becomeTrivialFalse()
            | _ -> addNewConstraintWithMerge newConstraint

        member this.Map mapper =
            let mapped = PathCondition()
            Seq.iter (mapper >> mapped.Add) constraints
            mapped

        member this.UnionWith (anotherPc : PathCondition) =
            let union = copy()
            anotherPc.ToSeq() |> Seq.iter union.Add
            union

        member this.Fragments =
            if isTrivialFalse then
                Seq.singleton this
            else
                let getSubsetByRepresentative =
                    function
                    | Tail(representative, constraints) ->
                        let constants = Dictionary<term, node>()
                        addSubset constants (subset  representative) constraints
                        let constraints = HashSet(PersistentSet.toSeq constraints)
                        Some(PathCondition(constants, constraints, false))
                    | _ -> None
                Seq.choose getSubsetByRepresentative constants.Values
            