namespace VSharp.Core
open VSharp
open VSharp.Utils
open System.Collections.Generic

module internal PC2 =

    type private node =
        | Tail of term * term pset
        | Node of term
        | Empty
    
    type PathCondition() =
        
        let constants = Dictionary<term, node>() 
        let constraints = HashSet<term>()
        let mutable isTrivialFalse = false

        let rec findPrevious term =
            let mutable found = Empty
            if constants.TryGetValue(term, &found)
            then
                match found with
                | Tail(representative, constraints) -> Some(term)
                | Node nextTerm -> findPrevious nextTerm
                | Empty -> __unreachable__()
            else
                None

        let rec find term = 
            let mutable found = Empty
            if constants.TryGetValue(term, &found)
            then
                match found with
                | Tail(representative, constraints) -> Some(representative, constraints)
                | Node nextTerm -> find nextTerm
                | Empty -> __unreachable__()
            else
                None

        let union oneConstant anotherConstant =
            

        let becomeTrivialFalse() =
            constants.Clear()
            constraints.Clear()
            isTrivialFalse <- true

        let addSubset constantsToAdd constraintsToAdd =
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

        let addNewConstraintWithMerge newConstraint = 
            let constraintConstants = discoverConstants [newConstraint]
            let newConstants = constraintConstants |> Seq.filter (fun c -> constants.ContainsKey(c) |> not)

            addSubset newConstants (PersistentSet.add PersistentSet.empty newConstraint)
            constraints.Add(newConstraint)


        member this.IsTrivialFalse = isTrivialFalse

        member this.Add newConstraint =
            match newConstraint with
            | True -> ()
            | False -> becomeTrivialFalse()
            | _ when isTrivialFalse -> ()
            | _ when constraints.Contains(newConstraint) -> ()
            | _ when constraints.Contains(!!newConstraint) -> becomeTrivialFalse()
            | _ -> addWithMerge pc cond
            