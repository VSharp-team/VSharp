namespace VSharp.Core
open VSharp
open VSharp.Utils
open System.Collections.Generic

module public PC =
    
    type private node =
        | Tail of term * term pset
        | Node of term
        | Empty

    let private unwrapNode =
        function
        | Tail(constant, _) -> constant
        | Node(constant) -> constant
        | Empty -> invalidOp "Cannot unwrap empty node"
    
    (*
        Path condition maintains independent subsets of constants and constraints ("constraint independence")
        
        constants -- dictionary used as union-find structure for constants. Constants of one subset are
            cyclically mapping to each other. There is only one node.Tail in subset and it is the representative
            element of the subset. Tail also contains the constraints corresponding to the constants subset
            
        constraints -- contains all constraints of the PC. Used to avoid picking constraints from subsets each time
            when the complete PC is needed
            
        isFalse -- flag used to determine if the PC is false trivially (i. e. c an !c were added to it).
            Invariant: PC doesn't contain True or False as elements.
    *)
    type public PathCondition private(constants : Dictionary<term, node>, constraints : HashSet<term>, isFalse : bool) =
        
        let mutable isFalse = isFalse

        let nextNode constant =
            let mutable found = Empty
            constants.TryGetValue(constant, &found) |> ignore
            found
        
        /// <summary>
        /// Find operation of the union-find structure. Returns not the representative element, but the element before it
        /// (for convenience)
        /// </summary>
        let rec findPrevious constant =            
            match nextNode constant with
            | Tail _ -> Some(constant)
            | Node nextTerm -> findPrevious nextTerm
            | Empty -> None

        /// <summary>
        /// Union operation of the union-find structure. Subsets containing oneConstant and anotherConstant are merged.
        /// oneConstant and anotherConstant don't need to be the representatives
        /// </summary>
        let union oneConstant anotherConstant =
            match (findPrevious oneConstant), (findPrevious anotherConstant) with
            | Some(onePrevious), Some(anotherPrevious) ->
                match (nextNode onePrevious), (nextNode anotherPrevious) with
                | Tail(oneRepresentative, oneConstraints), Tail(anotherRepresentative, anotherConstraints) when
                    oneRepresentative <> anotherRepresentative ->
                        let constraintsUnion = PersistentSet.union oneConstraints anotherConstraints
                        constants.[onePrevious] <- Tail(anotherRepresentative, constraintsUnion)
                        constants.[anotherPrevious] <- Node(oneRepresentative)
                | _ -> ()
            | _ -> invalidOp "Constant not found in dictionary"

        /// <summary>
        /// Returns union-find subset (of constants) containing the specified constant
        /// </summary>
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
            isFalse <- true

        /// <summary>
        /// Adds a cyclic subset to the union-find structure
        /// </summary>
        /// <param name="constants">Union-find structure</param>
        /// <param name="constantsToAdd">Constants to add as nodes to the subset</param>
        /// <param name="constraintsToAdd">Constraints to add to the tail</param>
        let addSubset (constants : Dictionary<term, node>) constantsToAdd constraintsToAdd =
            let firstConstant = constantsToAdd |> Seq.head
            if Seq.length constantsToAdd = 1 then
                constants.[firstConstant] <- Tail(firstConstant, constraintsToAdd)
            else
                constantsToAdd 
                |> Seq.pairwise 
                |> Seq.iteri (fun i (previous, next) ->
                        if (i <> Seq.length constantsToAdd - 2) then
                            constants.[previous] <- Node(next)
                        else
                            constants.[previous] <- Tail(next, constraintsToAdd)
                            constants.[next] <- Node(firstConstant)
                    )
                
        let addConstraintsToSubset subsetConstant constraintsToAdd =
            match findPrevious subsetConstant with
            | Some(previous) ->
                match nextNode previous with
                | Tail(representative, constraints) ->
                        let constraintsUnion = PersistentSet.union constraints constraintsToAdd
                        constants.[previous] <- Tail(representative, constraintsUnion)
                | _ -> __unreachable__()
            | _ -> __unreachable__()

        let constSourcesIndependent =
            function
                | ConstantT(_, oneSrc, _), ConstantT(_, anotherSrc, _) -> oneSrc.IndependentWith anotherSrc
                | _ -> true

        let addNewConstraintWithMerge newConstraint = 
            let constraintConstants = discoverConstants [newConstraint]
            let oldConstants, newConstants = 
                constraintConstants
                    |> Seq.splitBy constants.ContainsKey
            
            // are there constraints without constants at all?
            // answer: yes, in ArrayConcreteUnsafeRead, is it ok?
            let newConstraintSet = PersistentSet.add PersistentSet.empty newConstraint
            if Seq.isEmpty newConstants |> not then
                addSubset constants newConstants newConstraintSet
            else if Seq.isEmpty oldConstants |> not then
                addConstraintsToSubset (Seq.head oldConstants) newConstraintSet
                
            constraints.Add(newConstraint) |> ignore

            Seq.allPairs newConstants constants.Keys
                |> Seq.filter (constSourcesIndependent >> not)
                |> Seq.iter (fun (oneConst, anotherConst) -> union oneConst anotherConst)

            match (Seq.tryHead oldConstants) with
            | Some(someOldConstant) -> 
                Seq.iter (union someOldConstant) oldConstants
                match (Seq.tryHead newConstants) with
                | Some(someNewConstant) -> union someNewConstant someOldConstant
                | _ -> ()
            | _ -> ()

        new() =
            PathCondition(Dictionary<term, node>(), HashSet<term>(), false)

        override this.ToString() =
            Seq.map toString constraints |> Seq.sort |> join " /\ "

        member this.Copy() =
            PathCondition(Dictionary(constants), HashSet(constraints), isFalse)

        member this.IsFalse = isFalse

        member this.IsEmpty = constraints.Count = 0

        member this.ToSeq() = seq constraints

        member this.Add newConstraint =
            match newConstraint with
            | True -> ()
            | False -> becomeTrivialFalse()
            | _ when isFalse -> ()
            | _ when constraints.Contains(newConstraint) -> ()
            // what if constraint is not equal to newConstraint structurally, but is equal logically?
            | _ when constraints.Contains(!!newConstraint) -> becomeTrivialFalse()
            | _ -> addNewConstraintWithMerge newConstraint

        member this.Map mapper =
            let mapped = PathCondition()
            Seq.iter (mapper >> mapped.Add) constraints
            mapped

        member this.UnionWith (anotherPc : PathCondition) =
            let union = this.Copy()
            anotherPc.ToSeq() |> Seq.iter union.Add
            union

        /// <summary>
        /// Returns the sequence of path conditions such that constants contained in
        /// one path condition are independent with constants contained in another one 
        /// </summary>
        member this.Fragments =
            if isFalse then
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

    let public add newConstraint (pc : PathCondition) =
        let copy = pc.Copy()
        copy.Add newConstraint
        copy

    let public toSeq (pc : PathCondition) = pc.ToSeq()
    
    let public map mapper (pc : PathCondition) = pc.Map mapper
    
    let public unionWith anotherPc (pc : PathCondition) = pc.UnionWith anotherPc
