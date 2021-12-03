namespace VSharp.Core
open VSharp
open VSharp.Utils

(*
    Path condition is represented as a union-find of sets disjoint by independence of
    constants in them and a dictionary where a condition is mapped to some constant
    contained in it or None if the condition doesn't contain any constants
*)
type pathCondition =
    private {constants : pUnionFind<term>; conditionsWithConstants : pdict<term, term option>}

// Invariants:
// - PC does not contain True
// - if PC contains False then False is the only element in PC

module internal PC =

    let public empty =
        {constants = PersistentUnionFind.empty
         conditionsWithConstants = PersistentDict.empty}
        
    let public isEmpty pc = PersistentDict.isEmpty pc.conditionsWithConstants

    let public toSeq pc = PersistentDict.keys pc.conditionsWithConstants

    let private falsePC =
        {constants = PersistentUnionFind.empty
         conditionsWithConstants = PersistentDict.add False None PersistentDict.empty}
       
    let public isFalse pc =
        let isFalsePC = pc.conditionsWithConstants |> PersistentDict.keys |> Seq.contains False 
        if isFalsePC then assert(toSeq pc |> Seq.length = 1)
        isFalsePC
            
    let private someSetElement = PersistentSet.toSeq >> Seq.tryHead
    
    let private constSourcesIndependent =
        function
            | ConstantT(_, oneSrc, _), ConstantT(_, anotherSrc, _) -> oneSrc.IndependentWith anotherSrc
            | _ -> true
    
    let private addWithMerge pc cond : pathCondition =
        let condConsts = discoverConstants [cond] |> PersistentSet.ofSeq
        
        let pufWithNewConsts =
            condConsts
            |> PersistentSet.filter (fun t -> None = PersistentUnionFind.tryFind t pc.constants)
            |> PersistentSet.fold PersistentUnionFind.add pc.constants
            
        // Merge sets of constants dependent in terms of ISymbolicConstantSource 
        let pufMergedByConstantSource =
            Seq.allPairs
                (condConsts |> PersistentSet.toSeq)
                (pc.constants |> PersistentUnionFind.toSeq)
            |> Seq.filter (constSourcesIndependent >> not)
            |> Seq.fold (fun puf (const1, const2) -> PersistentUnionFind.union const1 const2 puf) pufWithNewConsts
            
        (*
            Merge sets of constants dependent in terms of reachability in graph where
            edge between constants means that they are contained in the same condition
        *) 
        let pufMergedByDependentCondition =
            condConsts
            |> someSetElement
            |> function
                | Some(parent) ->
                    PersistentSet.fold (fun puf t -> PersistentUnionFind.union t parent puf) pufMergedByConstantSource condConsts
                | None -> pufMergedByConstantSource
                
        {constants = pufMergedByDependentCondition
         conditionsWithConstants =
             condConsts
             |> someSetElement
             |> (fun head -> PersistentDict.add cond head pc.conditionsWithConstants)}
    
    let public add pc cond : pathCondition =
        match cond with
        | True -> pc
        | False -> falsePC
        | _ when isFalse pc -> falsePC
        | _ when PersistentDict.contains !!cond pc.conditionsWithConstants -> falsePC
        | _ -> addWithMerge pc cond            
            
    let public mapPC mapper (pc : pathCondition) : pathCondition =
        let mapAndAdd acc cond k =
            let acc' = mapper cond |> add acc
            if isFalse acc' then falsePC else k acc'
        Cps.Seq.foldlk mapAndAdd empty (toSeq pc) id
    let public mapSeq mapper (pc : pathCondition) = toSeq pc |> Seq.map mapper

    let toString pc = mapSeq toString pc |> Seq.sort |> join " /\ "

    let public union (pc1 : pathCondition) (pc2 : pathCondition) = Seq.fold add pc1 (toSeq pc2)
    
    /// <summary>
    /// Returns the sequence of path conditions such that constants contained in
    /// one path condition are independent with constants contained in another one 
    /// </summary>
    let public fragments pc =
        let groupConditionsByUnionFindParent groups cond constant =
            let parent = constant |> Option.bind (fun constant -> PersistentUnionFind.tryFind constant pc.constants)
            let updatedGroup =
                PersistentDict.tryFind groups parent
                |> function
                    | Some(condSet) -> PersistentSet.add condSet cond
                    | None -> PersistentSet.add PersistentSet.empty cond
            PersistentDict.add parent updatedGroup groups
        
        let conditionsGroupToPathCondition (parent, conds) =
            let fragmentConstants =
                match parent with
                | Some(parent) -> PersistentUnionFind.subset parent pc.constants
                | None -> PersistentUnionFind.empty
            let fragmentConditionsWithConstants =
                PersistentSet.fold (fun dict cond -> PersistentDict.add cond parent dict) PersistentDict.empty conds
            {constants = fragmentConstants; conditionsWithConstants = fragmentConditionsWithConstants}
        
        pc.conditionsWithConstants
        |> PersistentDict.fold groupConditionsByUnionFindParent PersistentDict.empty
        |> PersistentDict.toSeq
        |> Seq.map conditionsGroupToPathCondition
