namespace VSharp.Core
open VSharp
open VSharp.Utils
open VSharp.Utils.PersistentUnionFind

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
        let isFalsePC = PersistentDict.contains False pc.conditionsWithConstants
        if isFalsePC then assert(toSeq pc |> Seq.length = 1)
        isFalsePC
            
    let private someSetElement = PersistentSet.toSeq >> Seq.tryHead
    
    let public add pc cond : pathCondition =
        match cond with
        | True -> pc
        | False -> falsePC
        | _ when isFalse pc -> falsePC
        | _ when PersistentDict.contains !!cond pc.conditionsWithConstants -> falsePC
        | _ ->
            let condConsts = discoverConstants [cond] |> PersistentSet.ofSeq
            let pufWithNewConsts =
                condConsts
                |> PersistentSet.filter (fun t -> None = PersistentUnionFind.tryFind pc.constants t)
                |> PersistentSet.fold PersistentUnionFind.add pc.constants
            let constsWithSources =
                Seq.map
                    (function
                    | ConstantT(_, src, _) as constant -> constant, src
                    | _ -> __unreachable__()
                    )
            let pufMergedByConstantSource =
                Seq.allPairs
                    (condConsts |> PersistentSet.toSeq |> constsWithSources)
                    (pc.constants |> PersistentUnionFind.toSeq |> constsWithSources)
                |> Seq.filter (fun ((_, src1), (_, src2)) -> not <| src1.IndependentWith src2)
                |> Seq.fold (fun puf ((const1, _), (const2, _)) -> PersistentUnionFind.union const1 const2 puf) pufWithNewConsts
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
            
    let public mapPC mapper (pc : pathCondition) : pathCondition =
        let mapAndAdd acc cond k =
            let acc' = mapper cond |> add acc
            if isFalse acc' then falsePC else k acc'
        Cps.Seq.foldlk mapAndAdd empty (toSeq pc) id
    let public mapSeq mapper (pc : pathCondition) = toSeq pc |> Seq.map mapper

    let toString pc = mapSeq toString pc |> Seq.sort |> join " /\ "

    let public union (pc1 : pathCondition) (pc2 : pathCondition) = Seq.fold add pc1 (toSeq pc2)
    
(*    let public fragments pc =
        let groups = PersistentDict.fold (fun groups cond constants ->
            let parent =
                constants
                |> someSetElement
                |> function
                    | Some(someConst) -> PersistentUnionFind.tryFind pc.constants someConst
                    | None -> None
            let updatedGroup =
                PersistentDict.tryFind groups parent
                |> function
                    | Some(condList) -> cond :: condList
                    | None -> [cond]
            PersistentDict.add parent updatedGroup groups
            ) PersistentDict.empty pc.conditionsWithConstants
        groups
        |> PersistentDict.map (fun _ conds ->
            let 
            )*)