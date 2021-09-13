﻿module VSharp.SILI.Core.PathConditionIndependent

open VSharp
open VSharp.Core
open VSharp.Utils.PersistentUnionFind

type pathConditionIndependent =
    private {constants : pUnionFind<term>; constraintsWithConstants : pdict<term, term option>}

// Invariants:
// - PCI does not contain True
// - if PCI contains False then False is the only element in PCI

module internal PCI =

    let public empty =
        {constants = PersistentUnionFind.empty
         constraintsWithConstants = PersistentDict.empty}
        
    let public isEmpty pc = PersistentDict.isEmpty pc.constraintsWithConstants

    let public toSeq pc = PersistentDict.keys pc.constraintsWithConstants

    let private falsePC =
        {constants = PersistentUnionFind.empty
         constraintsWithConstants = PersistentDict.add False None PersistentDict.empty}
        
    let public isFalse pc =
        let isFalsePC = PersistentDict.contains False pc.constraintsWithConstants
        if isFalsePC then assert(toSeq pc |> Seq.length = 1)
        isFalsePC
            
    let public add pc cond : pathConditionIndependent =
        match cond with
        | True -> pc
        | False -> falsePC
        | _ when isFalse pc -> falsePC
        | _ when PersistentDict.contains !!cond pc.constraintsWithConstants -> falsePC
        | _ ->
            let consts = discoverConstantsRec cond
            let tryHead = PersistentSet.toSeq >> Seq.tryHead
            let pufWithNewConsts =
                consts
                |> PersistentSet.filter (fun t -> None = PersistentUnionFind.tryFind pc.constants t)
                |> PersistentSet.fold PersistentUnionFind.add pc.constants
            let pufWithMergedConsts =
                consts
                |> tryHead
                |> function
                    | Some(parent) ->
                        PersistentSet.fold (fun puf t -> PersistentUnionFind.union t parent puf) pufWithNewConsts consts
                    | None -> pufWithNewConsts
            {constants = pufWithMergedConsts
             constraintsWithConstants =
                 consts
                 |> tryHead
                 |> (fun head -> PersistentDict.add cond head pc.constraintsWithConstants)}
            
    let public mapPC mapper (pc : pathConditionIndependent) : pathConditionIndependent =
        let mapAndAdd acc cond k =
            let acc' = mapper cond |> add acc
            if isFalse acc' then falsePC else k acc'
        Cps.Seq.foldlk mapAndAdd empty (toSeq pc) id
    let public mapSeq mapper (pc : pathConditionIndependent) = toSeq pc |> Seq.map mapper

    let toString pc = mapSeq toString pc |> Seq.sort |> join " /\ "

    let union (pc1 : pathConditionIndependent) (pc2 : pathConditionIndependent) = Seq.fold add pc1 (toSeq pc2)
    
    let public slice pc cond : pathCondition =
        PersistentDict.find pc.constraintsWithConstants cond
        |> function
            | Some(constant) -> PersistentUnionFind.tryFind pc.constants constant
            | None -> None
        |> function
            | Some _ as parent ->
                pc.constraintsWithConstants
                |> PersistentDict.toSeq
                |> Seq.filter
                   (function
                        | _, Some c -> parent = PersistentUnionFind.tryFind pc.constants c
                        | _ -> false)
                |> Seq.map
                    (function
                        | t, Some _ -> t
                        | _ -> __unreachable__())
                |> PersistentSet.ofSeq
            | None -> PC.empty
                