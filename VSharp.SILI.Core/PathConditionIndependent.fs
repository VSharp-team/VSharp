module VSharp.SILI.Core.PathConditionIndependent

open VSharp
open VSharp.Core
open VSharp.Utils.PersistentUnionFind

type pathConditionIndependent =
    private {constraints : pset<term>; constants : pUnionFind<term>; constraintConstants : pdict<term, term>}

// Invariants:
// - PCI does not contain True
// - if PCI contains False then False is the only element in PCI

module internal PCI =

    let public empty =
        {constraints = PersistentSet.empty
         constants = PersistentUnionFind.empty
         constraintConstants = PersistentDict.empty}
        
    let public isEmpty pc = PersistentSet.isEmpty pc.constraints

    let public toSeq pc = PersistentSet.toSeq pc.constraints

    let private falsePC =
        {constraints = PersistentSet.add PersistentSet.empty False
         constants = PersistentUnionFind.empty
         constraintConstants = PersistentDict.empty}
        
    let public isFalse pc =
        let isFalsePC = PersistentSet.contains False pc.constraints
        if isFalsePC then assert(toSeq pc |> Seq.length = 1)
        isFalsePC

    let public add pc cond : pathConditionIndependent =
        match cond with
        | True -> pc
        | False -> falsePC
        | _ when isFalse pc -> falsePC
        | _ when PersistentSet.contains !!cond pc.constraints -> falsePC
        | _ ->
            let parents = discoverConstantsRec cond |> PersistentSet.map (PersistentUnionFind.tryFind pc.constants)
            falsePC

    let public mapPC mapper (pc : pathCondition) : pathCondition =
        let mapAndAdd acc cond k =
            let acc' = mapper cond |> add acc
            if isFalse acc' then falsePC else k acc'
        Cps.Seq.foldlk mapAndAdd empty (toSeq pc) id
    let public mapSeq mapper (pc : pathCondition) = toSeq pc |> Seq.map mapper

    let toString pc = mapSeq toString pc |> Seq.sort |> join " /\ "

    let union (pc1 : pathCondition) (pc2 : pathCondition) = Seq.fold add pc1 (toSeq pc2)