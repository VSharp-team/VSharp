namespace VSharp.Core
open VSharp

type pathCondition = pset<term>

// Invariants:
// - PC does not contain True
// - if PC contains False then False is the only element in PC

module internal PC =

    let public empty : pathCondition = PersistentSet.empty<term>
    let public isEmpty pc = PersistentSet.isEmpty pc

    let public toSeq pc = PersistentSet.toSeq pc

    let private falsePC = PersistentSet.add empty False
    let public isFalse pc =
        let isFalsePC = PersistentSet.contains False pc
        if isFalsePC then assert(toSeq pc |> Seq.length = 1)
        isFalsePC

    let public add pc cond : pathCondition =
        match cond with
        | True -> pc
        | False -> falsePC
        | _ when isFalse pc -> falsePC
        | _ when PersistentSet.contains !!cond pc -> falsePC
        | _ -> PersistentSet.add pc cond

    let public mapPC mapper (pc : pathCondition) : pathCondition =
        let mapAndAdd acc cond k =
            let acc' = mapper cond |> add acc
            if isFalse acc' then falsePC else k acc'
        Cps.Seq.foldlk mapAndAdd empty (toSeq pc) id
    let public mapSeq mapper (pc : pathCondition) = toSeq pc |> Seq.map mapper

    let toString pc = mapSeq toString pc |> Seq.sort |> join " /\ "

    let union (pc1 : pathCondition) (pc2 : pathCondition) = Seq.fold add pc1 (toSeq pc2)
