namespace VSharp.Core

open VSharp
open Memory

module internal Branching =

    let checkSat state = SolverInteraction.checkSatWithSubtyping state

    let commonGuardedStatedApplyk f state term mergeResults k =
        match term.term with
        | Union gvs ->
            let filterUnsat (g, v) k =
                let pc = PC.add state.pc g
                if PC.isFalse pc then k None
                else Some (pc, v) |> k
            Cps.List.choosek filterUnsat gvs (fun pcs ->
            match pcs with
            | [] -> k []
            | (pc, v)::pcs ->
                let copyState (pc, v) k = f (state.Copy pc) v k
                Cps.List.mapk copyState pcs (fun results ->
                    state.pc <- pc
                    f state v (fun r ->
                    r::results |> mergeResults |> k)))
        | _ -> f state term (List.singleton >> k)
    let guardedStatedApplyk f state term k = commonGuardedStatedApplyk f state term State.mergeResults k
    let guardedStatedApply f state term = guardedStatedApplyk (Cps.ret2 f) state term id

    let guardedStatedMap mapper state term =
        commonGuardedStatedApplyk (fun state term k -> mapper state term |> k) state term id id

    let mutable branchesReleased = false

    let commonStatedConditionalExecutionk (state : state) conditionInvocation thenBranch elseBranch merge2Results k =
        let execution thenState elseState condition k =
            assert (condition <> True() && condition <> False())
            thenBranch thenState (fun thenResult ->
            elseBranch elseState (fun elseResult ->
            merge2Results thenResult elseResult |> k))
        conditionInvocation state (fun (condition, conditionState) ->
        let pc = state.pc
        assert(PC.toSeq pc |> conjunction |> state.model.Eval |> isTrue)
        let typeStorage = conditionState.typeStorage
        let evaled = state.model.Eval condition
        let notCondition = !!condition
        if isTrue evaled then
            assert(state.model.Eval notCondition |> isFalse)
            let elsePc = PC.add pc notCondition
            if PC.isFalse elsePc then
                thenBranch conditionState (List.singleton >> k)
            elif not branchesReleased then
                let typeStorageCopy = typeStorage.Copy()
                conditionState.pc <- elsePc
                TypeStorage.addTypeConstraint typeStorage.Constraints notCondition
                match checkSat conditionState with
                | SolverInteraction.SmtUnsat _ ->
                    conditionState.pc <- pc
                    TypeStorage.addTypeConstraint typeStorageCopy.Constraints condition
                    conditionState.typeStorage <- typeStorageCopy
                    TypeSolver.refineTypes conditionState
                    thenBranch conditionState (List.singleton >> k)
                | SolverInteraction.SmtUnknown _ ->
                    conditionState.pc <- PC.add pc condition
                    TypeStorage.addTypeConstraint typeStorageCopy.Constraints condition
                    conditionState.typeStorage <- typeStorageCopy
                    TypeSolver.refineTypes conditionState
                    thenBranch conditionState (List.singleton >> k)
                | SolverInteraction.SmtSat model ->
                    let thenState = conditionState
                    let elseState = conditionState.Copy elsePc
                    elseState.model <- model.mdl
                    assert(PC.toSeq elsePc |> conjunction |> elseState.model.Eval |> isTrue)
                    thenState.pc <- PC.add pc condition
                    TypeStorage.addTypeConstraint typeStorageCopy.Constraints condition
                    thenState.typeStorage <- typeStorageCopy
                    TypeSolver.refineTypes thenState
                    execution thenState elseState condition k
            else
                conditionState.pc <- PC.add pc condition
                TypeStorage.addTypeConstraint typeStorage.Constraints condition
                thenBranch conditionState (List.singleton >> k)
        elif isFalse evaled then
            assert(state.model.Eval notCondition |> isTrue)
            let thenPc = PC.add state.pc condition
            if PC.isFalse thenPc then
                elseBranch conditionState (List.singleton >> k)
            elif not branchesReleased then
                let typeStorageCopy = typeStorage.Copy()
                conditionState.pc <- thenPc
                TypeStorage.addTypeConstraint typeStorage.Constraints condition
                match checkSat conditionState with
                | SolverInteraction.SmtUnsat _ ->
                    conditionState.pc <- pc
                    TypeStorage.addTypeConstraint typeStorageCopy.Constraints notCondition
                    conditionState.typeStorage <- typeStorageCopy
                    TypeSolver.refineTypes conditionState
                    elseBranch conditionState (List.singleton >> k)
                | SolverInteraction.SmtUnknown _ ->
                    conditionState.pc <- PC.add pc notCondition
                    TypeStorage.addTypeConstraint typeStorageCopy.Constraints notCondition
                    conditionState.typeStorage <- typeStorageCopy
                    TypeSolver.refineTypes conditionState
                    elseBranch conditionState (List.singleton >> k)
                | SolverInteraction.SmtSat model ->
                    let thenState = conditionState
                    let elseState = conditionState.Copy (PC.add pc notCondition)
                    thenState.model <- model.mdl
                    assert(PC.toSeq thenPc |> conjunction |> thenState.model.Eval |> isTrue)
                    TypeStorage.addTypeConstraint typeStorageCopy.Constraints notCondition
                    elseState.typeStorage <- typeStorageCopy
                    TypeSolver.refineTypes elseState
                    execution thenState elseState condition k
            else
                conditionState.pc <- PC.add pc notCondition
                TypeStorage.addTypeConstraint typeStorage.Constraints notCondition
                elseBranch conditionState (List.singleton >> k)
        else __unreachable__())

    let statedConditionalExecutionWithMergek state conditionInvocation thenBranch elseBranch k =
        commonStatedConditionalExecutionk state conditionInvocation thenBranch elseBranch State.merge2Results k
    let statedConditionalExecutionWithMerge state conditionInvocation thenBranch elseBranch =
        statedConditionalExecutionWithMergek state conditionInvocation thenBranch elseBranch id
