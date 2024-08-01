namespace VSharp.Core

open VSharp
open Memory

module internal Branching =

    let checkSat state = SolverInteraction.checkSatWithSubtyping state

    let commonGuardedStatedApplyk f state term mergeResults k =
        match term.term with
        | Ite iteType ->
            let filterUnsat (g, v) k =
                let pc' = PC.add state.pc g
                if PC.isFalse pc' then k None
                else Some (pc', v) |> k
            Cps.List.choosek filterUnsat iteType.branches (fun filteredBranches ->
            let statedApply (pc, v) k = f (state.Copy pc) v k
            Cps.List.mapk statedApply filteredBranches (fun appliedBranches ->
            f state iteType.elseValue (fun appliedElse ->
            appliedBranches @ [appliedElse] |> mergeResults |> k)))
        | _ -> f state term (List.singleton >> k)
    let guardedStatedApplyk f state term k = commonGuardedStatedApplyk f state term State.mergeResults k
    let guardedStatedApply f state term = guardedStatedApplyk (Cps.ret2 f) state term id
    let guardedStatedMap mapper state term = commonGuardedStatedApplyk (Cps.ret2 mapper) state term id id

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

    let assumeStatedConditionalExecution (state : state) assume =
        assert(isBool assume)
        let pc = state.pc
        assert(PC.toSeq pc |> conjunction |> state.model.Eval |> isTrue)
        let assumePc = PC.add pc assume
        state.AddConstraint assume
        let typeStorage = state.typeStorage
        TypeStorage.addTypeConstraint typeStorage.Constraints assume
        if state.model.Eval assume |> isFalse then
            match checkSat state with
            | SolverInteraction.SmtUnsat _
            | SolverInteraction.SmtUnknown _ ->
                internalfail "assumeStatedConditionalExecution: fail"
            | SolverInteraction.SmtSat model ->
                state.model <- model.mdl
                assert(PC.toSeq assumePc |> conjunction |> state.model.Eval |> isTrue)
