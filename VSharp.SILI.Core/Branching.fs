namespace VSharp.Core

open VSharp

module Branching =
    let checkSat state = TypeSolver.checkSatWithSubtyping state

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
                let copyState (pc, v) k = f (Memory.copy state pc) v k
                Cps.List.mapk copyState pcs (fun results ->
                    state.pc <- pc
                    f state v (fun r ->
                    r::results |> mergeResults |> k)))
        | _ -> f state term (List.singleton >> k)
    let guardedStatedApplyk f state term k = commonGuardedStatedApplyk f state term Memory.mergeResults k
    let guardedStatedApply f state term = guardedStatedApplyk (Cps.ret2 f) state term id

    let guardedStatedMap mapper state term =
        commonGuardedStatedApplyk (fun state term k -> mapper state term |> k) state term id id

    let mutable branchesReleased = false

    let checkSatAndExec condition conditionState pc thenPc elsePc thenBranch bothBranches k =
        if not branchesReleased then
            conditionState.pc <- elsePc
            match checkSat conditionState with
            | SolverInteraction.SmtUnsat _ ->
                conditionState.pc <- pc
                thenBranch conditionState (List.singleton >> k)
            | SolverInteraction.SmtUnknown _ ->
                conditionState.pc <- thenPc
                thenBranch conditionState (List.singleton >> k)
            | SolverInteraction.SmtSat model ->
                let thenState = conditionState
                let elseState = Memory.copy conditionState elsePc
                elseState.model <- Some model.mdl
                thenState.pc <- thenPc
                bothBranches thenState elseState condition k
        else
            conditionState.pc <- thenPc
            thenBranch conditionState (List.singleton >> k)

    let commonStatedConditionalExecutionk (state : state) conditionInvocation thenBranch elseBranch merge2Results k =
        let execution thenState elseState condition k =
            assert (condition <> True && condition <> False)
            thenBranch thenState (fun thenResult ->
            elseBranch elseState (fun elseResult ->
            merge2Results thenResult elseResult |> k))
        conditionInvocation state (fun (condition, conditionState) ->
        let pc = state.pc
        let evaled =
            match state.model with
            | Some model -> model.Eval condition
            | None -> __unreachable__()
        if isTrue evaled then
            let elsePc = PC.add pc !!condition
            if PC.isFalse elsePc then
                thenBranch conditionState (List.singleton >> k)
            elif not branchesReleased then
                conditionState.pc <- elsePc
                match checkSat conditionState with
                | SolverInteraction.SmtUnsat _ ->
                    conditionState.pc <- pc
                    thenBranch conditionState (List.singleton >> k)
                | SolverInteraction.SmtUnknown _ ->
                    conditionState.pc <- PC.add pc condition
                    thenBranch conditionState (List.singleton >> k)
                | SolverInteraction.SmtSat model ->
                    let thenState = conditionState
                    let elseState = Memory.copy conditionState elsePc
                    elseState.model <- Some model.mdl
                    thenState.pc <- PC.add pc condition
                    execution thenState elseState condition k
            else
                conditionState.pc <- PC.add pc condition
                thenBranch conditionState (List.singleton >> k)
        elif isFalse evaled then
            let notCondition = !!condition
            let thenPc = PC.add state.pc condition
            if PC.isFalse thenPc then
                elseBranch conditionState (List.singleton >> k)
            elif not branchesReleased then
                conditionState.pc <- thenPc
                match checkSat conditionState with
                | SolverInteraction.SmtUnsat _ ->
                    conditionState.pc <- pc
                    elseBranch conditionState (List.singleton >> k)
                | SolverInteraction.SmtUnknown _ ->
                    conditionState.pc <- PC.add pc notCondition
                    elseBranch conditionState (List.singleton >> k)
                | SolverInteraction.SmtSat model ->
                    let thenState = conditionState
                    let elseState = Memory.copy conditionState (PC.add pc notCondition)
                    thenState.model <- Some model.mdl
                    elseState.pc <- PC.add pc notCondition
                    execution thenState elseState condition k
            else
                conditionState.pc <- PC.add pc notCondition
                elseBranch conditionState (List.singleton >> k)
        else __unreachable__())

    let statedConditionalExecutionWithMergek state conditionInvocation thenBranch elseBranch k =
        commonStatedConditionalExecutionk state conditionInvocation thenBranch elseBranch Memory.merge2Results k
    let statedConditionalExecutionWithMerge state conditionInvocation thenBranch elseBranch =
        statedConditionalExecutionWithMergek state conditionInvocation thenBranch elseBranch id
