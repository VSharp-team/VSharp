namespace VSharp.Core

open System.Collections.Generic
open VSharp

module internal Branching =
    
    let private withPc newPc state = { state with pc = newPc }
    
    let private noNewConstants condition (pc : IPathCondition) =
        let pcConstants = HashSet(pc.Constants)
        let condConstants = condition |> Seq.singleton |> discoverConstants
        pcConstants.IsSupersetOf condConstants
        
    let private keepDependentWith (pc : IPathCondition) cond =
        pc.Fragments
        |> Seq.tryFind (fun pc -> pc.ToSeq() |> Seq.contains cond)
        |> Option.defaultValue pc
    
    let private executionWithConstraintIndependence tryEval copy (state : state) conditionInvocation thenBranch elseBranch merge2Results k =
        let execution thenState elseState condition k =
            assert (condition <> True && condition <> False)
            thenBranch thenState (fun thenResult ->
            elseBranch elseState (fun elseResult ->
            merge2Results thenResult elseResult |> k))
        conditionInvocation state (fun (condition, conditionState) ->
        let negatedCondition = !!condition
        let thenPc = PC.add condition state.pc
        let elsePc = PC.add negatedCondition state.pc
        if thenPc.IsFalse then
            conditionState.pc <- elsePc
            elseBranch conditionState (List.singleton >> k)
        elif elsePc.IsFalse then
            conditionState.pc <- thenPc
            thenBranch conditionState (List.singleton >> k)
        else
            let independentThenPc = keepDependentWith thenPc condition
            // In fact, this call is redundant because independentElsePc == independentThenPc with negated cond
            let independentElsePc = keepDependentWith elsePc negatedCondition
            let checkBothBranches () =
                conditionState.pc <- independentThenPc
                match SolverInteraction.checkSat conditionState with
                | SolverInteraction.SmtUnknown _ ->
                    conditionState.pc <- independentElsePc
                    match SolverInteraction.checkSat conditionState with
                    | SolverInteraction.SmtUnsat _
                    | SolverInteraction.SmtUnknown _ ->
                        __insufficientInformation__ "Unable to witness branch"
                    | SolverInteraction.SmtSat elseModel ->
                        conditionState.pc <- elsePc
                        conditionState.model <- Some elseModel.mdl
                        elseBranch conditionState (List.singleton >> k)
                | SolverInteraction.SmtUnsat _ ->
                    conditionState.pc <- elsePc
                    elseBranch conditionState (List.singleton >> k)
                | SolverInteraction.SmtSat thenModel ->
                    conditionState.pc <- independentElsePc
                    match SolverInteraction.checkSat conditionState with
                    | SolverInteraction.SmtUnsat _
                    | SolverInteraction.SmtUnknown _ ->
                        conditionState.model <- Some thenModel.mdl
                        conditionState.pc <- thenPc
                        thenBranch conditionState (List.singleton >> k)
                    | SolverInteraction.SmtSat elseModel ->
                        conditionState.model <- Some thenModel.mdl
                        let thenState = conditionState
                        let elseState = copy conditionState |> withPc elsePc
                        elseState.model <- Some elseModel.mdl
                        thenState.pc <- thenPc
                        execution thenState elseState condition k
            if tryEval && noNewConstants condition state.pc then
                let evaluatedCondition = state.model.Value.Eval condition
                // Current model satisfies new condition, so we can keep it for 'then' branch
                if isTrue evaluatedCondition then
                    let elseState = copy conditionState |> withPc independentElsePc
                    conditionState.pc <- thenPc
                    match SolverInteraction.checkSat elseState with
                    | SolverInteraction.SmtUnsat _
                    | SolverInteraction.SmtUnknown _ ->
                        thenBranch conditionState (List.singleton >> k)
                    | SolverInteraction.SmtSat elseModel ->
                        elseState.pc <- elsePc
                        elseState.model <- Some elseModel.mdl
                        execution conditionState elseState condition k
                // Current model satisfies !condition, so we can keep it for 'else' branch
                elif isFalse evaluatedCondition then
                    let thenState = copy conditionState |> withPc independentThenPc
                    match SolverInteraction.checkSat thenState with
                    | SolverInteraction.SmtUnsat _
                    | SolverInteraction.SmtUnknown _ ->
                        conditionState.pc <- elsePc
                        elseBranch conditionState (List.singleton >> k)
                    | SolverInteraction.SmtSat thenModel ->
                        let elseState = copy conditionState |> withPc elsePc
                        conditionState.pc <- thenPc
                        conditionState.model <- Some thenModel.mdl
                        execution conditionState elseState condition k
                else
                    checkBothBranches()
            else
                checkBothBranches())
    
    let private executionWithoutConstraintIndependence tryEval copy (state : state) conditionInvocation thenBranch elseBranch merge2Results k =
        let execution thenState elseState condition k =
            assert (condition <> True && condition <> False)
            thenBranch thenState (fun thenResult ->
            elseBranch elseState (fun elseResult ->
            merge2Results thenResult elseResult |> k))
        conditionInvocation state (fun (condition, conditionState) ->
        let negatedCondition = !!condition
        let thenPc = PC.add condition state.pc
        let elsePc = PC.add negatedCondition state.pc
        if thenPc.IsFalse then
            conditionState.pc <- elsePc
            elseBranch conditionState (List.singleton >> k)
        elif elsePc.IsFalse then
            conditionState.pc <- thenPc
            thenBranch conditionState (List.singleton >> k)
        else
            let checkBothBranches () =
                conditionState.pc <- thenPc
                match SolverInteraction.checkSat conditionState with
                | SolverInteraction.SmtUnknown _ ->
                    conditionState.pc <- elsePc
                    match SolverInteraction.checkSat conditionState with
                    | SolverInteraction.SmtUnsat _
                    | SolverInteraction.SmtUnknown _ ->
                        __insufficientInformation__ "Unable to witness branch"
                    | SolverInteraction.SmtSat elseModel ->
                        conditionState.pc <- elsePc
                        conditionState.model <- Some elseModel.mdl
                        elseBranch conditionState (List.singleton >> k)
                | SolverInteraction.SmtUnsat _ ->
                    conditionState.pc <- elsePc
                    elseBranch conditionState (List.singleton >> k)
                | SolverInteraction.SmtSat thenModel ->
                    conditionState.pc <- elsePc
                    match SolverInteraction.checkSat conditionState with
                    | SolverInteraction.SmtUnsat _
                    | SolverInteraction.SmtUnknown _ ->
                        conditionState.model <- Some thenModel.mdl
                        conditionState.pc <- thenPc
                        thenBranch conditionState (List.singleton >> k)
                    | SolverInteraction.SmtSat elseModel ->
                        conditionState.model <- Some thenModel.mdl
                        let thenState = conditionState
                        let elseState = copy conditionState |> withPc elsePc
                        elseState.model <- Some elseModel.mdl
                        thenState.pc <- thenPc
                        execution thenState elseState condition k
            if tryEval && noNewConstants condition state.pc then
                let evaluatedCondition = state.model.Value.Eval condition
                // Current model satisfies new condition, so we can keep it for 'then' branch
                if isTrue evaluatedCondition then
                    let elseState = copy conditionState |> withPc elsePc
                    conditionState.pc <- thenPc
                    match SolverInteraction.checkSat elseState with
                    | SolverInteraction.SmtUnsat _
                    | SolverInteraction.SmtUnknown _ ->
                        thenBranch conditionState (List.singleton >> k)
                    | SolverInteraction.SmtSat elseModel ->
                        elseState.pc <- elsePc
                        elseState.model <- Some elseModel.mdl
                        execution conditionState elseState condition k
                // Current model satisfies !condition, so we can keep it for 'else' branch
                elif isFalse evaluatedCondition then
                    let thenState = copy conditionState |> withPc thenPc
                    match SolverInteraction.checkSat thenState with
                    | SolverInteraction.SmtUnsat _
                    | SolverInteraction.SmtUnknown _ ->
                        conditionState.pc <- elsePc
                        elseBranch conditionState (List.singleton >> k)
                    | SolverInteraction.SmtSat thenModel ->
                        let elseState = copy conditionState |> withPc elsePc
                        conditionState.pc <- thenPc
                        conditionState.model <- Some thenModel.mdl
                        execution conditionState elseState condition k
                else
                    checkBothBranches()
            else
                checkBothBranches())
        
    let branch copy (state : state) conditionInvocation thenBranch elseBranch merge2Results k =
        if FeatureFlags.isConstraintIndependenceEnabled() then
            executionWithConstraintIndependence true copy state conditionInvocation thenBranch elseBranch merge2Results k
        else
            executionWithoutConstraintIndependence true copy state conditionInvocation thenBranch elseBranch merge2Results k

namespace VSharp.Core

open VSharp

module Branching =
    let checkSat state = TypeCasting.checkSatWithSubtyping state

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
