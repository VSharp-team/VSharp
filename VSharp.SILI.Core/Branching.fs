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
