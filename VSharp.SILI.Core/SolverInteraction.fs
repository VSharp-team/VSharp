namespace VSharp.Core

open System.Collections.Generic
open FSharpx.Collections
open VSharp

module public SolverInteraction =

    type unsatCore() = class end

    type satInfo = { mdl : model }
    type unsatInfo = { core : term[] }

    type smtResult =
        | SmtSat of satInfo
        | SmtUnsat of unsatInfo
        | SmtUnknown of string

    type ISolver =
        abstract CheckSat : term -> smtResult
        abstract Assert : term -> unit

        abstract SetMaxBufferSize : int -> unit

    let mutable private solver : ISolver option = None
    let mutable private onSolverStarted : unit -> unit = id
    let mutable private onSolverStopped : unit -> unit = id

    let configureSolver s = solver <- Some s
    let setOnSolverStarted action = onSolverStarted <- action
    let setOnSolverStopped action = onSolverStopped <- action

    let setMaxBufferSize size =
        match solver with
        | Some s -> s.SetMaxBufferSize size
        | None -> ()

    let private checkSatWithCtx state context =
        let formula = PC.toSeq state.pc |> Seq.append context |> conjunction
        match solver with
        | Some s ->
            onSolverStarted()
            let result = s.CheckSat formula
            onSolverStopped()
            result
        | None -> SmtUnknown ""

    let checkSat state =
        checkSatWithCtx state Seq.empty

    let rec private createContext (unequal : HashSet<term * term>) =
        let createCondition (a1, a2) =
            ((a1 === zeroAddress()) &&& (a2 === zeroAddress())) ||| (!!(a1 === a2))
        Seq.map createCondition unequal

    let checkSatWithSubtyping (state : state) =
        match TypeSolver.checkInequality state with
        | Some unequal ->
            let context = createContext unequal
            match checkSatWithCtx state context with
            | SmtSat {mdl = model} as satWithModel ->
                try
                    match TypeSolver.solveTypes model state with
                    | TypeUnsat -> SmtUnsat {core = Array.empty}
                    | TypeSat -> satWithModel
                with :? InsufficientInformationException as e ->
                    SmtUnknown e.Message
            | result -> result
        | None -> SmtUnsat {core = Array.empty}
