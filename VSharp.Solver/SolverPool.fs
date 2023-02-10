namespace VSharp.Solver
open VSharp.Core.SolverInteraction

type public SupportedSolvers =
    | Z3

module public SolverPool =

    let mutable private currentSolver = Z3

    let switchSolver (solver : SupportedSolvers) =
        currentSolver <- solver

    let mkSolver (timeoutMs : int) : ISolver =
        let timeoutOpt = if timeoutMs > 0 then Some(uint timeoutMs) else None
        match currentSolver with
        | Z3 -> Z3.Z3Solver timeoutOpt :> ISolver

    let reset() =
        Z3.reset()
