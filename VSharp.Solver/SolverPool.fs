namespace VSharp.Solver

type public SupportedSolvers =
    | Z3

module public SolverPool =

    let mutable private currentSolver = Z3

    let switchSolver (solver : SupportedSolvers) =
        currentSolver <- solver

    let mkSolver () : ISolver =
        match currentSolver with
        | Z3 -> new Z3.Z3Solver() :> ISolver
