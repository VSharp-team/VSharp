namespace VSharp.Solver

open VSharp.Core

type model() = class end
type unsatCore() = class end

type satInfo = { mdl : model; usedPaths : path seq }
type unsatInfo = { core : unsatCore }

type smtResult =
    | SmtSat of satInfo
    | SmtUnsat of unsatInfo
    | SmtUnknown of string

type ISolver =
    abstract CheckSat : query -> smtResult
    abstract Assert : level -> formula -> unit
    abstract AddPath : path -> unit
