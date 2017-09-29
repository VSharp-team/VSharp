namespace VSharp

type SmtResult =
    | SmtSat of Microsoft.Z3.Model
    | SmtUnsat
    | SmtUnknown of string

type HornResult =
    | HornSat
    | HornUnsat
    | HornUnknown
