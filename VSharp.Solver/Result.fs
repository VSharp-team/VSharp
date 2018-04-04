namespace VSharp

type SmtResult =
    | SmtSat of Microsoft.Z3.Model
    | SmtUnsat
    | SmtUnknown of string
