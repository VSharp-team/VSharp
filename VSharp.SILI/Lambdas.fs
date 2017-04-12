namespace VSharp

open JetBrains.Decompiler.Ast
open JetBrains.Metadata.Reader.API

module Lambdas =
    type SymbolicLambda<'a> = State.state -> Term list -> (StatementResult * State.state -> 'a) -> 'a

    let public Make (metadataMethod : IMetadataMethod) (lambda : SymbolicLambda<'a>) =
        let typ = Types.FromMetadataMethodSignature metadataMethod in
        Concrete(lambda, typ)

    let public Make2 (signature : IFunctionSignature) (returnMetadataType : IMetadataType) (lambda : SymbolicLambda<'a>) =
        let typ = Types.FromDecompiledSignature signature returnMetadataType in
        Concrete(lambda, typ)

    let (|Lambda|_|) = function
        | Concrete(lambda, t) when Types.IsFunction t && (lambda :? SymbolicLambda<'a>) ->
            Some(Lambda(lambda :?> SymbolicLambda<'a>))
        | _ -> None
