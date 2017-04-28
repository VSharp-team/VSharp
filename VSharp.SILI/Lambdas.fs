namespace VSharp

open JetBrains.Decompiler.Ast
open JetBrains.Metadata.Reader.API

module Lambdas =
    type SymbolicLambda<'a> = State.state -> Term list -> (StatementResult * State.state -> 'a) -> 'a

    let public Make state (metadataMethod : IMetadataMethod) (lambda : SymbolicLambda<'a>) =
        let typ = Types.FromMetadataMethodSignature metadataMethod in
        let term = Concrete(lambda, typ) in
        Memory.allocateInHeap state term false

    let public Make2 state (signature : IFunctionSignature) (returnMetadataType : IMetadataType) (lambda : SymbolicLambda<'a>) =
        let typ = Types.FromDecompiledSignature signature returnMetadataType in
        let term = Concrete(lambda, typ) in
        Memory.allocateInHeap state term false

    let (|Lambda|_|) = function
        | Concrete(lambda, t) when Types.IsFunction t && (lambda :? SymbolicLambda<'a>) ->
            Some(Lambda(lambda :?> SymbolicLambda<'a>))
        | _ -> None
