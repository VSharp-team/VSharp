namespace VSharp.Interpreter

open VSharp.Core
open JetBrains.Decompiler.Ast
open JetBrains.Metadata.Reader.API

module Functions =

    let internal MakeLambda state (metadataMethod : IMetadataMethod) (lambda : 'a symbolicLambda) =
        let typ = MetadataTypes.fromMetadataMethodSignature state metadataMethod
        let term = Concrete lambda typ
        Memory.AllocateInHeap state typ term

    let internal MakeLambda2 state (signature : IFunctionSignature) (returnMetadataType : IMetadataType) (lambda : 'a symbolicLambda) =
        let typ = MetadataTypes.fromDecompiledSignature state signature returnMetadataType
        let term = Terms.MakeLambda lambda typ
        if Transformations.isInlinedSignatureCall signature
            then Memory.AllocateInHeap state typ term
            else term, state
