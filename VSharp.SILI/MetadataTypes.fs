namespace VSharp.Interpreter

open VSharp
open VSharp.Core
open global.System
open System.Reflection
open JetBrains.Metadata.Reader.API

module internal MetadataTypes =

    let genericParameterFromMetadata (arg : IMetadataGenericArgument) =
        match arg with
        | _ when arg.TypeOwner <> null -> Type.GetType(arg.TypeOwner.AssemblyQualifiedName, true).GetGenericArguments().[int arg.Index]
        | _ when arg.MethodOwner <> null ->
            let metadataMethod = arg.MethodOwner
            let metadataToken = metadataMethod.Token
            let meth =
                Type.GetType(metadataMethod.DeclaringType.AssemblyQualifiedName, true).Module.ResolveMethod((int)metadataToken.Value)
            meth.GetGenericArguments().[int arg.Index]
        | _ -> __notImplemented__()

    let rec metadataToDotNetType (arg : IMetadataType) =
        match arg with
        | null -> null
        | _ when arg.AssemblyQualifiedName = "__Null" -> null
        | _ when arg.FullName = "System.Void" -> typedefof<System.Void>
        | :? IMetadataGenericArgumentReferenceType as g -> genericParameterFromMetadata g.Argument
        | :? IMetadataArrayType as a ->
            if a.IsVector
                then (a.ElementType |> metadataToDotNetType).MakeArrayType()
                else (a.ElementType |> metadataToDotNetType).MakeArrayType(int(a.Rank))
        | :? IMetadataClassType as c ->
            let originType = Type.GetType(c.Type.AssemblyQualifiedName, true)
            if not originType.IsGenericType || Array.isEmpty c.Arguments then originType
            else originType.MakeGenericType(c.Arguments |> Array.map metadataToDotNetType)
        | _ -> Type.GetType(arg.AssemblyQualifiedName, true)

    let isReferenceType (t : IMetadataType) =
        let dnt = metadataToDotNetType t
        not dnt.IsValueType

    let rec fromMetadataType state (t : IMetadataType) =
        match t with
        | null -> ClassType(hierarchy typedefof<obj>, [])
        | _ when t.AssemblyQualifiedName = "__Null" -> Null
        | _ when t.FullName = "System.Void" -> Core.Void
        | :? IMetadataGenericArgumentReferenceType as g ->
            let arg = metadataToDotNetType g
            Types.FromDotNetType state arg
        | :? IMetadataArrayType as a ->
            let elementType = fromMetadataType state a.ElementType |> Types.WrapReferenceType
            ArrayType(elementType, if a.IsVector then Vector else a.Rank |> int |> ConcreteDimension)
        | :? IMetadataClassType as ct ->
            let dotnetType = metadataToDotNetType ct
            Types.FromDotNetType state dotnetType
        | :? IMetadataPointerType as pt ->
            let dotnetType = metadataToDotNetType pt
            Types.FromDotNetType state dotnetType
        | _ -> Type.GetType(t.AssemblyQualifiedName, true) |> Types.FromDotNetType state

    let fromDecompiledSignature state (signature : JetBrains.Decompiler.Ast.IFunctionSignature) (returnMetadataType : IMetadataType) =
        let returnType = fromMetadataType state returnMetadataType
        let paramToType (param : JetBrains.Decompiler.Ast.IMethodParameter) =
            fromMetadataType state param.Type
        let args = Seq.map paramToType signature.Parameters |> List.ofSeq
        Func(args, returnType)

    let fromMetadataMethodSignature state (m : IMetadataMethod) =
        let returnType = fromMetadataType state m.ReturnValue.Type
        let paramToType (param : IMetadataParameter) =
            fromMetadataType state param.Type
        let args = Seq.map paramToType m.Parameters |> List.ofSeq
        Func(args, returnType)

    let getMetadataTypeOfNode (node : JetBrains.Decompiler.Ast.INode) =
        DecompilerServices.getTypeOfNode node

    let getSystemTypeOfNode (node : JetBrains.Decompiler.Ast.INode) =
        let mt = getMetadataTypeOfNode node
        if mt = null then typedefof<obj>
        else metadataToDotNetType mt
