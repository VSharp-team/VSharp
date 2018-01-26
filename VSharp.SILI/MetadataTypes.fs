namespace VSharp.Interpreter

open VSharp
open VSharp.Core
open global.System
open System.Reflection
open JetBrains.Metadata.Reader.API

module internal MetadataTypes =

    let private genericParameterFromMetadata (arg : IMetadataGenericArgument) =
        match arg with
        | _ when arg.TypeOwner <> null -> Type.GetType(arg.TypeOwner.AssemblyQualifiedName, true).GetGenericArguments().[int arg.Index]
        | _ when arg.MethodOwner <> null ->
            let metadataMethod = arg.MethodOwner
            let lengthGenericArguments = metadataMethod.GenericArguments.Length
            let parameters = metadataMethod.Parameters
            let declaringType = metadataMethod.DeclaringType
            let meth =
                Type.GetType(metadataMethod.DeclaringType.AssemblyQualifiedName, true).GetMethods() |>
                Seq.filter (fun (m : MethodInfo) -> m.Name = metadataMethod.Name) |>
                Seq.map (fun (m : MethodInfo) -> m, m.GetParameters(), m.GetGenericArguments().Length) |>
                Seq.filter (fun (m, (p : ParameterInfo[]), gLen) -> (gLen = lengthGenericArguments) && (p.Length = parameters.Length)) |>
                Seq.map (fun (m, p, _) -> (m, p)) |>
                Seq.filter (fun (m, (p : ParameterInfo[])) ->
                    Seq.forall2 (fun (l : ParameterInfo) (r : IMetadataParameter) ->
                        l.ParameterType.FullName |? l.ParameterType.Name = r.Type.FullName) p parameters) |>
                Seq.map fst |>
                Array.ofSeq
            assert(meth.Length = 1)
            meth.[0].GetGenericArguments().[int arg.Index]
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

    let variableFromMetadataType = metadataToDotNetType >> Types.NewTypeVariable

    let isReferenceType (t : IMetadataType) =
        let dnt = metadataToDotNetType t
        not dnt.IsValueType

    let rec fromMetadataType (t : IMetadataType) =
        match t with
        | null -> ClassType(hierarchy typedefof<obj>, [], [])
        | _ when t.AssemblyQualifiedName = "__Null" -> Null
        | _ when t.FullName = "System.Void" -> Core.Void
        | :? IMetadataGenericArgumentReferenceType as g ->
            let arg = metadataToDotNetType g
            Types.FromDotNetType arg
        | :? IMetadataArrayType as a ->
            let elementType = fromMetadataType a.ElementType |> Types.WrapReferenceType
            ArrayType(elementType, if a.IsVector then Vector else a.Rank |> int |> ConcreteDimension)
        | :? IMetadataClassType as ct ->
            let dotnetType = metadataToDotNetType ct
            Types.FromDotNetType dotnetType
        | :? IMetadataPointerType as pt ->
            let dotnetType = metadataToDotNetType pt
            Types.FromDotNetType dotnetType
        | _ -> Type.GetType(t.AssemblyQualifiedName, true) |> Types.FromDotNetType

    let fromDecompiledSignature (signature : JetBrains.Decompiler.Ast.IFunctionSignature) (returnMetadataType : IMetadataType) =
        let returnType = variableFromMetadataType returnMetadataType
        let paramToType (param : JetBrains.Decompiler.Ast.IMethodParameter) =
            param.Type |> fromMetadataType
        let args = Seq.map paramToType signature.Parameters |> List.ofSeq
        Func(args, returnType)

    let fromMetadataMethodSignature (m : IMetadataMethod) =
        let returnType = variableFromMetadataType m.ReturnValue.Type
        let paramToType (param : IMetadataParameter) =
            param.Type |> fromMetadataType
        let args = Seq.map paramToType m.Parameters |> List.ofSeq
        Func(args, returnType)

    let getMetadataTypeOfNode (node : JetBrains.Decompiler.Ast.INode) =
        DecompilerServices.getTypeOfNode node

    let getSystemTypeOfNode (node : JetBrains.Decompiler.Ast.INode) =
        let mt = getMetadataTypeOfNode node
        if mt = null then typedefof<obj>
        else metadataToDotNetType mt
