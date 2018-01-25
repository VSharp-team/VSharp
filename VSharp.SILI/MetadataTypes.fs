namespace VSharp.Interpreter

open VSharp
open VSharp.Core
open VSharp.Core.Types
open VSharp.Core.Types.Constructor
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

    let rec MetadataToDotNetType (arg : IMetadataType) =
        match arg with
        | null -> null
        | _ when arg.AssemblyQualifiedName = "__Null" -> null
        | _ when arg.FullName = "System.Void" -> typedefof<System.Void>
        | :? IMetadataGenericArgumentReferenceType as g -> genericParameterFromMetadata g.Argument
        | :? IMetadataArrayType as a ->
            if a.IsVector
                then (a.ElementType |> MetadataToDotNetType).MakeArrayType()
                else (a.ElementType |> MetadataToDotNetType).MakeArrayType(int(a.Rank))
        | :? IMetadataClassType as c ->
            let originType = Type.GetType(c.Type.AssemblyQualifiedName, true)
            if not originType.IsGenericType || Array.isEmpty c.Arguments then originType
            else originType.MakeGenericType(c.Arguments |> Array.map MetadataToDotNetType)
        | _ -> Type.GetType(arg.AssemblyQualifiedName, true)

    let VariableFromMetadataType = MetadataToDotNetType >> Variable.fromDotNetType

    let IsReferenceType (t : IMetadataType) =
        let dnt = MetadataToDotNetType t
        not dnt.IsValueType

    let rec FromMetadataType (t : IMetadataType) =
        match t with
        | null -> ClassType(hierarchy typedefof<obj>, [], [])
        | _ when t.AssemblyQualifiedName = "__Null" -> Null
        | _ when t.FullName = "System.Void" -> Core.Void
        | :? IMetadataGenericArgumentReferenceType as g ->
            let arg = MetadataToDotNetType g
            FromDotNetType arg
        | :? IMetadataArrayType as a ->
            let elementType = FromMetadataType a.ElementType |> WrapReferenceType
            ArrayType(elementType, if a.IsVector then Vector else a.Rank |> int |> ConcreteDimension)
        | :? IMetadataClassType as ct ->
            let dotnetType = MetadataToDotNetType ct
            FromDotNetType dotnetType
        | :? IMetadataPointerType as pt ->
            let dotnetType = MetadataToDotNetType pt
            FromDotNetType dotnetType
        | _ -> Type.GetType(t.AssemblyQualifiedName, true) |> FromDotNetType

    let FromDecompiledSignature (signature : JetBrains.Decompiler.Ast.IFunctionSignature) (returnMetadataType : IMetadataType) =
        let returnType = VariableFromMetadataType returnMetadataType
        let paramToType (param : JetBrains.Decompiler.Ast.IMethodParameter) =
            param.Type |> FromMetadataType
        let args = Seq.map paramToType signature.Parameters |> List.ofSeq
        Func(args, returnType)

    let FromMetadataMethodSignature (m : IMetadataMethod) =
        let returnType = VariableFromMetadataType m.ReturnValue.Type
        let paramToType (param : IMetadataParameter) =
            param.Type |> FromMetadataType
        let args = Seq.map paramToType m.Parameters |> List.ofSeq
        Func(args, returnType)

    let GetMetadataTypeOfNode (node : JetBrains.Decompiler.Ast.INode) =
        DecompilerServices.getTypeOfNode node

    let GetSystemTypeOfNode (node : JetBrains.Decompiler.Ast.INode) =
        let mt = GetMetadataTypeOfNode node
        if mt = null then typedefof<obj>
        else MetadataToDotNetType mt
