namespace VSharp

open System
open System.Reflection

module public Reflection =

    // ----------------------------- Binding Flags ------------------------------

    let staticBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        BindingFlags.IgnoreCase ||| BindingFlags.DeclaredOnly |||
        BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public
    let instanceBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        BindingFlags.IgnoreCase ||| BindingFlags.DeclaredOnly |||
        BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public
    let allBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        staticBindingFlags ||| instanceBindingFlags

    // --------------------------- Metadata Resolving ---------------------------

    let private retrieveMethodsGenerics (method : MethodBase) =
        match method with
        | :? MethodInfo as mi -> mi.GetGenericArguments()
        | :? ConstructorInfo -> null
        | _ -> __notImplemented__()

    let resolveField (method : MethodBase) fieldToken =
        let methodsGenerics = retrieveMethodsGenerics method
        let typGenerics = method.DeclaringType.GetGenericArguments()
        method.Module.ResolveField (fieldToken, typGenerics, methodsGenerics)

    let resolveType (method : MethodBase) typeToken =
        let typGenerics = method.DeclaringType.GetGenericArguments()
        let methodGenerics = retrieveMethodsGenerics method
        method.Module.ResolveType(typeToken, typGenerics, methodGenerics)

    let resolveMethod (method : MethodBase) methodToken =
        let typGenerics = method.DeclaringType.GetGenericArguments()
        let methodGenerics = retrieveMethodsGenerics method
        method.Module.ResolveMethod(methodToken, typGenerics, methodGenerics)

    let resolveToken (method : MethodBase) token =
        let typGenerics = method.DeclaringType.GetGenericArguments()
        let methodGenerics = retrieveMethodsGenerics method
        method.Module.ResolveMember(token, typGenerics, methodGenerics)

    // --------------------------------- Methods --------------------------------

    let public GetFullMethodName (methodBase : MethodBase) =
        let returnType =
            match methodBase with
            | :? ConstructorInfo -> typedefof<System.Void>.FullName
            | :? MethodInfo as mi -> mi.ReturnType.FullName
            | _ -> internalfail "unknown MethodBase"
        methodBase.GetParameters()
        |> Seq.map (fun param -> param.ParameterType.FullName)
        |> if methodBase.IsStatic then id else Seq.cons "this"
        |> join ", "
        |> sprintf "%s %s.%s(%s)" returnType methodBase.DeclaringType.FullName methodBase.Name

    // --------------------------------- Fields ---------------------------------

    let getFullNameOfField (field : FieldInfo) =
        let getBlockName (field : FieldInfo) =
            (safeGenericTypeDefinition field.DeclaringType).FullName.Replace(".", "::").Replace("+", "::")
        if field.IsStatic then field.Name
        else sprintf "%s::%s" (getBlockName field) field.Name

    let rec fieldsOf isStatic (t : System.Type) =
        let staticFlag = if isStatic then BindingFlags.Static else BindingFlags.Instance
        let flags = BindingFlags.Public ||| BindingFlags.NonPublic ||| staticFlag
        let fields = t.GetFields(flags)
        let extractFieldInfo (field : FieldInfo) =
            // Events may appear at this point. Filtering them out...
            if field.FieldType.IsSubclassOf(typeof<MulticastDelegate>) then None
            else Some (getFullNameOfField field, field.FieldType)
        let ourFields = fields |> FSharp.Collections.Array.choose extractFieldInfo
        if isStatic || t.BaseType = null then ourFields
        else Array.append (fieldsOf false t.BaseType) ourFields
