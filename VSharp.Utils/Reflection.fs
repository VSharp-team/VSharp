namespace VSharp

open System
open System.Reflection
open VSharp.CSharpUtils

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

    // TODO: what if return type is generic?
    let public GetMethodReturnType : MethodBase -> Type = function
        | :? ConstructorInfo -> typeof<System.Void>
        | :? MethodInfo as m -> m.ReturnType
        | _ -> internalfail "unknown MethodBase"

    let public GetFullMethodName (methodBase : MethodBase) =
        let returnType = GetMethodReturnType methodBase
        methodBase.GetParameters()
        |> Seq.map (fun param -> param.ParameterType.FullName)
        |> if methodBase.IsStatic then id else Seq.cons "this"
        |> join ", "
        |> sprintf "%s %s.%s(%s)" returnType.FullName methodBase.DeclaringType.FullName methodBase.Name

    let public IsArrayConstructor (methodBase : MethodBase) =
        methodBase.IsConstructor && methodBase.DeclaringType.IsArray

    let public IsDelegateConstructor (methodBase : MethodBase) =
        methodBase.IsConstructor && methodBase.DeclaringType.IsSubclassOf typedefof<System.Delegate>

    let public TryGetGenericMethodDefinition (methodBase : MethodBase) =
        if not <| methodBase.IsGenericMethod then None
        else
            let methodInfo =
                assert(not <| methodBase.IsConstructor)
                methodBase :?> MethodInfo
            let args = methodInfo.GetGenericArguments()
            let genericMethodInfo = methodInfo.GetGenericMethodDefinition()
            let parameters = genericMethodInfo.GetGenericArguments()
            Some (genericMethodInfo :> MethodBase, parameters, args)

    let public TryGetMethodWithGenericDeclaringType (methodBase : MethodBase) =
        let t = methodBase.DeclaringType
        if not <| t.IsGenericType then None
        else
            let actualArgs = t.GetGenericArguments()
            let genericType = t.GetGenericTypeDefinition()
            let genericMethod = genericType.GetMethod(methodBase.Name, allBindingFlags)
            if genericMethod = null then None
            else Some (genericMethod :> MethodBase, genericType.GetGenericArguments(), actualArgs)

    let public IsGenericOrDeclaredInGenericType (methodBase : MethodBase) =
        methodBase.IsGenericMethod || methodBase.DeclaringType.IsGenericType

    // --------------------------------- Concretization ---------------------------------

    let rec public concretizeType (subst : Type -> Type) (typ : Type)=
        if typ.IsGenericParameter then subst typ
        elif typ.IsGenericType then
            let args = typ.GetGenericArguments()
            typ.GetGenericTypeDefinition().MakeGenericType(Array.map (concretizeType subst) args)
        else typ

    let private concretizeMethod subst (m : MethodBase) getMethod =
        let concreteType = concretizeType subst m.DeclaringType
        let concreteParameters = m.GetParameters() |> Array.map (fun pi -> pi.ParameterType |> subst)
        getMethod concreteType m.Name concreteParameters

    let private concretizeMethodInfo subst (mi : MethodInfo) =
        let concretize method =
            let mi = concretizeMethod subst method (fun t name parameters -> t.GetMethod(name, parameters))
            assert(mi <> null)
            mi
        let concretizeGeneric (mi : MethodInfo) =
            let args = mi.GetGenericArguments()
            let genericMethod = mi.GetGenericMethodDefinition()
            let mi = concretize genericMethod
            mi.MakeGenericMethod(args |> Array.map subst)
        if mi.IsGenericMethod then concretizeGeneric mi else concretize mi
        :> MethodBase

    let private concretizeCtorInfo subst ci =
        let ci = concretizeMethod subst ci (fun t _ parameters -> t.GetConstructor(parameters))
        assert(ci <> null)
        ci :> MethodBase

    let concretizeMethodBase (m : MethodBase) (subst : Type -> Type) =
        match m with
        | _ when not m.DeclaringType.IsGenericType -> m
        | :? MethodInfo as mi ->
            concretizeMethodInfo subst mi
        | :? ConstructorInfo as ci ->
            concretizeCtorInfo subst ci
        | _ -> __unreachable__()

    let concretizeParameter (p : ParameterInfo) (subst : Type -> Type) =
        if not (p.Member :? MethodBase) then __notImplemented__()
        else
            (concretizeMethodBase (p.Member :?> MethodBase) subst).GetParameters() |> Array.find (fun pi -> pi.Name = p.Name)

    let concretizeLocalVariable (l : LocalVariableInfo) (m : MethodBase) (subst : Type -> Type) =
        let m = concretizeMethodBase m subst
        let mb = m.GetMethodBody()
        assert(mb <> null)
        mb.LocalVariables.[l.LocalIndex], m

    let concretizeField (f : fieldId) (subst : Type -> Type) =
        let declaringType = concretizeType subst f.declaringType
        {declaringType = declaringType; name = f.name; typ = concretizeType subst f.typ}

    // --------------------------------- Fields ---------------------------------

    let wrapField (field : FieldInfo) =
        {declaringType = safeGenericTypeDefinition field.DeclaringType; name = field.Name; typ = field.FieldType}

    let rec private retrieveFields isStatic f (t : System.Type) =
        let staticFlag = if isStatic then BindingFlags.Static else BindingFlags.Instance
        let flags = BindingFlags.Public ||| BindingFlags.NonPublic ||| staticFlag
        let fields = t.GetFields(flags)
        let ourFields = f fields
        if isStatic || t.BaseType = null then ourFields
        else Array.append (retrieveFields false f t.BaseType) ourFields

    let retrieveNonStaticFields t = retrieveFields false id t

    let fieldsOf isStatic (t : System.Type) =
        let extractFieldInfo (field : FieldInfo) =
            // Events may appear at this point. Filtering them out...
            if field.FieldType.IsSubclassOf(typeof<MulticastDelegate>) then None
            else Some (wrapField field, field.FieldType)
        retrieveFields isStatic (FSharp.Collections.Array.choose extractFieldInfo) t

    // Returns pair (valueFieldInfo, hasValueFieldInfo)
    let fieldsOfNullable typ =
        let fs = fieldsOf false typ
        match fs with
        | [|(f1, _); (f2, _)|] when f1.name.Contains("value", StringComparison.OrdinalIgnoreCase) && f2.name.Contains("hasValue", StringComparison.OrdinalIgnoreCase) -> f1, f2
        | [|(f1, _); (f2, _)|] when f1.name.Contains("hasValue", StringComparison.OrdinalIgnoreCase) && f2.name.Contains("value", StringComparison.OrdinalIgnoreCase) -> f2, f1
        | _ -> internalfailf "%O has unexpected fields {%O}! Probably your .NET implementation is not supported :(" typ.FullName (fs |> Array.map (fun (f, _) -> f.name) |> join ", ")

    let stringLengthField, stringFirstCharField =
        let fs = fieldsOf false typeof<string>
        match fs with
        | [|(f1, _); (f2, _)|] when f1.name.Contains("length", StringComparison.OrdinalIgnoreCase) && f2.name.Contains("firstChar", StringComparison.OrdinalIgnoreCase) -> f1, f2
        | [|(f1, _); (f2, _)|] when f1.name.Contains("firstChar", StringComparison.OrdinalIgnoreCase) && f2.name.Contains("length", StringComparison.OrdinalIgnoreCase) -> f2, f1
        | _ -> internalfailf "System.String has unexpected fields {%O}! Probably your .NET implementation is not supported :(" (fs |> Array.map (fun (f, _) -> f.name) |> join ", ")

    let emptyStringField =
        let fs = fieldsOf true typeof<string>
        match fs |> Array.tryFind (fun (f, _) -> f.name.Contains("empty", StringComparison.OrdinalIgnoreCase)) with
        | Some(f, _) -> f
        | None -> internalfailf "System.String has unexpected static fields {%O}! Probably your .NET implementation is not supported :(" (fs |> Array.map (fun (f, _) -> f.name) |> join ", ")
