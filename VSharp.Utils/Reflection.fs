namespace VSharp

open System
open System.Collections.Generic
open System.Reflection

module public Reflection =

    // ----------------------------- Binding Flags ------------------------------

    let staticBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        BindingFlags.IgnoreCase ||| BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public
    let instanceBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        BindingFlags.IgnoreCase ||| BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public
    let allBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        staticBindingFlags ||| instanceBindingFlags

    // ------------------------------- Assemblies -------------------------------

    let loadAssembly (assemblyName : string) =
        let assemblies = AppDomain.CurrentDomain.GetAssemblies()
        let dynamicAssemblies = assemblies |> Array.filter (fun a -> a.IsDynamic)
        let dynamicOption = dynamicAssemblies |> Array.tryFind (fun a -> a.FullName.Contains(assemblyName))
        match dynamicOption with
        | Some a -> a
        | None -> Assembly.Load(assemblyName)

    // --------------------------- Metadata Resolving ---------------------------

    let resolveModule (assemblyName : string) (moduleName : string) =
        let assembly =
            match AppDomain.CurrentDomain.GetAssemblies() |> Array.tryFindBack (fun assembly -> assembly.FullName = assemblyName) with
            | Some assembly -> assembly
            | None ->
                try
                    Assembly.Load(assemblyName)
                with _ ->
                AssemblyManager.Resolve moduleName
        assembly.Modules |> Seq.find (fun m -> m.FullyQualifiedName = moduleName)

    let resolveMethodBase (assemblyName : string) (moduleName : string) (token : int32) =
        let m = resolveModule assemblyName moduleName
        m.ResolveMethod(token)

    let private retrieveMethodsGenerics (method : MethodBase) =
        match method with
        | :? MethodInfo as mi -> mi.GetGenericArguments()
        | :? ConstructorInfo -> null
        | _ -> __notImplemented__()

    let resolveModuleFromAssembly (assembly : Assembly) (moduleName : string) =
        assembly.GetModule moduleName

    let resolveTypeFromModule (m : Module) typeToken =
        m.ResolveType(typeToken, null, null)

    let resolveField (method : MethodBase) fieldToken =
        let methodsGenerics = retrieveMethodsGenerics method
        let typGenerics = method.DeclaringType.GetGenericArguments()
        method.Module.ResolveField(fieldToken, typGenerics, methodsGenerics)

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
    let getMethodReturnType : MethodBase -> Type = function
        | :? ConstructorInfo -> typeof<Void>
        | :? MethodInfo as m -> m.ReturnType
        | _ -> internalfail "unknown MethodBase"

    let hasNonVoidResult m = (getMethodReturnType m).FullName <> typeof<Void>.FullName

    let hasThis (m : MethodBase) = m.CallingConvention.HasFlag(CallingConventions.HasThis)

    let getFullTypeName (typ : Type) = typ.ToString()

    let getFullMethodName (methodBase : MethodBase) =
        let returnType = getMethodReturnType methodBase |> getFullTypeName
        let declaringType = getFullTypeName methodBase.DeclaringType
        let parameters =
            methodBase.GetParameters()
            |> Seq.map (fun param -> getFullTypeName param.ParameterType)
            |> if methodBase.IsStatic then id else Seq.cons "this"
            |> join ", "
//        let typeParams =
//            if not methodBase.IsGenericMethod then ""
//            else methodBase.GetGenericArguments() |> Seq.map getFullTypeName |> join ", " |> sprintf "[%s]"
        sprintf "%s %s.%s(%s)" returnType declaringType methodBase.Name parameters

    let isArrayConstructor (methodBase : MethodBase) =
        methodBase.IsConstructor && methodBase.DeclaringType.IsArray

    let isDelegateConstructor (methodBase : MethodBase) =
        methodBase.IsConstructor && TypeUtils.isSubtypeOrEqual methodBase.DeclaringType typedefof<Delegate>

    let isDelegate (methodBase : MethodBase) =
        TypeUtils.isSubtypeOrEqual methodBase.DeclaringType typedefof<Delegate> && methodBase.Name = "Invoke"

    let isGenericOrDeclaredInGenericType (methodBase : MethodBase) =
        methodBase.IsGenericMethod || methodBase.DeclaringType.IsGenericType

    let isStaticConstructor (m : MethodBase) =
        m.IsStatic && m.Name = ".cctor"

    let isExternalMethod (methodBase : MethodBase) =
        let isInternalCall = methodBase.GetMethodImplementationFlags() &&& MethodImplAttributes.InternalCall
        let isPInvokeImpl = methodBase.Attributes.HasFlag(MethodAttributes.PinvokeImpl)
        int isInternalCall <> 0 || isPInvokeImpl

    let getAllMethods (t : Type) = t.GetMethods(allBindingFlags)

    let getMethodDescriptor (m : MethodBase) =
        let declaringType = m.DeclaringType
        let declaringTypeVars =
            if declaringType.IsGenericType then declaringType.GetGenericArguments() |> Array.map (fun t -> t.TypeHandle.Value)
            else [||]
        let methodVars =
            if m.IsGenericMethod then m.GetGenericArguments() |> Array.map (fun t -> t.TypeHandle.Value)
            else [||]
        m.MethodHandle.Value, declaringTypeVars, methodVars, m.ReflectedType.TypeHandle.Value

    let compareMethods (m1 : MethodBase) (m2 : MethodBase) =
        compare (getMethodDescriptor m1) (getMethodDescriptor m2)

    let getArrayMethods (arrayType : Type) =
        let methodsFromHelper = Type.GetType("System.SZArrayHelper") |> getAllMethods
        let makeSuitable (m : MethodInfo) =
            if m.IsGenericMethod then m.MakeGenericMethod(arrayType.GetElementType()) else m
        let concreteMethods = Array.map makeSuitable methodsFromHelper
        Array.concat [concreteMethods; getAllMethods typeof<Array>; getAllMethods arrayType]

    let isOverrideOf (sourceMethod : MethodInfo) (method : MethodInfo) =
        sourceMethod.GetBaseDefinition() = method.GetBaseDefinition()
        ||
        // Return type covariance case
        Attribute.IsDefined(method, typeof<System.Runtime.CompilerServices.PreserveBaseOverridesAttribute>) &&
        method.Name = sourceMethod.Name &&
            let sourceSig = sourceMethod.GetParameters()
            let targetSig = method.GetParameters()
            targetSig.Length = sourceSig.Length &&
            Array.forall2 (fun (p : ParameterInfo) (p' : ParameterInfo) -> p.ParameterType = p'.ParameterType) sourceSig targetSig

    let resolveInterfaceOverride (targetType : Type) (interfaceMethod : MethodInfo) =
        let interfaceType = interfaceMethod.DeclaringType
        assert interfaceType.IsInterface
        if interfaceType = targetType then interfaceMethod
        else
            let createSignature (m : MethodInfo) =
                m.GetParameters()
                |> Seq.map (fun p -> getFullTypeName p.ParameterType)
                |> join ","
            let onlyLastName (m : MethodInfo) =
                match m.Name.LastIndexOf('.') with
                | i when i < 0 -> m.Name
                | i -> m.Name.Substring(i + 1)
            let sign = createSignature interfaceMethod
            let lastName = onlyLastName interfaceMethod
            let methods =
                match targetType with
                | _ when targetType.IsArray -> getArrayMethods targetType
                | _ when targetType.IsInterface -> getAllMethods targetType
                | _ -> targetType.GetInterfaceMap(interfaceType).TargetMethods
            methods |> Seq.find (fun mi -> createSignature mi = sign && onlyLastName mi = lastName)

    let resolveOverridingMethod targetType (virtualMethod : MethodInfo) =
        assert virtualMethod.IsVirtual
        match virtualMethod.DeclaringType with
        | i when i.IsInterface -> resolveInterfaceOverride targetType virtualMethod
        | _ ->
            let rec resolve targetType =
                assert(targetType <> null)
                if targetType = virtualMethod.DeclaringType then virtualMethod
                else
                    let (|||) = Microsoft.FSharp.Core.Operators.(|||)
                    let bindingFlags = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.InvokeMethod ||| BindingFlags.DeclaredOnly
                    let declaredMethods = targetType.GetMethods(bindingFlags)
                    let resolvedMethod = declaredMethods |> Seq.tryFind (isOverrideOf virtualMethod)
                    match resolvedMethod with
                    | Some resolvedMethod -> resolvedMethod
                    | None -> resolve targetType.BaseType
            resolve targetType


    // ----------------------------------- Creating objects ----------------------------------

    let createObject (t : Type) =
        match t with
        | _ when t = typeof<String> -> String.Empty :> obj
        | _ when TypeUtils.isNullable t -> null
        | _ when t.IsArray -> Array.CreateInstance(typeof<obj>, 1)
        | _ -> System.Runtime.Serialization.FormatterServices.GetUninitializedObject t

    let defaultOf (t : Type) =
        if t.IsValueType && Nullable.GetUnderlyingType(t) = null && not t.ContainsGenericParameters
            then Activator.CreateInstance t
            else null

    // --------------------------------- Substitute generics ---------------------------------

    let private substituteMethod methodType (m : MethodBase) getMethods =
        let method = getMethods methodType |> Array.tryFind (fun (x : #MethodBase) -> x.MetadataToken = m.MetadataToken)
        match method with
        | Some x -> x
        | None -> internalfailf "unable to find method %s token" m.Name

    let private substituteMethodInfo methodType (mi : MethodInfo) groundK genericK =
        let getMethods (t : Type) = getAllMethods t
        let substituteGeneric (mi : MethodInfo) =
            let args = mi.GetGenericArguments()
            let genericMethod = mi.GetGenericMethodDefinition()
            let mi = substituteMethod methodType genericMethod getMethods
            genericK mi args
        if mi.IsGenericMethod then substituteGeneric mi
        else groundK (substituteMethod methodType mi getMethods :> MethodBase)

    let private substituteCtorInfo methodType ci k =
        let getCtor (t : Type) = t.GetConstructors(allBindingFlags)
        k (substituteMethod methodType ci getCtor :> MethodBase)

    let private substituteMethodBase<'a> methodType (m : MethodBase) (groundK : MethodBase -> 'a) genericK =
        match m with
        | _ when not <| isGenericOrDeclaredInGenericType m -> groundK m
        | :? MethodInfo as mi ->
            substituteMethodInfo methodType mi groundK genericK
        | :? ConstructorInfo as ci ->
            substituteCtorInfo methodType ci groundK
        | _ -> __unreachable__()

    // --------------------------------- Generalization ---------------------------------

    let getGenericTypeDefinition (typ : Type) =
        if typ.IsGenericType then
            let args = typ.GetGenericArguments()
            let genericType = typ.GetGenericTypeDefinition()
            let parameters = genericType.GetGenericArguments()
            genericType, args, parameters
        else typ, [||], [||]

    let generalizeMethodBase (methodBase : MethodBase) =
        let genericType, tArgs, tParams = getGenericTypeDefinition methodBase.DeclaringType
        let genericCase m args = m :> MethodBase, args, m.GetGenericArguments()
        let groundCase m = m, [||], [||]
        let genericMethod, mArgs, mParams = substituteMethodBase genericType methodBase groundCase genericCase
        let genericArgs = Array.append mArgs tArgs
        let genericDefs = Array.append mParams tParams
        genericMethod, genericArgs, genericDefs

    let fullGenericMethodName (methodBase : MethodBase) =
        let genericMethod = generalizeMethodBase methodBase |> fst3
        getFullMethodName genericMethod

    // --------------------------------- Concretization ---------------------------------

    let rec concretizeType (subst : Type -> Type) (typ : Type) =
        if typ.IsGenericParameter then subst typ
        elif typ.IsGenericType then
            let args = typ.GetGenericArguments()
            let args' = args |> Array.map (concretizeType subst)
            if args = args' then typ
            else
                typ.GetGenericTypeDefinition().MakeGenericType(args')
        else typ

    let concretizeMethodBase (m : MethodBase) (subst : Type -> Type) =
        let concreteType = concretizeType subst m.DeclaringType
        let substArgsIntoMethod (mi : MethodInfo) args =
            mi.MakeGenericMethod(args |> Array.map subst) :> MethodBase
        substituteMethodBase concreteType m id substArgsIntoMethod

    let concretizeParameter (p : ParameterInfo) (subst : Type -> Type) =
        assert(p.Member :? MethodBase)
        let method = concretizeMethodBase (p.Member :?> MethodBase) subst
        method.GetParameters() |> Array.find (fun pi -> pi.Name = p.Name)

    let concretizeField (f : fieldId) (subst : Type -> Type) =
        let declaringType = concretizeType subst f.declaringType
        {declaringType = declaringType; name = f.name; typ = concretizeType subst f.typ}

    let public methodToString (m : MethodBase) =
        let hasThis = hasThis m
        let returnsSomething = hasNonVoidResult m
        let argsCount = m.GetParameters().Length
        if m.DeclaringType = null then m.Name
        else sprintf "%s %s.%s(%s)" (if returnsSomething then "nonvoid" else "void") m.DeclaringType.Name m.Name (if hasThis then sprintf "%d+1" argsCount else toString argsCount)

    let concretizeTypeParameters (typ : Type) (values : Type[]) =
        if typ.IsGenericType then
            assert(values.Length = typ.GetGenericArguments().Length)
            typ.MakeGenericType(values)
        else
            assert(values.Length = 0)
            typ

    let concretizeMethodParameters (declaringType : Type) (method : MethodBase) (values : Type[]) =
        match method with
        | :? MethodInfo as mi ->
            let method = declaringType.GetMethods() |> Array.find (fun x -> x.MetadataToken = mi.MetadataToken)
            if method.IsGenericMethod then
                assert(values.Length = method.GetGenericArguments().Length)
                method.MakeGenericMethod(values) :> MethodBase
            else
                method :> MethodBase
        | :? ConstructorInfo as ci ->
            assert(values.Length = 0)
            declaringType.GetConstructors() |> Array.find (fun x -> x.MetadataToken = ci.MetadataToken) :> MethodBase
        | _ -> __notImplemented__()

    // --------------------------------- Fields ---------------------------------

    // TODO: add cache: map from wrapped field to unwrapped

    let wrapField (field : FieldInfo) =
        {declaringType = field.DeclaringType; name = field.Name; typ = field.FieldType}

    let getFieldInfo (field : fieldId) =
        let result = field.declaringType.GetField(field.name, allBindingFlags)
        if result <> null then result
        else field.declaringType.GetRuntimeField(field.name)

    let rec private retrieveFields isStatic f (t : Type) =
        let flags = if isStatic then staticBindingFlags else instanceBindingFlags
        let fields = t.GetFields(flags) |> Array.sortBy (fun field -> field.Name)
        let ourFields = f fields
        if isStatic || t.BaseType = null then ourFields
        else Array.append (retrieveFields false f t.BaseType) ourFields

    let retrieveNonStaticFields t = retrieveFields false id t

    let fieldsOf isStatic (t : Type) =
        let extractFieldInfo (field : FieldInfo) =
            // Events may appear at this point. Filtering them out...
            if TypeUtils.isSubtypeOrEqual field.FieldType typeof<MulticastDelegate> then None
            else Some (wrapField field, field)
        retrieveFields isStatic (FSharp.Collections.Array.choose extractFieldInfo) t

    let fieldIntersects (field : fieldId) =
        let fieldInfo = getFieldInfo field
        let offset = CSharpUtils.LayoutUtils.GetFieldOffset fieldInfo
        let size = TypeUtils.internalSizeOf fieldInfo.FieldType
        let intersects o s = o + s > offset && o < offset + size
        let fields = fieldsOf false field.declaringType
        let checkIntersects (_, fieldInfo : FieldInfo) =
            let o = CSharpUtils.LayoutUtils.GetFieldOffset fieldInfo
            let s = TypeUtils.internalSizeOf fieldInfo.FieldType
            intersects o s
        let intersectingFields = Array.filter checkIntersects fields
        Array.length intersectingFields > 1

    // Returns pair (valueFieldInfo, hasValueFieldInfo)
    let fieldsOfNullable typ =
        let fs = fieldsOf false typ
        match fs with
        | [|(f1, _); (f2, _)|] when f1.name.Contains("value", StringComparison.OrdinalIgnoreCase) && f2.name.Contains("hasValue", StringComparison.OrdinalIgnoreCase) -> f1, f2
        | [|(f1, _); (f2, _)|] when f1.name.Contains("hasValue", StringComparison.OrdinalIgnoreCase) && f2.name.Contains("value", StringComparison.OrdinalIgnoreCase) -> f2, f1
        | _ -> internalfailf "%O has unexpected fields {%O}! Probably your .NET implementation is not supported :(" (getFullTypeName typ) (fs |> Array.map (fun (f, _) -> f.name) |> join ", ")

    let stringLengthField, stringFirstCharField =
        let fs = fieldsOf false typeof<string>
        match fs with
        | [|(f1, _); (f2, _)|] when f1.name.Contains("length", StringComparison.OrdinalIgnoreCase) && f2.name.Contains("firstChar", StringComparison.OrdinalIgnoreCase) -> f1, f2
        | [|(f1, _); (f2, _)|] when f1.name.Contains("firstChar", StringComparison.OrdinalIgnoreCase) && f2.name.Contains("length", StringComparison.OrdinalIgnoreCase) -> f2, f1
        | _ -> internalfailf "System.String has unexpected fields {%O}! Probably your .NET implementation is not supported :(" (fs |> Array.map (fun (f, _) -> f.name) |> join ", ")

    let exceptionMessageField =
        {declaringType = typeof<Exception>; name = "_message"; typ = typeof<string>}

    let emptyStringField =
        let fs = fieldsOf true typeof<string>
        match fs |> Array.tryFind (fun (f, _) -> f.name.Contains("empty", StringComparison.OrdinalIgnoreCase)) with
        | Some(f, _) -> f
        | None -> internalfailf "System.String has unexpected static fields {%O}! Probably your .NET implementation is not supported :(" (fs |> Array.map (fun (f, _) -> f.name) |> join ", ")

    let getFieldOffset fieldId =
        if fieldId = stringFirstCharField then 0
        else getFieldInfo fieldId |> CSharpUtils.LayoutUtils.GetFieldOffset

    let private cachedTypes = Dictionary<Type, bool>()

    let rec private isReferenceOrContainsReferencesHelper (t : Type) =
        if t.IsValueType |> not then true
        else
            t.GetFields(allBindingFlags)
            |> Array.exists (fun field -> field.FieldType = t || isReferenceOrContainsReferencesHelper field.FieldType)

    let isReferenceOrContainsReferences (t : Type) =
        let result = ref false
        if cachedTypes.TryGetValue(t, result) then result.Value
        else
            let result = isReferenceOrContainsReferencesHelper t
            cachedTypes.Add(t, result)
            result
