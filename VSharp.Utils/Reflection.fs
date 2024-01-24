namespace VSharp

open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open System.Runtime.InteropServices

[<CustomEquality; CustomComparison>]
type methodDescriptor = {
    methodHandle : nativeint
    declaringTypeVarHandles : nativeint array
    methodVarHandles : nativeint array
    typeHandle : nativeint
}
with
    override x.GetHashCode() =
        HashCode.Combine(x.methodHandle, hash x.declaringTypeVarHandles, hash x.methodVarHandles, x.typeHandle)

    override x.Equals(another) =
        match another with
        | :? methodDescriptor as d -> (x :> IComparable).CompareTo d = 0
        | _ -> false

    interface IComparable with
        override x.CompareTo y =
            match y with
            | :? methodDescriptor as y ->
                compare
                    (x.methodHandle, x.declaringTypeVarHandles, x.methodVarHandles, x.typeHandle)
                    (y.methodHandle, y.declaringTypeVarHandles, y.methodVarHandles, y.typeHandle)
            | _ -> -1

module public Reflection =

    // ----------------------------- Binding Flags ------------------------------

    let staticBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public
    let instanceBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public
    let instanceNonPublicBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        BindingFlags.Instance ||| BindingFlags.NonPublic
    let instancePublicBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        BindingFlags.Instance ||| BindingFlags.Public
    let allBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        staticBindingFlags ||| instanceBindingFlags

    // ------------------------------- Assemblies -------------------------------

    let loadAssembly (assemblyName : string) =
        let assemblies = AssemblyManager.GetAssemblies()
        let dynamicAssemblies = assemblies |> Seq.filter (fun a -> a.IsDynamic)
        let dynamicOption = dynamicAssemblies |> Seq.tryFind (fun a -> a.FullName.Contains(assemblyName))
        match dynamicOption with
        | Some a -> a
        | None -> AssemblyManager.LoadFromAssemblyName assemblyName

    let mscorlibAssembly = typeof<int>.Assembly

    // --------------------------- Metadata Resolving ---------------------------

    let resolveModule (assemblyName : string) (moduleName : string) =
        let assembly =
            match AssemblyManager.GetAssemblies() |> Seq.tryFindBack (fun assembly -> assembly.FullName = assemblyName) with
            | Some assembly -> assembly
            | None ->
                try
                    AssemblyManager.LoadFromAssemblyName assemblyName
                with _ ->
                    AssemblyManager.LoadFromAssemblyPath moduleName
        assembly.Modules |> Seq.find (fun m -> m.FullyQualifiedName = moduleName)

    let resolveMethodBase (assemblyName : string) (moduleName : string) (token : int32) =
        let m = resolveModule assemblyName moduleName
        m.ResolveMethod(token)

    let resolveMethodBaseFromAssembly (assembly : Assembly) (moduleName : string) (token : int32) =
        let m =
            assembly.Modules
            |> Seq.find (fun m -> m.FullyQualifiedName = moduleName)
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

    let hasNonVoidResult m =
        getMethodReturnType m <> typeof<Void> && not m.IsConstructor

    let hasThis (m : MethodBase) = m.CallingConvention.HasFlag(CallingConventions.HasThis)

    let getFullTypeName (typ : Type) =
        if typ <> null then typ.ToString() else String.Empty

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
        let declaringType = methodBase.DeclaringType
        methodBase.IsGenericMethod || declaringType <> null && declaringType.IsGenericType

    let isStaticConstructor (m : MethodBase) =
        m.IsStatic && m.Name = ".cctor"

    let isExternalMethod (methodBase : MethodBase) =
        methodBase.Attributes.HasFlag(MethodAttributes.PinvokeImpl)

    let getAllMethods (t : Type) = t.GetMethods(allBindingFlags)

    let getAllConstructors (t : Type) = t.GetConstructors(allBindingFlags)

    // MethodInfo's GetGenericMethodDefinition erases ReflectedType, this function overcomes that
    let getGenericMethodDefinition (method : MethodInfo) =
        let reflectedType = method.ReflectedType
        let genericDefinition = method.GetGenericMethodDefinition()
        let foundMember = reflectedType.GetMemberWithSameMetadataDefinitionAs(genericDefinition)
        foundMember :?> MethodInfo

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
            let genericMethod = getGenericMethodDefinition mi
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
        if typ <> null && typ.IsGenericType then
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

    // --------------------------------- Methods --------------------------------

    let methodIsDynamic (m : MethodBase) =
        m.DeclaringType = null

    let getMethodDescriptor (m : MethodBase) =
        let reflectedType = m.ReflectedType
        let typeHandle =
            if reflectedType <> null then reflectedType.TypeHandle.Value
            else IntPtr.Zero
        let declaringTypeVars =
            if reflectedType <> null && reflectedType.IsGenericType then
                reflectedType.GetGenericArguments() |> Array.map (fun t -> t.TypeHandle.Value)
            else [||]
        let methodVars =
            if m.IsGenericMethod then m.GetGenericArguments() |> Array.map (fun t -> t.TypeHandle.Value)
            else [||]
        let methodHandle =
            if methodIsDynamic m then m.GetHashCode() |> nativeint
            else m.MethodHandle.Value
        {
            methodHandle = methodHandle
            declaringTypeVarHandles = declaringTypeVars
            methodVarHandles = methodVars
            typeHandle = typeHandle
        }

    let compareMethods (m1 : MethodBase) (m2 : MethodBase) =
        compare (getMethodDescriptor m1) (getMethodDescriptor m2)

    let getArrayMethods (arrayType : Type) =
        let methodsFromHelper = Type.GetType("System.SZArrayHelper") |> getAllMethods
        let makeSuitable (m : MethodInfo) =
            if m.IsGenericMethod then m.MakeGenericMethod(arrayType.GetElementType()) else m
        let concreteMethods = Array.map makeSuitable methodsFromHelper
        Array.concat [concreteMethods; getAllMethods typeof<Array>; getAllMethods arrayType]

    let private isOverrideWithCovarianceReturnType (sourceMethod : MethodInfo) (method : MethodInfo) =
        // Return type covariance case
        Attribute.IsDefined(method, typeof<System.Runtime.CompilerServices.PreserveBaseOverridesAttribute>) &&
        method.Name = sourceMethod.Name &&
            let sourceSig = sourceMethod.GetParameters()
            let targetSig = method.GetParameters()
            targetSig.Length = sourceSig.Length &&
            Array.forall2 (fun (p : ParameterInfo) (p' : ParameterInfo) -> p.ParameterType = p'.ParameterType) sourceSig targetSig

    let isOverrideOf (sourceMethod : MethodInfo) (method : MethodInfo) =
        sourceMethod.GetBaseDefinition() = method.GetBaseDefinition()
        || isOverrideWithCovarianceReturnType sourceMethod method

    let private createSignature (m : MethodInfo) =
        let onlyLastName =
            match m.Name.LastIndexOf('.') with
            | i when i < 0 -> m.Name
            | i -> m.Name.Substring(i + 1)
        if m.IsHideBySig then
            let args =
                m.GetParameters()
                |> Seq.map (fun p -> getFullTypeName p.ParameterType)
                |> join ","
            let genericArgs =
                // If type is 'System.SZArrayHelper' then generic arguments should not be added, because
                // 'SZArrayHelper' implements methods for all vector arrays in .NET and they don't have generics
                if m.IsGenericMethod && m.DeclaringType <> TypeUtils.szArrayHelper.Value then
                    m.GetGenericArguments()
                    |> Seq.map getFullTypeName
                    |> join ","
                else String.Empty
            let returnType = getMethodReturnType m |> toString
            returnType + onlyLastName + genericArgs + args
        else onlyLastName

    let resolveInterfaceOverride (targetType : Type) (interfaceMethod : MethodInfo) =
        let interfaceType = interfaceMethod.DeclaringType
        assert interfaceType.IsInterface
        if interfaceType = targetType then interfaceMethod
        else
            let genericMethod, _, _ = generalizeMethodBase interfaceMethod
            assert(genericMethod :? MethodInfo)
            let sign = createSignature (genericMethod :?> MethodInfo)
            let hasTargetSignature (mi : MethodInfo) =
                let genericMethod, _, _ = generalizeMethodBase mi
                assert(genericMethod :? MethodInfo)
                // TODO: try to check this without signatures
                createSignature (genericMethod :?> MethodInfo) = sign
            match targetType with
            | _ when targetType.IsArray -> getArrayMethods targetType |> Seq.find hasTargetSignature
            | _ when targetType.IsInterface -> getAllMethods targetType |> Seq.find hasTargetSignature
            | _ ->
                let interfaceMap = targetType.GetInterfaceMap(interfaceType)
                let targetMethodIndex = Array.findIndex hasTargetSignature interfaceMap.InterfaceMethods
                interfaceMap.TargetMethods[targetMethodIndex]

    let private virtualBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.InvokeMethod

    let isNewSlot (m : MethodInfo) =
        m.Attributes.HasFlag(MethodAttributes.NewSlot)

    let blocksOverride virtualMethod (m : MethodInfo) =
        m.IsFinal || not m.IsVirtual
        || isNewSlot m && not (isOverrideWithCovarianceReturnType virtualMethod m)

    // Finds last type that can override 'virtualMethod' starting from 'targetType'
    // If it's free to override in derived classes of 'targetType', result will be 'targetType'
    // If some type 't' in the hierarchy defines same method or adds new slot for it, result will be base type of 't'
    // If in some type 't' in the hierarchy this method marked 'final', result will be 't'
    let lastCanOverrideType (targetType : Type) (virtualMethod : MethodInfo) =
        match virtualMethod.DeclaringType with
        | t when not virtualMethod.IsVirtual -> t
        | i when i.IsInterface && TypeUtils.typeImplementsInterface targetType i -> targetType
        | i when i.IsInterface -> i
        | t when targetType.IsAssignableTo(t) |> not -> t
        | declaringType ->
            let createHierarchy (t : Type) =
                // TODO: care about generics (GetGenericTypeDefinition) ?
                if t <> declaringType && t <> null then Some (t, t.BaseType)
                else None
            let hierarchy = List.unfold createHierarchy targetType
            let sign = createSignature virtualMethod
            let mutable newSlot = false
            let cancelsOverride (t : Type) =
                let matchedMethods =
                    t.GetMethods(virtualBindingFlags)
                    |> Array.filter (fun m -> createSignature m = sign)
                let canNotOverride (m : MethodInfo) =
                    let blocks = blocksOverride virtualMethod m
                    if blocks && (isNewSlot m || not m.IsVirtual) then
                        newSlot <- true
                    blocks
                not <| Array.isEmpty matchedMethods && Array.forall canNotOverride matchedMethods
            match List.tryFindBack cancelsOverride hierarchy with
            | Some t when newSlot -> t.BaseType
            | Some t -> t
            | None -> targetType

    let canOverrideMethod targetType (virtualMethod : MethodInfo) =
        lastCanOverrideType targetType virtualMethod = targetType

    // TODO: unify with 'lastOverrideType'
    let resolveOverridingMethod targetType (virtualMethod : MethodInfo) =
        assert virtualMethod.IsVirtual
        let isGeneric = virtualMethod.IsGenericMethod
        let genericDefinition =
            if isGeneric then
                getGenericMethodDefinition virtualMethod
            else
                virtualMethod
        let resolved =
            match genericDefinition.DeclaringType with
            | i when i.IsInterface -> resolveInterfaceOverride targetType genericDefinition
            | _ ->
                assert(targetType <> null)
                if targetType = genericDefinition.DeclaringType then genericDefinition
                else
                    let declaredMethods = targetType.GetMethods(virtualBindingFlags)
                    let resolvedMethod = declaredMethods |> Seq.tryFind (isOverrideOf genericDefinition)
                    match resolvedMethod with
                    | Some resolvedMethod -> resolvedMethod
                    | None -> genericDefinition
        if isGeneric then
            let genericArgs = virtualMethod.GetGenericArguments()
            let resolvedDefinition = getGenericMethodDefinition resolved
            resolvedDefinition.MakeGenericMethod(genericArgs)
        else
            resolved

    let typeImplementsMethod targetType (virtualMethod : MethodInfo) =
        let method = resolveOverridingMethod targetType virtualMethod
        not targetType.IsAbstract && method.ReflectedType = targetType

    let hasByRefLikes (method : MethodInfo) =
        method.DeclaringType <> null && method.DeclaringType.IsByRefLike ||
            method.GetParameters() |> Seq.exists (fun pi -> pi.ParameterType.IsByRefLike) ||
            method.ReturnType.IsByRefLike;

    let private delegatesModule =
        lazy(
            let dynamicAssemblyName = $"VSharpCombinedDelegates.{Guid.NewGuid()}"
            let assemblyBuilder = AssemblyManager.DefineDynamicAssembly(AssemblyName dynamicAssemblyName, AssemblyBuilderAccess.Run)
            assemblyBuilder.DefineDynamicModule dynamicAssemblyName
        )

    let private delegatesType() =
        let typeName = $"CombinedDelegates.{Guid.NewGuid()}"
        let flags =
            TypeAttributes.Class ||| TypeAttributes.NotPublic
            ||| TypeAttributes.Sealed ||| TypeAttributes.Abstract
        delegatesModule.Value.DefineType(typeName, flags)

    let createCombinedDelegate (methods : MethodInfo seq) (argTypes : Type seq) =
        let methodsCount = Seq.length methods
        assert(methodsCount > 1)
        let argsCount = Seq.length argTypes
        let args = Array.append (Array.ofSeq argTypes) (Array.init methodsCount (fun _ -> typeof<obj>))
        let returnType = (Seq.last methods).ReturnType
        let declaringType = delegatesType()
        let methodName = "CombinedDelegate"
        let flags = MethodAttributes.Static ||| MethodAttributes.Private ||| MethodAttributes.HideBySig
        let methodBuilder = declaringType.DefineMethod(methodName, flags, returnType, args)
        let il = methodBuilder.GetILGenerator()
        let mutable i = 0
        for m in methods do
            il.Emit(OpCodes.Ldarg, argsCount + i)
            for j = 0 to argsCount - 1 do
                il.Emit(OpCodes.Ldarg, j)
            il.Emit(OpCodes.Callvirt, m)
            i <- i + 1
            // Popping each result, except last
            if i <> methodsCount && returnType <> typeof<Void> then
                il.Emit(OpCodes.Pop)
        il.Emit(OpCodes.Ret)
        let t = declaringType.CreateType()
        t.GetMethod(methodName, BindingFlags.Static ||| BindingFlags.NonPublic)

    // ----------------------------------- Creating objects ----------------------------------

    let createObject (t : Type) =
        assert(not t.IsByRefLike)
        match t with
        | _ when t = typeof<String> -> String.Empty :> obj
        | _ when TypeUtils.isNullable t -> null
        | _ when t.IsArray -> Array.CreateInstance(typeof<obj>, 1)
        | _ when t.ContainsGenericParameters -> internalfail $"Creating object of open generic type {t}"
        | _ -> System.Runtime.Serialization.FormatterServices.GetUninitializedObject t

    let defaultOf (t : Type) =
        assert(not t.IsByRefLike)
        if t.IsValueType && Nullable.GetUnderlyingType(t) = null && not t.ContainsGenericParameters
            then Activator.CreateInstance t
            else null

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
            let method =
                declaringType.GetMethods(allBindingFlags)
                |> Array.find (fun x -> x.MetadataToken = mi.MetadataToken)
            if method.IsGenericMethod then
                assert(values.Length = method.GetGenericArguments().Length)
                method.MakeGenericMethod(values) :> MethodBase
            else
                method :> MethodBase
        | :? ConstructorInfo as ci ->
            assert(values.Length = 0)
            declaringType.GetConstructors(allBindingFlags)
            |> Array.find (fun x -> x.MetadataToken = ci.MetadataToken) :> MethodBase
        | _ -> __notImplemented__()

    // --------------------------------- Fields ---------------------------------

    // TODO: add cache: map from wrapped field to unwrapped

    let wrapField (field : FieldInfo) =
        { declaringType = field.DeclaringType; name = field.Name; typ = field.FieldType }

    let getFieldInfo (field : fieldId) =
        let result = field.declaringType.GetField(field.name, allBindingFlags)
        if result <> null then result
        else field.declaringType.GetRuntimeField(field.name)

    let private retrieveFields isStatic (t : Type) : FieldInfo[] =
        let flags = if isStatic then staticBindingFlags else instanceBindingFlags
        let fields = Dictionary<Type * Type * string, FieldInfo>()
        let mutable current = t
        while current <> null do
            for f in current.GetFields(flags) do
                fields[(f.DeclaringType, f.FieldType, f.Name)] <- f
            current <- current.BaseType
        Seq.toArray fields.Values
        |> Array.sortBy (fun field -> $"{field.Name}{field.DeclaringType}")

    let retrieveNonStaticFields t = retrieveFields false t

    let fieldsOf isStatic (t : Type) =
        let fields = retrieveFields isStatic t
        let extractFieldInfo (field : FieldInfo) = wrapField field, field
        FSharp.Collections.Array.map extractFieldInfo fields

    let delegateTargetField = lazy(
            let field = typeof<Delegate>.GetField("_target", instanceBindingFlags)
            assert(field <> null)
            field
        )

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

    let private reinterpretValueTypeAsByteArray (value : obj) size =
        let rawData = Array.create size Byte.MinValue
        let handle = GCHandle.Alloc(rawData, GCHandleType.Pinned)
        try
            Marshal.StructureToPtr(value, handle.AddrOfPinnedObject(), false)
        finally
            handle.Free()
        rawData

    let byteArrayFromField (fieldInfo : FieldInfo) =
        let fieldValue : obj = fieldInfo.GetValue null
        let size = TypeUtils.internalSizeOf fieldInfo.FieldType
        reinterpretValueTypeAsByteArray fieldValue size

    let private reinterpretValueTypeAsArray (value : obj) (elemType : Type) (size : int) =
        let rawData = Array.CreateInstance(elemType, size)
        let handle = GCHandle.Alloc(rawData, GCHandleType.Pinned)
        try
            Marshal.StructureToPtr(value, handle.AddrOfPinnedObject(), false)
        finally
            handle.Free()
        rawData

    let arrayFromField (fieldInfo : FieldInfo) (elemType : Type) =
        if elemType = typeof<byte> then
            byteArrayFromField fieldInfo :> Array
        else
            let fieldValue : obj = fieldInfo.GetValue null
            let byteSize = TypeUtils.internalSizeOf fieldInfo.FieldType
            let elemSize = TypeUtils.internalSizeOf elemType
            assert(byteSize % elemSize = 0)
            let size = byteSize / elemSize
            reinterpretValueTypeAsArray fieldValue elemType size

    // ------------------------------ Layout Utils ------------------------------

    let getFieldOffset field =
        if wrapField field = stringFirstCharField then 0
        else CSharpUtils.LayoutUtils.GetFieldOffset field

    let getFieldIdOffset fieldId =
        if fieldId = stringFirstCharField then 0
        else getFieldInfo fieldId |> CSharpUtils.LayoutUtils.GetFieldOffset

    let blockSize (t : Type) =
        if t.IsValueType then TypeUtils.internalSizeOf t
        else CSharpUtils.LayoutUtils.ClassSize t

    let arrayElementsOffset = CSharpUtils.LayoutUtils.ArrayElementsOffset

    let stringElementsOffset = CSharpUtils.LayoutUtils.StringElementsOffset

    let fieldIntersects (fieldId : fieldId) =
        let fieldInfo = getFieldInfo fieldId
        let fieldType = fieldInfo.FieldType
        if fieldType.ContainsGenericParameters then false
        else
            let offset = getFieldIdOffset fieldId
            let size = TypeUtils.internalSizeOf fieldType
            let intersects o s = o + s > offset && o < offset + size
            let fields = fieldsOf false fieldId.declaringType
            let checkIntersects (_, fieldInfo : FieldInfo) =
                let o = CSharpUtils.LayoutUtils.GetFieldOffset fieldInfo
                let s = TypeUtils.internalSizeOf fieldInfo.FieldType
                intersects o s
            let intersectingFields = Array.filter checkIntersects fields
            Array.length intersectingFields > 1

    // -------------------------------- Types --------------------------------

    let private cachedTypes = Dictionary<Type, bool>()

    let private hasNoRefs (t : Type) =
        t.IsPrimitive || t.IsEnum || t.IsPointer
        || t = typeof<IntPtr> || t = typeof<UIntPtr>

    let isReferenceOrContainsReferences (t : Type) =
        let result = ref false
        if cachedTypes.TryGetValue(t, result) then result.Value
        else
            let queue = Queue<Type>()
            let cache = HashSet<Type>()
            queue.Enqueue t
            let mutable result = false
            while (queue.Count > 0 && not result) do
                let t = queue.Dequeue()
                if not (hasNoRefs t) && cache.Add t then
                    if not t.IsValueType then
                        result <- true
                    else
                        for field in t.GetFields(allBindingFlags) do
                            queue.Enqueue field.FieldType
            cachedTypes.Add(t, result)
            result

    let isBuiltInType (t : Type) =
        let builtInAssembly = mscorlibAssembly
        t.Assembly = builtInAssembly

    let rec isUnmanaged (t : Type) =
        t.IsPrimitive || t = typeof<decimal> || t.IsEnum || t.IsPointer
        || t.IsValueType && fieldsOf false t |> Array.forall (fun (f, _) -> isUnmanaged f.typ)

    let hasNonPublicAbstractMethods (t : Type) =
        t.GetMethods(instanceNonPublicBindingFlags) |> Seq.exists (fun m -> m.IsAbstract)

    let isInstanceOfType (typeOfObj : Type) =
        typeOfObj = typeof<Type> || typeOfObj = TypeUtils.systemRuntimeType

    // -------------------------------- Types --------------------------------

    let private typeAttributes = lazy (
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

    let private methodAttributes = lazy (
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.Static)

    let dynamicModule = lazy (
        // Defining dynamic assembly
        let dynamicAssemblyName = $"AspNetSupport.{Guid.NewGuid()}"
        let assemblyBuilder = AssemblyManager.DefineDynamicAssembly(AssemblyName dynamicAssemblyName, AssemblyBuilderAccess.Run)
        assemblyBuilder.DefineDynamicModule dynamicAssemblyName)

    let createAspNetStartMethod (requestDelegateType : Type) (iHttpContextFactoryType : Type) =
        // Getting all needed types and assemblies
        let assemblies = AssemblyManager.GetAssemblies()
        let httpAssembly =
            assemblies |> Seq.find (fun a -> a.FullName.Contains("Microsoft.AspNetCore.Http,"))
        let httpAssemblyTypes = httpAssembly.GetExportedTypes()
        let featuresAssembly =
            assemblies |> Seq.find (fun a -> a.FullName.Contains("Microsoft.Extensions.Features,"))
        let featuresAssemblyTypes = featuresAssembly.GetTypes()
        let abstractionsAssembly = requestDelegateType.Assembly
        let abstractionsAssemblyTypes = abstractionsAssembly.GetExportedTypes()
        let featureCollectionType =
            featuresAssemblyTypes |> Array.find (fun t -> t.Name = "FeatureCollection")
        let httpContextType =
            abstractionsAssemblyTypes |> Array.find (fun t -> t.Name = "HttpContext")
        let httpRequestFeatureType =
            httpAssemblyTypes |> Array.find (fun t -> t.Name = "HttpRequestFeature")
        let iHttpRequestFeatureType =
            let interfaces = httpRequestFeatureType.GetInterfaces()
            assert(Array.length interfaces = 1)
            interfaces[0]
        let httpResponseFeatureType =
            httpAssemblyTypes |> Array.find (fun t -> t.Name = "HttpResponseFeature")
        let iHttpResponseFeatureType =
            let interfaces = httpResponseFeatureType.GetInterfaces()
            assert(Array.length interfaces = 1)
            interfaces[0]
        let streamResponseBodyFeatureType =
            httpAssemblyTypes |> Array.find (fun t -> t.Name = "StreamResponseBodyFeature")
        let iHttpResponseBodyFeatureType =
            let interfaces = streamResponseBodyFeatureType.GetInterfaces()
            assert(Array.length interfaces = 1)
            interfaces[0]
        let memoryStreamType = typeof<System.IO.MemoryStream>
        let streamType = typeof<System.IO.Stream>
        let streamWriterType = typeof<System.IO.StreamWriter>

        let typeName = $"AspNetStart{Guid.NewGuid()}"
        let typeBuilder = dynamicModule.Value.DefineType(typeName, typeAttributes.Value)
        let methodName = $"AspNetStartMethod{Guid.NewGuid()}"
        let methodBuilder = typeBuilder.DefineMethod(methodName, methodAttributes.Value)
        methodBuilder.SetReturnType typeof<Void>
        // Setting arguments
        methodBuilder.SetParameters([|
            // RequestDelegate
            requestDelegateType
            // IHttpContextFactory
            iHttpContextFactoryType
            // Path, Method, Body
            typeof<string>; typeof<string>; typeof<string>
        |])
        let ilGenerator = methodBuilder.GetILGenerator()

        // Declaring local variables
        let memoryStreamLocal = ilGenerator.DeclareLocal(memoryStreamType)
        let streamWriterLocal = ilGenerator.DeclareLocal(streamWriterType)
        let featureCollectionLocal = ilGenerator.DeclareLocal(featureCollectionType)
        let httpRequestFeatureLocal = ilGenerator.DeclareLocal(httpRequestFeatureType)
        let httpResponseFeatureLocal = ilGenerator.DeclareLocal(httpResponseFeatureType)
        let httpResponseBodyFeatureLocal = ilGenerator.DeclareLocal(streamResponseBodyFeatureType)
        let contextLocal = ilGenerator.DeclareLocal(httpContextType)

        // var memoryStreamLocal = new MemoryStream();
        let memoryStreamCtor = memoryStreamType.GetConstructor(Array.empty)
        ilGenerator.Emit(OpCodes.Newobj, memoryStreamCtor)
        ilGenerator.Emit(OpCodes.Stloc, memoryStreamLocal)

        // var streamWriterLocal = new StreamWriter(stream);
        ilGenerator.Emit(OpCodes.Ldloc, memoryStreamLocal)
        let streamWriterCtor = streamWriterType.GetConstructor(Array.singleton memoryStreamType)
        ilGenerator.Emit(OpCodes.Newobj, streamWriterCtor)
        ilGenerator.Emit(OpCodes.Stloc, streamWriterLocal)

        // streamWriterLocal.Write(body);
        ilGenerator.Emit(OpCodes.Ldloc, streamWriterLocal)
        let writeMethod = streamWriterType.GetMethod("Write", instancePublicBindingFlags, Array.singleton typeof<string>)
        ilGenerator.Emit(OpCodes.Ldarg, 4)
        ilGenerator.Emit(OpCodes.Callvirt, writeMethod)

        // streamWriterLocal.Flush();
        ilGenerator.Emit(OpCodes.Ldloc, streamWriterLocal)
        let flushMethod = streamWriterType.GetMethod("Flush", instancePublicBindingFlags, Array.empty)
        ilGenerator.Emit(OpCodes.Callvirt, flushMethod)

        // memoryStreamLocal.Position = 0;
        ilGenerator.Emit(OpCodes.Ldloc, memoryStreamLocal)
        ilGenerator.Emit(OpCodes.Ldc_I4_0)
        ilGenerator.Emit(OpCodes.Conv_I8)
        let streamPositionProperty = memoryStreamType.GetProperty("Position", instancePublicBindingFlags)
        let streamPositionSetMethod = streamPositionProperty.SetMethod
        ilGenerator.Emit(OpCodes.Callvirt, streamPositionSetMethod)

        // var featureCollectionLocal = new FeatureCollection();
        let featureCollectionCtor = featureCollectionType.GetConstructor(Array.empty)
        ilGenerator.Emit(OpCodes.Newobj, featureCollectionCtor)
        ilGenerator.Emit(OpCodes.Stloc, featureCollectionLocal)

        // var httpRequestFeatureLocal = new HttpRequestFeature();
        let httpRequestFeatureCtor = httpRequestFeatureType.GetConstructor(Array.empty)
        ilGenerator.Emit(OpCodes.Newobj, httpRequestFeatureCtor)
        ilGenerator.Emit(OpCodes.Stloc, httpRequestFeatureLocal)

        // httpRequestFeatureLocal.Path = path;
        ilGenerator.Emit(OpCodes.Ldloc, httpRequestFeatureLocal)
        let requestPathProperty = httpRequestFeatureType.GetProperty("Path", instancePublicBindingFlags)
        let requestPathSetMethod = requestPathProperty.SetMethod
        ilGenerator.Emit(OpCodes.Ldarg_2)
        ilGenerator.Emit(OpCodes.Callvirt, requestPathSetMethod)

        // httpRequestFeatureLocal.Method = method;
        ilGenerator.Emit(OpCodes.Ldloc, httpRequestFeatureLocal)
        let requestMethodProperty = httpRequestFeatureType.GetProperty("Method", instancePublicBindingFlags)
        let requestMethodSetMethod = requestMethodProperty.SetMethod
        ilGenerator.Emit(OpCodes.Ldarg_3)
        ilGenerator.Emit(OpCodes.Callvirt, requestMethodSetMethod)

        // httpRequestFeatureLocal.Body = stream;
        ilGenerator.Emit(OpCodes.Ldloc, httpRequestFeatureLocal)
        let requestBodyProperty = httpRequestFeatureType.GetProperty("Body", instancePublicBindingFlags)
        let requestBodySetMethod = requestBodyProperty.SetMethod
        ilGenerator.Emit(OpCodes.Ldloc, memoryStreamLocal)
        ilGenerator.Emit(OpCodes.Callvirt, requestBodySetMethod)

        // featureCollectionLocal.Set<IHttpRequestFeature>(httpRequestFeatureLocal);
        let featuresSetGenericMethod = featureCollectionType.GetMethod("Set", instancePublicBindingFlags)
        assert featuresSetGenericMethod.IsGenericMethod
        let featuresSetMethod = featuresSetGenericMethod.MakeGenericMethod(iHttpRequestFeatureType)
        ilGenerator.Emit(OpCodes.Ldloc, featureCollectionLocal)
        ilGenerator.Emit(OpCodes.Ldloc, httpRequestFeatureLocal)
        ilGenerator.Emit(OpCodes.Callvirt, featuresSetMethod)

        // var httpResponseFeatureLocal = new HttpResponseFeature();
        let httpResponseFeatureCtor = httpResponseFeatureType.GetConstructor(Array.empty)
        ilGenerator.Emit(OpCodes.Newobj, httpResponseFeatureCtor)
        ilGenerator.Emit(OpCodes.Stloc, httpResponseFeatureLocal)

        // memoryStreamLocal = new MemoryStream();
        ilGenerator.Emit(OpCodes.Newobj, memoryStreamCtor)
        ilGenerator.Emit(OpCodes.Stloc, memoryStreamLocal)

        // httpResponseFeatureLocal.Body = memoryStreamLocal;
        ilGenerator.Emit(OpCodes.Ldloc, httpResponseFeatureLocal)
        let responseBodyProperty = httpResponseFeatureType.GetProperty("Body", instancePublicBindingFlags)
        let responseBodySetMethod = responseBodyProperty.SetMethod
        ilGenerator.Emit(OpCodes.Ldloc, memoryStreamLocal)
        ilGenerator.Emit(OpCodes.Callvirt, responseBodySetMethod)

        // httpResponseBodyFeatureLocal = new StreamResponseBodyFeature(memoryStreamLocal);
        let streamResponseBodyFeatureCtor = streamResponseBodyFeatureType.GetConstructor(Array.singleton streamType)
        assert(streamResponseBodyFeatureCtor <> null)
        ilGenerator.Emit(OpCodes.Ldloc, memoryStreamLocal)
        ilGenerator.Emit(OpCodes.Newobj, streamResponseBodyFeatureCtor)
        ilGenerator.Emit(OpCodes.Stloc, httpResponseBodyFeatureLocal)

        // featureCollectionLocal.Set<IHttpResponseFeature>(httpResponseFeatureLocal);
        let featuresSetMethod = featuresSetGenericMethod.MakeGenericMethod(iHttpResponseFeatureType)
        ilGenerator.Emit(OpCodes.Ldloc, featureCollectionLocal)
        ilGenerator.Emit(OpCodes.Ldloc, httpResponseFeatureLocal)
        ilGenerator.Emit(OpCodes.Callvirt, featuresSetMethod)

        // featureCollectionLocal.Set<IHttpResponseBodyFeature>(httpResponseBodyFeatureLocal);
        let featuresSetMethod = featuresSetGenericMethod.MakeGenericMethod(iHttpResponseBodyFeatureType)
        ilGenerator.Emit(OpCodes.Ldloc, featureCollectionLocal)
        ilGenerator.Emit(OpCodes.Ldloc, httpResponseBodyFeatureLocal)
        ilGenerator.Emit(OpCodes.Callvirt, featuresSetMethod)

        // defaultContextLocal = iHttpContextFactory.Create(features);
        let createMethod = iHttpContextFactoryType.GetMethod("Create", instancePublicBindingFlags)
        assert(createMethod <> null)
        ilGenerator.Emit(OpCodes.Ldarg_1)
        ilGenerator.Emit(OpCodes.Ldloc, featureCollectionLocal)
        ilGenerator.Emit(OpCodes.Callvirt, createMethod)
        ilGenerator.Emit(OpCodes.Stloc, contextLocal)

        // app.Invoke(defaultContextLocal).Wait();
        // Calling 'Invoke' method
        let invokeMethod = requestDelegateType.GetMethod("Invoke", instanceBindingFlags)
        assert(invokeMethod <> null)
        ilGenerator.Emit(OpCodes.Ldarg_0)
        ilGenerator.Emit(OpCodes.Ldloc, contextLocal)
        ilGenerator.Emit(OpCodes.Callvirt, invokeMethod)
        // Calling 'Wait'
        let taskType = invokeMethod.ReturnType
        let waitMethod = taskType.GetMethod("Wait", instancePublicBindingFlags, Array.empty)
        assert(waitMethod <> null)
        ilGenerator.Emit(OpCodes.Callvirt, waitMethod)

        ilGenerator.Emit(OpCodes.Ret)

        let t = typeBuilder.CreateType()
        t.GetMethod(methodName, staticBindingFlags)

    let createAspNetConfigureMethod (webAppOptionsType : Type) =
        let typeName = $"AspNetConfigure{Guid.NewGuid()}"
        let typeBuilder = dynamicModule.Value.DefineType(typeName, typeAttributes.Value)
        let methodName = $"AspNetConfigureMethod{Guid.NewGuid()}"
        let methodBuilder = typeBuilder.DefineMethod(methodName, methodAttributes.Value)
        methodBuilder.SetReturnType typeof<Void>
        // WebApplicationOptions, EnvironmentName, ApplicationName, ContentRootPath
        methodBuilder.SetParameters([| webAppOptionsType; typeof<string>; typeof<string>; typeof<string> |])
        let ilGenerator = methodBuilder.GetILGenerator()

        // Calling parent (object) constructor
        let objectCtor = typeof<obj>.GetConstructor(Array.empty)
        ilGenerator.Emit(OpCodes.Ldarg_0)
        ilGenerator.Emit(OpCodes.Call, objectCtor)

        // Setting 'EnvironmentName' in 'WebApplicationOptions'
        let environmentNameProperty = webAppOptionsType.GetProperty("EnvironmentName", instancePublicBindingFlags)
        assert(environmentNameProperty <> null)
        let setEnvironmentNameMethod = environmentNameProperty.GetSetMethod()
        assert(setEnvironmentNameMethod <> null)
        ilGenerator.Emit(OpCodes.Ldarg_0)
        ilGenerator.Emit(OpCodes.Ldarg_1)
        ilGenerator.Emit(OpCodes.Callvirt, setEnvironmentNameMethod)

        // Setting 'ApplicationName' in 'WebApplicationOptions'
        let applicationNameProperty = webAppOptionsType.GetProperty("ApplicationName", instancePublicBindingFlags)
        assert(applicationNameProperty <> null)
        let setApplicationNameMethod = applicationNameProperty.GetSetMethod()
        assert(setApplicationNameMethod <> null)
        ilGenerator.Emit(OpCodes.Ldarg_0)
        ilGenerator.Emit(OpCodes.Ldarg_2)
        ilGenerator.Emit(OpCodes.Callvirt, setApplicationNameMethod)

        // Setting 'ContentRootPath' in 'WebApplicationOptions'
        let contentRootPathProperty = webAppOptionsType.GetProperty("ContentRootPath", instancePublicBindingFlags)
        assert(contentRootPathProperty <> null)
        let setContentRootPathMethod = contentRootPathProperty.GetSetMethod()
        assert(setContentRootPathMethod <> null)
        ilGenerator.Emit(OpCodes.Ldarg_0)
        ilGenerator.Emit(OpCodes.Ldarg_3)
        ilGenerator.Emit(OpCodes.Callvirt, setContentRootPathMethod)

        ilGenerator.Emit(OpCodes.Ret)

        let t = typeBuilder.CreateType()
        t.GetMethod(methodName, staticBindingFlags)

    let createInvokeMethod (methodInfo : MethodInfo) =
        assert(not methodInfo.ContainsGenericParameters)
        let typeName = $"Invoker{Guid.NewGuid()}"
        let typeBuilder = dynamicModule.Value.DefineType(typeName, typeAttributes.Value)
        let methodName = $"Invoke{Guid.NewGuid()}"
        let methodBuilder = typeBuilder.DefineMethod(methodName, methodAttributes.Value)
        methodBuilder.SetReturnType typeof<obj>
        methodBuilder.SetParameters([|
            // This, Args
            typeof<obj>; typeof<obj[]>
        |])
        let ilGenerator = methodBuilder.GetILGenerator()

        // Loading 'this', if needed
        let hasThis = hasThis methodInfo
        if hasThis then
            ilGenerator.Emit(OpCodes.Ldarg_0)

        // Loading parameters
        let mutable i = 0
        for arg in methodInfo.GetParameters() do
            ilGenerator.Emit(OpCodes.Ldarg_1)
            ilGenerator.Emit(OpCodes.Ldc_I4, i)
            ilGenerator.Emit(OpCodes.Ldelem, arg.ParameterType)
            i <- i + 1

        // Calling method
        if methodInfo.IsVirtual then
            assert hasThis
            ilGenerator.Emit(OpCodes.Callvirt, methodInfo)
        else ilGenerator.Emit(OpCodes.Call, methodInfo)

        // Boxing result value, if needed
        let returnType = methodInfo.ReturnType
        if returnType.IsValueType then
            ilGenerator.Emit(OpCodes.Box, returnType)

        ilGenerator.Emit(OpCodes.Ret)

        let t = typeBuilder.CreateType()
        t.GetMethod(methodName, staticBindingFlags)
