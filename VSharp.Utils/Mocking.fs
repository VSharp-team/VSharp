namespace VSharp

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Collections

module Mocking =

    exception UnexpectedMockCallException of string

    let storageFieldName (method : MethodInfo) = $"{method.Name}{method.MethodHandle.Value}_<Storage>"
    let counterFieldName (method : MethodInfo) = $"{method.Name}{method.MethodHandle.Value}_<Counter>"

    // TODO: properties!
    type Method(baseMethod : MethodInfo, clausesCount : int) =
        let returnValues : obj[] = Array.zeroCreate clausesCount
        let name = baseMethod.Name
        let storageFieldName = storageFieldName baseMethod
        let counterFieldName = counterFieldName baseMethod
        let mutable returnType = baseMethod.ReturnType

        do
            let hasOutParameter =
                baseMethod.GetParameters()
                |> Array.exists (fun x -> x.IsOut)

            if hasOutParameter then internalfail "Method with out parameters mocking not implemented"

        member x.BaseMethod = baseMethod
        member x.ReturnValues = returnValues

        member x.SetClauses (clauses : obj[]) =
            clauses |> Array.iteri (fun i o -> returnValues[i] <- o)

        member x.InitializeType (typ : Type) =
            if returnType <> typeof<Void> then
                let field = typ.GetField(storageFieldName, BindingFlags.NonPublic ||| BindingFlags.Static)
                if field = null then
                    internalfail $"Could not detect field %s{storageFieldName} of mock!"
                let storage = Array.CreateInstance(returnType, clausesCount)
                Array.Copy(returnValues, storage, clausesCount)
                field.SetValue(null, storage)

        member x.Build (typeBuilder : TypeBuilder) =
            let typeIsDelegate = TypeUtils.isDelegate baseMethod.DeclaringType
            let methodAttributes = MethodAttributes.Public ||| MethodAttributes.HideBySig
            let virtualFlags = MethodAttributes.Virtual ||| MethodAttributes.NewSlot ||| MethodAttributes.Final
            let methodAttributes =
                // For delegate mock, there is no need to make method virtual,
                // cause we can not derive from delegate
                if typeIsDelegate then methodAttributes
                else methodAttributes ||| virtualFlags

            let methodBuilder =
                typeBuilder.DefineMethod(baseMethod.Name, methodAttributes, CallingConventions.HasThis)
            if baseMethod.IsGenericMethod then
                let baseGenericArgs = baseMethod.GetGenericArguments()
                let genericsBuilder = methodBuilder.DefineGenericParameters(baseGenericArgs |> Array.map (fun p -> p.Name))
                baseGenericArgs |> Array.iteri (fun i p ->
                    let constraints = p.GetGenericParameterConstraints()
                    let builder = genericsBuilder[i]
                    let interfaceConstraints = constraints |> Array.filter (fun c -> if c.IsInterface then true else builder.SetBaseTypeConstraint c; false)
                    if interfaceConstraints.Length > 0 then
                        builder.SetInterfaceConstraints interfaceConstraints)
                let rec convertType (typ : Type) =
                    if typ.IsGenericMethodParameter then genericsBuilder.[Array.IndexOf(baseGenericArgs, typ)] :> Type
                    elif typ.IsGenericType then
                        let args = typ.GetGenericArguments()
                        let args' = args |> Array.map convertType
                        if args = args' then typ
                        else
                            typ.GetGenericTypeDefinition().MakeGenericType(args')
                    else typ
                methodBuilder.SetReturnType (convertType baseMethod.ReturnType)
                let parameters = baseMethod.GetParameters() |> Array.map (fun p -> convertType p.ParameterType)
                methodBuilder.SetParameters(parameters)
            else
                methodBuilder.SetReturnType baseMethod.ReturnType
                methodBuilder.SetParameters(baseMethod.GetParameters() |> Array.map (fun p -> p.ParameterType))
            returnType <- methodBuilder.ReturnType

            if not typeIsDelegate then
                typeBuilder.DefineMethodOverride(methodBuilder, baseMethod)

            let ilGenerator = methodBuilder.GetILGenerator()

            if returnType <> typeof<Void> then
                let storageField = typeBuilder.DefineField(storageFieldName, returnType.MakeArrayType(), FieldAttributes.Private ||| FieldAttributes.Static)
                let counterField = typeBuilder.DefineField(counterFieldName, typeof<int>, FieldAttributes.Private ||| FieldAttributes.Static)

                let normalCase = ilGenerator.DefineLabel()
                let count = returnValues.Length

                ilGenerator.Emit(OpCodes.Ldsfld, counterField)
                ilGenerator.Emit(OpCodes.Ldc_I4, count)
                ilGenerator.Emit(OpCodes.Blt, normalCase)

                ilGenerator.Emit(OpCodes.Ldstr, name)
                ilGenerator.Emit(OpCodes.Newobj, typeof<UnexpectedMockCallException>.GetConstructor([|typeof<string>|]))
                ilGenerator.Emit(OpCodes.Throw)
                // Or we can return the defaultField:
                // let defaultFieldName = baseMethod.Name + "_<Default>"
                // let defaultField = typeBuilder.DefineField(defaultFieldName, returnType, FieldAttributes.Private ||| FieldAttributes.Static)
                // ilGenerator.Emit(OpCodes.Ldsfld, defaultField)
                // ilGenerator.Emit(OpCodes.Ret)

                ilGenerator.MarkLabel(normalCase)
                ilGenerator.Emit(OpCodes.Ldsfld, storageField)
                ilGenerator.Emit(OpCodes.Ldsfld, counterField)
                ilGenerator.Emit(OpCodes.Ldelem, returnType)

                ilGenerator.Emit(OpCodes.Ldsfld, counterField)
                ilGenerator.Emit(OpCodes.Ldc_I4_1)
                ilGenerator.Emit(OpCodes.Add)
                ilGenerator.Emit(OpCodes.Stsfld, counterField)

            ilGenerator.Emit(OpCodes.Ret)

    // TODO: properties!
    type Type(name : string) =
        let methodMocksCache = Dictionary<MethodInfo, Method>()
        let calledMethods = ResizeArray<MethodInfo>()
        let mutable initialized = false

        let mutable baseClass : System.Type = null
        let interfaces = ResizeArray<System.Type>()

        let mutable rawClauses = null

        static member Empty = Type(String.Empty)

        static member Deserialize name baseClass interfaces baseMethods methodImplementations =
            let mockedType = Type(name)
            mockedType.BaseClass <- baseClass
            mockedType.Interfaces <- interfaces
            let deserializeMethod (m : MethodInfo) (c : obj[]) = Method(m, c.Length)
            mockedType.MethodMocks <- Array.map2 deserializeMethod baseMethods methodImplementations
            mockedType.MethodMocksClauses <- methodImplementations
            mockedType

        member private x.AddInterfaceMethods (t : System.Type) =
            assert t.IsInterface
            let interfaceMethods =
                TypeUtils.getBaseInterfaces t
                |> Seq.cons t
                |> Seq.distinct
                |> Seq.collect (fun i -> i.GetMethods())
            for m in interfaceMethods do
                methodMocksCache.TryAdd(m, Method(m, 0)) |> ignore

        member private x.AddClassMethods (t : System.Type) =
            assert(not (t.IsInterface || t.IsValueType || t.IsArray || t.IsPointer || t.IsByRef))
            let bindingFlags =
                BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.FlattenHierarchy ||| BindingFlags.Instance
            let superClassMethods = baseClass.GetMethods(bindingFlags)
            let isDelegate = TypeUtils.isDelegate baseClass
            let needToMock (m : MethodInfo) =
                // If base class abstract methods, need to mock them
                // If base class is delegate, need to mock 'Invoke' method to create delegate
                m.IsAbstract || isDelegate && m.Name = "Invoke"
            let methodsToImplement = superClassMethods |> Array.filter needToMock
            for m in methodsToImplement do
                methodMocksCache.TryAdd(m, Method(m, 0)) |> ignore

        member x.AddSuperType(t : System.Type) =
            if t.IsValueType || t.IsArray || t.IsPointer || t.IsByRef then
                raise (ArgumentException("Mock supertype should be class or interface!"))
            if t.IsInterface then
                x.AddInterfaceMethods t
                interfaces.RemoveAll(fun u -> t.IsAssignableTo u) |> ignore
                interfaces.Add t
            elif baseClass = null then
                baseClass <- t
                x.AddClassMethods t
            elif baseClass.IsAssignableTo t then ()
            elif t.IsAssignableTo baseClass then
                baseClass <- t
                x.AddClassMethods t
            else
                let message =
                    $"Attempt to assign another base class {t.FullName} for mock
                      with base class {baseClass.FullName}! Note that multiple inheritance is prohibited."
                raise (ArgumentException(message))

        member x.AddMethod(m : MethodInfo, returnValues : obj[]) =
            if calledMethods.Contains m |> not then
                let methodMock = Method(m, returnValues.Length)
                methodMock.SetClauses returnValues
                calledMethods.Add m
                methodMocksCache[m] <- methodMock

        member x.Id = name

        [<MaybeNull>]
        member x.BaseClass
            with get() = baseClass
            and private set typ =
                baseClass <- typ

        member x.Interfaces
            with get() = interfaces :> seq<_>
            and private set (types : System.Type seq) =
                assert(ResizeArray.isEmpty interfaces)
                interfaces.AddRange types

        member x.MethodMocks
            with get() = methodMocksCache.Values :> seq<_>
            and private set (methods : Method seq) =
                assert(methodMocksCache.Count = 0)
                for m in methods do
                    methodMocksCache[m.BaseMethod] <- m

        member x.MethodMocksClauses
            with private get() = rawClauses
            and private set clauses = rawClauses <- clauses

        member x.Build(moduleBuilder : ModuleBuilder) =
            let typeBuilder = moduleBuilder.DefineType(name, TypeAttributes.Public)

            if baseClass <> null && not (TypeUtils.isDelegate baseClass) then
                typeBuilder.SetParent baseClass
                let baseHasNoDefaultCtor = baseClass.GetConstructor Type.EmptyTypes = null
                if baseHasNoDefaultCtor then
                    // Defining non-default ctor to eliminate the default one
                    let nonDefaultCtor = typeBuilder.DefineConstructor(MethodAttributes.Private, CallingConventions.Standard, [|typeof<int32>|])
                    let body = nonDefaultCtor.GetILGenerator()
                    body.Emit(OpCodes.Ret)

            interfaces |> ResizeArray.iter typeBuilder.AddInterfaceImplementation

            methodMocksCache.Values |> Seq.iter (fun methodMock -> methodMock.Build typeBuilder)
            typeBuilder.CreateType()

        // Is used to initialize mock clauses if it was not initialized
        member x.EnsureInitialized (decode : obj -> obj) (t : System.Type) =
            if not initialized then
                initialized <- true
                x.Update decode t

        // Is used to update already initialized mock type
        // In memory graph, firstly, it is allocated with default values via 'EnsureInitialized'
        // Secondly, it is mutated with deserialized values via 'Update'
        member x.Update (decode : obj -> obj) (t : System.Type) =
            let updateOne (kvp : KeyValuePair<MethodInfo, Method>) (clauses : obj array) =
                let decodedClauses = Array.map decode clauses
                let m = kvp.Value
                m.SetClauses decodedClauses
                m.InitializeType t
            Seq.iter2 updateOne methodMocksCache x.MethodMocksClauses

    type Mocker() =
        let builtMocksCache = Dictionary<Type, System.Type>()
        let moduleBuilder = lazy(
            let dynamicAssemblyName = $"VSharpTypeMocks.{Guid.NewGuid()}"
            let assemblyBuilder = AssemblyManager.DefineDynamicAssembly(AssemblyName dynamicAssemblyName, AssemblyBuilderAccess.Run)
            assemblyBuilder.DefineDynamicModule dynamicAssemblyName)

        member x.BuildDynamicType (typeMock : Type) =
            let res = ref null
            if builtMocksCache.TryGetValue(typeMock, res) then res.Value
            else
                let built = typeMock.Build(moduleBuilder.Value)
                builtMocksCache.Add(typeMock, built)
                built

        static member CreateDelegate (t : System.Type) (builtMock : System.Type) =
            let invokeMethodInfo = builtMock.GetMethod("Invoke")
            Delegate.CreateDelegate(t, null, invokeMethodInfo) :> obj
