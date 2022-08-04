namespace VSharp

open System
open System.Diagnostics.CodeAnalysis
open System.Reflection
open System.Reflection.Emit
open System.Xml.Serialization
open Microsoft.FSharp.Collections
open VSharp

[<CLIMutable>]
[<Serializable>]
[<XmlInclude(typeof<typeRepr>)>]
type methodRepr = {
    declaringType : typeRepr
    token : int
}
with
    member x.Decode() =
        let declaringType = Serialization.decodeType x.declaringType
        declaringType.GetMethods() |> Seq.find (fun m -> m.MetadataToken = x.token)

    static member Encode(m : MethodBase) : methodRepr = {
        declaringType = Serialization.encodeType m.DeclaringType
        token = m.MetadataToken
    }

[<CLIMutable>]
[<Serializable>]
[<XmlInclude(typeof<structureRepr>)>]
[<XmlInclude(typeof<referenceRepr>)>]
[<XmlInclude(typeof<pointerRepr>)>]
[<XmlInclude(typeof<arrayRepr>)>]
[<XmlInclude(typeof<enumRepr>)>]
[<XmlInclude(typeof<methodRepr>)>]
type typeMockRepr = {
    name : string
    baseClass : typeRepr
    interfaces : typeRepr array
    baseMethods : methodRepr array
    methodImplementations : obj array array
}
with
    static member NullRepr = {name = null; baseClass = Serialization.encodeType null; interfaces = [||]; baseMethods = [||]; methodImplementations = [||]}

module Mocking =

    exception UnexpectedMockCallException of string

    // TODO: properties!
    type Method(baseMethod : MethodInfo, clausesCount : int) =
        let returnValues : obj[] = Array.zeroCreate clausesCount
        let name = baseMethod.Name
        let storageFieldName = $"{baseMethod.Name}{baseMethod.MethodHandle.Value}_<Storage>"
        let counterFieldName = $"{baseMethod.Name}{baseMethod.MethodHandle.Value}_<Counter>"
        let mutable returnType = baseMethod.ReturnType

        member x.BaseMethod = baseMethod
        member x.ReturnValues = returnValues

        member x.SetClauses (clauses : obj[]) =
            clauses |> Array.iteri (fun i o -> returnValues.[i] <- o)

        member x.InitializeType (typ : Type) =
            let field = typ.GetField(storageFieldName, BindingFlags.NonPublic ||| BindingFlags.Static)
            if field = null then
                internalfail $"Could not detect field %s{storageFieldName} of mock!"
            let storage = Array.CreateInstance(returnType, clausesCount)
            Array.Copy(returnValues, storage, clausesCount)
            field.SetValue(null, storage)

        member x.Build (typeBuilder : TypeBuilder) =
            let methodAttributes = MethodAttributes.Public
                                   ||| MethodAttributes.HideBySig
                                   ||| MethodAttributes.NewSlot
                                   ||| MethodAttributes.Virtual
                                   ||| MethodAttributes.Final

            let methodBuilder = typeBuilder.DefineMethod(baseMethod.Name,
                                                        methodAttributes,
                                                        CallingConventions.HasThis)
            if baseMethod.IsGenericMethod then
                let baseGenericArgs = baseMethod.GetGenericArguments()
                let genericsBuilder = methodBuilder.DefineGenericParameters(baseGenericArgs |> Array.map (fun p -> p.Name))
                baseGenericArgs |> Array.iteri (fun i p ->
                    let constraints = p.GetGenericParameterConstraints()
                    let builder = genericsBuilder.[i]
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
            typeBuilder.DefineMethodOverride(methodBuilder, baseMethod)

            let storageField = typeBuilder.DefineField(storageFieldName, returnType.MakeArrayType(), FieldAttributes.Private ||| FieldAttributes.Static)
            let counterField = typeBuilder.DefineField(counterFieldName, returnType, FieldAttributes.Private ||| FieldAttributes.Static)

            let ilGenerator = methodBuilder.GetILGenerator()

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
    type Type(repr : typeMockRepr) =
        let methods = ResizeArray<Method>(Array.map2 (fun (m : methodRepr) (c : obj[]) -> Method(m.Decode(), c.Length)) repr.baseMethods repr.methodImplementations)
        let initializedTypes = System.Collections.Generic.HashSet<System.Type>()

        let mutable baseClass : System.Type = Serialization.decodeType repr.baseClass
        let interfaces = ResizeArray<System.Type>(Array.map Serialization.decodeType repr.interfaces)

        new(name : string) =
            Type({name = name; baseClass = Serialization.encodeType null; interfaces = [||]; baseMethods = [||]; methodImplementations = [||]})

        member x.AddSuperType(t : System.Type) =
            if t.IsValueType || t.IsArray || t.IsPointer || t.IsByRef then
                raise (ArgumentException("Mock supertype should be class or interface!"))
            if t.IsInterface then
                let mutable foundSubtype = false
                let removedSupertypes = interfaces.RemoveAll(fun u -> t.IsAssignableTo u)
                assert(not foundSubtype || removedSupertypes = 0)
                if not foundSubtype then
                    interfaces.Add t
            elif baseClass = null then baseClass <- t
            elif baseClass.IsAssignableTo t then ()
            elif t.IsAssignableTo baseClass then baseClass <- t
            else raise (ArgumentException($"Attempt to assign another base class {t.FullName} for mock with base class {baseClass.FullName}! Note that multiple inheritance is prohibited."))

        member x.AddMethod(m : MethodInfo, retVals : obj[]) =
            let methodMock = Method(m, retVals.Length)
            methodMock.SetClauses retVals
            methods.Add(methodMock)

        [<MaybeNull>]
        member x.BaseClass with get() = baseClass
        member x.Interfaces with get() = interfaces :> seq<_>
        member x.Methods with get() = methods :> seq<_>

        member x.Build(moduleBuilder : ModuleBuilder) =
            let typeBuilder = moduleBuilder.DefineType(repr.name, TypeAttributes.Public)

            if baseClass <> null then
                typeBuilder.SetParent baseClass
                let baseHasNoDefaultCtor = baseClass.GetConstructor [||] = null
                if baseHasNoDefaultCtor then
                    // Defining non-default ctor to eliminate the default one
                    let nonDefaultCtor = typeBuilder.DefineConstructor(MethodAttributes.Private, CallingConventions.Standard, [|typeof<int32>|])
                    let body = nonDefaultCtor.GetILGenerator()
                    body.Emit(OpCodes.Ret)

            interfaces |> ResizeArray.iter typeBuilder.AddInterfaceImplementation

            methods |> ResizeArray.iter (fun methodMock -> methodMock.Build typeBuilder)
            typeBuilder.CreateType()

        member x.Serialize(encode : obj -> obj) =
            let interfaceMethods = interfaces |> ResizeArray.toArray |> Array.collect (fun i -> i.GetMethods())
            let methodsToImplement =
                match baseClass with
                | null -> interfaceMethods
                | _ ->
                    let superClassMethods = baseClass.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.FlattenHierarchy ||| BindingFlags.Instance) |> Array.filter (fun m -> m.IsAbstract)
                    Array.append superClassMethods interfaceMethods

            let abstractMethods =
                methodsToImplement |> Array.map (fun m ->
                        match methods |> ResizeArray.tryFind (fun mock -> mock.BaseMethod = m) with
                        | Some mock -> mock
                        | None -> Method(m, 0))
            let methods = Array.append abstractMethods (methods |> ResizeArray.filter (fun m -> not <| Array.contains m.BaseMethod methodsToImplement) |> ResizeArray.toArray)

            { name = repr.name
              baseClass = Serialization.encodeType baseClass
              interfaces = interfaces |> ResizeArray.map Serialization.encodeType |> ResizeArray.toArray
              baseMethods = methods |> Array.map (fun m -> methodRepr.Encode m.BaseMethod)
              methodImplementations = methods |> Array.map (fun m -> m.ReturnValues |> Array.map encode) }

        member x.EnsureInitialized (decode : obj -> obj) (t : System.Type) =
            if initializedTypes.Add t then
                Seq.iter2 (fun (m : Method) (clauses : obj array) ->
                    let decodedClauses = Array.map decode clauses
                    m.SetClauses decodedClauses
                    m.InitializeType t) methods repr.methodImplementations


    [<CLIMutable>]
    [<Serializable>]
    type mockObject = {typeMockIndex : int}

    type Mocker(mockTypeReprs : typeMockRepr array) =
        let mockTypes : System.Type option array = Array.zeroCreate mockTypeReprs.Length
        let moduleBuilder = lazy(
            let dynamicAssemblyName = "VSharpTypeMocks"
            let assemblyBuilder = AssemblyBuilder.DefineDynamicAssembly(AssemblyName dynamicAssemblyName, AssemblyBuilderAccess.Run)
            assemblyBuilder.DefineDynamicModule dynamicAssemblyName)

        member x.MakeMockObject (mockTypeIndex : int) =
            {typeMockIndex = mockTypeIndex}

        interface ITypeMockSerializer with
            override x.IsMockObject obj =
                match obj with
                | :? mockObject -> true
                | _ -> false
            override x.IsMockRepresentation obj =
                match obj with
                | :? mockObject -> true
                | _ -> false

            override x.Serialize obj = obj

            override x.Deserialize (decode : obj -> obj) obj =
                match obj with
                | :? mockObject as mock ->
                    let index = mock.typeMockIndex
                    let dynamicMockType =
                        match mockTypes.[index] with
                        | Some t -> t
                        | None ->
                            let mockType, typ = x.BuildDynamicType mockTypeReprs.[index]
                            mockType.EnsureInitialized decode typ
                            mockTypes.[index] <- Some typ
                            typ
                    Reflection.createObject dynamicMockType
                | _ -> __unreachable__()

        member x.BuildDynamicType (repr : typeMockRepr) =
            let mockType = Type(repr)
            let moduleBuilder = moduleBuilder.Force()
            mockType, mockType.Build(moduleBuilder)
