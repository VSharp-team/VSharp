namespace VSharp

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Xml.Serialization
open VSharp

[<CLIMutable>]
[<Serializable>]
[<XmlInclude(typeof<structureRepr>)>]
[<XmlInclude(typeof<arrayRepr>)>]
[<XmlInclude(typeof<referenceRepr>)>]
[<XmlInclude(typeof<pointerRepr>)>]
[<XmlInclude(typeof<enumRepr>)>]
[<XmlInclude(typeof<stringRepr>)>]
type testInfo = {
    assemblyName : string
    moduleFullyQualifiedName : string
    errorMessage : string
    token : int32
    thisArg : obj
    args : obj array
    isError : bool
    expectedResult : obj
    throwsException : typeRepr
    classTypeParameters : typeRepr array
    methodTypeParameters : typeRepr array
    mockClassTypeParameters : Nullable<int> array
    mockMethodTypeParameters : Nullable<int> array
    memory : memoryRepr
    typeMocks : typeMockRepr array
    extraAssemblyLoadDirs : string array
    externMocks : extMockRepr array
}
with
    static member OfMethod(m : MethodBase) = {
        assemblyName = m.Module.Assembly.FullName
        moduleFullyQualifiedName = m.Module.FullyQualifiedName
        errorMessage = null
        token = m.MetadataToken
        thisArg = null
        args = null
        isError = false
        expectedResult = null
        classTypeParameters = Array.empty
        methodTypeParameters = Array.empty
        mockClassTypeParameters = Array.empty
        mockMethodTypeParameters = Array.empty
        throwsException = {assemblyName = null; moduleFullyQualifiedName = null; name = null; genericArgs = null}
        memory = {objects = Array.empty; types = Array.empty}
        typeMocks = Array.empty
        extraAssemblyLoadDirs = Array.empty
        externMocks = Array.empty
    }

type UnitTest private (m : MethodBase, info : testInfo, mockStorage : MockStorage, createCompactRepr : bool) =
    let memoryGraph = MemoryGraph(info.memory, mockStorage, createCompactRepr)
    let exceptionInfo = info.throwsException
    let throwsException =
        if exceptionInfo = {assemblyName = null; moduleFullyQualifiedName = null; name = null; genericArgs = null} then null
        else exceptionInfo.Decode()
    let thisArg = memoryGraph.DecodeValue info.thisArg
    let args = if info.args = null then null else info.args |> Array.map memoryGraph.DecodeValue
    let isError = info.isError
    let errorMessage = info.errorMessage
    let expectedResult = memoryGraph.DecodeValue info.expectedResult
    let compactRepresentations = memoryGraph.CompactRepresentations()
    let mutable extraAssemblyLoadDirs : string list = [Directory.GetCurrentDirectory()]
    let mutable patchId = 0 
    let mutable externMocks = info.externMocks |> ResizeArray
    let mutable detours = Seq.empty

    new(m : MethodBase) =
        UnitTest(m, testInfo.OfMethod m, MockStorage(), false)

    member x.Method with get() = m
    member x.ThisArg
        with get() = thisArg
        and set this =
            let t = typeof<testInfo>
            let p = t.GetProperty("thisArg")
            p.SetValue(info, memoryGraph.Encode this)

    member x.HasExternMocks with get() = externMocks.Count > 0
    member x.Args with get() = args
    member x.IsError
        with get() = isError
        and set (e : bool) =
            let t = typeof<testInfo>
            let p = t.GetProperty("isError")
            p.SetValue(info, e)
    member x.ErrorMessage
        with get() = errorMessage
        and set (m : string) =
            let t = typeof<testInfo>
            let p = t.GetProperty("errorMessage")
            p.SetValue(info, m)
    member x.Expected
        with get() = expectedResult
        and set r =
            let t = typeof<testInfo>
            let p = t.GetProperty("expectedResult")
            p.SetValue(info, r)

    member x.Exception
        with get() = throwsException
        and set (e : Type) =
            let t = typeof<testInfo>
            let p = t.GetProperty("throwsException")
            let v = typeRepr.Encode e
            p.SetValue(info, v)

    member x.TypeMocks with get() : ResizeArray<Mocking.Type> = mockStorage.TypeMocks

    member x.CompactRepresentations with get() = compactRepresentations

    member private x.SerializeMock (m : Mocking.Type option) =
        match m with
        | Some m -> Nullable(mockStorage.RegisterMockedType m)
        | None -> Nullable()

    member x.GetPatchId =
        let name = $"patch_{patchId}"
        patchId <- patchId + 1
        name

    member x.AllocateExternMock methodRepr isExtern results =
        let extMock = {name = x.GetPatchId; isExtern = isExtern
                       baseMethod = methodRepr; methodImplementation = results}
        externMocks.Add extMock

    member x.ApplyExternMocks(testName: string) =
        Seq.iter (ExtMocking.BuildAndPatch testName memoryGraph.DecodeValue) externMocks

    member x.ReverseExternMocks() =
        if not <| Seq.isEmpty externMocks then
            ExtMocking.Unpatch()

    // @concreteParameters and @mockedParameters should have equal lengths and be complementary:
    // if @concreteParameters[i] is null, then @mockedParameters[i] is non-null and vice versa
    member x.SetTypeGenericParameters (concreteParameters : Type array) (mockedParameters : Mocking.Type option array) =
        let t = typeof<testInfo>
        let cp = t.GetProperty("classTypeParameters")
        cp.SetValue(info, concreteParameters |> Array.map typeRepr.Encode)
        let mp = t.GetProperty("mockClassTypeParameters")
        mp.SetValue(info, mockedParameters |> Array.map x.SerializeMock)

    // @concreteParameters and @mockedParameters should have equal lengths and be complementary:
    // if @concreteParameters[i] is null, then @mockedParameters[i] is non-null and vice versa
    member x.SetMethodGenericParameters (concreteParameters : Type array) (mockedParameters : Mocking.Type option array) =
        let t = typeof<testInfo>
        let cp = t.GetProperty("methodTypeParameters")
        cp.SetValue(info, concreteParameters |> Array.map typeRepr.Encode)
        let mp = t.GetProperty("mockMethodTypeParameters")
        mp.SetValue(info, mockedParameters |> Array.map x.SerializeMock)

    member x.MemoryGraph with get() = memoryGraph

    member x.ExtraAssemblyLoadDirs with get() = info.extraAssemblyLoadDirs

    member x.AddArg (arg : ParameterInfo) (value : obj) =
        if info.args = null then
            let t = typeof<testInfo>
            let p = t.GetProperty("args")
            p.SetValue(info, Array.zeroCreate <| m.GetParameters().Length)
        let value = memoryGraph.Encode value
        info.args[arg.Position] <- value

    member x.AddExtraAssemblySearchPath path =
        if not <| List.contains path extraAssemblyLoadDirs then
            extraAssemblyLoadDirs <- path::extraAssemblyLoadDirs

    member x.Serialize(destination : string) =
        memoryGraph.Serialize info.memory
        let t = typeof<testInfo>
        let extraAssempliesProperty = t.GetProperty("extraAssemblyLoadDirs")
        extraAssempliesProperty.SetValue(info, Array.ofList extraAssemblyLoadDirs)
        let typeMocksProperty = t.GetProperty("typeMocks")
        let typeMocks =
            mockStorage.TypeMocks.ToArray()
            |> Array.map (fun m -> typeMockRepr.Encode m memoryGraph.Encode)
        typeMocksProperty.SetValue(info, typeMocks)
        let extMocksProperty = t.GetProperty("externMocks")
        extMocksProperty.SetValue(info, externMocks.ToArray())

        let serializer = XmlSerializer t
        use stream = File.Create(destination)
        serializer.Serialize(stream, info)

    static member DeserializeTestInfo(stream : FileStream) =
        let serializer = XmlSerializer(typeof<testInfo>)
        try
            serializer.Deserialize(stream) :?> testInfo
        with child ->
            let exn = InvalidDataException("Input test is incorrect", child)
            raise exn

    static member DeserializeFromTestInfo(ti : testInfo, createCompactRepr : bool) =
        try
            let mdle = Reflection.resolveModule ti.assemblyName ti.moduleFullyQualifiedName
            if mdle = null then
                raise <| InvalidOperationException(sprintf "Could not resolve module %s!" ti.moduleFullyQualifiedName)
            let mockStorage = MockStorage()
            mockStorage.Deserialize ti.typeMocks
            let decodeTypeParameter (concrete : typeRepr) (mockIndex : Nullable<int>) =
                if mockIndex.HasValue then
                    mockStorage[mockIndex.Value] |> snd
                else
                    let res = concrete.Decode()
                    assert(res <> null)
                    res
            let mp = Array.map2 decodeTypeParameter ti.methodTypeParameters ti.mockMethodTypeParameters
            let method = mdle.ResolveMethod(ti.token)
            let declaringType = method.DeclaringType
            let tp = Array.map2 decodeTypeParameter ti.classTypeParameters ti.mockClassTypeParameters
            let concreteDeclaringType = Reflection.concretizeTypeParameters method.DeclaringType tp
            let method = Reflection.concretizeMethodParameters concreteDeclaringType method mp

            // Ensure that parameters are substituted in memoryRepr
            if not method.IsStatic && declaringType.IsGenericType && ti.memory.types.Length > 0 then
                let getGenericTypeDefinition (typ : typeRepr) =
                    let decoded = typ.Decode()
                    if decoded <> null && decoded.IsGenericType then
                        decoded.GetGenericTypeDefinition()
                    else decoded
                let typeDefinitions = ti.memory.types |> Array.map getGenericTypeDefinition
                let declaringTypeIndex = Array.IndexOf(typeDefinitions, declaringType)
                Debug.Assert(declaringTypeIndex >= 0)
                ti.memory.types[declaringTypeIndex] <- typeRepr.Encode concreteDeclaringType

            UnitTest(method, ti, mockStorage, createCompactRepr)
        with child ->
            let exn = InvalidDataException("Input test is incorrect", child)
            raise exn

    static member Deserialize(stream : FileStream) =
        let testInfo = UnitTest.DeserializeTestInfo(stream)
        UnitTest.DeserializeFromTestInfo(testInfo, false)

    static member Deserialize(source : string) =
        use stream = new FileStream(source, FileMode.Open, FileAccess.Read)
        UnitTest.Deserialize stream
