namespace VSharp

open System
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
[<XmlInclude(typeof<Mocking.mockObject>)>]
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
    mockClassTypeParameters : typeMockRepr array
    mockMethodTypeParameters : typeMockRepr array
    memory : memoryRepr
    extraAssemblyLoadDirs : string array
    typeMocks : typeMockRepr array
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
        throwsException = {assemblyName = null; moduleFullyQualifiedName = null; fullName = null}
        memory = {objects = Array.empty; types = Array.empty}
        extraAssemblyLoadDirs = Array.empty
        typeMocks = Array.empty
    }

type UnitTest private (m : MethodBase, info : testInfo, createCompactRepr : bool) =
    let mocker = Mocking.Mocker(info.typeMocks)
    let typeMocks = info.typeMocks |> Array.map Mocking.Type |> ResizeArray
    let memoryGraph = MemoryGraph(info.memory, mocker, createCompactRepr)
    let exceptionInfo = info.throwsException
    let throwsException =
        if exceptionInfo = {assemblyName = null; moduleFullyQualifiedName = null; fullName = null} then null
        else Serialization.decodeType exceptionInfo
    let thisArg = memoryGraph.DecodeValue info.thisArg
    let args = if info.args = null then null else info.args |> Array.map memoryGraph.DecodeValue
    let isError = info.isError
    let errorMessage = info.errorMessage
    let expectedResult = memoryGraph.DecodeValue info.expectedResult
//    let classTypeParameters = info.classTypeParameters |> Array.map Serialization.decodeType
//    let methodTypeParameters = info.methodTypeParameters |> Array.map Serialization.decodeType
    let mutable extraAssemblyLoadDirs : string list = [Directory.GetCurrentDirectory()]

    new(m : MethodBase) =
        UnitTest(m, testInfo.OfMethod m, false)

    member x.Method with get() = m
    member x.ThisArg
        with get() = thisArg
        and set this =
            let t = typeof<testInfo>
            let p = t.GetProperty("thisArg")
            p.SetValue(info, memoryGraph.Encode this)

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
            let v = Serialization.encodeType e
            p.SetValue(info, v)

    member x.TypeMocks with get() = typeMocks

    member x.DefineTypeMock(name : string) =
        let mock = Mocking.Type(name)
        typeMocks.Add mock
        mock

    member x.AllocateMockObject (typ : Mocking.Type) =
        let index = typeMocks.IndexOf typ
        mocker.MakeMockObject index :> obj

    // @concreteParameters and @mockedParameters should have equal lengths and be complementary:
    // if @concreteParameters[i] is null, then @mockedParameters[i] is non-null and vice versa
    member x.SetTypeGenericParameters (concreteParameters : Type array) (mockedParameters : Mocking.Type option array) =
        let t = typeof<testInfo>
        let cp = t.GetProperty("classTypeParameters")
        cp.SetValue(info, concreteParameters |> Array.map Serialization.encodeType)
        let mp = t.GetProperty("mockClassTypeParameters")
        mp.SetValue(info, mockedParameters |> Array.map (fun m ->
            match m with
            | Some m -> m.Serialize memoryGraph.Encode
            | None -> typeMockRepr.NullRepr))

    // @concreteParameters and @mockedParameters should have equal lengths and be complementary:
    // if @concreteParameters[i] is null, then @mockedParameters[i] is non-null and vice versa
    member x.SetMethodGenericParameters (concreteParameters : Type array) (mockedParameters : Mocking.Type option array) =
        let t = typeof<testInfo>
        let cp = t.GetProperty("methodTypeParameters")
        cp.SetValue(info, concreteParameters |> Array.map Serialization.encodeType)
        let mp = t.GetProperty("mockMethodTypeParameters")
        mp.SetValue(info, mockedParameters |> Array.map (fun m ->
            match m with
            | Some m -> m.Serialize memoryGraph.Encode
            | None -> typeMockRepr.NullRepr))

    member x.MemoryGraph with get() = memoryGraph

    member x.ExtraAssemblyLoadDirs with get() = info.extraAssemblyLoadDirs

    member x.AddArg (arg : ParameterInfo) (value : obj) =
        if info.args = null then
            let t = typeof<testInfo>
            let p = t.GetProperty("args")
            p.SetValue(info, Array.zeroCreate <| m.GetParameters().Length)
        let value = memoryGraph.Encode value
        info.args.[arg.Position] <- value

    member x.AddExtraAssemblySearchPath path =
        if not <| List.contains path extraAssemblyLoadDirs then
            extraAssemblyLoadDirs <- path::extraAssemblyLoadDirs

    member x.Serialize(destination : string) =
        memoryGraph.Serialize info.memory
        let t = typeof<testInfo>
        let extraAssempliesProperty = t.GetProperty("extraAssemblyLoadDirs")
        extraAssempliesProperty.SetValue(info, Array.ofList extraAssemblyLoadDirs)
        let typeMocksProperty = t.GetProperty("typeMocks")
        typeMocksProperty.SetValue(info, typeMocks.ToArray() |> Array.map (fun m -> m.Serialize memoryGraph.Encode))
        let serializer = XmlSerializer t
        use stream = File.Create(destination)
        serializer.Serialize(stream, info)

    static member DeserializeTestInfo(stream : FileStream) = // TODO: fix style #style
        let serializer = XmlSerializer(typeof<testInfo>)
        try
            serializer.Deserialize(stream) :?> testInfo
        with child ->
            let exn = InvalidDataException("Input test is incorrect", child)
            raise exn

    static member DeserializeFromTestInfo(ti : testInfo, createCompactRepr : bool) =
        try
            let mdle = Reflection.resolveModule ti.assemblyName ti.moduleFullyQualifiedName
            if mdle = null then raise <| InvalidOperationException(sprintf "Could not resolve module %s!" ti.moduleFullyQualifiedName)
            let mocker = Mocking.Mocker([||])
            let decodeTypeParameter (concrete : typeRepr) (mock : typeMockRepr) =
                let t = Serialization.decodeType concrete
                if t = null then
                    mocker.BuildDynamicType mock |> snd
                else t
            let tp = Array.map2 decodeTypeParameter ti.classTypeParameters ti.mockClassTypeParameters
            let mp = Array.map2 decodeTypeParameter ti.methodTypeParameters ti.mockMethodTypeParameters
            let method = mdle.ResolveMethod(ti.token)

            let declaringType = method.DeclaringType

            let getGenericTypeDefinition typ =
                let decoded = Serialization.decodeType typ
                if decoded <> null && decoded.IsGenericType then
                    decoded.GetGenericTypeDefinition()
                else
                    decoded
            let typeDefinitions = ti.memory.types |> Array.map getGenericTypeDefinition
            let declaringTypeIndex = Array.IndexOf(typeDefinitions, declaringType)

            let declaringType = Reflection.concretizeTypeParameters declaringType tp

            // Ensure that parameters are substituted in memoryRepr
            if not method.IsStatic && declaringType.IsGenericType && ti.memory.types.Length > 0 then
                ti.memory.types.[declaringTypeIndex] <- Serialization.encodeType declaringType

            let method = Reflection.concretizeMethodParameters declaringType method mp

            if mdle = null then raise <| InvalidOperationException(sprintf "Could not resolve method %d!" ti.token)
            UnitTest(method, ti, createCompactRepr)
        with child ->
            let exn = InvalidDataException("Input test is incorrect", child)
            raise exn

    static member Deserialize(stream : FileStream) =
        let testInfo = UnitTest.DeserializeTestInfo(stream)
        UnitTest.DeserializeFromTestInfo(testInfo, false)

    static member Deserialize(source : string) =
        use stream = new FileStream(source, FileMode.Open, FileAccess.Read)
        UnitTest.Deserialize stream
