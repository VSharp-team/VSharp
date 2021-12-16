namespace VSharp

open System
open System.IO
open System.Reflection
open System.Xml.Serialization

[<CLIMutable>]
[<Serializable>]
[<XmlInclude(typeof<structureRepr>)>]
[<XmlInclude(typeof<arrayRepr>)>]
[<XmlInclude(typeof<referenceRepr>)>]
[<XmlInclude(typeof<pointerRepr>)>]
[<XmlInclude(typeof<enumRepr>)>]
type testInfo = {
    assemblyName : string
    moduleFullyQualifiedName : string
    token : int32
    thisArg : obj
    args : obj array
    isError : bool
    expectedResult : obj
    throwsException : typeRepr
    classTypeParameters : typeRepr array
    methodTypeParameters : typeRepr array
    memory : memoryRepr
    extraAssemblyLoadDirs : string array
}
with
    static member OfMethod(m : MethodBase) = {
        assemblyName = m.Module.Assembly.FullName
        moduleFullyQualifiedName = m.Module.FullyQualifiedName
        token = m.MetadataToken
        thisArg = null
        args = null
        isError = false
        expectedResult = null
        classTypeParameters = Array.empty
        methodTypeParameters = Array.empty
        throwsException = {assemblyName = null; moduleFullyQualifiedName = null; fullName = null}
        memory = {objects = Array.empty; types = Array.empty}
        extraAssemblyLoadDirs = Array.empty
    }

type UnitTest private (m : MethodBase, info : testInfo) =
    let memoryGraph = MemoryGraph(info.memory)
    let exceptionInfo = info.throwsException
    let throwsException =
        if exceptionInfo = {assemblyName = null; moduleFullyQualifiedName = null; fullName = null} then null
        else Serialization.decodeType exceptionInfo
    let thisArg = memoryGraph.DecodeValue info.thisArg
    let args = if info.args = null then null else info.args |> Array.map memoryGraph.DecodeValue
    let isError = info.isError
    let expectedResult = memoryGraph.DecodeValue info.expectedResult
    let classTypeParameters = info.classTypeParameters |> Array.map Serialization.decodeType
    let methodTypeParameters = info.methodTypeParameters |> Array.map Serialization.decodeType
    let mutable extraAssemblyLoadDirs : string list = [Directory.GetCurrentDirectory()]
    new(m : MethodBase) =
        UnitTest(m, testInfo.OfMethod m)

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

    member x.SetTypeGenericParameters (parameters : Type array) =
            let t = typeof<testInfo>
            let p = t.GetProperty("classTypeParameters")
            p.SetValue(info, parameters |> Array.map Serialization.encodeType)

    member x.SetMethodGenericParameters (parameters : Type array) =
        let t = typeof<testInfo>
        let p = t.GetProperty("methodTypeParameters")
        p.SetValue(info, parameters |> Array.map Serialization.encodeType)

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
        let p = t.GetProperty("extraAssemblyLoadDirs")
        p.SetValue(info, Array.ofList extraAssemblyLoadDirs)
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

    static member DeserializeFromTestInfo(ti : testInfo) =
        try
            let mdle = Reflection.resolveModule ti.assemblyName ti.moduleFullyQualifiedName
            if mdle = null then raise <| InvalidOperationException(sprintf "Could not resolve module %s!" ti.moduleFullyQualifiedName)
            let tp = ti.classTypeParameters |> Array.map Serialization.decodeType
            let mp = ti.methodTypeParameters |> Array.map Serialization.decodeType
            let method = mdle.ResolveMethod(ti.token)
            let declaringType = method.DeclaringType
            let declaringType =
                if method.DeclaringType.IsGenericType then
                    assert(tp.Length = declaringType.GetGenericArguments().Length)
                    declaringType.MakeGenericType(tp)
                else
                    assert(tp.Length = 0)
                    declaringType
            let method =
                match method with
                | :? MethodInfo as mi ->
                    let method = declaringType.GetMethods() |> Array.find (fun x -> x.MetadataToken = mi.MetadataToken)
                    if method.IsGenericMethod then
                        assert(mp.Length = method.GetGenericArguments().Length)
                        method.MakeGenericMethod(mp) :> MethodBase
                    else
                        assert(mp.Length = 0)
                        method :> MethodBase
                | :? ConstructorInfo as ci ->
                    assert(mp.Length = 0)
                    declaringType.GetConstructors() |> Array.find (fun x -> x.MetadataToken = ci.MetadataToken) :> MethodBase
                | _ -> __notImplemented__()
            if mdle = null then raise <| InvalidOperationException(sprintf "Could not resolve method %d!" ti.token)
            UnitTest(method, ti)
        with child ->
            let exn = InvalidDataException("Input test is incorrect", child)
            raise exn

    static member Deserialize(stream : FileStream) =
        let testInfo = UnitTest.DeserializeTestInfo(stream)
        UnitTest.DeserializeFromTestInfo(testInfo)

    static member Deserialize(source : string) =
        use stream = new FileStream(source, FileMode.Open, FileAccess.Read)
        UnitTest.Deserialize stream
