namespace VSharp

open System
open System.IO
open System.Reflection
open System.Xml.Serialization

[<CLIMutable>]
[<Serializable>]
type testInfo = {
    assemblyLocation : string
    moduleFullyQualifiedName : string
    token : int32
    args : obj array
    expectedResult : obj
}
with
    static member OfMethod(m : MethodBase) = {
        assemblyLocation = m.Module.Assembly.Location
        moduleFullyQualifiedName = m.Module.FullyQualifiedName
        token = m.MetadataToken
        args = Array.zeroCreate <| m.GetParameters().Length
        expectedResult = null
    }

type Test private (m : MethodBase, info : testInfo) =
    new(m : MethodBase) =
        Test(m, testInfo.OfMethod m)

    member x.Method with get() = m
    member x.Args with get() = info.args
    member x.Expected
        with get() = info.expectedResult
        and set r =
            let t = typeof<testInfo>
            let p = t.GetProperty("expectedResult")
            p.SetValue(info, r)

    member x.AddArg (arg : ParameterInfo) (value : obj) =
        info.args.[arg.Position] <- value

    member x.Serialize(destination : string) =
        let serializer = XmlSerializer(typeof<testInfo>)
        use stream = new FileStream(destination, FileMode.OpenOrCreate, FileAccess.Write)
        serializer.Serialize(stream, info)

    member x.Deserialize(source : string) =
        let serializer = XmlSerializer(typeof<testInfo>)
        use stream = new FileStream(source, FileMode.Open, FileAccess.Read)
        try
            let ti = serializer.Deserialize(stream) :?> testInfo
            let assembly = Assembly.LoadFile(ti.assemblyLocation)
            let mdle = assembly.Modules |> Seq.find (fun m -> m.FullyQualifiedName = ti.moduleFullyQualifiedName)
            let method = mdle.ResolveMethod(ti.token)
            Test(method, ti)
        with child ->
            let exn = InvalidDataException("Input test was of invalid format", child)
            raise exn
