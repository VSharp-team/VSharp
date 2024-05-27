namespace VSharp

open System
open System.Runtime.InteropServices
open System.Runtime.Serialization
open System.Text

type CoverageLocation = {
    assemblyName: string
    moduleName: string
    methodToken: int
    offset: int
}

type CoverageReport = {
    threadId: int
    coverageLocations: CoverageLocation[]
}

#nowarn "9"
[<Struct; CLIMutable; DataContract>]
[<StructLayout(LayoutKind.Explicit, Size = 28)>]
type RawCoverageLocation = {
    [<FieldOffset(00); DataMember(Order = 1)>] offset: uint32
    [<FieldOffset(04); DataMember(Order = 2)>] event: int32
    [<FieldOffset(08); DataMember(Order = 3)>] methodId: int32
    [<FieldOffset(12); DataMember(Order = 4)>] threadId: uint64
    [<FieldOffset(20); DataMember(Order = 5)>] timeInMicroseconds: int64
}

type RawMethodInfo = {
    methodToken: uint32
    moduleName: string
    assemblyName: string
}

type RawCoverageReport = {
    threadId: int
    rawCoverageLocations: RawCoverageLocation[]
}

type RawCoverageReports = {
    methods: System.Collections.Generic.Dictionary<int, RawMethodInfo>
    reports: RawCoverageReport[]
}

module CoverageDeserializer =

    let mutable private data = [||]
    let mutable private dataOffset = 0
    let mutable private deserializedMethods = System.Collections.Generic.Dictionary()

    let inline private increaseOffset i =
        dataOffset <- dataOffset + i

    let inline private readInt32 () =
        let result = BitConverter.ToInt32(data, dataOffset)
        increaseOffset sizeof<int32>
        result

    let inline private readInt64 () =
        let result = BitConverter.ToInt64(data, dataOffset)
        increaseOffset sizeof<int64>
        result

    let inline private readUInt32 () =
        let result = BitConverter.ToUInt32(data, dataOffset)
        increaseOffset sizeof<uint32>
        result

    let inline private readUInt64 () =
        let result = BitConverter.ToUInt64(data, dataOffset)
        increaseOffset sizeof<uint64>
        result

    let inline private readString () =
        let size = readUInt32 () |> int
        let result = Array.sub data dataOffset (2 * size - 2)
        increaseOffset (2 * size)
        let result = Encoding.Unicode.GetString(result)
        result

    let inline private deserializeMethodData () =
        let methodToken = readUInt32 ()
        let assemblyName = readString ()
        let moduleName = readString ()
        { methodToken = methodToken; assemblyName = assemblyName; moduleName = moduleName }

    let inline private deserializeCoverageInfo () =
        let offset = readUInt32 ()
        let event = readInt32 ()
        let methodId = readInt32 ()
        let threadId = readUInt64 ()
        let time = readInt64 ()
        { offset = offset; event = event; methodId = methodId; threadId = threadId; timeInMicroseconds = time }

    let inline private deserializeArray elementDeserializer =
        let arraySize = readInt32 ()
        Array.init arraySize (fun _ -> elementDeserializer ())

    let inline private deserializeDictionary keyDeserializer elementDeserializer =
        let dictionarySize = readInt32 ()
        let dictionary = System.Collections.Generic.Dictionary()
        for _ in 0..dictionarySize - 1 do
            let index = keyDeserializer ()
            let element = elementDeserializer ()
            dictionary.Add(index, element)
        dictionary

    let inline private deserializeCoverageInfoFast () =
        let count = readInt32 ()
        let bytesCount = sizeof<RawCoverageLocation> * count
        let targetBytes = Array.zeroCreate bytesCount
        let targetSpan = Span(targetBytes)
        data.AsSpan().Slice(dataOffset, bytesCount).CopyTo(targetSpan)
        let span = MemoryMarshal.Cast<byte, RawCoverageLocation> targetSpan
        increaseOffset bytesCount
        span.ToArray()

    let private deserializeRawReport () =
        let threadId = readInt32 ()
        let threadAborted = readInt32 ()
        if threadAborted = 1 then
            {
                threadId = threadId
                rawCoverageLocations = [||]
            }
        else
            {
                threadId = threadId
                rawCoverageLocations = deserializeCoverageInfoFast ()
            }

    let private deserializeRawReports () =
        let methods = deserializeDictionary readInt32 deserializeMethodData
        let reports = deserializeArray deserializeRawReport
        {
            methods = methods
            reports = reports
        }

    let private startNewDeserialization bytes =
        data <- bytes
        dataOffset <- 0

    let private getMethods () =
        deserializedMethods <- deserializeDictionary readInt32 deserializeMethodData

    let getRawReports bytes =
        try
            startNewDeserialization bytes
            let result = deserializeRawReports ()
            result
        with
        | e ->
            Logger.error $"{dataOffset}"
            Logger.error $"{e.Message}\n\n{e.StackTrace}"
            failwith "CoverageDeserialization failed!"

    let reportsFromRawReports (rawReports : RawCoverageReports) =

        let toLocation (x : RawCoverageLocation) =
            let method = rawReports.methods[x.methodId]
            {
                assemblyName = method.assemblyName
                moduleName = method.moduleName
                methodToken = method.methodToken |> int
                offset = x.offset |> int
            }

        let toReport (x : RawCoverageReport) =
            {
                threadId = x.threadId
                coverageLocations = x.rawCoverageLocations |> Array.map toLocation
            }

        rawReports.reports |> Array.map toReport
