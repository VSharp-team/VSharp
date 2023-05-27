namespace VSharp

open System
open System.Text

type CoverageLocation = {
    assemblyName: string
    moduleName: string
    methodToken: int
    offset: int
} with
        static member serialize x =
            $"%s{x.assemblyName}|%s{x.moduleName}|%d{x.methodToken}|%d{x.offset}"

        static member deserialize (x: string) =
            let parts = x.Split "|"
            {
                assemblyName = parts[0]
                moduleName = parts[1]
                methodToken = int parts[2]
                offset = int parts[3]
            }

module CoverageDeserializer =
    let mutable private dataOffset = 0

    let private increaseOffset i =
        dataOffset <- dataOffset + i

    let private readInt32 data =
        let result = BitConverter.ToInt32(data, dataOffset)
        increaseOffset sizeof<int32>
        result

    let private readUInt32 data =
        let result = BitConverter.ToUInt32(data, dataOffset)
        increaseOffset sizeof<uint32>
        result

    let private readString data =
        let size = readUInt32 data |> int
        let result = Array.sub data dataOffset (2 * size - 2)
        increaseOffset (2 * size)
        Encoding.Unicode.GetString(result)

    let private deserializeMethodData data =
        let methodToken = readUInt32 data
        let assemblyName = readString data
        let moduleName = readString data
        {| MethodToken = methodToken; AssemblyName = assemblyName; ModuleName = moduleName |}

    let private deserializeCoverageInfo data =
        let offset = readUInt32 data
        let event = readInt32 data
        let methodId = readInt32 data
        {| Offset = offset; Event = event; MethodId = methodId |}

    let private deserializeArray elementDeserializer data =
        let arraySize = readInt32 data
        Array.init arraySize (fun _ -> elementDeserializer data)

    let private deserializeHistory data =
        let methodsData = deserializeArray deserializeMethodData data
        let coverageInfo = deserializeArray deserializeCoverageInfo data

        coverageInfo
        |> Seq.map (fun x ->
            let methodData = methodsData[x.MethodId]
            {
                assemblyName = methodData.AssemblyName
                moduleName = methodData.ModuleName
                methodToken = int methodData.MethodToken
                offset = int x.Offset
            }
        )
        |> Seq.toArray

    let getHistory bytes =
        dataOffset <- 0
        try
            deserializeArray deserializeHistory bytes
        with
        | e ->
            Logger.error $"{dataOffset}"
            Logger.error $"{e.Message}\n\n{e.StackTrace}"
            failwith "CoverageDeserialization failed!"
