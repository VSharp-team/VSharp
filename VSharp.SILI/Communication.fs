namespace VSharp.Concolic

open System
open System.IO
open System.IO.Pipes
open System.Text
open System.Runtime.InteropServices
open VSharp
open VSharp.Core.API

type CorElementType =
    | ELEMENT_TYPE_END            = 0x0uy
    | ELEMENT_TYPE_VOID           = 0x1uy
    | ELEMENT_TYPE_BOOLEAN        = 0x2uy
    | ELEMENT_TYPE_CHAR           = 0x3uy
    | ELEMENT_TYPE_I1             = 0x4uy
    | ELEMENT_TYPE_U1             = 0x5uy
    | ELEMENT_TYPE_I2             = 0x6uy
    | ELEMENT_TYPE_U2             = 0x7uy
    | ELEMENT_TYPE_I4             = 0x8uy
    | ELEMENT_TYPE_U4             = 0x9uy
    | ELEMENT_TYPE_I8             = 0xauy
    | ELEMENT_TYPE_U8             = 0xbuy
    | ELEMENT_TYPE_R4             = 0xcuy
    | ELEMENT_TYPE_R8             = 0xduy
    | ELEMENT_TYPE_STRING         = 0xeuy

    | ELEMENT_TYPE_PTR            = 0xfuy
    | ELEMENT_TYPE_BYREF          = 0x10uy

    | ELEMENT_TYPE_VALUETYPE      = 0x11uy
    | ELEMENT_TYPE_CLASS          = 0x12uy
    | ELEMENT_TYPE_VAR            = 0x13uy
    | ELEMENT_TYPE_ARRAY          = 0x14uy
    | ELEMENT_TYPE_GENERICINST    = 0x15uy
    | ELEMENT_TYPE_TYPEDBYREF     = 0x16uy

    | ELEMENT_TYPE_I              = 0x18uy
    | ELEMENT_TYPE_U              = 0x19uy
    | ELEMENT_TYPE_FNPTR          = 0x1Buy
    | ELEMENT_TYPE_OBJECT         = 0x1Cuy
    | ELEMENT_TYPE_SZARRAY        = 0x1Duy
    | ELEMENT_TYPE_MVAR           = 0x1euy

    | ELEMENT_TYPE_CMOD_REQD      = 0x1Fuy
    | ELEMENT_TYPE_CMOD_OPT       = 0x20uy

    | ELEMENT_TYPE_INTERNAL       = 0x21uy
    | ELEMENT_TYPE_MAX            = 0x22uy

    | ELEMENT_TYPE_MODIFIER       = 0x40uy
    | ELEMENT_TYPE_SENTINEL       = 0x41uy
    | ELEMENT_TYPE_PINNED         = 0x45uy

type evalStackArgType =
    | OpSymbolic = 1
    | OpI4 = 2
    | OpI8 = 3
    | OpR4 = 4
    | OpR8 = 5
    | OpRef = 6

type evalStackOperand =
    | NumericOp of evalStackArgType * int64
    | PointerOp of uint64 * uint64

[<type: StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
type private execCommandStatic = {
    offset : uint32
    isBranch : uint32
    newCallStackFramesCount : uint32
    callStackFramesPops : uint32
    evaluationStackPushesCount : uint32
    evaluationStackPops : uint32
    newAddressesCount : uint32
}
type execCommand = {
    offset : uint32
    isBranch : uint32
    callStackFramesPops : uint32
    evaluationStackPops : uint32
    newCallStackFrames : int32 array
    evaluationStackPushes : evalStackOperand array // NOTE: operands for executing instruction
    newAddresses : UIntPtr array
    newAddressesTypes : Type array
    // TODO: add deleted addresses
}

[<type: StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
type execResponseStaticPart = {
    framesCount : uint32
    lastPush : byte
    opsLength : int // -1 if operands were not concretized, length otherwise
    hasResult : byte
}

type commandFromConcolic =
    | Instrument of rawMethodBody
    | ExecuteInstruction of execCommand
    | Terminate

type commandForConcolic =
    | ReadMethodBody
    | ReadString

type Communicator(pipeFile) =

    let confirmationByte = byte(0x55)
    let instrumentCommandByte = byte(0x56)
    let executeCommandByte = byte(0x57)
    let readMethodBodyByte = byte(0x58)
    let readStringByte = byte(0x59)
    let confirmation = Array.singleton confirmationByte

    let server = new NamedPipeServerStream(pipeFile, PipeDirection.InOut)
    let stream = server :> Stream

    let reportError (exn : IOException) =
        Logger.error "Error occured during communication with the concolic client! Message: %s" exn.Message
        false

    let fail format = Printf.ksprintf (fun (s : string) -> raise <| IOException s) format

    let unexpectedlyTerminated() = fail "Communication with CLR: interaction unexpectedly terminated"

    let readConfirmation () =
        let buffer : byte[] = Array.zeroCreate 1
        let bytesRead = stream.Read(buffer, 0, 1)
        if bytesRead <> 1 || buffer.[0] <> confirmationByte then
            fail "Communication with CLR: could not get the confirmation message. Instead read %d bytes with message [%s]" bytesRead (Array.map toString buffer |> join " ")

    let writeConfirmation () =
        stream.Write(confirmation, 0, 1)

    let readCount () =
        let countBytes : byte[] = Array.zeroCreate 4
        let countCount = stream.Read(countBytes, 0, 4)
        if countCount <> 4 then
            fail "Communication with CLR: could not get the amount of bytes of the next message. Instead read %d bytes" countCount
        BitConverter.ToInt32(countBytes, 0)

    let readBuffer () =
        let chunkSize = 8192
        let count = readCount()
        assert(count <> 0)
        if count < 0 then None
        else
            writeConfirmation()
            let buffer : byte[] = Array.zeroCreate count
            let mutable bytesRead = 0
            while bytesRead < count do
                let length = min chunkSize (count - bytesRead)
                let chunk : byte[] = Array.zeroCreate length
                let offset = bytesRead
                bytesRead <- bytesRead + server.Read(chunk, 0, length)
                Array.Copy(chunk, 0, buffer, offset, length)
            if bytesRead <> count then
                fail "Communication with CLR: expected %d bytes, but read %d bytes" count bytesRead
            else
                writeConfirmation()
                Some buffer

    let writeBuffer (buffer : byte[]) =
        if buffer.LongLength > int64(Int32.MaxValue) then
            fail "Communication with CLR: too large message (length = %s)!" (buffer.LongLength.ToString())
        let countBuffer = BitConverter.GetBytes(buffer.Length)
        assert(countBuffer.Length = 4)
        stream.Write(countBuffer, 0, 4)
        readConfirmation()
        stream.Write(buffer, 0, buffer.Length)
        readConfirmation()

    let readString () =
        match readBuffer() with
        | None -> unexpectedlyTerminated()
        | Some buffer -> Encoding.ASCII.GetString(buffer)

    // NOTE: all strings, sent to concolic should end with null terminator
    let writeString (str : string) =
        // NOTE: adding null terminator
        let buffer = Encoding.ASCII.GetBytes(str + Char.MinValue.ToString())
        writeBuffer buffer

    let waitClient () =
        Logger.trace "Waiting for client connection..."
        server.WaitForConnection()
        Logger.trace "Client connected!"

    let handshake () =
        let message = "Hi!"
        writeString message
        let expectedMessage = "Hi!"
        let s = readString ()
        if s <> expectedMessage then
            fail "Communication with CLR: handshake failed: got %s instead of %s" s expectedMessage

    override x.Finalize() =
        server.Close()

    member private x.Deserialize<'a> (bytes : byte array, startIndex : int) =
        let result = Reflection.createObject typeof<'a> :?> 'a
        let size = Marshal.SizeOf(typeof<'a>)
        let unmanagedPtr = Marshal.AllocHGlobal(size)
        Marshal.Copy(bytes, startIndex, unmanagedPtr, size)
        Marshal.PtrToStructure(unmanagedPtr, result)
        Marshal.FreeHGlobal(unmanagedPtr)
        result

    member private x.Deserialize<'a> (bytes : byte array) = x.Deserialize<'a>(bytes, 0)

    member private x.Serialize<'a> (structure : 'a, bytes : byte array, startIndex : int) =
        let size = Marshal.SizeOf(typeof<'a>)
        let unmanagedPtr = Marshal.AllocHGlobal(size)
        Marshal.StructureToPtr(structure, unmanagedPtr, false)
        Marshal.Copy(unmanagedPtr, bytes, startIndex, size)
        Marshal.FreeHGlobal(unmanagedPtr)

    member private x.Serialize<'a> (structure : 'a) =
        let size = Marshal.SizeOf(typeof<'a>)
        let result = Array.zeroCreate size
        x.Serialize<'a>(structure, result, 0)
        result

    member private x.SerializeCommand command =
        let byte =
            match command with
            | ReadString -> readStringByte
            | ReadMethodBody -> readMethodBodyByte
        Array.singleton byte

    member x.Connect() =
        try
            waitClient()
            handshake()
            true
        with
        | :? IOException as e -> reportError e

    member private x.ReadStructure<'a>() =
        match readBuffer() with
        | Some bytes -> x.Deserialize<'a> bytes
        | None -> unexpectedlyTerminated()

    member x.ReadProbes() = x.ReadStructure<probes>()

    member x.SendEntryPoint (moduleName : string) (metadataToken : int) =
        let moduleNameBytes = Encoding.Unicode.GetBytes moduleName
        let moduleSize = BitConverter.GetBytes moduleName.Length
//        let moduleID = BitConverter.GetBytes m.Module.MetadataToken
        let methodDef = BitConverter.GetBytes metadataToken
        Array.concat [moduleSize; methodDef; moduleNameBytes] |> writeBuffer

    member x.SendCommand (command : commandForConcolic) =
        let bytes = x.SerializeCommand command
        writeBuffer bytes

    member x.SendStringAndReadItsIndex (str : string) : uint32 =
        x.SendCommand ReadString
        writeString str
        match readBuffer() with
        | Some bytes -> BitConverter.ToUInt32(bytes, 0)
        | None -> unexpectedlyTerminated()

    member x.ReadMethodBody() =
        match readBuffer() with
        | Some bytes ->
            let propertiesBytes, rest = Array.splitAt (Marshal.SizeOf typeof<rawMethodProperties>) bytes
            let properties = x.Deserialize<rawMethodProperties> propertiesBytes
            let sizeOfSignatureTokens = Marshal.SizeOf typeof<signatureTokens>
            if int properties.signatureTokensLength <> sizeOfSignatureTokens then
                fail "Size of received signature tokens buffer mismatch the expected! Probably you've altered the client-side signatures, but forgot to alter the server-side structure (or vice-versa)"
            let signatureTokenBytes, rest = Array.splitAt sizeOfSignatureTokens rest
            let assemblyNameBytes, rest = Array.splitAt (int properties.assemblyNameLength) rest
            let moduleNameBytes, rest = Array.splitAt (int properties.moduleNameLength) rest
            let signatureTokens = x.Deserialize<signatureTokens> signatureTokenBytes
            let assemblyName = Encoding.Unicode.GetString(assemblyNameBytes)
            let moduleName = Encoding.Unicode.GetString(moduleNameBytes)
            let ilBytes, ehBytes  = Array.splitAt (int properties.ilCodeSize) rest
            let ehSize = Marshal.SizeOf typeof<rawExceptionHandler>
            let ehCount = Array.length ehBytes / ehSize
            let ehs = Array.init ehCount (fun i -> x.Deserialize<rawExceptionHandler>(ehBytes, i * ehSize))
            {properties = properties; tokens = signatureTokens; assembly = assemblyName; moduleName = moduleName; il = ilBytes; ehs = ehs}
        | None -> unexpectedlyTerminated()

    member private x.ToUIntPtr =
        if IntPtr.Size = 4 then fun (bytes : byte[]) index -> BitConverter.ToUInt32(bytes, index) |> UIntPtr
        else fun (bytes : byte[]) index -> BitConverter.ToUInt64(bytes, index) |> UIntPtr

    member private x.corElementTypeToType (elemType : CorElementType) =
        match elemType with
        | CorElementType.ELEMENT_TYPE_BOOLEAN -> Some(typeof<bool>)
        | CorElementType.ELEMENT_TYPE_CHAR    -> Some(typeof<char>)
        | CorElementType.ELEMENT_TYPE_I1      -> Some(typeof<int8>)
        | CorElementType.ELEMENT_TYPE_U1      -> Some(typeof<uint8>)
        | CorElementType.ELEMENT_TYPE_I2      -> Some(typeof<int16>)
        | CorElementType.ELEMENT_TYPE_U2      -> Some(typeof<uint16>)
        | CorElementType.ELEMENT_TYPE_I4      -> Some(typeof<int32>)
        | CorElementType.ELEMENT_TYPE_U4      -> Some(typeof<uint32>)
        | CorElementType.ELEMENT_TYPE_I8      -> Some(typeof<int64>)
        | CorElementType.ELEMENT_TYPE_U8      -> Some(typeof<uint64>)
        | CorElementType.ELEMENT_TYPE_R4      -> Some(typeof<float32>)
        | CorElementType.ELEMENT_TYPE_R8      -> Some(typeof<double>)
        | CorElementType.ELEMENT_TYPE_I       -> Some(typeof<IntPtr>)
        | CorElementType.ELEMENT_TYPE_U       -> Some(typeof<UIntPtr>)
        | _ -> None

    member x.ReadExecuteCommand() =
        match readBuffer() with
        | Some bytes ->
            let staticSize = Marshal.SizeOf typeof<execCommandStatic>
            let staticBytes, dynamicBytes = Array.splitAt staticSize bytes
            let staticPart = x.Deserialize<execCommandStatic> staticBytes
            let callStackEntrySize = Marshal.SizeOf typeof<int32>
            let callStackOffset = (int staticPart.newCallStackFramesCount) * callStackEntrySize
            let newCallStackFrames = Array.init (int staticPart.newCallStackFramesCount) (fun i -> BitConverter.ToInt32(dynamicBytes, i * callStackEntrySize))
            let mutable offset = callStackOffset
            let evaluationStackPushes = Array.init (int staticPart.evaluationStackPushesCount) (fun _ ->
                let evalStackArgTypeNum = BitConverter.ToInt32(dynamicBytes, offset)
                offset <- offset + sizeof<int32>
                let evalStackArgType = LanguagePrimitives.EnumOfValue evalStackArgTypeNum
                match evalStackArgType with
                | evalStackArgType.OpRef -> // TODO: mb use UIntPtr? #do
                    let baseAddr = BitConverter.ToUInt64(dynamicBytes, offset)
                    offset <- offset + sizeof<uint64>
                    let shift = BitConverter.ToUInt64(dynamicBytes, offset)
                    offset <- offset + sizeof<uint64>
                    PointerOp(baseAddr, shift)
                | evalStackArgType.OpSymbolic
                | evalStackArgType.OpI4
                | evalStackArgType.OpI8
                | evalStackArgType.OpR4
                | evalStackArgType.OpR8 ->
                    let content = BitConverter.ToInt64(dynamicBytes, offset)
                    offset <- offset + sizeof<int64>
                    NumericOp(evalStackArgType, content)
                | _ -> internalfailf "unexpected evaluation stack argument type %O" evalStackArgType)
            let newAddresses = Array.init (int staticPart.newAddressesCount) (fun _ ->
                let res = x.ToUIntPtr dynamicBytes offset in offset <- offset + IntPtr.Size; res)
            // TODO: 2Misha what's with these sizes?
//            let newAddressesTypesLengths = Array.init (int staticPart.newAddressesCount) (fun _ ->
//                let res = BitConverter.ToUInt64(dynamicBytes, offset) in offset <- offset + sizeof<uint64>; res)
            let newAddressesTypes = Array.init (int staticPart.newAddressesCount) (fun _ (*i*) ->
//                let size = int newAddressesTypesLengths.[i]
                let rec readType () =
                    let isValid = BitConverter.ToBoolean(dynamicBytes, offset)
                    offset <- offset + sizeof<bool>
                    if isValid then
                        let isArray = BitConverter.ToBoolean(dynamicBytes, offset)
                        offset <- offset + sizeof<bool>
                        if isArray then
                            let corElementType = Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<byte, CorElementType>(dynamicBytes.[offset])
                            offset <- offset + sizeof<byte>
                            let rank = BitConverter.ToInt32(dynamicBytes, offset)
                            offset <- offset + sizeof<int32>
                            match x.corElementTypeToType corElementType with
                            | Some t -> t.MakeArrayType(rank)
                            | None ->
                                let t : Type = readType()
                                t.MakeArrayType(rank)
                        else
                            let token = BitConverter.ToInt32(dynamicBytes, offset)
                            offset <- offset + sizeof<int>
                            let assemblySize = BitConverter.ToInt32(dynamicBytes, offset)
                            offset <- offset + sizeof<int>
                            // NOTE: truncating null terminator
                            let assemblyBytes = dynamicBytes.[offset .. offset + assemblySize - 3]
                            offset <- offset + assemblySize
                            let assemblyName = Encoding.Unicode.GetString(assemblyBytes)
                            let assembly = Reflection.loadAssembly assemblyName
                            let moduleSize = BitConverter.ToInt32(dynamicBytes, offset)
                            offset <- offset + sizeof<int>
                            let moduleBytes = dynamicBytes.[offset .. offset + moduleSize - 1]
                            offset <- offset + moduleSize
                            let moduleName = Encoding.Unicode.GetString(moduleBytes) |> Path.GetFileName
                            let typeModule = Reflection.resolveModuleFromAssembly assembly moduleName
                            let typeArgsCount = BitConverter.ToInt32(dynamicBytes, offset)
                            offset <- offset + sizeof<int>
                            let typeArgs = Array.init typeArgsCount (fun _ -> readType())
                            let resultType = Reflection.resolveTypeFromModule typeModule token
                            if Array.isEmpty typeArgs then resultType else resultType.MakeGenericType(typeArgs)
                    else typeof<Void>
                readType())
            { offset = staticPart.offset
              isBranch = staticPart.isBranch
              callStackFramesPops = staticPart.callStackFramesPops
              evaluationStackPops = staticPart.evaluationStackPops
              newCallStackFrames = newCallStackFrames
              evaluationStackPushes = evaluationStackPushes
              newAddresses = newAddresses
              newAddressesTypes = newAddressesTypes }
        | None -> unexpectedlyTerminated()

    member private x.SizeOfConcrete (typ : Type) =
        if Types.IsValueType typ then sizeof<int> + sizeof<int64>
        else sizeof<int> + 2 * sizeof<int64>

    member private x.IntegerBytesToLong (obj : obj) =
        let extended =
            match obj with
            | :? byte as v -> int64 v |> BitConverter.GetBytes
            | :? sbyte as v -> int64 v |> BitConverter.GetBytes
            | :? int16 as v -> int64 v |> BitConverter.GetBytes
            | :? uint16 as v -> int64 v |> BitConverter.GetBytes
            | :? char as v -> int64 v |> BitConverter.GetBytes
            | :? int32 as v -> int64 v |> BitConverter.GetBytes
            | :? uint32 as v -> uint64 v |> BitConverter.GetBytes
            | :? int64 as v -> BitConverter.GetBytes v
            | :? uint64 as v -> BitConverter.GetBytes v
            | _ -> internalfailf "IntegerBytesToLong: unexpected object %O" obj
        BitConverter.ToInt64 extended

    member private x.SerializeConcrete (obj : obj, typ : Type) =
        let bytes = x.SizeOfConcrete typ |> Array.zeroCreate
        let mutable index = 0
        if Types.IsValueType typ then
            let opType, (content : int64) =
                if Types.IsInteger typ then
                    let typ = if Types.SizeOf typ = sizeof<int64> then evalStackArgType.OpI8 else evalStackArgType.OpI4
                    typ, x.IntegerBytesToLong obj
                elif Types.IsReal typ then
                    if Types.SizeOf typ = sizeof<double> then
                        evalStackArgType.OpR8, BitConverter.DoubleToInt64Bits (obj :?> double)
                    else evalStackArgType.OpR4, BitConverter.DoubleToInt64Bits (obj :?> float |> double)
                elif Types.IsBool typ then
                    evalStackArgType.OpI4, if obj :?> bool then 1L else 0L
                else
                    // TODO: support structs
                    __notImplemented__()
            let success = BitConverter.TryWriteBytes(Span(bytes, index, sizeof<int>), LanguagePrimitives.EnumToValue opType) in assert success
            index <- index + sizeof<int>
            let success = BitConverter.TryWriteBytes(Span(bytes, index, sizeof<int64>), content) in assert success
            index <- index + sizeof<int64>
        elif isNull obj then
            // NOTE: null refs handling
            let success = BitConverter.TryWriteBytes(Span(bytes, index, sizeof<int>), LanguagePrimitives.EnumToValue evalStackArgType.OpRef) in assert success
            index <- index + sizeof<int>
            let success = BitConverter.TryWriteBytes(Span(bytes, index, sizeof<uint64>), 0UL) in assert success
            index <- index + sizeof<uint64>
            let success = BitConverter.TryWriteBytes(Span(bytes, index, sizeof<uint64>), 0UL) in assert success
            index <- index + sizeof<uint64>
        else
            // NOTE: nonnull refs handling
            let address, offset = obj :?> uint32 * uint64
            let success = BitConverter.TryWriteBytes(Span(bytes, index, sizeof<int>), LanguagePrimitives.EnumToValue evalStackArgType.OpRef) in assert success
            index <- index + sizeof<int>
            let success = BitConverter.TryWriteBytes(Span(bytes, index, sizeof<uint64>), uint64 address) in assert success
            index <- index + sizeof<int64>
            let success = BitConverter.TryWriteBytes(Span(bytes, index, sizeof<uint64>), offset) in assert success
            index <- index + sizeof<int64>
        bytes

    member private x.SerializeOperands (ops : (obj * Type) list) =
        let mutable index = 0
        let bytesCount = ops |> List.sumBy (snd >> x.SizeOfConcrete)
        let bytes = Array.zeroCreate bytesCount
        ops |> List.iter (fun concrete ->
            let op = x.SerializeConcrete concrete
            let size = op.Length
            Array.blit op 0 bytes index size
            index <- index + size)
        bytes

    member x.SendExecResponse (ops : (obj * Type) list option) (result : (obj * Type) option) lastPush (framesCount : int) =
        let lastPush =
            match lastPush with
            | Some isConcrete when isConcrete -> 2uy
            | Some _ -> 1uy
            | None -> 0uy
        let len, opsBytes =
            match ops with
            | Some ops -> ops.Length, x.SerializeOperands ops
            | None -> -1, Array.empty
        let hasInternalCallResult, resultBytes =
            match result with
            | Some r -> 1uy, x.SerializeConcrete r
            | None -> 0uy, Array.empty
        let staticPart = { framesCount = uint framesCount; lastPush = lastPush; opsLength = len; hasResult = hasInternalCallResult }
        let staticPartBytes = x.Serialize<execResponseStaticPart> staticPart
        let message = Array.concat [staticPartBytes; opsBytes; resultBytes]
        Logger.trace "Sending exec response! Total %d bytes" message.Length
        writeBuffer message

    member x.SendMethodBody (mb : instrumentedMethodBody) =
        x.SendCommand ReadMethodBody
        let propBytes = x.Serialize mb.properties
        let ehSize = Marshal.SizeOf typeof<rawExceptionHandler>
        let ehBytes : byte[] = Array.zeroCreate (ehSize * mb.ehs.Length)
        Array.iteri (fun i eh -> x.Serialize<rawExceptionHandler>(eh, ehBytes, i * ehSize)) mb.ehs
        let message = Array.concat [propBytes; mb.il; ehBytes]
        Logger.trace "Sending method body! Total %d bytes" message.Length
        writeBuffer message

    member x.ReadCommand() =
        match readBuffer() with
        | Some bytes ->
            if bytes.Length <> 1 then fail "Invalid command number!"
            match bytes.[0] with
            | b when b = instrumentCommandByte ->
                x.ReadMethodBody() |> Instrument
            | b when b = executeCommandByte ->
                x.ReadExecuteCommand() |> ExecuteInstruction
            | b -> fail "Unexpected command %d from client machine!" b
        | None -> Terminate

    interface IDisposable with
        member x.Dispose() =
            server.Dispose()
