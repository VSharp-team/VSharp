namespace VSharp.Concolic

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL

[<AllowNullLiteral>]
type ClientMachine(entryPoint : MethodBase, requestMakeStep : cilState -> unit, cilState : cilState) =
    let extension =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then ".dll"
        elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then ".so"
        elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then ".dylib"
        else __notImplemented__()
    let pathToClient = "libicsharpConcolic" + extension
    let tempTest (id : int) = Path.GetTempPath() + "start" + id.ToString() + ".vst"
    [<DefaultValue>] val mutable probes : probes
    [<DefaultValue>] val mutable instrumenter : Instrumenter

    let initSymbolicFrame state (method : MethodBase) =
        let parameters = method.GetParameters() |> Seq.map (fun param ->
            (ParameterKey param, None, Types.FromDotNetType param.ParameterType)) |> List.ofSeq
        let locals =
            match method.GetMethodBody() with
            | null -> []
            | body ->
                body.LocalVariables
                |> Seq.map (fun local -> (LocalVariableKey(local, method), None, Types.FromDotNetType local.LocalType))
                |> List.ofSeq
        let parametersAndThis =
            if Reflection.hasThis method then
                (ThisKey method, None, Types.FromDotNetType method.DeclaringType) :: parameters // TODO: incorrect type when ``this'' is Ref to stack
            else parameters
        Memory.NewStackFrame state method (parametersAndThis @ locals)

    let mutable cilState : cilState =
        cilState.suspended <- true
        cilState

    let bindNewCilState newState =
        if not <| LanguagePrimitives.PhysicalEquality cilState newState then
            cilState.suspended <- false
            newState.suspended <- true
            cilState <- newState

    let metadataSizeOfAddress state address =
        let t = TypeOfAddress state address
        if t = Types.String then CSharpUtils.LayoutUtils.StringElementsOffset
        elif Types.IsArrayType t then CSharpUtils.LayoutUtils.ArrayElementsOffset
        else 0

    static let mutable id = 0

    let mutable mainReached = false
    let mutable poppedSymbolics : list<_> = List.Empty
    let environment (method : MethodBase) pipeFile =
        let result = ProcessStartInfo()
        result.EnvironmentVariables.["CORECLR_PROFILER"] <- "{cf0d821e-299b-5307-a3d8-b283c03916dd}"
        result.EnvironmentVariables.["CORECLR_ENABLE_PROFILING"] <- "1"
        result.EnvironmentVariables.["CORECLR_PROFILER_PATH"] <- Directory.GetCurrentDirectory() + "/" + pathToClient
        result.EnvironmentVariables.["CONCOLIC_PIPE"] <- pipeFile
        result.WorkingDirectory <- Directory.GetCurrentDirectory()
        result.FileName <- "dotnet"
        result.UseShellExecute <- false
        result.RedirectStandardOutput <- true
        result.RedirectStandardError <- true
        if method = (method.Module.Assembly.EntryPoint :> MethodBase) then
            result.Arguments <- method.Module.Assembly.Location
        else
            let runnerPath = "./VSharp.TestRunner.dll"
            result.Arguments <- runnerPath + " " + (tempTest id)
        result

    [<DefaultValue>] val mutable private communicator : Communicator
    member x.Spawn() =
        assert(entryPoint <> null)
        let test = UnitTest(entryPoint)
        test.Serialize(tempTest id)

        let pipeFile = sprintf "%sconcolic_fifo_%d" (Path.GetTempPath()) id
        let env = environment entryPoint pipeFile
        let proc = Process.Start env
        x.communicator <- new Communicator(pipeFile)
        id <- id + 1
        proc.OutputDataReceived.Add <| fun args -> Logger.trace "CONCOLIC OUTPUT: %s" args.Data
        proc.ErrorDataReceived.Add <| fun args -> Logger.trace "CONCOLIC ERROR: %s" args.Data
        proc.BeginOutputReadLine()
        proc.BeginErrorReadLine()
        Logger.info "Successfully spawned pid %d, working dir \"%s\"" proc.Id env.WorkingDirectory
        if x.communicator.Connect() then
            x.probes <- x.communicator.ReadProbes()
            x.instrumenter <- Instrumenter(x.communicator, entryPoint, x.probes)
            true
        else false

    member x.SynchronizeStates (c : execCommand) =
        Memory.ForcePopFrames (int c.callStackFramesPops) cilState.state
        assert(Memory.CallStackSize cilState.state > 0)
        let initFrame state token =
            let topMethod = Memory.GetCurrentExploringFunction state
            let method = Reflection.resolveMethod topMethod token
            initSymbolicFrame state method
        Array.iter (initFrame cilState.state) c.newCallStackFrames
        let evalStack = EvaluationStack.PopMany (int c.evaluationStackPops) cilState.state.evaluationStack |> snd
        let allocatedTypes = Array.fold2 (fun types address typ -> PersistentDict.add [int address] (Types.FromDotNetType typ) types) cilState.state.allocatedTypes c.newAddresses c.newAddressesTypes
        cilState.state.allocatedTypes <- allocatedTypes
        let mutable maxIndex = 0
        let newEntries = c.evaluationStackPushes |> Array.map (function
            | NumericOp(evalStackArgType, content) ->
                match evalStackArgType with
                | evalStackArgType.OpSymbolic ->
                    let idx = int content
                    maxIndex <- max maxIndex (idx + 1)
                    EvaluationStack.GetItem idx cilState.state.evaluationStack
                | evalStackArgType.OpI4 ->
                    Concrete (int content) TypeUtils.int32Type
                | evalStackArgType.OpI8 ->
                    Concrete content TypeUtils.int64Type
                | evalStackArgType.OpR4 ->
                    Concrete (BitConverter.Int32BitsToSingle (int content)) TypeUtils.float32Type
                | evalStackArgType.OpR8 ->
                    Concrete (BitConverter.Int64BitsToDouble content) TypeUtils.float64Type
                | _ -> __unreachable__()
            | PointerOp(baseAddress, offset) ->
                // TODO: what about StackLocation and StaticLocation? #do
                let address = ConcreteHeapAddress [int32 baseAddress]
                let typ = TypeOfAddress cilState.state address
                if offset = 0UL then
                    HeapRef address typ
                else
                    let offset = int offset - metadataSizeOfAddress cilState.state address
                    let offset = Concrete offset Types.TLength
                    Ptr (HeapLocation(address, typ)) Void offset)
        let ps, evalStack = EvaluationStack.PopMany maxIndex evalStack
        poppedSymbolics <- ps
        let evalStack = Array.fold (fun stack x -> EvaluationStack.Push x stack) evalStack newEntries
        cilState.state.evaluationStack <- evalStack
        cilState.ipStack <- [Instruction(int c.offset, Memory.GetCurrentExploringFunction cilState.state)]
        cilState.lastPushInfo <- None

    member x.State with get() = cilState

    member x.ExecCommand() =
        Logger.trace "Reading next command..."
        match x.communicator.ReadCommand() with
        | Instrument methodBody ->
            if int methodBody.properties.token = entryPoint.MetadataToken && methodBody.moduleName = entryPoint.Module.FullyQualifiedName then
                mainReached <- true
            let mb =
                if mainReached then
                    Logger.trace "Got instrument command! bytes count = %d, max stack size = %d, eh count = %d" methodBody.il.Length methodBody.properties.maxStackSize methodBody.ehs.Length
                    x.instrumenter.Instrument methodBody
                else x.instrumenter.Skip methodBody
            x.communicator.SendMethodBody mb
            true
        | ExecuteInstruction c ->
            Logger.trace "Got execute instruction command!"
            x.SynchronizeStates c
            cilState.suspended <- false
            requestMakeStep cilState
            true
        | Terminate ->
            Logger.trace "Got terminate command!"
            false

    member x.StepDone (steppedStates : cilState list) =
        let concretizedOps = steppedStates |> List.tryPick (fun cilState' ->
            match cilState'.state.model with
            | None -> None
            | Some model ->
                let mutable allConcrete = true
                // TODO: fix style #style
                let makeObjFromBaseAndOffset baseAddress offset =
                    match baseAddress, offset.term with
                    | HeapLocation({term = ConcreteHeapAddress [address]} as a, _), Concrete(offset, _) ->
                        (address, uint64 (offset :?> int + metadataSizeOfAddress cilState'.state a)) :> obj
                    // TODO: stack and statics location #do
                    | _ -> allConcrete <- false; null
                let concretizedSymbolics = poppedSymbolics |> List.map (fun term ->
                    let concretizedTerm = model.Eval term
                    match concretizedTerm.term with
                    | Concrete(obj, typ) -> (obj, typ)
                    | _ when concretizedTerm = NullRef -> (null, Null)
                    | HeapRef({term = ConcreteHeapAddress addr}, _) -> __notImplemented__()
                    | Ref address ->
                        let baseAddress, offset = AddressToBaseAndOffset address
                        makeObjFromBaseAndOffset baseAddress offset, TypeOf concretizedTerm
                    | Ptr(baseAddress, sightType, offset) ->
                        makeObjFromBaseAndOffset baseAddress offset, Pointer sightType
                    | _ -> allConcrete <- false; (null, Null))
                if allConcrete then
                    bindNewCilState cilState'
                    Some concretizedSymbolics
                else None)
        cilState.suspended <- true
        let lastPushInfo = // TODO: fix style #style
            match cilState.lastPushInfo with
            | Some x ->
                if IsConcrete x && CilStateOperations.currentIp cilState <> Exit entryPoint then
                    CilStateOperations.pop cilState |> ignore
                    Some true
                else Some false
            | None -> None
        let framesCount = Memory.CallStackSize cilState.state
        x.communicator.SendExecResponse concretizedOps lastPushInfo framesCount
        cilState
