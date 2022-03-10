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
    let pathToClient = "libvsharpConcolic" + extension
    let pathToTmp = sprintf "%s%c" (Directory.GetCurrentDirectory()) Path.DirectorySeparatorChar
    let tempTest (id : int) = sprintf "%sstart%d.vst" pathToTmp id
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

    let mutable callIsSkipped = false
    let mutable mainReached = false
    let mutable operands : list<_> = List.Empty
    let environment (method : MethodBase) pipePath =
        let result = ProcessStartInfo()
        let profiler = sprintf "%s%c%s" (Directory.GetCurrentDirectory()) Path.DirectorySeparatorChar pathToClient
        result.EnvironmentVariables.["CORECLR_PROFILER"] <- "{2800fea6-9667-4b42-a2b6-45dc98e77e9e}"
        result.EnvironmentVariables.["CORECLR_ENABLE_PROFILING"] <- "1"
        result.EnvironmentVariables.["CORECLR_PROFILER_PATH"] <- profiler
        result.EnvironmentVariables.["CONCOLIC_PIPE"] <- pipePath
        result.WorkingDirectory <- Directory.GetCurrentDirectory()
        result.FileName <- "dotnet"
        result.UseShellExecute <- false
        result.RedirectStandardOutput <- true
        result.RedirectStandardError <- true
        if method = (method.Module.Assembly.EntryPoint :> MethodBase) then
            result.Arguments <- method.Module.Assembly.Location
        else
            let runnerPath = "VSharp.TestRunner.dll"
            result.Arguments <- sprintf "%s %s %O" runnerPath (tempTest id) false
        result

    [<DefaultValue>] val mutable private communicator : Communicator
    member x.Spawn() =
        assert(entryPoint <> null)
        let test = UnitTest(entryPoint)
        test.Serialize(tempTest id)

        let pipe, pipePath =
            if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
                let pipe = sprintf "concolic_fifo_%d.pipe" id
                let pipePath = sprintf "\\\\.\\pipe\\%s" pipe
                pipe, pipePath
            else
                let pipeFile = sprintf "%sconcolic_fifo_%d.pipe" pathToTmp id
                pipeFile, pipeFile
        let env = environment entryPoint pipePath
        x.communicator <- new Communicator(pipe)
        let proc = Process.Start env
        id <- id + 1
        proc.OutputDataReceived.Add <| fun args -> Logger.trace "CONCOLIC OUTPUT: %s" args.Data
        proc.ErrorDataReceived.Add <| fun args -> Logger.trace "CONCOLIC ERROR: %s" args.Data
        proc.BeginOutputReadLine()
        proc.BeginErrorReadLine()
        Logger.info "Successfully spawned pid %d, working dir \"%s\"" proc.Id env.WorkingDirectory
        if x.communicator.Connect() then
            x.probes <- x.communicator.ReadProbes()
            x.communicator.SendEntryPoint entryPoint
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
        let _, evalStack = EvaluationStack.PopMany maxIndex evalStack
        operands <- Array.toList newEntries
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

    member private x.ConcreteToObj term =
        let evalRefType baseAddress offset typ =
            match baseAddress, offset.term with
            | HeapLocation({term = ConcreteHeapAddress [address]} as a, _), Concrete(offset, _) ->
                let obj = (address, uint64 (offset :?> int + metadataSizeOfAddress cilState.state a)) :> obj
                Some (obj, typ)
            // TODO: stack and statics location #do
            | _ -> None
        match term.term with
        | Concrete(obj, typ) -> Some (obj, typ)
        | _ when term = NullRef -> Some (null, Null)
        | HeapRef({term = ConcreteHeapAddress addr}, _) -> __notImplemented__()
        | Ref address ->
            let baseAddress, offset = AddressToBaseAndOffset address
            evalRefType baseAddress offset (TypeOf term)
        | Ptr(baseAddress, sightType, offset) ->
            evalRefType baseAddress offset (Pointer sightType)
        | _ -> None

    member private x.EvalOperands cilState =
        // NOTE: if there are no branching, TryGetModel forces solver to create model
        // NOTE: this made to check communication between Concolic and SILI
        // TODO: after all checks, change this to 'cilState.state.model'
        match TryGetModel cilState.state with
        | Some model ->
            let concretizedOps = operands |> List.choose (model.Eval >> x.ConcreteToObj)
            if List.length operands <> List.length concretizedOps then None
            else
                bindNewCilState cilState
                Some concretizedOps
        | None -> None

    member x.StepDone (steppedStates : cilState list) =
        if CilStateOperations.currentMethod cilState |> InstructionsSet.isFSharpInternalCall then
            callIsSkipped <- true
            cilState
        else
            let concretizedOps =
                if callIsSkipped then Some List.empty
                else steppedStates |> List.tryPick x.EvalOperands
            cilState.suspended <- true
            let lastPushInfo =
                match cilState.lastPushInfo with
                | Some x when IsConcrete x && CilStateOperations.currentIp cilState <> Exit entryPoint ->
                    CilStateOperations.pop cilState |> ignore
                    Some true
                | Some _ -> Some false
                | None -> None
            let internalCallResult =
                match cilState.lastPushInfo with
                | Some res when callIsSkipped ->
                    x.ConcreteToObj res
                | _ -> None
            let framesCount = Memory.CallStackSize cilState.state
            x.communicator.SendExecResponse concretizedOps internalCallResult lastPushInfo framesCount
            callIsSkipped <- false
            cilState
