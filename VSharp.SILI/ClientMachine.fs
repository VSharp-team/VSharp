namespace VSharp.Concolic

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL


type ClientMachine(entryPoint : MethodBase, requestMakeStep : cilState -> unit, state : state) =
    let extension =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then ".dll"
        elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then ".so"
        elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then ".dylib"
        else __notImplemented__()
    let pathToClient = "libicsharpConcolic" + extension
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
        initSymbolicFrame state entryPoint
        let cilState = CilStateOperations.makeInitialState entryPoint state
        cilState.ownedByConcolic <- true
        cilState

    let bindNewCilState newState =
        if not <| LanguagePrimitives.PhysicalEquality cilState newState then
            cilState.ownedByConcolic <- false
            newState.ownedByConcolic <- true
            cilState <- newState

    let mutable mainReached = false
    let mutable poppedSymbolics : list<_> = List.Empty
    let environment (method : MethodBase) =
        let result = ProcessStartInfo()
        result.EnvironmentVariables.["CORECLR_PROFILER"] <- "{cf0d821e-299b-5307-a3d8-b283c03916dd}"
        result.EnvironmentVariables.["CORECLR_ENABLE_PROFILING"] <- "1"
        result.EnvironmentVariables.["CORECLR_PROFILER_PATH"] <- Directory.GetCurrentDirectory() + "/" + pathToClient
        result.WorkingDirectory <- Directory.GetCurrentDirectory()
        result.FileName <- "dotnet"
        result.UseShellExecute <- false
        result.RedirectStandardOutput <- true
        result.RedirectStandardError <- true
        if method = (method.Module.Assembly.EntryPoint :> MethodBase) then
            result.Arguments <- entryPoint.Module.Assembly.Location
        else
            let runnerPath = "./VSharp.Runner.dll"
            let moduleFqn = method.Module.FullyQualifiedName
            let assemblyPath = method.Module.Assembly.Location
            let token = method.MetadataToken.ToString()
            result.Arguments <- runnerPath + " " + assemblyPath + " " + moduleFqn + " " + token
        result

    [<DefaultValue>] val mutable private communicator : Communicator
    member x.Spawn() =
        assert(entryPoint <> null)
        let env = environment entryPoint
        x.communicator <- new Communicator()
        let proc = Process.Start env
        proc.OutputDataReceived.Add <| fun args -> Logger.trace "CONCOLIC OUTPUT: %s" args.Data
        proc.ErrorDataReceived.Add <| fun args -> Logger.trace "CONCOLIC ERROR: %s" args.Data
        proc.BeginOutputReadLine()
        proc.BeginErrorReadLine()
        Logger.info "Successfully spawned pid %d, working dir \"%s\"" proc.Id env.WorkingDirectory
        if not <| x.communicator.Connect() then false
        else
            x.probes <- x.communicator.ReadProbes()
            x.instrumenter <- Instrumenter(x.communicator, entryPoint, x.probes)
            true

    member x.SynchronizeStates (c : execCommand) =
        Memory.ForcePopFrames (int c.callStackFramesPops) cilState.state
        assert(Memory.CallStackSize state > 0)
        let initFrame state token =
            let topMethod = Memory.GetCurrentExploringFunction state
            let method = Reflection.resolveMethod topMethod token
            initSymbolicFrame state method
        Array.iter (initFrame state) c.newCallStackFrames
        let evalStack = EvaluationStack.PopMany (int c.evaluationStackPops) state.evaluationStack |> snd
        let mutable maxIndex = 0
        let newEntries = c.evaluationStackPushes |> Array.map (function
            | NumericOp(evalStackArgType, content) ->
                match evalStackArgType with
                | evalStackArgType.OpSymbolic ->
                    let idx = int content
                    maxIndex <- max maxIndex (idx + 1)
                    EvaluationStack.GetItem idx state.evaluationStack
                | evalStackArgType.OpI4 ->
                    Concrete (int content) TypeUtils.int32Type
                | evalStackArgType.OpI8 ->
                    Concrete content TypeUtils.int32Type
                | evalStackArgType.OpR4 ->
                    Concrete (BitConverter.Int32BitsToSingle (int content)) TypeUtils.float32Type
                | evalStackArgType.OpR8 ->
                    Concrete (BitConverter.Int64BitsToDouble content) TypeUtils.float64Type
                | _ -> __unreachable__()
            | PointerOp(baseAddress, offset) ->
                    // TODO: 2misha: parse object ref here
                    __notImplemented__())
        let ps, evalStack = EvaluationStack.PopMany maxIndex evalStack
        poppedSymbolics <- ps
        let evalStack = Array.foldBack EvaluationStack.Push newEntries evalStack
        state.evaluationStack <- evalStack
        cilState.ipStack <- [Instruction(int c.offset, Memory.GetCurrentExploringFunction state)]
        cilState.lastPushInfo <- None

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
            // TODO: schedule it into interpreter, send back response
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
                let concretizedSymbolics = poppedSymbolics |> List.map (fun term ->
                    match model.Eval term with
                    | {term = Concrete(obj, typ)} -> (obj, typ)
                    | NullRef -> (null, Null)
                    | {term = HeapRef({term = ConcreteHeapAddress addr}, _)} -> __notImplemented__()
                    // TODO: concrete ref and ptr
                    | _ -> allConcrete <- false; (null, Null))
                if allConcrete then
                    cilState <- cilState'
                    Some concretizedSymbolics
                else None)
        let lastPushInfo = cilState.lastPushInfo |> Option.map IsConcrete
        x.communicator.SendExecResponse concretizedOps lastPushInfo
