namespace VSharp.Concolic

open System
open System.Collections.Generic
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

    let cilState : cilState =
        cilState.concolicState <- Running
        cilState

    let initSymbolicFrame (method : MethodBase) = // TODO: unify with InitFunctionFrame
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
        Memory.NewStackFrame cilState.state method (parametersAndThis @ locals)
        // NOTE: initializing all ipStack frames with -1 offset, because real offset of previous frames is unknown,
        //       but length of ipStack must be equal to length of stack frames
        CilStateOperations.pushToIp (ipOperations.instruction method -1) cilState

//    let bindNewCilState newState =
//        if not <| LanguagePrimitives.PhysicalEquality cilState newState then
//            cilState.suspended <- false
//            newState.suspended <- true
//            cilState <- newState

    let metadataSizeOfAddress state address =
        let t = TypeOfAddress state address |> Types.ToDotNetType
        CSharpUtils.LayoutUtils.MetadataSize t

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

    let mutable concolicProcess = new Process()
    let mutable isRunning = false
    let concolicStackKeys = Dictionary<stackKey, UIntPtr>()
    let registerStackKeyAddress stackKey (address : UIntPtr) =
        if concolicStackKeys.ContainsKey stackKey then
            assert(concolicStackKeys[stackKey] = address)
        else concolicStackKeys.Add(stackKey, address)
    let getStackKeyAddress stackKey =
        concolicStackKeys[stackKey]

    member private x.CreateTest() =
        assert(entryPoint <> null)
        let test = UnitTest(entryPoint)
        let typ = entryPoint.DeclaringType
        let typeParams =
            if typ.IsGenericType && not typ.IsConstructedGenericType then
                typ.GetGenericArguments()
            else Array.empty
        let methodParams =
            if entryPoint.IsGenericMethod && not entryPoint.IsConstructedGenericMethod then
                entryPoint.GetGenericArguments()
            else Array.empty
        let genericParams = Array.append typeParams methodParams
        if Array.isEmpty genericParams |> not then
            let paramsList = genericParams |> Array.toList
            match TypeSolver.solve [] paramsList with
            | TypeSat(_, genericParams) ->
                let classParams, methodParams = List.splitAt typeParams.Length genericParams
                test.SetTypeGenericParameters (Array.ofList classParams)
                test.SetMethodGenericParameters (Array.ofList methodParams)
            | TypeVariablesUnknown -> raise (InsufficientInformationException "Could not detect appropriate substitution of generic parameters")
            | TypesOfInputsUnknown -> raise (InsufficientInformationException "Could not detect appropriate types of inputs")
            | TypeUnsat -> __unreachable__()
        test.Serialize(tempTest id)

    member x.Spawn() =
        x.CreateTest()
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
        concolicProcess <- Process.Start env
        isRunning <- true
        id <- id + 1
        concolicProcess.OutputDataReceived.Add <| fun args -> Logger.trace "CONCOLIC OUTPUT: %s" args.Data
        concolicProcess.ErrorDataReceived.Add <| fun args -> Logger.trace "CONCOLIC ERROR: %s" args.Data
        concolicProcess.BeginOutputReadLine()
        concolicProcess.BeginErrorReadLine()
        Logger.info "Successfully spawned pid %d, working dir \"%s\"" concolicProcess.Id env.WorkingDirectory
        cilState.state.concreteMemory <- ConcolicMemory(x.communicator)
        if x.communicator.Connect() then
            x.probes <- x.communicator.ReadProbes()
            x.communicator.SendEntryPoint entryPoint
            x.communicator.SendCoverageInformation cilState.path
            x.instrumenter <- Instrumenter(x.communicator, entryPoint, x.probes)
            true
        else false

    member x.Terminate() =
        Logger.trace "ClientMachine.Terminate()"
        concolicProcess.Kill()
        concolicProcess.WaitForExit()

    member x.IsRunning = isRunning

    member private x.MarshallRefFromConcolic baseAddress offset key =
        match key with
        | ReferenceType ->
            let cm = cilState.state.concreteMemory
            let address = cm.GetVirtualAddress baseAddress |> ConcreteHeapAddress
            let typ = TypeOfAddress cilState.state address
            match offset with
            | _ when offset = UIntPtr.Zero && not (Types.IsValueType typ) -> HeapRef address typ
            | _ when offset = UIntPtr.Zero -> HeapRef address Types.ObjectType
            | _ ->
                let offset = int offset - metadataSizeOfAddress cilState.state address
                let offset = Concrete offset Types.TLength
                Ptr (HeapLocation(address, typ)) Void offset
        | LocalVariable(frame, idx) ->
            let stackKey = Memory.FindLocalVariableByIndex cilState.state (int frame) (int idx)
            registerStackKeyAddress stackKey baseAddress
            let offset = int offset |> MakeNumber
            Ptr (StackLocation stackKey) Void offset
        | Parameter(frame, idx) ->
            let stackKey = Memory.FindParameterByIndex cilState.state (int frame) (int idx)
            registerStackKeyAddress stackKey baseAddress
            let offset = int offset |> MakeNumber
            Ptr (StackLocation stackKey) Void offset
        | Statics(staticFieldID) ->
            let fieldInfo = x.instrumenter.StaticFieldByID (int staticFieldID)
            let typ = Types.FromDotNetType fieldInfo.DeclaringType
            let fieldOffset = CSharpUtils.LayoutUtils.GetFieldOffset fieldInfo
            let offset = MakeNumber (fieldOffset + int offset)
            Ptr (StaticLocation typ) Void offset

    member private x.CalleeArgTypesIfPossible() =
        let m = Memory.GetCurrentExploringFunction cilState.state
        let offset = CilStateOperations.currentOffset cilState
        match offset with
        | Some offset ->
            let opCode = Instruction.parseInstruction m offset
            let cfg = m |> CFG.findCfg
            let opcodeValue = LanguagePrimitives.EnumOfValue opCode.Value
            match opcodeValue with
            | OpCodeValues.Call
            | OpCodeValues.Callvirt
            | OpCodeValues.Newobj ->
                let callee = TokenResolver.resolveMethodFromMetadata cfg.methodBase cfg.ilBytes (offset + opCode.Size)
                let argTypes = callee.GetParameters() |> Array.map (fun p -> p.ParameterType)
                if Reflection.hasThis callee then Array.append [|callee.DeclaringType|] argTypes else argTypes
            | _ -> internalfail "CalleeParamsIfPossible: unexpected opcode"
        | None -> internalfail "CalleeParamsIfPossible: could not get offset"

    member x.SynchronizeStates (c : execCommand) =
        let toPop = int c.callStackFramesPops
        if toPop > 0 then CilStateOperations.popFramesOf toPop cilState
        assert(Memory.CallStackSize cilState.state > 0)
        let initFrame (moduleToken, methodToken) =
            let method = Coverage.resolveMethod moduleToken methodToken
            initSymbolicFrame method
        Array.iter initFrame c.newCallStackFrames
        let setIp ip offset =
            match ip with
            | Instruction(_, m) -> Instruction(offset, m)
            | _ -> internalfailf "SynchronizeStates: unexpected ip in ipStack: %O" ip
        cilState.ipStack <- List.map2 setIp cilState.ipStack (List.rev c.ipStack)
        let evalStack = EvaluationStack.PopMany (int c.evaluationStackPops) cilState.state.evaluationStack |> snd
        let concreteMemory = cilState.state.concreteMemory
        let allocateAddress address typ =
            let typ = Types.FromDotNetType typ
            let concreteAddress = lazy(Memory.AllocateEmptyType cilState.state typ)
            concreteMemory.Allocate address concreteAddress
        Array.iter2 allocateAddress c.newAddresses c.newAddressesTypes
        let argTypes = lazy x.CalleeArgTypesIfPossible()
        let mutable maxIndex = 0
        let createTerm i operand =
            match operand with
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
                    Concrete (BitConverter.Int64BitsToDouble content |> float32) TypeUtils.float32Type
                | evalStackArgType.OpR8 ->
                    Concrete (BitConverter.Int64BitsToDouble content) TypeUtils.float64Type
                | _ -> __unreachable__()
            | PointerOp(baseAddress, offset, key) ->
                x.MarshallRefFromConcolic baseAddress offset key
            | EmptyOp ->
                let argTypes = argTypes.Value
                Types.FromDotNetType argTypes[i] |> Memory.DefaultOf
        let newEntries = c.evaluationStackPushes |> Array.mapi createTerm
        let _, evalStack = EvaluationStack.PopMany maxIndex evalStack
        operands <- Array.toList newEntries
        let evalStack = Array.fold (fun stack x -> EvaluationStack.Push x stack) evalStack newEntries
        cilState.state.evaluationStack <- evalStack
        cilState.lastPushInfo <- None
        cilState.path <- c.newCoveragePath @ cilState.path

    member x.State with get() = cilState

    member x.ExecCommand() =
        Logger.trace "Reading next command..."
        try
            match x.communicator.ReadCommand() with
            | Instrument methodBody ->
                if int methodBody.properties.token = entryPoint.MetadataToken && methodBody.moduleName = entryPoint.Module.FullyQualifiedName then
                    mainReached <- true
                let methodBody =
                    if mainReached then
                        Logger.trace "Got instrument command! bytes count = %d, max stack size = %d, eh count = %d" methodBody.il.Length methodBody.properties.maxStackSize methodBody.ehs.Length
                        x.instrumenter.Instrument methodBody
                    else x.instrumenter.Skip methodBody
                x.communicator.SendMethodBody methodBody
                true
            | ExecuteInstruction c ->
                Logger.trace "Got execute instruction command!"
                x.SynchronizeStates c
                cilState.concolicState <- Waiting
                requestMakeStep cilState
                true
            | Terminate ->
                Logger.trace "Got terminate command!"
                isRunning <- false
                false
        with
        | :? IOException ->
            Logger.trace "exception caught in concolic machine, waiting process to terminate..."
            if concolicProcess.WaitForExit(1000) |> not then x.Terminate()
            Logger.trace "process terminated, exit code = %d" concolicProcess.ExitCode
            reraise()

    member private x.ConcreteToObj term =
        let evalRefType baseAddress offset typ =
            // NOTE: deserialization of object location is not needed, because Concolic needs only address and offset
            match baseAddress, offset.term with
            | HeapLocation({term = ConcreteHeapAddress address} as a, _), Concrete(offset, _) ->
                let address = cilState.state.concreteMemory.GetPhysicalAddress address
                let offset = offset :?> int + metadataSizeOfAddress cilState.state a
                assert(offset > 0)
                let obj = (address, offset) :> obj
                Some (obj, typ)
            | StackLocation stackKey, Concrete(offset, _) ->
                let address = getStackKeyAddress stackKey
                let obj = (address, offset :?> int) :> obj
                Some (obj, typ)
            | StaticLocation _, Concrete _ ->
                internalfail "Unmarshalling for ptr on static location is not implemented!"
            | _ -> None
        match term.term with
        | Concrete(obj, typ) -> Some (obj, typ)
        | _ when term = NullRef -> Some (null, Null)
        | HeapRef({term = ConcreteHeapAddress address}, typ) ->
            let address = cilState.state.concreteMemory.GetPhysicalAddress address
            let content = (address, 0) :> obj
            Some (content, typ)
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
//        match TryGetModel cilState.state with
//        | Some model ->
//            let concretizedOps = operands |> List.choose (model.Eval >> x.ConcreteToObj)
//            if List.length operands <> List.length concretizedOps then None
//            else
//                bindNewCilState cilState
//                Some concretizedOps
//        | None -> None
        None

    member x.StepDone (steppedStates : cilState list) =
        let methodEnded = CilStateOperations.methodEnded cilState
        let notEndOfEntryPoint = CilStateOperations.currentIp cilState <> Exit entryPoint
        let isIIEState = CilStateOperations.isIIEState cilState
        if methodEnded && notEndOfEntryPoint then
            let method = CilStateOperations.currentMethod cilState
            if InstructionsSet.isFSharpInternalCall method then callIsSkipped <- true
            cilState
        else
            let concretizedOps =
                if callIsSkipped then Some List.empty
                else steppedStates |> List.tryPick x.EvalOperands
            let concolicMemory = cilState.state.concreteMemory
            let disconnectConcolic cilState =
                cilState.concolicState <- Disabled
                cilState.state.concreteMemory <- Memory.EmptyConcreteMemory()
            steppedStates |> List.iter disconnectConcolic
            let connectConcolic cilState =
                cilState.concolicState <- Running
                cilState.state.concreteMemory <- concolicMemory
            if notEndOfEntryPoint && not isIIEState then connectConcolic cilState
            let topIsEmpty = EvaluationStack.TopIsEmpty cilState.state.evaluationStack
            let lastPushInfo =
                match cilState.lastPushInfo with
                | Some x when IsConcrete x && notEndOfEntryPoint && topIsEmpty ->
                    // NOTE: Newobj case
                    let evaluationStack = EvaluationStack.PopFromAnyFrame cilState.state.evaluationStack |> snd
                    cilState.state.evaluationStack <- evaluationStack
                    None
                | Some x when IsConcrete x && notEndOfEntryPoint ->
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
