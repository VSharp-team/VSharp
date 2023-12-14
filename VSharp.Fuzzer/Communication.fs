module public VSharp.Fuzzer.Communication

open System.Runtime.Serialization
open System.Threading
open System.Threading.Tasks
open Grpc.Core
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Server.Kestrel.Core
open Microsoft.Extensions.DependencyInjection
open ProtoBuf.Grpc
open ProtoBuf.Grpc.Client
open ProtoBuf.Grpc.Configuration
open Microsoft.Extensions.Logging
open ProtoBuf
open ProtoBuf.Grpc.Server
open Grpc.Net.Client
open VSharp

module Contracts =

    [<DataContract>]
    type UnitData() =
        class
        end

    [<DataContract; CLIMutable>]
    type BooleanData =  {
        [<DataMember(Order = 1)>] boolValue: bool
    }

    [<DataContract; CLIMutable>]
    type StringData =  {
        [<DataMember(Order = 1)>] stringValue: string
    }

    [<DataContract; CLIMutable>]
    type CoverageData = {
        [<DataMember(Order = 1)>] rawData: RawCoverageReport
        [<DataMember(Order = 2)>] methods: System.Collections.Generic.Dictionary<int, RawMethodInfo>
    }

    [<DataContract; CLIMutable>]
    type ExecutionData = {
        [<DataMember(Order = 1)>] moduleName: string 
        [<DataMember(Order = 2)>] methodId: int
        [<DataMember(Order = 3)>] threadId: int
        [<DataMember(Order = 4)>] fuzzerSeed: int
        [<DataMember(Order = 5)>] typeSolverSeed: int
    }

    [<DataContract; CLIMutable>]
    type AssemblyData = {
        [<DataMember(Order = 1)>] assemblyName: string
    }

    [<DataContract; CLIMutable>]
    type FuzzingData = {
        [<DataMember(Order = 1)>] moduleName: string 
        [<DataMember(Order = 2)>] methodId: int
    }

    [<Service>]
    type IMasterProcessService =
        abstract NotifyFinished: UnitData -> Task
        abstract TrackCoverage: CoverageData -> Task<BooleanData>
        abstract TrackExecutionSeed: ExecutionData -> Task
        abstract WaitForReady: UnitData -> CallContext -> Task

    [<Service>]
    type IFuzzerService =
        abstract Finish: UnitData -> Task
        abstract SetupAssembly: AssemblyData -> Task
        abstract SetupOutputDirectory: StringData -> Task
        abstract Fuzz: FuzzingData -> Task
        abstract WaitForReady: UnitData -> CallContext -> Task



module private Grpc =
    let runGrpcServer<'service when 'service: not struct> port (service: 'service) token =
        WebHost
           .CreateDefaultBuilder()
           .ConfigureKestrel(fun options ->
                options.ListenLocalhost(port, fun listenOptions ->
                    listenOptions.Protocols <- HttpProtocols.Http2
                )
           )
           .ConfigureServices(fun services ->
               services
                   .AddSingleton(service)
                   .AddLogging(fun loggingBuilder ->
                        loggingBuilder
                            .AddConsole()
                            .SetMinimumLevel(LogLevel.Critical) |> ignore
                    )
                    .AddCodeFirstGrpc()
            )
           .Configure(fun app ->
               app
                   .UseRouting()
                   .UseEndpoints(fun endpoints -> endpoints.MapGrpcService<'service>() |> ignore)
               |> ignore
            )
           .Build()
           .RunAsync(token)

    let runGrpcClient<'service when 'service: not struct> (port: int) =
        GrpcClientFactory.AllowUnencryptedHttp2 <- true
        let channel = GrpcChannel.ForAddress($"http://localhost:{port}");
        let service = channel.CreateGrpcService<'service>()
        service

module Services =
    let private traceData x = Logger.traceCommunication $"Received: {x}"

    type FuzzerService (onFinish, onFuzz, onSetupAssembly, onSetupDir) =
        interface Contracts.IFuzzerService with
            member this.Finish _ =
                traceData "Finish"
                onFinish ()
            member this.Fuzz data =
                traceData data
                onFuzz data.moduleName data.methodId
            member this.SetupAssembly data =
                traceData data
                onSetupAssembly data.assemblyName
            member this.SetupOutputDirectory data =
                traceData data
                onSetupDir data.stringValue
            member this.WaitForReady _ _ =
                traceData "WaitForReady"
                Task.FromResult() :> Task

    type MasterProcessService (onTrackCoverage, onTrackExecutionSeed, onFinished) =
        interface Contracts.IMasterProcessService with
            member this.TrackCoverage data =
                traceData data
                onTrackCoverage data.methods data.rawData
            member this.TrackExecutionSeed data =
                traceData data
                onTrackExecutionSeed data
            member this.WaitForReady _ _ =
                traceData "WaitForReady"
                Task.FromResult() :> Task
            member this.NotifyFinished _ =
                traceData "Finished"
                onFinished ()


let private fuzzerPort = 10042
let private masterProcessPort = 10043

let connectFuzzerService () = Grpc.runGrpcClient<Contracts.IFuzzerService> fuzzerPort
let connectMasterProcessService () = Grpc.runGrpcClient<Contracts.IMasterProcessService> masterProcessPort

let private waitTimeout = 1000000
let private waitServiceForReady wait =
    let tokenSource = new CancellationTokenSource()
    let mutable callOptions = CallOptions().WithWaitForReady(true).WithCancellationToken(tokenSource.Token)
    let (task: Task) = wait (Contracts.UnitData()) (CallContext.op_Implicit(&callOptions))
    tokenSource.CancelAfter waitTimeout
    task.Wait()
    task.IsCompleted

let waitFuzzerForReady (service: Contracts.IFuzzerService) =
    let connected = waitServiceForReady service.WaitForReady
    if not connected then
        internalfail "Failed to connect fuzzer"

let waitMasterProcessForReady (service: Contracts.IMasterProcessService) =
    let connected = waitServiceForReady service.WaitForReady
    if not connected then
        internalfail "Failed to connect symbolic execution"

let startFuzzerService service =
    let service = Grpc.runGrpcServer<Services.FuzzerService> fuzzerPort service
    Logger.traceCommunication "Fuzzer service started"
    service

let startMasterProcessService service =
    let service = Grpc.runGrpcServer<Services.MasterProcessService> masterProcessPort service
    Logger.traceCommunication "Symbolic execution service started"
    service
