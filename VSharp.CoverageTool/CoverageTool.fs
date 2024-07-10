namespace VSharp.CoverageTool

open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open FSharpx.Collections
open Microsoft.FSharp.NativeInterop

open VSharp
open VSharp.Utils.EnvironmentUtils
open VSharp.CSharpUtils


#nowarn "9"

module private ExternalCalls =
    [<DllImport(CoverageToolInfo.DllName, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
    extern void SetEntryMain(byte* assemblyName, int assemblyNameLength, byte* moduleName, int moduleNameLength, int methodToken)

    [<DllImport(CoverageToolInfo.DllName, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
    extern void GetHistory(nativeint size, nativeint data)

    // frees up the memory used to pass history information; a must call after every GetHistory
    [<DllImport(CoverageToolInfo.DllName, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
    extern void ClearHistory()

    [<DllImport(CoverageToolInfo.DllName, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
    extern void SetCurrentThreadId(int id)

module private Configuration =

    // TODO: a enum to avoid unnecessary code copy?
    let private availableCoverageToolOptions = [|
        "CORECLR_PROFILER";
        "CORECLR_PROFILER_PATH";
        "CORECLR_ENABLE_PROFILING";
        "COVERAGE_TOOL_INSTRUMENT_MAIN_ONLY";
        "COVERAGE_TOOL_RESULT_NAME";
        "COVERAGE_TOOL_SPECIFY_MAIN_METHOD";
        "COVERAGE_TOOL_METHOD_ASSEMBLY_NAME";
        "COVERAGE_TOOL_METHOD_MODULE_NAME";
        "COVERAGE_TOOL_METHOD_TOKEN";
        "COVERAGE_TOOL_ENABLE_PASSIVE";
        "COVERAGE_TOOL_RESULT_NAME";
        "COVERAGE_TOOL_EXPECT_TEST_SUITE";
        "COVERAGE_TOOL_ASSEMBLY_PATHS_FILE";
        "COVERAGE_TOOL_MAIN_ASSEMBLY_PATHS_FILE";
        |]
    
    let createUnspecifiedCoverageToolConfiguration() =
        let result = EnvironmentCollection()
        for envVar in availableCoverageToolOptions do
            result[envVar] <- EnvVarUnspecified
        result

    [<EnvironmentConfiguration>]
    type private BaseCoverageToolConfiguration = {
        [<EnvironmentVariable("CORECLR_PROFILER")>]
        coreclrProfiler: string
        [<EnvironmentVariable("CORECLR_PROFILER_PATH")>]
        coreclrProfilerPath: string
        [<EnvironmentVariable("CORECLR_ENABLE_PROFILING")>]
        coreclrEnableProfiling: string
        [<EnvironmentVariable("COVERAGE_TOOL_INSTRUMENT_MAIN_ONLY")>]
        instrumentMainOnly: string
    }

    [<EnvironmentConfiguration>]
    type private PassiveModeConfiguration = {
        [<EnvironmentVariable("COVERAGE_TOOL_ENABLE_PASSIVE")>]
        passiveModeEnable: string
        [<EnvironmentVariable("COVERAGE_TOOL_RESULT_NAME")>]
        resultName: string
        [<EnvironmentVariable("COVERAGE_TOOL_SPECIFY_MAIN_METHOD")>]
        mainMethodSpecified: string
        [<EnvironmentVariable("COVERAGE_TOOL_METHOD_ASSEMBLY_NAME")>]
        assemblyName: string
        [<EnvironmentVariable("COVERAGE_TOOL_METHOD_MODULE_NAME")>]
        moduleName: string
        [<EnvironmentVariable("COVERAGE_TOOL_METHOD_TOKEN")>]
        methodToken: string
    }

    [<EnvironmentConfiguration>]
    type private UnderDotnetTestConfiguration = {
        [<EnvironmentVariable("COVERAGE_TOOL_ENABLE_PASSIVE")>]
        passiveModeEnable: string
        [<EnvironmentVariable("COVERAGE_TOOL_RESULT_NAME")>]
        resultNamePath: string
        [<EnvironmentVariable("COVERAGE_TOOL_EXPECT_TEST_SUITE")>]
        expectTestSuite: string
        [<EnvironmentVariable("COVERAGE_TOOL_ASSEMBLY_PATHS_FILE")>]
        assemblyPathsFile: string
        [<EnvironmentVariable("COVERAGE_TOOL_MAIN_ASSEMBLY_PATHS_FILE")>]
        mainAssemblyPathsFile: string
    }

    let private withCoverageToolConfiguration mainOnly processInfo =
        let configuration =
            {
                coreclrProfiler = CoverageToolInfo.ProfilerUid
                coreclrProfilerPath = CoverageToolInfo.fullPath
                coreclrEnableProfiling = CoverageToolInfo.Enable
                instrumentMainOnly = if mainOnly then CoverageToolInfo.Enable else ""
            }
        withConfiguration configuration processInfo

    let withMainOnlyCoverageToolConfiguration =
        withCoverageToolConfiguration true

    let withAllMethodsCoverageToolConfiguration (collection: EnvironmentCollection) =
        withCoverageToolConfiguration false collection

    let withPassiveModeConfiguration (method: MethodBase) resultName processInfo =
        let configuration =
            {
                passiveModeEnable = CoverageToolInfo.Enable
                resultName = resultName
                mainMethodSpecified = CoverageToolInfo.Enable 
                assemblyName = method.Module.Assembly.FullName
                moduleName = method.Module.FullyQualifiedName
                methodToken = method.MetadataToken.ToString()
            }
        withConfiguration configuration processInfo
        
    let withUnderDotnetTestConfiguration allAssembliesPath mainAssembliesPath resultNamePath processInfo =
        let configuration =
            {
                passiveModeEnable = CoverageToolInfo.Enable
                resultNamePath = resultNamePath
                expectTestSuite = CoverageToolInfo.Enable
                assemblyPathsFile = allAssembliesPath
                mainAssemblyPathsFile = mainAssembliesPath
            }
        withConfiguration configuration processInfo
    
    let isCoverageToolAttached () = isConfigured<BaseCoverageToolConfiguration> ()

type InteractionCoverageTool() =
    let mutable entryMainWasSet = false

    let castPtr ptr =
        NativePtr.toVoidPtr ptr |> NativePtr.ofVoidPtr

    do
        if Configuration.isCoverageToolAttached () |> not then internalfail "Coverage tool wasn't attached"

    member this.GetRawHistory () =
        if not entryMainWasSet then
            Prelude.internalfail "Calling GetRawHistory when entryMain isn't set"
        let sizePtr = NativePtr.stackalloc<uint> 1
        let dataPtrPtr = NativePtr.stackalloc<nativeint> 1

        ExternalCalls.GetHistory(NativePtr.toNativeInt sizePtr, NativePtr.toNativeInt dataPtrPtr)

        let size = NativePtr.read sizePtr |> int
        let dataPtr = NativePtr.read dataPtrPtr

        let data = Array.zeroCreate<byte> size
        Marshal.Copy(dataPtr, data, 0, size)
        
        ExternalCalls.ClearHistory()
        
        data

    member this.SetEntryMain (assembly : Assembly) (moduleName : string) (methodToken : int) =
        entryMainWasSet <- true
        let assemblyNamePtr = fixed assembly.FullName.ToCharArray()
        let moduleNamePtr = fixed moduleName.ToCharArray()
        let assemblyNameLength = assembly.FullName.Length
        let moduleNameLength = moduleName.Length

        ExternalCalls.SetEntryMain(
            castPtr assemblyNamePtr,
            assemblyNameLength,
            castPtr moduleNamePtr,
            moduleNameLength,
            methodToken
        )

    member this.SetCurrentThreadId id =
        ExternalCalls.SetCurrentThreadId(id)

    static member WithCoverageTool processInfo =
        let configuration =
            Configuration.createUnspecifiedCoverageToolConfiguration()
            |> Configuration.withMainOnlyCoverageToolConfiguration
        setProcInfoEnvironment configuration processInfo

type PassiveCoverageTool(workingDirectory: DirectoryInfo, method: MethodBase) =

    let resultName = "coverage.cov"

    let getHistory () =
        let coverageFile = workingDirectory.EnumerateFiles(resultName) |> Seq.tryHead
        match coverageFile with
        | Some coverageFile ->
            File.ReadAllBytes(coverageFile.FullName)
            |> CoverageDeserializer.getRawReports
            |> CoverageDeserializer.reportsFromRawReports
            |> Some
        | None -> None

    let printCoverage (allBlocks: ResizeArray<BasicBlock>) (visited: HashSet<BasicBlock>) =
        Logger.writeLine $"Coverage for method {method.Name}:"

        let mutable allCovered = true
        for block in allBlocks do
            if visited.Contains block |> not then
                Logger.writeLine $"Block [0x{block.StartOffset:X} .. 0x{block.FinalOffset:X}] not covered"
                allCovered <- false
        if allCovered then
            Logger.writeLine "All blocks are covered"

    let computeCoverage (cfg: CfgInfo) (visited: CoverageReport[]) =
        let visitedBlocks = HashSet<BasicBlock>()

        let token = method.MetadataToken
        let moduleName = method.Module.FullyQualifiedName
        for coverageReport in visited do
            for loc in coverageReport.coverageLocations do
                // Filtering coverage records that are only relevant to this method
                if loc.methodToken = token && loc.moduleName = moduleName then
                    let offset = Offset.from loc.offset
                    let block = cfg.ResolveBasicBlock offset
                    if block.FinalOffset = offset then
                        visitedBlocks.Add block |> ignore

        printCoverage cfg.SortedBasicBlocks visitedBlocks
        let coveredSize = visitedBlocks |> Seq.sumBy (fun x -> x.BlockSize)
        (double coveredSize) / (double cfg.MethodSize) * 100. |> int

    member this.RunWithCoverage (args: string) =
        let passiveEnv =
            Configuration.createUnspecifiedCoverageToolConfiguration()
            |> Configuration.withMainOnlyCoverageToolConfiguration
            |> Configuration.withPassiveModeConfiguration method resultName
        
        let procInfo = ProcessStartInfo()
        procInfo.Arguments <- args
        procInfo.FileName <- DotnetExecutablePath.ExecutablePath
        procInfo.WorkingDirectory <- workingDirectory.FullName
        setProcInfoEnvironment passiveEnv procInfo

        let method = Application.getMethod method
        if not method.HasBody then
            Logger.warning "CoverageRunner was given a method without body; 100%% coverage assumed"
            100
        else
            let proc = procInfo.StartWithLogging(
                (fun x -> Logger.info $"{x}"),
                (fun x -> Logger.error $"{x}")
            )
            proc.WaitForExit()

            if proc.IsSuccess() then
                match getHistory () with
                | Some history -> computeCoverage method.CFG history
                | None ->
                    Logger.error "Failed to retrieve coverage locations"
                    -1
            else
                Logger.error $"Run with coverage failed with exit code: {proc.ExitCode}"
                -1

