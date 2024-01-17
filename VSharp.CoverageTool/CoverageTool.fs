namespace VSharp.CoverageTool

open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

open VSharp
open VSharp.Utils.EnvironmentUtils
open VSharp.CSharpUtils


#nowarn "9"

module private ExternalCalls =
    [<DllImport("libvsharpCoverage", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
    extern void SetEntryMain(byte* assemblyName, int assemblyNameLength, byte* moduleName, int moduleNameLength, int methodToken)

    [<DllImport("libvsharpCoverage", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
    extern void GetHistory(nativeint size, nativeint data)

    [<DllImport("libvsharpCoverage", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
    extern void SetCurrentThreadId(int id)

module private Configuration =

    let (|Windows|MacOs|Linux|) _ =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then Windows
        elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then Linux
        elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then MacOs
        else Prelude.__notImplemented__()

    let libExtension =
        match () with
        | Windows -> ".dll"
        | Linux -> ".so"
        | MacOs -> ".dylib"

    let private enabled = "1"

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
        [<EnvironmentVariable("COVERAGE_TOOL_METHOD_ASSEMBLY_NAME")>]
        assemblyName: string
        [<EnvironmentVariable("COVERAGE_TOOL_METHOD_MODULE_NAME")>]
        moduleName: string
        [<EnvironmentVariable("COVERAGE_TOOL_METHOD_TOKEN")>]
        methodToken: string
    }

    let private withCoverageToolConfiguration mainOnly processInfo =
        let currentDirectory = Directory.GetCurrentDirectory()
        let configuration =
            {
                coreclrProfiler = "{2800fea6-9667-4b42-a2b6-45dc98e77e9e}"
                coreclrProfilerPath = $"{currentDirectory}{Path.DirectorySeparatorChar}libvsharpCoverage{libExtension}"
                coreclrEnableProfiling = enabled
                instrumentMainOnly = if mainOnly then enabled else ""
            }
        withConfiguration configuration processInfo

    let withMainOnlyCoverageToolConfiguration =
        withCoverageToolConfiguration true

    let withAllMethodsCoverageToolConfiguration =
        withCoverageToolConfiguration false

    let withPassiveModeConfiguration (method : MethodBase) resultName processInfo =
        let configuration =
            {
                passiveModeEnable = enabled
                resultName = resultName
                assemblyName = method.Module.Assembly.FullName
                moduleName = method.Module.FullyQualifiedName
                methodToken = method.MetadataToken.ToString()
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
            Prelude.internalfail "Try call GetRawHistory, while entryMain wasn't set"
        let sizePtr = NativePtr.stackalloc<uint> 1
        let dataPtrPtr = NativePtr.stackalloc<nativeint> 1

        ExternalCalls.GetHistory(NativePtr.toNativeInt sizePtr, NativePtr.toNativeInt dataPtrPtr)

        let size = NativePtr.read sizePtr |> int
        let dataPtr = NativePtr.read dataPtrPtr

        let data = Array.zeroCreate<byte> size
        Marshal.Copy(dataPtr, data, 0, size)
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

    static member WithCoverageTool (procInfo : ProcessStartInfo) =
        Configuration.withMainOnlyCoverageToolConfiguration procInfo

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
        let procInfo = ProcessStartInfo()
        procInfo.Arguments <- args
        procInfo.FileName <- DotnetExecutablePath.ExecutablePath
        procInfo.WorkingDirectory <- workingDirectory.FullName
        Configuration.withMainOnlyCoverageToolConfiguration procInfo
        Configuration.withPassiveModeConfiguration method resultName procInfo

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
