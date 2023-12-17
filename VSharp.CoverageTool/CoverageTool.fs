namespace VSharp.CoverageTool
open System.Diagnostics
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open VSharp.Utils.EnvironmentUtils
open VSharp.CSharpUtils

open VSharp

#nowarn "9"

module private ExternalCalls =
    [<DllImport("libvsharpCoverage", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
    extern void SetEntryMain(byte* assemblyName, int assemblyNameLength, byte* moduleName, int moduleNameLength, int methodToken)

    [<DllImport("libvsharpCoverage", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
    extern void GetHistory(nativeint size, nativeint data)

    [<DllImport("libvsharpCoverage", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
    extern void SetCurrentThreadId(int id)

    let inline castPtr ptr =
        ptr |> NativePtr.toVoidPtr |> NativePtr.ofVoidPtr

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

    let private withCoverageToolConfiguration mainOnly =
        withConfiguration {
            coreclrProfiler = "{2800fea6-9667-4b42-a2b6-45dc98e77e9e}"
            coreclrProfilerPath = $"{Directory.GetCurrentDirectory()}{Path.DirectorySeparatorChar}libvsharpCoverage{libExtension}"
            coreclrEnableProfiling = enabled
            instrumentMainOnly = if mainOnly then enabled else ""
        }

    let withMainOnlyCoverageToolConfiguration =
        withCoverageToolConfiguration true

    let withAllMethodsCoverageToolConfiguration =
        withCoverageToolConfiguration false

    let withPassiveModeConfiguration (method: MethodBase) resultName =
        withConfiguration {
            passiveModeEnable = enabled
            resultName = resultName
            assemblyName = method.Module.Assembly.FullName
            moduleName = method.Module.FullyQualifiedName
            methodToken = method.MetadataToken.ToString()
        }

    let isCoverageToolAttached () = isConfigured<BaseCoverageToolConfiguration> ()

type InteractionCoverageTool() =
    let mutable entryMainWasSet = false

    do
        if Configuration.isCoverageToolAttached () |> not then internalfail "Coverage tool wasn't attached"

    member this.GetRawHistory () =
        if not entryMainWasSet then Prelude.internalfail "Try call GetRawHistory, while entryMain wasn't set"
        let sizePtr = NativePtr.stackalloc<uint> 1
        let dataPtrPtr = NativePtr.stackalloc<nativeint> 1

        ExternalCalls.GetHistory(NativePtr.toNativeInt sizePtr, NativePtr.toNativeInt dataPtrPtr)

        let size = NativePtr.read sizePtr |> int
        let dataPtr = NativePtr.read dataPtrPtr

        let data = Array.zeroCreate<byte> size
        Marshal.Copy(dataPtr, data, 0, size)
        data

    member this.SetEntryMain (assembly: Assembly) (moduleName: string) (methodToken: int) =
        entryMainWasSet <- true
        let assemblyNamePtr = fixed assembly.FullName.ToCharArray()
        let moduleNamePtr = fixed moduleName.ToCharArray()
        let assemblyNameLength = assembly.FullName.Length
        let moduleNameLength = moduleName.Length

        ExternalCalls.SetEntryMain(
            ExternalCalls.castPtr assemblyNamePtr,
            assemblyNameLength,
            ExternalCalls.castPtr moduleNamePtr,
            moduleNameLength,
            methodToken
        )

    member this.SetCurrentThreadId id =
        ExternalCalls.SetCurrentThreadId(id)


    static member WithCoverageTool (procInfo: ProcessStartInfo) =
        Configuration.withMainOnlyCoverageToolConfiguration procInfo

type PassiveCoverageTool(workingDirectory: DirectoryInfo, method: MethodBase) =

    let resultName = "coverage.cov"

    let getHistory () =
        workingDirectory.EnumerateFiles(resultName)
        |> Seq.tryHead
        |> Option.map (fun x -> File.ReadAllBytes(x.FullName))
        |> Option.map CoverageDeserializer.getRawReports
        |> Option.map CoverageDeserializer.reportsFromRawReports


    let printCoverage (allBlocks: seq<BasicBlock>) (visited: seq<BasicBlock>) =
        Logger.writeLine $"Coverage for method {method.Name}:"

        let hasNonCovered = allBlocks |> Seq.exists (fun b -> Seq.contains b visited |> not)

        if hasNonCovered then
            allBlocks
            |> Seq.iter (fun block ->
                if Seq.contains block visited |> not then
                    Logger.writeLine $"Block [0x{block.StartOffset:X} .. 0x{block.FinalOffset:X}] not covered"
            )
        else
            Logger.writeLine "All blocks are covered"

    let computeCoverage (cfg: CfgInfo) (visited: seq<CoverageReport>) =
        // filtering coverage records that are only relevant to this method
        let visitedInMethod =
            visited
            |> Seq.map (fun x -> x.coverageLocations)
            |> Seq.concat
            |> Seq.filter (fun x ->
                x.methodToken = method.MetadataToken
                && x.moduleName = method.Module.FullyQualifiedName
            )

        let visitedBlocks = System.Collections.Generic.HashSet<BasicBlock>()

        for location in visitedInMethod do
            let offset = LanguagePrimitives.Int32WithMeasure location.offset
            let block = cfg.ResolveBasicBlock(offset)
            if block.FinalOffset = offset then
                visitedBlocks.Add block |> ignore

        printCoverage cfg.SortedBasicBlocks visitedBlocks

        let coveredSize = visitedBlocks |> Seq.sumBy (fun x -> x.BlockSize)

        (double coveredSize) / (double cfg.MethodSize) * 100. |> int


    member this.RunWithCoverage (args: string) =
        let procInfo =
            ProcessStartInfo()
            |> (fun x ->
                    x.Arguments <- args
                    x.FileName <- DotnetExecutablePath.ExecutablePath
                    x.WorkingDirectory <- workingDirectory.FullName
                    x
            )
            |> Configuration.withMainOnlyCoverageToolConfiguration
            |> Configuration.withPassiveModeConfiguration method resultName


        let applicationMethod = Application.getMethod(method)

        if applicationMethod.HasBody |> not then
            Logger.warning $"CoverageRunner was given a method without body; 100%% coverage assumed"
            100
        else

            let proc = procInfo.StartWithLogging(
                (fun x -> Logger.info $"{x}"),
                (fun x-> Logger.error $"{x}")
            )
            proc.WaitForExit()

            if proc.ExitCode <> 0 then
                Logger.error $"Run with coverage failed with exit code: {proc.ExitCode}"
                -1
            else
                match getHistory () with
                | Some history -> computeCoverage applicationMethod.CFG history
                | None ->
                    Logger.error $"Failed to retrieve coverage locations"
                    -1
