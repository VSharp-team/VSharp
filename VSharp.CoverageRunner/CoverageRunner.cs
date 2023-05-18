using System.Diagnostics;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Runtime.Loader;
using Microsoft.FSharp.Core;

namespace VSharp.CoverageRunner
{
    public static class CoverageRunner
    {
        private const string ResultName = "coverage.cov";

        private static string GetProfilerPath()
        {
            string extension;
            if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
                extension = ".dll";
            else if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
                extension = ".so";
            else if (RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
                extension = ".dylib";
            else
            {
                throw new PlatformNotSupportedException("CoverageRunner started on unknown platform");
            }

            var clientName = $"libvsharpCoverage{extension}";
            return Path.Combine(Directory.GetCurrentDirectory(), clientName);
        }

        public static bool RunDotNetWithLogging(ProcessStartInfo procInfo)
        {
            procInfo.RedirectStandardError = true;
            procInfo.RedirectStandardOutput = true;
            procInfo.UseShellExecute = false;

            var proc = new Process();
            proc.StartInfo = procInfo;

            proc.OutputDataReceived +=
                (object sender, DataReceivedEventArgs e) =>
                {
                    var data = e.Data;
                    if (String.IsNullOrEmpty(data))
                        return;
                    Logger.printLogString(Logger.Info, data);
                };

            proc.ErrorDataReceived +=
                (object sender, DataReceivedEventArgs e) =>
                {
                    var data = e.Data;
                    if (String.IsNullOrEmpty(data))
                        return;
                    Logger.printLogString(Logger.Error, data);
                };

            proc.Start();
            proc.BeginOutputReadLine();
            proc.BeginErrorReadLine();
            proc.WaitForExit();

            return proc.ExitCode == 0;
        }

        private static bool StartCoverageTool(string args, DirectoryInfo workingDirectory, MethodInfo method)
        {
            var profilerPath = GetProfilerPath();

            var info = new ProcessStartInfo
            {
                EnvironmentVariables =
                    {
                        ["CORECLR_PROFILER"] = "{2800fea6-9667-4b42-a2b6-45dc98e77e9e}",
                        ["CORECLR_ENABLE_PROFILING"] = "1",
                        ["CORECLR_PROFILER_PATH"] = profilerPath,
                        ["COVERAGE_ENABLE_PASSIVE"] = "1",
                        ["COVERAGE_RESULT_NAME"] = ResultName,
                        ["COVERAGE_METHOD_ASSEMBLY_NAME"] = method.Module.Assembly.FullName,
                        ["COVERAGE_METHOD_MODULE_NAME"] = method.Module.FullyQualifiedName,
                        ["COVERAGE_METHOD_TOKEN"] = method.MetadataToken.ToString(),
                        ["COVERAGE_INSTRUMENT_MAIN_ONLY"] = "1"
                    },
                WorkingDirectory = workingDirectory.FullName,
                FileName = "dotnet",
                Arguments = args
            };

            return RunDotNetWithLogging(info);
        }

        private static CoverageLocation[][]? GetHistory(DirectoryInfo workingDirectory)
        {
            byte[] covHistory;
            try
            {
                var coverageFile = workingDirectory.EnumerateFiles(ResultName).Single();
                covHistory = File.ReadAllBytes(coverageFile.FullName);
            }
            catch
            {
                Logger.printLogString(Logger.Error, "CoverageRunner could not read/access coverage history file");
                return null;
            }

            return CoverageDeserializer.getHistory(covHistory);
        }

        private static int ComputeCoverage(CfgInfo cfg, CoverageLocation[][] visited, MethodInfo methodInfo)
        {
            // filtering coverage records that are only relevant to this method
            var visitedInMethod =
                visited.SelectMany(x => x)
                    .Where(x => x.methodToken == methodInfo.MetadataToken &&
                                x.moduleName == methodInfo.Module.FullyQualifiedName);
            var uniqueBlocks = new HashSet<int>();
            foreach (var loc in visitedInMethod)
            {
                var bbId = cfg.ResolveBasicBlockIndex(loc.offset);
                // counting only those blocks that were fully executed
                if (cfg.SortedBasicBlocks[bbId].FinalOffset == loc.offset)
                    uniqueBlocks.Add(bbId);
            }

            var coveredSize =
                uniqueBlocks.Sum(blockOffset =>
                    cfg.SortedBasicBlocks[blockOffset].BlockSize);

            return (int)Math.Floor(100 * ((double)coveredSize / cfg.MethodSize));
        }

        public static int RunAndGetCoverage(string args, DirectoryInfo workingDirectory, MethodInfo methodInfo)
        {
            var success = StartCoverageTool(args, workingDirectory, methodInfo);
            if (!success)
            {
                Logger.printLogString(Logger.Error, "TestRunner with Coverage failed to run!");
                return -1;
            }

            var method = Application.getMethod(methodInfo);
            var cfg = method.CFG;
            if (FSharpOption<CfgInfo>.get_IsNone(cfg))
            {
                Logger.printLogString(Logger.Warning,
                    "CoverageRunner was given a method without body; 100% coverage assumed");
                return 100;
            }

            var visited = GetHistory(workingDirectory);
            if (visited is null)
            {
                Logger.printLogString(Logger.Error, "CoverageRunner could not deserialize coverage history");
                return -1;
            }

            return ComputeCoverage(cfg.Value, visited, methodInfo);
        }
    }
}
