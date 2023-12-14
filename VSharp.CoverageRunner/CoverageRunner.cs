using System.Diagnostics;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;

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

        public static bool RunWithLogging(ProcessStartInfo procInfo)
        {
            procInfo.RedirectStandardError = true;
            procInfo.RedirectStandardOutput = true;
            procInfo.UseShellExecute = false;

            var proc = new Process();
            proc.StartInfo = procInfo;

            proc.OutputDataReceived +=
                (_, e) =>
                {
                    var data = e.Data;
                    if (string.IsNullOrEmpty(data))
                        return;
                    Logger.printLogString(Logger.Info, data);
                };

            proc.ErrorDataReceived +=
                (_, e) =>
                {
                    var data = e.Data;
                    if (string.IsNullOrEmpty(data))
                        return;
                    Logger.printLogString(Logger.Error, data);
                };

            proc.Start();
            proc.BeginOutputReadLine();
            proc.BeginErrorReadLine();
            proc.WaitForExit();

            return proc.ExitCode == 0;
        }

        private static bool StartCoverageTool(string args, DirectoryInfo workingDirectory, MethodBase method)
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

            return RunWithLogging(info);
        }

        private static CoverageReport[]? GetHistory(DirectoryInfo workingDirectory)
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

            var raw = CoverageDeserializer.getRawReports(covHistory);
            return CoverageDeserializer.reportsFromRawReports(raw);
        }

        private static void PrintCoverage(
            IEnumerable<BasicBlock> allBlocks,
            IReadOnlySet<BasicBlock> visited,
            MethodBase methodInfo)
        {
            var sb = new StringBuilder();
            sb.AppendLine($"Coverage for method {methodInfo}:");
            var allCovered = true;
            foreach (var block in allBlocks)
            {
                if (!visited.Contains(block))
                {
                    allCovered = false;
                    sb.AppendLine($"Block [0x{block.StartOffset:X} .. 0x{block.FinalOffset:X}] not covered");
                }
            }
            if (allCovered)
                sb.AppendLine("All blocks are covered");

            Logger.writeLine(sb.ToString());
        }

        private static int ComputeCoverage(CfgInfo cfg, CoverageReport[] visited, MethodBase methodInfo)
        {
            // filtering coverage records that are only relevant to this method
            var visitedInMethod =
                visited.SelectMany(x => x.coverageLocations)
                    .Where(x => x.methodToken == methodInfo.MetadataToken &&
                                x.moduleName == methodInfo.Module.FullyQualifiedName);
            var visitedBlocks = new HashSet<BasicBlock>();
            foreach (var loc in visitedInMethod)
            {
                var offset = loc.offset;
                var block = cfg.ResolveBasicBlock(offset);
                // counting only those blocks that were fully executed
                if (block.FinalOffset == offset)
                    visitedBlocks.Add(block);
            }

            var coveredSize = visitedBlocks.Sum(block => block.BlockSize);

            PrintCoverage(cfg.SortedBasicBlocks, visitedBlocks, methodInfo);

            return (int)Math.Floor(100 * ((double)coveredSize / cfg.MethodSize));
        }

        public static int RunAndGetCoverage(string args, DirectoryInfo workingDirectory, MethodBase methodInfo)
        {
            // TODO: delete non-main methods from serialization
            var success = StartCoverageTool(args, workingDirectory, methodInfo);
            if (!success)
            {
                Logger.printLogString(Logger.Error, "TestRunner with Coverage failed to run!");
                return -1;
            }

            var method = Application.getMethod(methodInfo);

            if (!method.HasBody)
            {
                Logger.printLogString(Logger.Warning,
                    "CoverageRunner was given a method without body; 100% coverage assumed");
                return 100;
            }

            var reports = GetHistory(workingDirectory);
            if (reports is null)
            {
                Logger.printLogString(Logger.Error, "CoverageRunner could not deserialize coverage history");
                return -1;
            }
            return ComputeCoverage(method.CFG, reports, methodInfo);
        }
    }
}
