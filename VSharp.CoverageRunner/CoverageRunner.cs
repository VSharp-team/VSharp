using System.Diagnostics;
using System.Reflection;
using System.Runtime.InteropServices;

namespace VSharp.CoverageRunner
{
    public static class CoverageRunner
    {
        private const string ResultName = "coverage.cov";

        private static string? GetProfilerPath()
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
                Logger.printLogString(Logger.Error, "CoverageRunner started on unknown platform");
                return null;
            }

            var clientName = $"libvsharpCoverage{extension}";
            return $"{Directory.GetCurrentDirectory()}{Path.DirectorySeparatorChar}{clientName}";
        }

        private static bool StartCoverageTool(string args, DirectoryInfo workingDirectory, MethodInfo method)
        {
            var profilerPath = GetProfilerPath();
            if (profilerPath == null) return false;

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
                Arguments = args,
                UseShellExecute = false,
                RedirectStandardInput = false,
                RedirectStandardOutput = true,
                RedirectStandardError = true
            };

            var proc = Process.Start(info);
            if (proc == null)
            {
                Logger.printLogString(Logger.Error, "CoverageRunner could not start dotnet process");
                return false;
            }

            proc.WaitForExit();

            var outputString = proc.StandardOutput.ReadToEnd();
            if (!String.IsNullOrEmpty(outputString))
                Logger.printLogString(Logger.Info, outputString);

            var errorString = proc.StandardError.ReadToEnd();
            if (!String.IsNullOrEmpty(errorString))
                Logger.printLogString(Logger.Error, errorString);

            return true;
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

            return (int)Math.Ceiling(100 * ((double)coveredSize / cfg.MethodSize));
        }

        public static int RunAndGetCoverage(string args, DirectoryInfo workingDirectory, MethodInfo methodInfo)
        {
            var success = StartCoverageTool(args, workingDirectory, methodInfo);
            if (!success) return -1;

            var method = Application.getMethod(methodInfo);
            if (!method.HasBody)
            {
                Logger.printLogString(Logger.Error,
                    "CoverageRunner was given a method without body; 100% coverage assumed");
                return 100;
            }
            var cfg = method.ForceCFG;

            var visited = GetHistory(workingDirectory);
            if (visited is null)
            {
                Logger.printLogString(Logger.Error, "CoverageRunner could not deserialize coverage history");
                return -1;
            }

            return ComputeCoverage(cfg, visited, methodInfo);
        }
    }
}
