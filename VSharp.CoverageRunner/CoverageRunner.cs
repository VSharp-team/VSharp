using System.Diagnostics;
using System.Reflection;
using System.Runtime.InteropServices;
using VSharp;
using System.Text;

namespace VSharp.CoverageRunner
{
    public static class CoverageRunner
    {
        private static readonly string ResultName = "coverage.cov";

        public static int RunAndGetCoverage(string args, string workingDirectory, MethodInfo method)
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
                return -1;
            }

            var pathToClient = $"libvsharpCoverage{extension}";

            var profiler =
                $"{Directory.GetCurrentDirectory()}{Path.DirectorySeparatorChar}{pathToClient}";

            var info = new ProcessStartInfo
            {
                EnvironmentVariables =
                {
                    ["CORECLR_PROFILER"] = "{2800fea6-9667-4b42-a2b6-45dc98e77e9e}",
                    ["CORECLR_ENABLE_PROFILING"] = "1",
                    ["CORECLR_PROFILER_PATH"] = profiler,
                    ["COVERAGE_ENABLE_PASSIVE"] = "1",
                    ["COVERAGE_RESULT_NAME"] = ResultName,
                    ["COVERAGE_METHOD_ASSEMBLY_NAME"] = method.Module.Assembly.FullName,
                    ["COVERAGE_METHOD_MODULE_NAME"] = method.Module.FullyQualifiedName,
                    ["COVERAGE_METHOD_TOKEN"] = method.MetadataToken.ToString(),
                    ["COVERAGE_INSTRUMENT_MAIN_ONLY"] = "1"
                },
                WorkingDirectory = workingDirectory,
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
                return -3;
            }
            proc.WaitForExit();

            var md = Application.getMethod(method);
            if (!md.HasBody)
            {
                Logger.printLogString(Logger.Error,
                    "CoverageRunner was given a method without body; 100% coverage assumed");
                return 100;
            }
            var cfg = md.ForceCFG;

            byte[] covHistory;
            try
            {
                covHistory = File.ReadAllBytes(workingDirectory + Path.DirectorySeparatorChar + ResultName);
            }
            catch
            {
                Logger.printLogString(Logger.Error, "CoverageRunner could not read/access coverage history file");
                return 0;
            }
            var visited = CoverageDeserializer.getHistory(covHistory);
            if (visited is null)
            {
                Logger.printLogString(Logger.Error, "CoverageRunner could not deserialize coverage history");
                return 0;
            }

            // filtering coverage records that are only relevant to this method
            var visitedInMethod =
                visited.SelectMany(x => x)
                    .Where(x => x.methodToken == method.MetadataToken &&
                                x.moduleName == method.Module.FullyQualifiedName);
            var uniqueBlocks = new HashSet<int>();
            foreach (var loc in visitedInMethod)
            {
                uniqueBlocks.Add(cfg.ResolveBasicBlockIndex(loc.offset));
            }

            int coveredSize = 0;
            foreach (var blockOffset in uniqueBlocks)
            {
                coveredSize += cfg.SortedBasicBlocks[blockOffset].BlockSize;
            }

            var coverage = (int)Math.Ceiling(100 * ((double)coveredSize / cfg.MethodSize));

            return coverage;
        }
    }
}