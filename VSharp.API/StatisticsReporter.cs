using System;
using System.Globalization;
using System.IO;
using System.Linq;
using CsvHelper;
using CsvHelper.Configuration;
using VSharp.Interpreter.IL;

namespace VSharp
{
    internal class StatisticsReporter
    {
        private record ContinuousStatisticsCsvRecord(
            string ElapsedMillis,
            string CoveringStepsInsideZone,
            string NonCoveringStepsInsideZone,
            string CoveringStepsOutsideZone,
            string NonCoveringStepsOutsideZone,
            string TestsCount,
            string BranchesReleased,
            string StatesCount,
            string ErrorsCount,
            string CoveringStatesCount
        );

        public static void SaveReports(string outputDir, SILIStatistics stats, bool reportContinuousStats = false)
        {
            var reportsDir = Path.Combine(outputDir, "reports");
            Directory.CreateDirectory(reportsDir);

            if (reportContinuousStats)
            {
                SaveContinuousStats(reportsDir, stats);
            }
        }

        private static void SaveContinuousStats(string path, SILIStatistics stats)
        {
            var filePath = Path.Combine(path, "continuous.csv");
            var csvConfig = new CsvConfiguration(CultureInfo.InvariantCulture);
            using var writer = File.AppendText(filePath);
            using var csvWriter = new CsvWriter(writer, csvConfig);
            csvWriter.WriteHeader<ContinuousStatisticsCsvRecord>();
            csvWriter.NextRecord();
            csvWriter.WriteRecords(stats.ContinuousStatistics.Select(ToCsvRecord));
        }

        private static ContinuousStatisticsCsvRecord ToCsvRecord(continuousStatistics stats) =>
            new ContinuousStatisticsCsvRecord(
                ElapsedMillis: stats.millis.ToString(),
                CoveringStepsInsideZone: stats.coveringStepsInsideZone.ToString(),
                NonCoveringStepsInsideZone: stats.nonCoveringStepsInsideZone.ToString(),
                CoveringStepsOutsideZone: stats.coveringStepsOutsideZone.ToString(),
                NonCoveringStepsOutsideZone: stats.nonCoveringStepsOutsideZone.ToString(),
                TestsCount: stats.testsCount.ToString(),
                BranchesReleased: stats.branchesReleased.ToString(),
                StatesCount: stats.statesCount.ToString(),
                ErrorsCount: stats.internalFailsCount.ToString(),
                CoveringStatesCount: stats.coveringStatesCount.ToString()
            );
    }
}
