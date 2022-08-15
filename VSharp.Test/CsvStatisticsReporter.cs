using System;
using System.Globalization;
using System.IO;
using CsvHelper;
using CsvHelper.Configuration;

namespace VSharp.Test;

internal class CsvStatisticsReporter : IStatisticsReporter
{
    private class CsvRecord
    {
        public string TimeReported { get; init; }
        public string RunId { get; init; }
        public string MethodName { get; init; }
        public string SearchStrategy { get; init; }
        public string CoverageZone { get; init; }
        public string Exception { get; init; }
        public string Duration { get; init; }
        public string TestsGenerated { get; init; }
        public string Coverage { get; init; }
        public string CoveringStepsInsideZone { get; init; }
        public string NonCoveringStepsInsideZone { get; init; }
        public string CoveringStepsOutsideZone { get; init; }
        public string NonCoveringStepsOutsideZone { get; init; }
        public string TestsOutputDirectory { get; init; }
    }

    private const string DateFormat = "yyyy-MM-ddTHH-mm-ss";

    private readonly string _outputFilePath;
    private readonly string _runId;
    
    public CsvStatisticsReporter(string outputDir, string filename, string runId)
    {
        if (String.IsNullOrEmpty(outputDir))
        {
            throw new ArgumentException("Output directory is null or empty");
        }
        
        if (String.IsNullOrEmpty(filename))
        {
            throw new ArgumentException("Filename is null or empty");
        }
        
        Directory.CreateDirectory(outputDir);

        _outputFilePath = Path.Combine(outputDir, $"{filename}.csv");
        _runId = runId;
    }

    private CsvRecord StatisticsToCsvRecord(TestStatistics testStatistics)
    {
        var searchStrategyString = testStatistics.IsGuidedMode
            ? $"Guided({testStatistics.SearchStrategy})"
            : testStatistics.SearchStrategy.ToString();

        return new CsvRecord
        {
            TimeReported = DateTime.Now.ToString(DateFormat),
            RunId = _runId,
            MethodName = testStatistics.TestMethodInfo.Name,
            SearchStrategy = searchStrategyString,
            CoverageZone = testStatistics.CoverageZone.ToString(),
            Exception = testStatistics.Exception?.GetType().Name ?? "",
            Duration = testStatistics.SiliStatisticsDump?.time.ToString() ?? "",
            TestsGenerated = testStatistics.TestsGenerated.ToString() ?? "",
            Coverage = testStatistics.Coverage?.ToString() ?? "",
            CoveringStepsInsideZone = testStatistics.SiliStatisticsDump?.coveringStepsInsideZone.ToString() ?? "",
            NonCoveringStepsInsideZone = testStatistics.SiliStatisticsDump?.nonCoveringStepsInsideZone.ToString() ?? "",
            CoveringStepsOutsideZone = testStatistics.SiliStatisticsDump?.coveringStepsOutsideZone.ToString() ?? "",
            NonCoveringStepsOutsideZone = testStatistics.SiliStatisticsDump?.nonCoveringStepsOutsideZone.ToString() ?? "",
            TestsOutputDirectory = testStatistics.TestsOutputDirectory ?? ""
        };
    }

    public void Report(TestStatistics testStatistics)
    {
        var csvRecord = StatisticsToCsvRecord(testStatistics);

        var csvConfig = new CsvConfiguration(CultureInfo.InvariantCulture);

        var writeHeader = !File.Exists(_outputFilePath);

        using var writer = File.AppendText(_outputFilePath);
        using var csvWriter = new CsvWriter(writer, csvConfig);

        if (writeHeader)
        {
            csvWriter.WriteHeader<CsvRecord>();
            csvWriter.NextRecord();
        }

        csvWriter.WriteRecord(csvRecord);
        csvWriter.NextRecord();
    }
}
