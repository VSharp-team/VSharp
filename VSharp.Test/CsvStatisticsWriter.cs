using System;
using System.IO;

namespace VSharp.Test;

internal class CsvStatisticsWriter
{
    private readonly record struct CsvRecord(
        string TimeReported,
        string RunId,
        string MethodName,
        string SearchStrategy,
        string CoverageZone,
        string Exception,
        string Duration,
        int TestsGenerated,
        int Coverage,
        int CoveringStepsInsideZone,
        int NonCoveringStepsInsideZone,
        int CoveringStepsOutsideZone,
        int NonCoveringStepsOutsideZone
    );

    private const string DateFormat = "yyyy-MM-ddTHH-mm-ss";

    private readonly FileInfo _outputFile;
    private readonly string _runId;
    
    public CsvStatisticsWriter(FileInfo outputFile, string runId)
    {
        _outputFile = outputFile;
        _runId = runId;
    }

    private CsvRecord StatisticsToCsvRecord(TestStatistics testStatistics)
    {
        var searchStrategyString = testStatistics.IsGuidedMode
            ? $"Guided({testStatistics.SearchStrategy})"
            : testStatistics.SearchStrategy.ToString();
        
        return new CsvRecord(
            DateTime.Now.ToString(DateFormat),
            _runId,
            testStatistics.TestMethodInfo.Name,
            searchStrategyString,
            testStatistics.CoverageZone.ToString(),
            testStatistics.Exception?.Message ?? "",
            
        );
    }

    public void Write(TestStatistics testStatistics)
    {
        
    }
}