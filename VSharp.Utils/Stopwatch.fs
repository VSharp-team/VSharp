namespace VSharp

open System
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
open System.IO
open CsvHelper
open CsvHelper.Configuration

/// <summary>
/// Contains functions for running code with time measurement and measurement results export
/// </summary>
module Stopwatch =
    
    type private measurement = { stopwatch: Stopwatch; mutable timesCalled: int }    
    type private csvRecord =
        {
            commitHash: string
            dateTime: string
            caseName: string
            tag: string
            timesCalled: int
            totalTicks: int64
            totalMs: int64
            testsGenerated: uint
        }
        
    let private csvPath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "VSharpBenchmark")
    let private csvFilename = "benchmark.csv"    
    let private measurements = Dictionary<string, measurement>()
    
    let private getGitCommitHash() =
        let procStartInfo = ProcessStartInfo("git", "rev-parse --short HEAD")

        procStartInfo.RedirectStandardOutput <- true
        procStartInfo.UseShellExecute <- false
        procStartInfo.CreateNoWindow <- true
        
        use proc = new Process()
        proc.StartInfo <- procStartInfo
        
        proc.Start() |> ignore        
        proc.WaitForExit()
        
        proc.StandardOutput.ReadLine()
    
    /// <summary>
    /// Runs function saving its execution time
    /// </summary>
    /// <param name="tag">Tag to save results with. Results from multiple runs with the same tag are added up</param>
    /// <param name="action">Function to run</param>
    let public runMeasuringTime tag action =
        let measurement =
            if measurements.ContainsKey tag then
                measurements.[tag]
            else
                let newMeasurement = { stopwatch = Stopwatch(); timesCalled = 0 }
                measurements.[tag] <- newMeasurement
                newMeasurement
        if measurement.stopwatch.IsRunning then
            measurement.timesCalled <- measurement.timesCalled + 1
            action()
        else
            try
                measurement.stopwatch.Start()
                measurement.timesCalled <- measurement.timesCalled + 1
                action()
            finally
                measurement.stopwatch.Stop()
        
    /// <summary>
    /// Stops all running measurements
    /// </summary>
    let public stopAll() =
        measurements
        |> Seq.map (|KeyValue|)
        |> Seq.iter (fun (_, m) -> m.stopwatch.Stop())
    
    /// <summary>
    /// Saves all current measurement results to .csv file. If the file already exists, appends lines
    /// </summary>
    /// <param name="caseName">Additional tag given to the saved measurements</param>
    /// <param name="testsGenerated">Number of tests generated during the run</param>
    let public saveMeasurements caseName testsGenerated =
        stopAll()

        let currentDateTime = DateTime.Now.ToString("yyyy-MM-ddTHH-mm-ss")
        let commitHash = getGitCommitHash()
        
        let createRecord (tag, measurement : measurement)  =
            {
                commitHash = commitHash
                dateTime = currentDateTime
                caseName = caseName
                tag = tag
                timesCalled = measurement.timesCalled
                totalTicks = measurement.stopwatch.ElapsedTicks
                totalMs = measurement.stopwatch.ElapsedMilliseconds
                testsGenerated = testsGenerated
            }
            
        let records =
            measurements
            |> Seq.map (|KeyValue|)
            |> Seq.map createRecord
        
        let targetPath = Path.Combine(csvPath, csvFilename)
        
        let configuration = CsvConfiguration(CultureInfo.InvariantCulture)
        configuration.IncludePrivateMembers <- true
        configuration.HasHeaderRecord <- File.Exists(targetPath) |> not

        Directory.CreateDirectory(csvPath) |> ignore
        use streamWriter = File.AppendText(targetPath)
        use csvWriter = new CsvWriter(streamWriter, configuration)
        
        csvWriter.WriteRecords(records)
    
    /// <summary>
    /// Clears all current measurements
    /// </summary>
    let public clear() = measurements.Clear()
