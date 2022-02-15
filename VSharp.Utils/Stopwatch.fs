namespace VSharp

open System
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
open System.IO
open CsvHelper
open CsvHelper.Configuration

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
        }
        
    let private csvPath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "VSharpBenchmark")
    let private csvFilename = "benchmark.csv"
    
    let private measurements = Dictionary<string, measurement>()
    
    let public runMeasuringTime tag action =
        let measurement =
            if (measurements.ContainsKey tag) then
                measurements.[tag]
            else
                let newMeasurement = { stopwatch = Stopwatch(); timesCalled = 0 }
                measurements.[tag] <- newMeasurement
                newMeasurement      
        try
            measurement.stopwatch.Start()
            measurement.timesCalled <- measurement.timesCalled + 1
            action()
        finally
            measurement.stopwatch.Stop()
        
    let public stopAll () =
        measurements
        |> Seq.map (|KeyValue|)
        |> Seq.iter (fun (_, m) ->
            m.stopwatch.Stop()
        )
        
    let private getGitCommitHash () =
        let procStartInfo = ProcessStartInfo("git", "rev-parse --short HEAD")

        procStartInfo.RedirectStandardOutput <- true
        procStartInfo.UseShellExecute <- false
        procStartInfo.CreateNoWindow <- true
        
        use proc = new Process()
        proc.StartInfo <- procStartInfo
        
        proc.Start() |> ignore        
        proc.WaitForExit()
        
        proc.StandardOutput.ReadLine()
        
    let public saveMeasurements caseName =
        stopAll()
        
        let commitHash = getGitCommitHash()
        let currentDateTime = DateTime.Now.ToString("yyyy-MM-ddTHH-mm-ss")
        
        let records =
            measurements
            |> Seq.map (|KeyValue|)
            |> Seq.map (fun (tag, m) ->
                {
                    commitHash = commitHash
                    dateTime = currentDateTime
                    caseName = caseName
                    tag = tag
                    timesCalled = m.timesCalled
                    totalTicks = m.stopwatch.ElapsedTicks
                    totalMs = m.stopwatch.ElapsedMilliseconds
                }
            )       
        
        let targetPath = Path.Combine(csvPath, csvFilename)
        
        let configuration = CsvConfiguration(CultureInfo.InvariantCulture)
        configuration.IncludePrivateMembers <- true
        configuration.HasHeaderRecord <- File.Exists(targetPath) |> not

        Directory.CreateDirectory(csvPath) |> ignore
        use streamWriter = File.AppendText(targetPath)
        use csvWriter = new CsvWriter(streamWriter, configuration)
        
        csvWriter.WriteRecords(records)
        
    let public clear () = measurements.Clear() 
