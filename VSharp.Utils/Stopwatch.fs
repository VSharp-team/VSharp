namespace VSharp

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO

module Stopwatch =
    
    type private measurement = { stopwatch: Stopwatch; mutable timesCalled: int }
    
    let private headerTag = "Tag"
    let private headerTimesCalled = "Times called"
    let private headerTime = "Total time (ms)"
    
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
        
    let private formatColumn (text : string) =
        String.Format("{0,-20}", text)
        
    let private header = $"{formatColumn headerTag}|{formatColumn headerTimesCalled}|{formatColumn headerTime}\n"
        
    let public saveCurrentResults path =
        let currentTime = DateTime.Now.ToString("yyyy-MM-ddTHH-mm-ss")
        let filename = $"times-{currentTime}.txt"
        stopAll()
        let lines =
            measurements
            |> Seq.map (|KeyValue|)
            |> Seq.map (fun (tag, m) ->
                $"{formatColumn tag}|{formatColumn (m.timesCalled.ToString())}|{formatColumn (m.stopwatch.ElapsedMilliseconds.ToString())}"
            )
        let lines = Seq.append (Seq.singleton header) lines
        File.WriteAllLines($"{path}%c{Path.DirectorySeparatorChar}{filename}", lines)
    