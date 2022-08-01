namespace VSharp

open System.Collections.Generic
open System.IO
open System.Text

/// <summary>
/// Contains functions for saving logs with tag keys.
/// May be used to save logs from different states separately 
/// </summary>
module TaggedLogger =
    
    let logs = Dictionary<string, StringBuilder>()
    
    /// <summary>
    /// Saves log with the tag
    /// </summary>
    /// <param name="tag">Tag to save the log with</param>
    /// <param name="message">Message to log</param>
    let public log tag (message : string) =
        if not <| logs.ContainsKey(tag) then logs.Add(tag, StringBuilder())
        logs.[tag].AppendLine(message) |> ignore
    
    /// <summary>
    /// Saves with toTag all logs with fromTag
    /// </summary>
    let public copy fromTag toTag =
        let from = logs.GetValueOrDefault(fromTag, StringBuilder()).ToString()
        logs.[toTag] <- StringBuilder().Append(from)
    
    /// <summary>
    /// Saves all logs with the specified tag to the file 
    /// </summary>
    /// <param name="tag">Tag to save the logs with</param>
    /// <param name="path">Path of the file to save</param>
    let public saveLog tag path =
        if logs.ContainsKey tag then
            let log = logs.[tag].ToString()
            File.WriteAllText(path, log)
