namespace VSharp

open System
open System.Collections.Generic
open System.IO
open System.Text

module StatedLogger =    
    let stateLogs = Dictionary<string, StringBuilder>()
    let public log stateId (message : string) =
        if not <| stateLogs.ContainsKey(stateId) then stateLogs.Add(stateId, StringBuilder())
        stateLogs.[stateId].AppendLine(message) |> ignore
        
    let public copy fromStateId toStateId =
        let from = stateLogs.GetValueOrDefault(fromStateId, StringBuilder()).ToString()
        stateLogs.[toStateId] <- StringBuilder().AppendLine(from)
    
    let public saveLog stateId path =
        let log = stateLogs.GetValueOrDefault(stateId, StringBuilder()).ToString()
        File.WriteAllText(path, log)