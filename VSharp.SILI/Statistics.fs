namespace VSharp.Interpreter.IL

open System
open System.Text
open System.Collections.Generic

open VSharp
open VSharp.Core
open VSharp.Utils

open CilStateOperations
open ipOperations


// TODO: transform to ``module'' with functions #mb do
type public BidirectionalEngineStatistics() =
    let startIp2currentIp = Dictionary<codeLocation, Dictionary<codeLocation, uint>>()
    let totalVisited = Dictionary<codeLocation, uint>()
    let unansweredPobs = List<pob>()
    let isHeadOfBasicBlock (codeLocation : codeLocation) =
        let cfg = CFG.findCfg codeLocation.method
        cfg.sortedOffsets.BinarySearch(codeLocation.offset) >= 0

    let printDict' placeHolder (d : Dictionary<codeLocation, uint>) sb (m, locs) =
        let sb = PrettyPrinting.appendLine sb (sprintf "%sMethod = %s: [" placeHolder (Reflection.getFullMethodName m))
        let sb = Seq.fold (fun sb (loc : codeLocation) ->
            PrettyPrinting.appendLine sb (sprintf "%s\t\t%s <- %d" placeHolder (loc.offset.ToString("X")) d.[loc])) sb locs
        PrettyPrinting.appendLine sb (sprintf "%s]" placeHolder)

    let printDict placeHolder sb (d : Dictionary<codeLocation, uint>) =
        let keys = d.Keys
        let sortedKeys = keys |> Seq.sort |> Seq.groupBy (fun location -> location.method)
        Seq.fold (printDict' placeHolder d) sb sortedKeys

    let printPart (sb : StringBuilder) i (k : KeyValuePair<codeLocation, Dictionary<codeLocation, uint>>) =
        let sb = PrettyPrinting.appendLine sb (sprintf "Part %d; Start from %O" i k.Key)
//        let sb = PrettyPrinting.appendLine sb
        printDict "\t\t" sb k.Value
    let rememberForward (start : codeLocation, current : codeLocation) =
        if isHeadOfBasicBlock current then
            let mutable totalRef = ref 0u
            if not <| totalVisited.TryGetValue(current, totalRef) then
                totalRef <- ref 0u
                totalVisited.Add(current, 0u)
            totalVisited.[current] <- !totalRef + 1u

            let mutable startRefDict = ref null
            if not <| startIp2currentIp.TryGetValue(start, startRefDict) then
                startRefDict <- ref (Dictionary<codeLocation, uint>())
                startIp2currentIp.Add(start, !startRefDict)
            let startDict = !startRefDict

            let mutable currentRef = ref 0u
            if not <| startDict.TryGetValue(current, currentRef) then
                currentRef <- ref 0u
                startDict.Add(current, 0u)
            startDict.[current] <- !currentRef + 1u
    member x.RememberSearcherAction (action : action) =
        match action with
        | GoFront s ->
            let startLoc = ip2codeLocation s.startingIP
            let currentLoc = ip2codeLocation (currentIp s)
            match startLoc, currentLoc with
            | Some startLoc, Some currentLoc -> rememberForward(startLoc, currentLoc)
            | _ -> ()
        | _ -> ()
    member x.AddUnansweredPob (p : pob) = unansweredPobs.Add(p)
    member x.Clear() =
        startIp2currentIp.Clear()
        totalVisited.Clear()
        unansweredPobs.Clear()
    member x.PrintStatistics searcherName =
        let sb = StringBuilder()
        let sb = PrettyPrinting.appendLine sb searcherName
        let sb =
            if unansweredPobs.Count = 0 then sb
            else
                let sb = PrettyPrinting.dumpSection "Unanswered Pobs" sb
                Seq.fold (fun sb p -> PrettyPrinting.appendLine sb (p.ToString())) sb unansweredPobs
        let sb =
            if startIp2currentIp.Keys.Count > 1 then
                let sb = PrettyPrinting.dumpSection "Total" sb
                printDict "" sb totalVisited
            else sb
        let sb = PrettyPrinting.dumpSection "Parts" sb
        let sb = Seq.foldi printPart sb startIp2currentIp
        sb.ToString()


