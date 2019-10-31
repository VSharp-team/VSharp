namespace VSharp.Core

open System.Collections.Generic
open VSharp.Logger

type codeLocationSummary = { result : term; state : state }


module internal Database =
    let private summaries = new Dictionary<ICodeLocation, codeLocationSummary>()

    let reported codeLoc = summaries.ContainsKey codeLoc

    let report codeLoc result state =
        assert (List.length state.frames.f = 1)
        assert(not (summaries.ContainsKey codeLoc))
        let summary = { result = result; state = state}
        printLog Info "For %O got %O\n%O\n\n" codeLoc result (State.dumpMemory state)
        summaries.Add(codeLoc, summary) |> ignore
        summary

    let querySummary codeLoc =
        if summaries.ContainsKey codeLoc then Some(summaries.[codeLoc]) else None
