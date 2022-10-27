namespace VSharp.Interpreter.IL

open System.Linq

type InterleavedSearcher(searchersWithStepCount: (IForwardSearcher * int) list) =

    let searchers = seq { while true do yield! searchersWithStepCount |> Seq.collect Enumerable.Repeat }
    let searchersEnumerator = searchers.GetEnumerator()

    let getCurrentSearcher() =
        searchersEnumerator.MoveNext() |> ignore
        searchersEnumerator.Current

    let init states = for searcher, _ in searchersWithStepCount do searcher.Init states

    let pick() =
        let currentSearcher = getCurrentSearcher()
        match currentSearcher.Pick() with
        | Some _ as pickedFromCurrent -> pickedFromCurrent
        | None -> searchersWithStepCount |> Seq.filter (fun (s, _) -> s <> currentSearcher) |> Seq.tryPick (fun (s, _) -> s.Pick())

    let pickWithSelector selector =
        let currentSearcher = getCurrentSearcher()
        match currentSearcher.Pick selector with
        | Some _ as pickedFromCurrent -> pickedFromCurrent
        | None -> searchersWithStepCount |> Seq.filter (fun (s, _) -> s <> currentSearcher) |> Seq.tryPick (fun (s, _) -> s.Pick selector)

    let update (parent, newStates) =
        for searcher, _ in searchersWithStepCount do
            searcher.Update(parent, newStates)

    let states() = searchersWithStepCount |> Seq.collect (fun (s, _) -> s.States()) |> Seq.distinct

    let reset() = for searcher, _ in searchersWithStepCount do searcher.Reset()

    let remove cilState = for searcher, _ in searchersWithStepCount do searcher.Remove cilState

    interface IForwardSearcher with
        override x.Init states = init states
        override x.Pick() = pick()
        override x.Pick selector = pickWithSelector selector
        override x.Update (parent, newStates) = update (parent, newStates)
        override x.States() = states()
        override x.Reset() = reset()
        override x.Remove cilState = remove cilState
