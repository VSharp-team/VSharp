namespace VSharp.Interpreter.IL

open System.Linq

type InterleavedSearcher(searchersWithStepCount: (IForwardSearcher * int) list) =

    let searchers = seq { while true do yield! searchersWithStepCount |> Seq.collect Enumerable.Repeat }
    let searchersEnumerator = searchers.GetEnumerator()

    let getCurrentSearcher() =
        searchersEnumerator.MoveNext() |> ignore
        searchersEnumerator.Current

    let init states = for searcher, _ in searchersWithStepCount do searcher.Init states

    // TODO: handle None
    let pick() = getCurrentSearcher().Pick()

    let update (parent, newStates) =
        for searcher, _ in searchersWithStepCount do
            searcher.Update(parent, newStates)

    let states() = searchersWithStepCount |> Seq.collect (fun (s, _) -> s.States()) |> Seq.distinct

    let reset() = for searcher, _ in searchersWithStepCount do searcher.Reset()

    interface IForwardSearcher with
        override x.Init states = init states
        override x.Pick() = pick()
        override x.Update (parent, newStates) = update (parent, newStates)
        override x.States() = states()
        override x.Reset() = reset()
