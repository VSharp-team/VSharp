namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open VSharp

type internal ExecutionTreeSearcher(randomSeed : int option) =

    let random =
        match randomSeed with
        | Some randomSeedValue -> Random(randomSeedValue)
        | None -> Random()

    let getRandomInt() = random.Next(0, Int32.MaxValue)

    let trees = Dictionary<Method, ExecutionTree<cilState>>()
    let methods = List<Method>()

    let rec getAllStates() = Seq.collect (fun (t : ExecutionTree<cilState>) -> t.States) trees.Values

    let init initialStates =
        if trees.Count > 0 then
            invalidOp "Trying to init non-empty execution tree searcher"
        for method, methodStates in initialStates |> Seq.groupBy CilStateOperations.entryMethodOf do
            methods.Add method
            let methodStates = methodStates |> Seq.toList
            if List.length methodStates > 1 then
                invalidOp "Cannot init execution tree searcher with more than 1 initial state for method"
            trees[method] <- ExecutionTree(methodStates.Head)

    let pick() =
        let method = methods[getRandomInt() % methods.Count]
        let picked = trees[method].RandomPick getRandomInt
        picked

    // This function is optimized for selector based on entry method of state (like in fair searcher)
    let pickWithSelector selector =
        let isSuitableTree (tree : ExecutionTree<cilState>) =
            Seq.head tree.States |> selector
        let pickedOpt =
            trees.Values |>
            Seq.tryFind isSuitableTree |>
            Option.bind (fun t -> t.RandomPick getRandomInt)
        match pickedOpt with
        | Some picked when selector picked ->
            pickedOpt
        | _ ->
            // Fallback (inefficient, in case of plain fair searcher should not happen)
            getAllStates() |> Seq.tryFind selector

    let remove state =
        match Seq.tryFind (fun (KeyValue(_, t : ExecutionTree<cilState>)) -> t.Remove state) trees with
        | Some(KeyValue(method, tree)) ->
            if tree.StatesCount = 0 then
                let wasRemoved = trees.Remove method
                assert wasRemoved
                let wasRemoved = methods.Remove method
                assert wasRemoved
        | _ -> ()

    let reset() =
        trees.Clear()
        methods.Clear()

    let update parent newStates =
        match Seq.tryFind (fun (t : ExecutionTree<cilState>) -> t.AddFork parent newStates) trees.Values with
        | Some _ -> ()
        | _ -> invalidOp "Cannot update execution tree searcher: parent not found in none of trees"

    interface IForwardSearcher with
        member this.Init initialStates = init initialStates
        member this.Pick() = pick()
        member this.Pick selector = pickWithSelector selector
        member this.Remove state = remove state
        member this.Reset() = reset()
        member this.States() = getAllStates()
        member this.StatesCount = Seq.sumBy (fun (t : ExecutionTree<cilState>) -> t.StatesCount) trees.Values
        member this.Update(parent, newStates) = update parent newStates
