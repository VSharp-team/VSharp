namespace VSharp.Core

open FSharpx.Collections
open VSharp

module public SolverInteraction =

    type unsatCore() = class end

    type encodingContext =
        { addressOrder : Map<concreteHeapAddress, int>}

    type satInfo = { mdl : model }
    type unsatInfo = { core : term[] }

    type smtResult =
        | SmtSat of satInfo
        | SmtUnsat of unsatInfo
        | SmtUnknown of string

    type ISolver =
        abstract CheckSat : encodingContext -> term -> smtResult
        abstract Assert : encodingContext -> term -> unit

        abstract SetMaxBufferSize : int -> unit

    let mutable private solver : ISolver option = None
    let mutable private onSolverStarted : unit -> unit = id
    let mutable private onSolverStopped : unit -> unit = id    

    let configureSolver s = solver <- Some s
    let setOnSolverStarted action = onSolverStarted <- action
    let setOnSolverStopped action = onSolverStopped <- action

    let setMaxBufferSize size =
        match solver with
        | Some s -> s.SetMaxBufferSize size
        | None -> ()

    let getEncodingContext (state : state) =
        let addresses = PersistentDict.keys state.allocatedTypes
        let sortedAddresses = Seq.sortWith VectorTime.compare addresses
        let order = Seq.fold (fun (map, i) address -> Map.add address i map, i + 1) (Map.empty, 1) sortedAddresses |> fst
        let orderWithNull = Map.add VectorTime.zero 0 order
        { addressOrder = orderWithNull }

    let checkSat state = // TODO: need to solve types here? #do
        let ctx = getEncodingContext state
        let formula = PC.toSeq state.pc |> conjunction
        match solver with
        | Some s ->
            onSolverStarted()
            let result = s.CheckSat ctx formula
            onSolverStopped()
            result
        | None -> SmtUnknown ""
