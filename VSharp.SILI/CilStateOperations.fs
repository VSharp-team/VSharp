namespace VSharp.Interpreter.IL

open VSharp
open System.Text
open System.Reflection
open VSharp.Core.API

type cilState =
    { ip : ip
      state : state
      filterResult : term option
      iie : InsufficientInformationException option
      level : level
    }
    member x.CanBeExpanded () = x.ip.CanBeExpanded()
    member x.HasException = Option.isSome x.state.exceptionsRegister.ExceptionTerm

module internal CilStateOperations =

    let makeCilState curV state =
        { ip = curV
          state = state
          filterResult = None
          iie = None
          level = PersistentDict.empty
        }

    let makeInitialState state = makeCilState (ip.Instruction 0) state

    let compose (cilState1 : cilState) (cilState2 : cilState) k =
        let level =
            PersistentDict.fold (fun (acc : level) k v ->
                let oldValue = if PersistentDict.contains k acc then PersistentDict.find acc k else 0u
                PersistentDict.add k (v + oldValue) acc
            ) cilState1.level cilState2.level

        let states = VSharp.Core.API.Memory.ComposeStates cilState1.state cilState2.state id
        k <| List.map (fun state -> {cilState2 with state = state; level = level}) states

    let incrementLevel (cilState : cilState) k =
        let lvl = cilState.level
        let newValue = if PersistentDict.contains k lvl then PersistentDict.find lvl k + 1u else 1u
        {cilState with level = PersistentDict.add k newValue lvl}

    // ------------------------------- Helper functions for cilState and state interaction -------------------------------
    let stateOf (cilState : cilState) = cilState.state
    let popStackOf (cilState : cilState) = {cilState with state = Memory.PopStack cilState.state}

    let withCurrentTime time (cilState : cilState) = {cilState with state = {cilState.state with currentTime = time}}

    let withOpStack opStack (cilState : cilState) = {cilState with state = {cilState.state with opStack = opStack}}

    let withState state (cilState : cilState) = {cilState with state = state}
    let changeState (cilState : cilState) state = {cilState with state = state}
    let withResult result (cilState : cilState) = {cilState with state = {cilState.state with returnRegister = Some result}}
    let withNoResult (cilState : cilState) = {cilState with state = {cilState.state with returnRegister = None}}
    let withResultState result (state : state) = {state with returnRegister = Some result}
    let withIp ip (cilState : cilState) = {cilState with ip = ip}
    let pushToOpStack v (cilState : cilState) = {cilState with state = {cilState.state with opStack = v :: cilState.state.opStack}}
    let withException exc (cilState : cilState) = {cilState with state = {cilState.state with exceptionsRegister = exc}}


    // ------------------------------- Helper functions for cilState -------------------------------



    let pushResultOnOpStack (cilState : cilState) (res, state) =
        if res <> Terms.Nop then cilState |> withState state |> pushToOpStack res
        else withState state cilState

    let pushResultToOperationalStack (cilStates : cilState list)  =
        cilStates |> List.map (fun (cilState : cilState) ->
            let state = cilState.state
            if state.exceptionsRegister.UnhandledError then cilState // TODO: check whether opStack = [] is needed
            else
                let opStack =
                    match state.returnRegister with
                    | None -> state.opStack
                    | Some r -> r :: state.opStack
                let state = {state with returnRegister = None; opStack = opStack}
                {cilState with state = state})

    let StatedConditionalExecutionCIL (cilState : cilState) (condition : state -> (term * state -> 'a) -> 'a) (thenBranch : cilState -> ('c list -> 'a) -> 'a) (elseBranch : cilState -> ('c list -> 'a) -> 'a) (k : 'c list -> 'a) =
        StatedConditionalExecution cilState.state condition
            (fun state k -> thenBranch {cilState with state = state} k)
            (fun state k -> elseBranch {cilState with state = state} k)
            (fun x y -> List.append x y |> List.singleton)
            (List.head >> k)
    let GuardedApplyCIL (cilState : cilState) term (f : cilState -> term -> ('a list -> 'b) -> 'b) (k : 'a list -> 'b) =
        GuardedStatedApplyk
            (fun state term k -> f {cilState with state = state} term k)
            cilState.state term id (List.concat >> k)

    let StatedConditionalExecutionAppendResultsCIL (cilState : cilState) conditionInvocation (thenBranch : (cilState -> (cilState list -> 'a) -> 'a)) elseBranch k =
         StatedConditionalExecution cilState.state conditionInvocation
            (fun state k -> thenBranch {cilState with state = state} k)
            (fun state k -> elseBranch {cilState with state = state} k)
            (fun x y -> [x; y])
            (List.concat >> k)

    let BranchOnNullCIL (cilState : cilState) term thenBranch elseBranch k =
        StatedConditionalExecutionAppendResultsCIL cilState
            (fun state k -> k (IsNullReference term, state))
            thenBranch
            elseBranch
            k

    // ------------------------------- Pretty printing for cilState -------------------------------
    let private dumpSection section (sb : StringBuilder) =
        sprintf "--------------- %s: ---------------" section |> sb.AppendLine

    let private dumpSectionValue section value (sb : StringBuilder) =
        sb |> dumpSection section |> (fun sb -> sb.AppendLine value)

    let private dumpDict section sort keyToString valueToString (sb : StringBuilder) d =
        if PersistentDict.isEmpty d then sb
        else
            let sb = dumpSection section sb
            PersistentDict.dump d sort keyToString valueToString |> sb.AppendLine

    let ipAndMethodBase2String (ip : ip, m : MethodBase) =
        sprintf "Method: %O, ip = %O" m ip

    // TODO: print filterResult and IIE ?
    let dump (cilState : cilState) : string =
        let sb = dumpSectionValue "IP" (sprintf "%O" cilState.ip) (StringBuilder())
        let sb = dumpDict "Level" id ipAndMethodBase2String id sb cilState.level

        let stateDump = VSharp.Core.API.Memory.Dump cilState.state
        let sb = dumpSectionValue "State" stateDump sb

        if sb.Length = 0 then "<EmptyCilState>" else sb.ToString()
