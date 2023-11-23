namespace VSharp.Interpreter.IL

open VSharp
open MethodBody
open System.Reflection.Emit

[<CustomEquality; NoComparison>]
type ip =
    | Exit of Method
    | Instruction of offset * Method
    | Leave of ip * ExceptionHandlingClause list * offset * Method
    // current observing clauses; observed clauses; frames to observe catch clause; frames to observe finally clauses
    | SearchingForHandler of ExceptionHandlingClause list option * ExceptionHandlingClause list * codeLocation list * codeLocation list
    // ip of filter function; handler offset; previous searching handler information
    | InFilterHandler of ip * offset * ExceptionHandlingClause list * ExceptionHandlingClause list * codeLocation list * codeLocation list
    // ``None'' -- we are seeking for next finally or fault handler, ``Some _'' -- we are executing handler;
    // current observing clauses; last clauses to observe; last location to check last clauses;
    // rest frames of possible handlers; starting code location of handler if one was valid
    | SecondBypass of ip option * ExceptionHandlingClause list option * ExceptionHandlingClause list * codeLocation option * codeLocation list * codeLocation option
    with
    override x.Equals y =
        match y with
        | :? ip as y ->
            match x, y with
            | Exit mx, Exit my -> mx = my
            | Instruction(ix, mx), Instruction(iy, my) -> ix = iy && mx.Equals(my)
            | Leave(ix, _, ipx, mx), Leave(iy, _, ipy, my) -> ix = iy && ipx.Equals(ipy) && mx.Equals(my)
            | SearchingForHandler(handlersToSearch1, finallyEhcs1, toObserve1, framesToPop1), SearchingForHandler(handlersToSearch2, finallyEhcs2, toObserve2, framesToPop2) ->
                toObserve1 = toObserve2 && framesToPop1 = framesToPop2 && finallyEhcs1 = finallyEhcs2 && handlersToSearch1 = handlersToSearch2
            | InFilterHandler(ip1, offset1, ehcs1, finallyEhcs1, toObserve1, toPop1), InFilterHandler(ip2, offset2, ehcs2, finallyEhcs2, toObserve2, toPop2) ->
                ip1.Equals(ip2) && offset1.Equals(offset2) && toObserve1.Equals(toObserve2) && finallyEhcs1.Equals(finallyEhcs2) && toPop1.Equals(toPop2) && ehcs1.Equals(ehcs2)
            | SecondBypass(ip1, ehcs1, lastEhcs1, loc, toFinalize1, location1), SecondBypass(ip2, ehcs2, lastEhcs2, method2, toFinalize2, location2) ->
                ip1.Equals(ip2) && loc.Equals(method2) && ehcs1.Equals(ehcs2) && lastEhcs1.Equals(lastEhcs2) && toFinalize1.Equals(toFinalize2) && location1 = location2
            | _ -> false
        | _ -> false
    override x.GetHashCode() = x.ToString().GetHashCode()

    override x.ToString() =
        match x with
        | Instruction(offset, m) -> sprintf "{Instruction = %s; M = %s}" ((int offset).ToString("X")) m.FullName
        | Exit m -> $"{{Exit from M = %s{m.FullName}}}"
        | Leave(ip, _, offset, m) -> $"{{M = %s{m.FullName}; Leaving to %d{offset}\n;Currently in {ip}}}"
        | SearchingForHandler(ehcs, finallyEhcs, toObserve, checkFinally) ->
            $"SearchingForHandler({ehcs}, {finallyEhcs}, {toObserve}, {checkFinally})"
        | SecondBypass(ip, ehcs, lastEhcs, method, restFrames, handler) ->
            $"SecondBypass({ip}, {ehcs}, {lastEhcs}, {method}, {restFrames}, {handler})"
        | InFilterHandler(ip, offset, ehcs, finallyEhcs, codeLocations, locations) ->
            $"InFilterHandler({ip}, {offset}, {ehcs}, {finallyEhcs}, {codeLocations}, {locations}"

and ipStack = ip list

type level = pdict<codeLocation, uint32>

module IpOperations =

    let exit m = Exit m

    let instruction m i = Instruction(i, m)

    let leave ip ehcs dst m =
        match ip with
        | Exit _ -> internalfail "Leave over Exit!"
        | _ -> Leave(ip, ehcs, dst, m)

    let moveInstruction (newOffset : offset) ip =
        let rec helper (newOffset : offset) ip k =
            match ip with
            | Instruction(_, m) -> Instruction(newOffset, m) |> k
            | InFilterHandler(ip, offset, m, f, x, y) ->
                helper newOffset ip (fun ip' ->
                InFilterHandler(ip', offset, m, f, x, y) |> k)
            | SecondBypass(Some ip, ehcs, lastEhcs, m, toPop, handler) ->
                helper newOffset ip (fun ip' ->
                SecondBypass(Some ip', ehcs, lastEhcs, m, toPop, handler) |> k)
            | Leave(ip, ehcs, dst, m) ->
                helper newOffset ip (fun ip' ->
                leave ip' ehcs dst m |> k)
            | SearchingForHandler _ -> internalfail "moveInstruction: SearchingForHandler is not implemented"
            | Exit _ -> __unreachable__()
            | _ -> __unreachable__()
        helper newOffset ip id

    let rec offsetOf = function
        | Exit _ -> None
        | Instruction(offset, _) -> Some offset
        | Leave(ip, _, _, _) -> offsetOf ip
        | SearchingForHandler _ -> None
        | InFilterHandler(ip, _, _, _, _, _) -> offsetOf ip
        | SecondBypass(None, _, _, _, _, _) -> None
        | SecondBypass(Some ip, _, _, _, _, _) -> offsetOf ip

    let rec methodOf = function
        | Exit m
        | Instruction(_, m)
        | Leave(_, _, _, m) -> Some m
        | SearchingForHandler(_, _, [], []) -> None
        | SearchingForHandler(_, _, codeLocation :: _, []) -> Some codeLocation.method
        | SearchingForHandler(_, _, _, codeLocations) ->
            let codeLoc = List.last codeLocations
            Some codeLoc.method
        | InFilterHandler(ip, _, _, _, _, _) -> methodOf ip
        | SecondBypass(None, _, _, _, _, _) -> None
        | SecondBypass(Some ip, _, _, _, _, _) -> methodOf ip

    let forceMethodOf ip =
        match methodOf ip with
        | Some method -> method
        | None -> internalfail $"Getting current method: unexpected ip {ip}"

    let ip2codeLocation (ip : ip) =
        match offsetOf ip, methodOf ip with
        | None, _ -> None
        | Some offset, Some m ->
            let loc = {offset = offset; method = m}
            Some loc
        | _ -> __unreachable__()

    let isFilter = function
        | InFilterHandler _ -> true
        | _ -> false

    let rec ipIsInFilter ip =
        match ip with
        | InFilterHandler _ -> true
        | SecondBypass(Some ip, _, _, _, _, _) -> ipIsInFilter ip
        | _ -> false

    let bypassShouldPopFrame bypassLocations locationToJump currentLoc =
        List.isEmpty bypassLocations |> not ||
        match locationToJump with
        | Some loc -> loc.method <> currentLoc.method
        | None -> false

    let emptySearchingForHandler =
        SearchingForHandler(Some List.empty, List.empty, List.empty, List.empty)

    let (|EndFinally|_|) = function
        | Instruction(offset, m) when parseInstruction m offset = OpCodes.Endfinally -> Some()
        | _ -> None

    let rec (|InstructionEndingIp|_|) = function
        | Instruction(offset, m) -> Some (offset, m)
        | SecondBypass(Some ip, _, _, _, _, _) -> (|InstructionEndingIp|_|) ip
        | InFilterHandler(ip, _, _, _, _, _) -> (|InstructionEndingIp|_|) ip
        | Leave(ip, _, _, _) -> (|InstructionEndingIp|_|) ip
        | _ -> None

    let (|EmptySecondBypass|_|) = function
        | SecondBypass(None, None, [], _, [], None) -> Some()
        | _ -> None

    let (|EmptySecondBypassWithCatch|_|) = function
        | SecondBypass(None, None, [], _, [], Some loc) -> Some loc
        | _ -> None

    let (|EmptySearchingForHandler|_|) = function
        | SearchingForHandler(Some [], [], [], []) -> Some()
        | _ -> None

    let isCallIp (ip : ip) =
        match ip with
        | InstructionEndingIp(offset, m) ->
            let opCode = parseInstruction m offset
            isDemandingCallOpCode opCode
        | _ -> false

module Level =
    // TODO: implement level
    let zero : level = PersistentDict.empty
    let inf : level = PersistentDict.empty

    let isZero (l : level) = (l = zero)
    let isInf (l : level) = (l = inf)

    let next (_ : level) : level = __notImplemented__()

    let prev (_ : level) : level = __notImplemented__()
    let toUInt (_ : level) : uint32 = __notImplemented__()

    let toInt (_ : level) : int = 0

    let toString (lvl : level) =
        if isInf lvl then "inf" else lvl.ToString()
