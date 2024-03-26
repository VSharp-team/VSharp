namespace VSharp.Interpreter.IL

open VSharp
open MethodBody
open System.Reflection.Emit

[<CustomEquality; NoComparison>]
type instructionPointer =
    | Exit of Method
    | Instruction of offset * Method
    | Leave of instructionPointer * exceptionHandlingClause list * offset * Method
    // current observing clauses; observed clauses; frames to observe catch clause; frames to observe finally clauses
    | SearchingForHandler of exceptionHandlingClause list option * exceptionHandlingClause list * codeLocation list * codeLocation list
    // ip of filter function; handler offset; previous searching handler information
    | InFilterHandler of instructionPointer * offset * exceptionHandlingClause list * exceptionHandlingClause list * codeLocation list * codeLocation list
    // ``None'' -- we are seeking for next finally or fault handler, ``Some _'' -- we are executing handler;
    // current observing clauses; last clauses to observe; last location to check last clauses;
    // rest frames of possible handlers; starting code location of handler if one was valid
    | SecondBypass of instructionPointer option * exceptionHandlingClause list option * exceptionHandlingClause list * codeLocation option * codeLocation list * codeLocation option
    with

    static member internal CreateLeave ip ehcs dst m =
        match ip with
        | Exit _ -> internalfail "Leave over Exit!"
        | _ -> Leave(ip, ehcs, dst, m)

    static member internal EmptySearchingForHandler =
        SearchingForHandler(Some List.empty, List.empty, List.empty, List.empty)

    member internal x.ChangeInnerIp newIp =
        match x with
        | Leave(ip, e, i, m) ->
            Leave(ip.ChangeInnerIp newIp, e, i, m)
        | InFilterHandler(ip, offset, e, f, lc, l) ->
            InFilterHandler(ip.ChangeInnerIp newIp, offset, e, f, lc, l)
        | SecondBypass(Some ip, ehcs, lastBlocks, loc, cl, ftp) ->
            SecondBypass(Some (ip.ChangeInnerIp newIp), ehcs, lastBlocks, loc, cl, ftp)
        | _ -> newIp

    member private x.IsNonRecIp with get() =
        match x with
        | Instruction _
        | Exit _
        | SearchingForHandler _
        | SecondBypass(None, _, _, _, _, _) -> true
        | _ -> false

    member internal x.ReplaceRecIp newIp =
        match x with
        | InFilterHandler(ip, _, _, _, _, _)
        | Leave(ip, _, _, _)
        | SecondBypass(Some ip, _, _, _, _, _) when ip.IsNonRecIp -> newIp
        | InFilterHandler(ip, offset, e, f, lc, l) ->
            InFilterHandler(ip.ReplaceRecIp newIp, offset, e, f, lc, l)
        | Leave(ip, e, i, m) -> Leave(ip.ReplaceRecIp newIp, e, i, m)
        | SecondBypass(Some ip, ehcs, lastBlocks, lastLocation, locations, handlerLoc) ->
            SecondBypass(Some (ip.ReplaceRecIp newIp), ehcs, lastBlocks, lastLocation, locations, handlerLoc)
        | _ -> newIp

    member internal x.MoveInstruction (newOffset : offset) =
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
                instructionPointer.CreateLeave ip' ehcs dst m |> k)
            | SearchingForHandler _ -> internalfail "moveInstruction: SearchingForHandler is not implemented"
            | Exit _ -> __unreachable__()
            | _ -> __unreachable__()
        helper newOffset x id

    member x.Offset with get() =
        match x with
        | Exit _ -> None
        | Instruction(offset, _) -> Some offset
        | Leave(ip, _, _, _) -> ip.Offset
        | SearchingForHandler _ -> None
        | InFilterHandler(ip, _, _, _, _, _) -> ip.Offset
        | SecondBypass(None, _, _, _, _, _) -> None
        | SecondBypass(Some ip, _, _, _, _, _) -> ip.Offset

    member internal x.Method with get() =
        match x with
        | Exit m
        | Instruction(_, m)
        | Leave(_, _, _, m) -> Some m
        | SearchingForHandler(_, _, [], []) -> None
        | SearchingForHandler(_, _, codeLocation :: _, []) -> Some codeLocation.method
        | SearchingForHandler(_, _, _, codeLocations) ->
            let codeLoc = List.last codeLocations
            Some codeLoc.method
        | InFilterHandler(ip, _, _, _, _, _) -> ip.Method
        | SecondBypass(None, _, _, _, _, _) -> None
        | SecondBypass(Some ip, _, _, _, _, _) -> ip.Method

    member x.ForceMethod() =
        match x.Method with
        | Some method -> method
        | None -> internalfail $"Getting current method: unexpected ip {x}"

    member x.ToCodeLocation() =
        match x.Offset, x.Method with
        | None, _ -> None
        | Some offset, Some m ->
            let loc = {offset = offset; method = m}
            Some loc
        | _ -> __unreachable__()

    member x.ForceCodeLocation() =
        match x.Offset, x.Method with
        | None, _ -> internalfail $"ForceCodeLocation: unable to get code location {x}"
        | Some offset, Some m -> {offset = offset; method = m}
        | _ -> __unreachable__()

    member x.IsFilter with get() =
        match x with
        | InFilterHandler _ -> true
        | _ -> false

    member x.IsInFilter with get() =
        match x with
        | InFilterHandler _ -> true
        | SecondBypass(Some ip, _, _, _, _, _) -> ip.IsInFilter
        | _ -> false

    override x.Equals y =
        match y with
        | :? instructionPointer as y ->
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
        | Instruction(offset, m) -> $"{{Instruction = 0x{int offset:X}; M = {m.FullName}}}"
        | Exit m -> $"{{Exit from M = {m.FullName}}}"
        | Leave(ip, _, offset, m) -> $"{{M = {m.FullName}; Leaving to 0x{offset:X}\n;Currently in {ip}}}"
        | SearchingForHandler(ehcs, finallyEhcs, toObserve, checkFinally) ->
            $"SearchingForHandler({ehcs}, {finallyEhcs}, {toObserve}, {checkFinally})"
        | SecondBypass(ip, ehcs, lastEhcs, method, restFrames, handler) ->
            $"SecondBypass({ip}, {ehcs}, {lastEhcs}, {method}, {restFrames}, {handler})"
        | InFilterHandler(ip, offset, ehcs, finallyEhcs, codeLocations, locations) ->
            $"InFilterHandler({ip}, {offset}, {ehcs}, {finallyEhcs}, {codeLocations}, {locations}"

and ipStack = instructionPointer list

type level = pdict<codeLocation, uint32>

module IpOperations =

    let bypassShouldPopFrame bypassLocations locationToJump currentLoc =
        List.isEmpty bypassLocations |> not ||
        match locationToJump with
        | Some loc -> loc.method <> currentLoc.method
        | None -> false

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

    let isCallIp (ip : instructionPointer) =
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

    let levelToUnsignedInt (lvl : level) =
        // TODO: remove it when ``level'' subtraction would be generalized
        PersistentDict.fold (fun acc _ v -> max acc v) 0u lvl
