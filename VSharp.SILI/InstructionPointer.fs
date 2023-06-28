namespace VSharp.Interpreter.IL

open VSharp

[<CustomEquality; CustomComparison>]
type ip =
    | Exit of Method
    | Instruction of offset * Method
    | Leave of ip * ExceptionHandlingClause list * offset * Method
    // frames to observe catch clause; frames to observe finally clauses
    | SearchingForHandler of codeLocation list * codeLocation list
    // ip of filter function; previous searching handler information
    | InFilterHandler of offset * Method * codeLocation list * int
    // ``None'' -- we are seeking for next finally or fault handler, ``Some _'' -- we are executing handler;
    // rest frames of possible handlers; starting code location of handler
    | SecondBypass of ip option * codeLocation list * codeLocation
    with
    member x.CanBeExpanded () =
        match x with
        | Exit _ -> false
        | _ -> false
    member x.Offset () =
        match x with
        | Instruction(i, _) -> i
        | _ -> internalfail "Could not get vertex from destination"
    override x.Equals y =
        match y with
        | :? ip as y ->
            match x, y with
            | Exit mx, Exit my -> mx = my
            | Instruction(ix, mx), Instruction(iy, my) -> ix = iy && mx.Equals(my)
            | Leave(ix, _, ipx, mx), Leave(iy, _, ipy, my) -> ix = iy && ipx.Equals(ipy) && mx.Equals(my)
            | SearchingForHandler(toObserve1, framesToPop1), SearchingForHandler(toObserve2, framesToPop2) ->
                toObserve1.Equals(toObserve2) && framesToPop1.Equals(framesToPop2)
            | InFilterHandler(offset1, m1, toObserve1, n1), InFilterHandler(offset2, m2, toObserve2, n2) ->
                offset1 = offset2 && m1.Equals(m2) && toObserve1.Equals(toObserve2) && n1 = n2
            | SecondBypass(ip1, toFinalize1, location1), SecondBypass(ip2, toFinalize2, location2) ->
                ip1.Equals(ip2) && toFinalize1.Equals(toFinalize2) && location1 = location2
            | _ -> false
        | _ -> false
    override x.GetHashCode() = x.ToString().GetHashCode()
    interface System.IComparable with
        override x.CompareTo y =
            match y with
            | :? ip as y ->
                match x, y with
                | Exit m1, Exit m2 -> compare m1 m2
                | Exit _, _ -> -1
                | Instruction(offset1, m1), Instruction(offset2, m2) when compare m1 m2 = 0 ->
                    compare offset1 offset2
                | Instruction(_, m1), Instruction(_, m2) -> compare m1 m2
                | Instruction _, _ -> -1
                | Leave(ip1, _, offset1, m1), Leave(ip2, _, offset2, m2)
                    when compare ip1 ip2 = 0 && compare m1 m2 = 0 ->compare offset1 offset2
                | Leave(ip1, _, _, m1), Leave(ip2, _, _, m2) when compare ip1 ip2 = 0 -> compare m1 m2
                | Leave(ip1,_,_,_), Leave(ip2,_,_, _) -> compare ip1 ip2
                | Leave _, _ -> -1
                | SearchingForHandler(toObserve1, pop1), SearchingForHandler(toObserve2, pop2)
                    when compare toObserve1 toObserve2 = 0 -> compare pop1 pop2
                | SearchingForHandler(toObserve1, _), SearchingForHandler(toObserve2, _) -> compare toObserve1 toObserve2
                | SearchingForHandler _, _ -> -1
                | InFilterHandler(offset1, m1, toObserve1, pop1), InFilterHandler(offset2, m2, toObserve2, pop2)
                    when compare offset1 offset2 = 0 && compare m1 m2 = 0 &&  compare toObserve1 toObserve2 = 0 -> compare pop1 pop2
                | InFilterHandler(offset1, m1, toObserve1, _), InFilterHandler(offset2, m2 ,toObserve2, _)
                    when compare offset1 offset2 = 0 && compare m1 m2 = 0 -> compare toObserve1 toObserve2
                | InFilterHandler(offset1, m1, _, _), InFilterHandler(offset2, m2 ,_, _) when compare offset1 offset2 = 0 -> compare m1 m2
                | InFilterHandler _, _ -> -1
                | SecondBypass(ip1, toFinalize1, location1), SecondBypass(ip2, toFinalize2, location2)
                    when compare ip1 ip2 = 0 && compare toFinalize1 toFinalize2 = 0 -> compare location1 location2
                | SecondBypass(ip1, toFinalize1,_), SecondBypass(ip2, toFinalize2, _)
                    when compare ip1 ip2 = 0 -> compare toFinalize1 toFinalize2
                | SecondBypass(ip1, _, _), SecondBypass(ip2, _, _) -> compare ip1 ip2
                | SecondBypass _, _ -> -1
            | _ -> -1
    override x.ToString() =
        match x with
        | Instruction(offset, m) -> sprintf "{Instruction = %s; M = %s}" ((int offset).ToString("X")) m.FullName
        | Exit m -> $"{{Exit from M = %s{m.FullName}}}"
        | Leave(ip, _, offset, m) -> $"{{M = %s{m.FullName}; Leaving to %d{offset}\n;Currently in {ip}}}"
        | SearchingForHandler(toObserve, checkFinally) -> $"SearchingForHandler({toObserve}, {checkFinally})"
        | SecondBypass(ip, restFrames, handler) -> $"SecondBypass({ip}, {restFrames}, {handler})"
        | _ -> __notImplemented__()

and ipStack = ip list

type level = pdict<codeLocation, uint32>

module ipOperations =
    let exit m = Exit m
    let isExit = function
        | Exit _ -> true
        | _ -> false
    let instruction m i = Instruction(i, m)
    let leave ip ehcs dst m =
        match ip with
        | Exit _ -> internalfail "Leave over Exit!"
        | _ -> Leave(ip, ehcs, dst, m)

    let moveInstruction (newOffset : offset) ip =
        let rec helper (newOffset : offset) ip k =
            match ip with
            | Instruction(_, m) -> Instruction(newOffset, m) |> k
            | InFilterHandler(_, m, x, y) -> InFilterHandler(newOffset, m, x, y) |> k
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
        | InFilterHandler(offset, _, _, _) -> Some offset
        | SecondBypass(None, _, _) -> None
        | SecondBypass(Some ip, _, _) -> offsetOf ip

    let rec methodOf = function
        | Exit m
        | Instruction(_, m)
        | Leave(_, _, _, m) -> Some m
        | SearchingForHandler([], []) -> None
        | SearchingForHandler(codeLocation :: _, []) -> Some codeLocation.method
        | SearchingForHandler(_, codeLocations) ->
            let codeLoc = List.last codeLocations
            Some codeLoc.method
        | InFilterHandler(_, method, _, _) -> Some method
        | SecondBypass(None, _, _) -> None
        | SecondBypass(Some ip, _, _) -> methodOf ip

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
