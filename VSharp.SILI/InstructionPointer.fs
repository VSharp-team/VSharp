namespace VSharp.Interpreter.IL

open VSharp

[<CustomEquality; CustomComparison>]
type ip =
    | Exit of Method
    | Instruction of offset * Method
    | Leave of ip * ExceptionHandlingClause list * offset * Method
    // frames to observe catch clause; frames to observe finally clauses
    | SearchingForHandler of ExceptionHandlingClause list option * codeLocation list * codeLocation list
    // ip of filter function; previous searching handler information
    | InFilterHandler of ip * ExceptionHandlingClause list * codeLocation list * codeLocation list
    // ``None'' -- we are seeking for next finally or fault handler, ``Some _'' -- we are executing handler;
    // rest frames of possible handlers; starting code location of handler if one was valid
    | SecondBypass of ip option * codeLocation list * codeLocation option
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
            | SearchingForHandler(handlersToSearch1, toObserve1, framesToPop1), SearchingForHandler(handlersToSearch2, toObserve2, framesToPop2) ->
                toObserve1.Equals(toObserve2) && framesToPop1.Equals(framesToPop2) && handlersToSearch1.Equals(handlersToSearch2)
            | InFilterHandler(ip1, ehcs1, toObserve1, toPop1), InFilterHandler(ip2, ehcs2,toObserve2, toPop2) ->
                ip1.Equals(ip2) && toObserve1.Equals(toObserve2) && toPop1.Equals(toPop2) && ehcs1.Equals(ehcs2)
            | SecondBypass(ip1, toFinalize1, location1), SecondBypass(ip2, toFinalize2, location2) ->
                ip1.Equals(ip2) && toFinalize1.Equals(toFinalize2) && location1 = location2
            | _ -> false
        | _ -> false
    override x.GetHashCode() = x.ToString().GetHashCode()
    interface System.IComparable with
        override x.CompareTo y =
            // Chain compare
            let inline (>==>) x y = if x = 0 then y else x
            // TODO: Add proper comparison to Exception Handling Clause
            let inline compareEhcsOptions a b =
                match a, b with
                | None, None -> 0
                | None, Some _ -> -1
                | Some _, None -> 1
                | Some a, Some b -> compare (List.length a) (List.length b)
            let compareEhcs a b =
                compareEhcsOptions (Some a) (Some b)
            match y with
            | :? ip as y ->
                match x, y with
                | Exit m1, Exit m2 ->
                    compare m1 m2
                | Exit _, _ -> -1
                | Instruction(offset1, m1), Instruction(offset2, m2) ->
                    (compare offset1 offset2) >==>
                    (compare m1 m2)
                | Instruction _, _ -> -1
                | Leave(ip1, _, offset1, m1), Leave(ip2, _, offset2, m2) ->
                    (compare ip1 ip2) >==>
                    (compare offset1 offset2) >==>
                    (compare m1 m2)
                | Leave _, _ -> -1
                | SearchingForHandler(ehcs1, toObserve1, pop1), SearchingForHandler(ehcs2, toObserve2, pop2)  ->
                    (compareEhcsOptions ehcs1 ehcs2) >==>
                    (compare toObserve1 toObserve2) >==>
                    (compare pop1 pop2)
                | SearchingForHandler _, _ -> -1
                | InFilterHandler(ip1, ehcs1, toObserve1, pop1), InFilterHandler(ip2, ehcs2, toObserve2, pop2) ->
                    (compare ip1 ip2) >==>
                    (compareEhcs ehcs1 ehcs2) >==>
                    (compare toObserve1 toObserve2) >==>
                    (compare pop1 pop2)
                | InFilterHandler _, _ -> -1
                | SecondBypass(ip1, toFinalize1, location1), SecondBypass(ip2, toFinalize2, location2) ->
                    (compare ip1 ip2) >==>
                    (compare toFinalize1 toFinalize2) >==>
                    (compare location1 location2)
                | SecondBypass _, _ -> -1
            | _ -> -1
    override x.ToString() =
        match x with
        | Instruction(offset, m) -> sprintf "{Instruction = %s; M = %s}" ((int offset).ToString("X")) m.FullName
        | Exit m -> $"{{Exit from M = %s{m.FullName}}}"
        | Leave(ip, _, offset, m) -> $"{{M = %s{m.FullName}; Leaving to %d{offset}\n;Currently in {ip}}}"
        | SearchingForHandler(ehcs, toObserve, checkFinally) -> $"SearchingForHandler({ehcs}, {toObserve}, {checkFinally})"
        | SecondBypass(ip, restFrames, handler) -> $"SecondBypass({ip}, {restFrames}, {handler})"
        | InFilterHandler(ip , ehcs, codeLocations, locations) ->
            $"InFilterHandler({ip.ToString()}, {ehcs}, {codeLocations}, {locations}"

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
            | InFilterHandler(ip, m, x, y) ->
                helper newOffset ip (fun ip' ->
                InFilterHandler(ip', m, x, y) |> k)
            | SecondBypass(Some ip, m, x) ->
                helper newOffset ip (fun ip' ->
                SecondBypass(Some ip', m, x) |> k)
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
        | InFilterHandler(ip, _, _, _) -> offsetOf ip
        | SecondBypass(None, _, _) -> None
        | SecondBypass(Some ip, _, _) -> offsetOf ip

    let rec methodOf = function
        | Exit m
        | Instruction(_, m)
        | Leave(_, _, _, m) -> Some m
        | SearchingForHandler(_, [], []) -> None
        | SearchingForHandler(_, codeLocation :: _, []) -> Some codeLocation.method
        | SearchingForHandler(_, _, codeLocations) ->
            let codeLoc = List.last codeLocations
            Some codeLoc.method
        | InFilterHandler(ip, _, _, _) -> methodOf ip
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
