namespace VSharp.Interpreter.IL

open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open FSharpx.Collections
open System.Reflection

open VSharp
open VSharp.Core
open CilStateOperations

type public PobsInterpreter(searcher : INewSearcher) =
    inherit ExplorerBase()
    let infty = System.Int32.MaxValue
    let GlobalBound = 20
    let initialOpStackSize = 0u
    let qFront = List<cilState>()
    let qBack = List<pob * cilState>()
    let transition = Dictionary<ip, cilState list>()
    let witnesses = Dictionary<pob, cilState list>()
    let blockedLocs = Dictionary<pob, ip list>()
    let possiblePobsLocs = List<ip>()
    let sPobs = Dictionary<cilState, pob list>()
    let mutable curLvl = 0

    let mainPobs = List<pob>()
    let currentPobs = List<pob>()
    let answeredPobs = Dictionary<pob, pobStatus>()
    let parents = Dictionary<pob, pob>()
    let canReach source target (blockedLocs : ip list) =
        //TODO: use CFG-reachability analysis
        true
    let addToDictionaryWithListValue (d : Dictionary<'a, 'b list>) k elem =
        let v = d.GetValueOrDefault(k, [])
        d.[k] <- (elem :: v)

    let removeFromDictionaryWithListValue (d : Dictionary<'a, 'b list>) k elem =
        let v = d.GetValueOrDefault(k, [])
        let v' = List.filter (fun x -> x <> elem) v
        d.[k] <- v'

    let removeFromGlobalVars (s : cilState, p : pob) =
        removeFromDictionaryWithListValue sPobs s p
        removeFromDictionaryWithListValue witnesses p s

    let addWitness(s : cilState, p : pob) =
        let sLvl = levelToInt s.level
        try
            if sLvl <= p.lvl && canReach (currentIp s) p.loc blockedLocs.[p] then
                addToDictionaryWithListValue witnesses p s
                addToDictionaryWithListValue sPobs s p
        with
        | :? KeyNotFoundException -> __notImplemented__()
    let rec blockWitness(s', p') =
        removeFromGlobalVars(s', p')
        match List.exists (fun (s : cilState) -> s.startingIP = s'.startingIP) witnesses.[p'] with
        | true -> ()
        | false ->
            addToDictionaryWithListValue blockedLocs p' s'.startingIP
            List.iter (fun (s : cilState) ->
                if not <| canReach (currentIp s) p'.loc blockedLocs.[p'] then blockWitness(s, p')) witnesses.[p']

    let addPob(parent : pob, child : pob) =
        assert(parents.ContainsKey(child) |> not)
        parents.Add(child, parent)
        currentPobs.Add child
        Seq.iter (fun s -> addWitness(s, child)) qFront
        Seq.iter (fun s -> qBack.Add(child, s)) transition.[child.loc]

    let rec answerYes (s' : cilState, p' : pob) =
        if Seq.contains p' mainPobs then mainPobs.Remove(p') |> ignore
        if answeredPobs.ContainsKey p' |> not then answeredPobs.Add(p', Witnessed s')
        else answeredPobs.[p'] <- Witnessed s'
        removeFromGlobalVars(s', p')
        if parents.ContainsKey p' then answerYes(s', parents.[p'])

    member x.MakeCilStateForIp (ip : ip) =
        let m = methodOf ip
        let cilStates = x.FormInitialState m
        match cilStates with
        | [cilState] -> {cilState with startingIP = ip} |> withIpStack [ip]
        | _ -> __notImplemented__()

    //NOTE: Must be called for ip with empty evaluation stack!
    member x.Start (loc : ip) =
        possiblePobsLocs.Add(loc)
        let start = x.MakeCilStateForIp loc
        qFront.Add(start)
        Seq.iter (fun p -> addWitness(start, p)) currentPobs

    member x.Forward (s : cilState) =
        assert(sPobs.ContainsKey s)
//        if (not <| sPobs.ContainsKey s) then ()
        let removed = qFront.Remove(s) in assert(removed)
        let goodStates, incompleteStates, errors = ILInterpreter(x).ExecuteOnlyOneInstruction s
        qFront.AddRange (goodStates @ incompleteStates @ errors)
        goodStates |> List.iter (fun (s' : cilState) ->
            if not <| sPobs.ContainsKey(s') then sPobs.Add(s', [])
            let ip' = currentIp s'
            if Seq.contains ip' possiblePobsLocs then addToDictionaryWithListValue transition ip' s'
            sPobs.[s] |> List.iter (fun p ->
                addWitness(s', p)
                assert(sPobs.ContainsKey s')
                if p.loc = currentIp s' then qBack.Add(p, s')                )
        )
        List.iter (fun (p : pob) -> if p.loc <> currentIp s then removeFromGlobalVars(s, p)) sPobs.[s]

    member x.OverApproximate(_ : ip, _ : int) : term = Terms.True

    member x.Backward (p' : pob, s' : cilState, EP : ip) =
        let removed = qBack.Remove(p',s') in assert(removed)
        assert(currentIp s' = p'.loc)
        let lvl = p'.lvl - (levelToInt s'.level)
        let fml = Memory.WLP s'.state p'.fml
        match Memory.IsSAT fml with
        | true when s'.startingIP = EP ->
            let removed = currentPobs.Remove(p') in assert(removed)
            let removed = witnesses.Remove(p') in assert(removed)
            let removed = blockedLocs.Remove(p') in assert(removed)
            qBack.RemoveAll(fun (p, _) -> p = p') |> ignore
            answerYes(s', p')
        | true ->
            let p = {loc = s'.startingIP; lvl = lvl; fml = fml}
            addPob(p', p)
        | false ->
            Logger.info "UNSAT for pob = %O and s' = %O" p' s'
    member x.PropDirSymExec (main : MethodBase) (EP : ip) mainPobsList =
        Seq.iter (fun p -> mainPobs.Add p) mainPobsList
//        let clearStructures () =
//            currentPobs.Clear()
//            witnesses.Clear()
        let createPobs () =
            mainPobs |> Seq.iter (fun (mp : pob) ->
                let p = {mp with lvl = curLvl}
                blockedLocs.Add(p, [])
                witnesses.Add(p, [])
                parents.Add(p, mp)
                currentPobs.Add p)
        while mainPobs.Count > 0 && curLvl <= GlobalBound do
//            clearStructures()
            createPobs()
//            let pobs : List<pob> = List(createPobs ())
            while currentPobs.Count > 0 do
                match searcher.ChooseAction(List.ofSeq qFront, List.ofSeq qBack, main) with
                | Stop -> currentPobs.Clear()
                | Start loc -> x.Start(loc)
                | GoForward s ->
                    Logger.info "GoForward: ip = %O" (currentIp s)
                    x.Forward(s)
                | GoBackward(p', s') -> x.Backward(p',s', EP)
            curLvl <- curLvl + 1

//    member x.FindPobs (t : System.Type) : pob list =
//        let methods = t.GetMethods(Reflection.staticBindingFlags) |> List.ofSeq
//        let cfgs = methods |> List.map CFG.build
//        let rec bypassCfg (cfg : cfg) (pobs : pob list, used : codeLocation list) (v : offset)  =
//            let m = cfg.methodBase
//            let loc = {offset = v; method = m}
//            if List.contains loc used then (pobs, used)
//            else
//                let used = loc :: used
//                let opCode = parseInstruction m v
//                let pobs =
//                    if opCode <> OpCodes.Throw then pobs
//                    else {loc = Instruction(v, m); lvl = infty; fml = Terms.True} :: pobs
//                Seq.fold (bypassCfg cfg) (pobs, used) (cfg.graph.[v])
//        //TODO: what about EHC?
//        let pobs, _ = Seq.fold (fun acc cfg -> bypassCfg cfg acc 0) ([], []) cfgs
//        pobs

    member x.ClearStructures () =
        mainPobs.Clear()
        qFront.Clear()
        qBack.Clear()
        transition.Clear()
        witnesses.Clear()
        blockedLocs.Clear()
        possiblePobsLocs.Clear()
        sPobs.Clear()
        mainPobs.Clear()
        currentPobs.Clear()
        answeredPobs.Clear()
        parents.Clear()
    override x.Invoke _ _ _ = __notImplemented__()
    override x.AnswerPobs entryMethod codeLocations k =
        x.ClearStructures()
        let mainPobs = Seq.map (fun (loc : codeLocation) ->
            {loc = Instruction(loc.offset, loc.method); lvl = infty; fml = Terms.True}) codeLocations
        Seq.iter (fun p -> answeredPobs.Add(p, Unknown)) mainPobs
        let EP = Instruction(0, entryMethod)
        curLvl <- 10
        x.PropDirSymExec entryMethod EP mainPobs
        let showResultFor (mp : pob) =
            match answeredPobs.[mp] with
            | Unreachable -> Logger.info "NO: MainPob = %O" mp
            | Witnessed s -> Logger.info "YES: MainPob = %O, witness = %O" mp s
            | Unknown -> Logger.info "Unknown: MainPoob = %O" mp
        Seq.iter showResultFor mainPobs
        let addLocationStatus (acc : Dictionary<codeLocation, string>) (loc : codeLocation) =
            let pob = {loc = Instruction(loc.offset, loc.method); lvl = infty; fml = Terms.True}
            let result =
                match answeredPobs.[pob] with
                | Witnessed _ -> "Witnessed"
                | status -> status.ToString()
            acc.Add(loc, result)
            acc
        let result = codeLocations |> Seq.fold addLocationStatus (Dictionary<codeLocation, string>())
        k result

type TargetedSearcher(codeLocations : codeLocation list) =
    inherit ForwardSearcher() with

    let intraReachability = Dictionary<codeLocation, codeLocation list>()
    let interReachability = Dictionary<codeLocation, MethodBase list>()

    let buildReachability (usedMethods : MethodBase list) methodsQueue (currentMethod : MethodBase) =
        let cfg = CFG.build currentMethod

        let appendComponent acc newOffset =
            cfg.sccOut.[newOffset] :: acc

        let addReachabilityInfo s t =
            if intraReachability.ContainsKey s |> not then intraReachability.Add(s, [t])
            else
                let old = intraReachability.[s]
                intraReachability.[s] <- t :: old

        let rec dfsSCC (cfg : cfg) (usedSCC : int list) (v : offset) : int list =
            let currentSCC = cfg.sccOut.[v]
            if List.contains currentSCC usedSCC then usedSCC
            else
                let usedSCC = currentSCC :: usedSCC
                let currentSCCOffsets = Seq.filter (fun offset -> currentSCC = cfg.sccOut.[offset]) cfg.sortedOffsets
                let newUsed = Seq.fold (fun acc u1 -> Seq.fold (fun acc u2 -> dfsSCC cfg acc u2) acc cfg.graph.[u1]) usedSCC currentSCCOffsets
                let reachableOffsets = HashSet<codeLocation>()
                Seq.iter (fun u -> reachableOffsets.Add({offset = u; method = currentMethod}) |> ignore) currentSCCOffsets
                let appendRest u1 =
                    Seq.iter (fun u2 -> List.iter (fun loc -> reachableOffsets.Add(loc) |> ignore) intraReachability.[{offset = u2; method = currentMethod}]) cfg.graph.[u1]
                Seq.iter appendRest currentSCCOffsets
                Seq.iter (fun u -> intraReachability.[{offset = u; method = currentMethod}] <- List.ofSeq reachableOffsets) currentSCCOffsets
                newUsed
//                List.iter (fun offset -> List.iter addReachabilityInfo )

        __notImplemented__()

    override x.PickNext fq =
        let canBePropagated (s : cilState) =
            not (isIIEState s || isUnhandledError s) && isExecutable s && not (x.Used s)
        let states = fq |> List.filter canBePropagated
        match states with
        | _ :: _ -> List.last states |> Some
        | [] -> None

