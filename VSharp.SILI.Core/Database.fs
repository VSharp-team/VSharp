namespace VSharp.Core

open System
open System.Collections.Generic
open System.Reflection
open VSharp
open VSharp.Logger

type codeLocationSummary = { result : term; state : state }
type codeLocationSummaries = codeLocationSummary list

type ip =
    | Instruction of offset
    | Exit
    | FindingHandler of offset // offset -- source of exception
    with
    member x.CanBeExpanded () =
        match x with
        | Instruction _ -> true
        | _ -> false
    member x.Offset () =
        match x with
        | Instruction i -> i
        | _              -> internalfail "Could not get vertex from destination"

type level = uint32

module Level =
    let zero : level = UInt32.MinValue
    let inf : level = UInt32.MaxValue

    let isZero (l : level) = (l = zero)
    let isInf (l : level) = (l = inf)

    let next (l : level) : level =
        if isInf l then l else l + 1u
    let prev (l : level) : level =
        if isZero l then internalfail "Taking previous of zero level!"
        if isInf l then l else l - 1u

    let toUInt (l : level) : uint32 =
        l
    let toInt (l : level) : int =
        int(l)
    let toString (lvl : level) =
        if isInf lvl then "inf" else lvl.ToString()

type formula = term

type lemma =
    { lvl : level; lemFml : formula } with
    override x.ToString() =
        sprintf "{lemma [lvl %s]: %O}" (Level.toString x.lvl) x.lemFml

type path =
    { lvl : level; state : state } with
    override x.ToString() =
        sprintf "{path [lvl %s]: %O}" (Level.toString x.lvl) x.state.pc

type query =
    { lvl : level; queryFml : formula } with
    override x.ToString() =
        sprintf "{query [lvl %s]: %O}" (Level.toString x.lvl) x.queryFml

type databaseId =
    { m : MethodBase; ip : ip } with
    override x.ToString() =
        sprintf "%O.%O[ip=%O]" x.m.DeclaringType.FullName x.m.Name x.ip

module internal Database =
    let private lemmas = new Dictionary<databaseId, HashSet<lemma>>()
    let private paths = new Dictionary<databaseId, HashSet<path>>()
    let private queries = new Dictionary<databaseId, HashSet<query>>()

    let idOfVertex (m : MethodBase) (ip : ip) : databaseId = { m=m; ip=ip }

    let addLemma (id : databaseId) (lemma : lemma) =
        let lemmas = Dict.tryGetValue2 lemmas id (fun () -> new HashSet<_>())
        if not <| lemmas.Add lemma then
            internalfailf "Vertex %O: Added twice %O" id lemma

    let addPath (id : databaseId) (path : path) =
        let paths = Dict.tryGetValue2 paths id (fun () -> new HashSet<_>())
        if not <| paths.Add path then
            internalfail "Vertex %O: Added twice %O" id path

    let addQuery (id : databaseId) (query : query) =
        let queries = Dict.tryGetValue2 queries id (fun () -> new HashSet<_>())
        if not <| queries.Add query then
            internalfailf "Vertex %O: Added twice %O" id query

    let removeQuery (id : databaseId) (query : query) =
        let noQueryError() =
            internalfail "Removing unknown query of vertex %O: %O" id query
        let queries = Dict.tryGetValue2 queries id noQueryError
        if not <| queries.Remove query then
            noQueryError()

type Lemmas(m : MethodBase, ip : ip) =
    let id = Database.idOfVertex m ip
    let parsed = new Dictionary<level, HashSet<lemma>>()
    member x.Add (lemma : lemma) =
        Database.addLemma id lemma
        let lemmas = Dict.tryGetValue2 parsed lemma.lvl (fun () -> new HashSet<_>())
        lemmas.Add lemma |> ignore

type Paths(m : MethodBase, ip : ip) =
    let id = Database.idOfVertex m ip
    let parsed = new Dictionary<level, HashSet<path>>()
    let used = HashSet<path>() // TODO: ``used'' set should be moved to Analyzer
    member x.Add (path : path) =
        if used.Contains path then used.Remove path |> ignore
        Database.addPath id path
        let paths = Dict.getValueOrUpdate parsed path.lvl (fun () -> new HashSet<_>())
        paths.Add path |> ignore
    member x.OfLevel lvl =
        let paths  = Dict.tryGetValue2 parsed lvl (fun () -> new HashSet<_>()) |> List.ofSeq
        let paths = List.filter (used.Contains >> not) paths
        List.iter (fun (path : path) -> Prelude.releaseAssert(used.Add(path))) paths
        paths


type Queries(m : MethodBase, ip : ip) =
    let id = Database.idOfVertex m ip
    let parsed = new Dictionary<level, HashSet<query>>()
    member x.Add (query : query) =
        Database.addQuery id query
        let queries = Dict.tryGetValue2 parsed query.lvl (fun () -> new HashSet<_>())
        queries.Add query |> ignore

    member x.Close (query : query) =
        Database.addQuery id query
        assert parsed.[query.lvl].Remove query
