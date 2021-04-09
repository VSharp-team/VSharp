namespace VSharp.Core

open System.Collections.Generic
open System.Reflection
open VSharp

type label =
    | Instruction of offset
    | Exit
    | Leave of offset
    | FindingHandler of offset // offset -- source of exception

[<CustomComparison; CustomEquality>]
type ip = { label : label; method : MethodBase}
    with
    member x.CanBeExpanded () =
        match x.label with
        | Instruction _ -> true
        | _ -> false
    member x.Offset () =
        match x.label with
        | Instruction i -> i
        | _              -> internalfail "Could not get vertex from destination"
    override x.Equals y =
        match y with
        | :? ip as y -> x.label = y.label && x.method = y.method
        | _ -> false
    override x.GetHashCode() = (x.label, x.method).GetHashCode()
    interface System.IComparable with
        override x.CompareTo y =
            match y with
            | :? ip as y when x.method.Equals(y.method) -> compare x.label y.label
            | :? ip as y -> x.method.MetadataToken.CompareTo(y.method.MetadataToken)
            | _ -> -1
    override x.ToString() = sprintf "{label = %O; method = %s}" x.label (Reflection.getFullMethodName x.method)

type level = pdict<ip, uint>
type ipStack = ip list

module ipOperations =
    let exit m = {label = Exit; method = m}
    let instruction m i = {label = Instruction i; method = m}
    let findingHandler m i = {label = FindingHandler i; method = m}
    let withExit ip = {ip with label = Exit}
    let withOffset offset ip = {ip with label = Instruction offset}
    let labelOf (ip : ip) = ip.label
    let methodOf (ip : ip) = ip.method

module Level =
    // TODO: implement level
    let zero : level = PersistentDict.empty
    let inf : level = PersistentDict.empty

    let isZero (l : level) = (l = zero)
    let isInf (l : level) = (l = inf)

    let next (l : level) : level = __notImplemented__()

    let prev (l : level) : level = __notImplemented__()
    let toUInt (l : level) : uint32 = __notImplemented__()

    let toInt (l : level) : int = 0

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
        sprintf "%O.%O[ip=%O]" (Reflection.getFullTypeName x.m.DeclaringType) x.m.Name x.ip

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
        let paths = Dict.tryGetValue2 parsed lvl (fun () -> new HashSet<_>()) |> List.ofSeq
        let paths = List.filter (used.Contains >> not) paths
        List.iter (fun (path : path) -> assert(used.Add(path))) paths
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
