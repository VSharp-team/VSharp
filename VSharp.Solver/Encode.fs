namespace VSharp

open FSharpx.Collections
open VSharp.Core
open System.Collections.Generic

type internal SourceSchema =
    { id : string; inputs : term list; outputs : term list }
type internal SourceSchemaApp =
    { schema : SourceSchema; args : PersistentHashMap<term, term>; resultConst : term; resultNum : int }

type internal EmptySource() =
    interface INonComposableSymbolicConstantSource with
        override x.SubTerms = seq[]

module internal Encode =
    let schemas = new Dictionary<string, SourceSchema>()
    let rules = new Dictionary<string, HOCHC list>()

    let private compose state constant =
        match constant.term with
        | Constant(_, source, _) ->
            match source with
            | :? INonComposableSymbolicConstantSource -> constant
            | :? IStatedSymbolicConstantSource as source -> source.Compose compositionContext.Empty state
            | _ -> __notImplemented__()
        | _ -> __unreachable__()

    let private isInputConstant = function
    | { term = Constant(_, source, _) } ->
        match source with
        | LazyInstantiation({term = Ref(TopLevelStack _, _)}, None, _) -> true
        | _ -> false
    | _ -> false


    module private FunctionSummaries =
        let private keys = new Dictionary<IFunctionIdentifier * term option, string>()

        let clear () = keys.Clear()

        let private toKey funcId = function
            | None -> sprintf "%O#res" funcId
            | Some _ -> IdGenerator.startingWith (toString funcId)

        let private encodeBody funcId loc =
            let pair = (funcId, loc)
            if not <| keys.ContainsKey (funcId, loc) then
                let key = toKey funcId loc
                keys.Add(pair, key)
                let summary = Database.QuerySummary funcId
                match loc with
                | None ->
                    let results = [summary.result]
                    schemas.[key] <- { id = key; inputs = ConstantsOf results |> Seq.filter isInputConstant |> List.ofSeq; outputs = results }
                | Some _ ->
                    __notImplemented__()
            keys.[pair]

        let encode constant funcId state location =
            let schemaKey = encodeBody funcId location
            let schema = schemas.[schemaKey]
            assert(schema.outputs.Length = 1)
            let args = List.map (fun arg -> (arg, compose state arg)) schema.inputs
            // TODO: constraints must be generated for args too!
            { schema = schema; args = PersistentHashMap.ofSeq args; resultConst = constant; resultNum = 0 }

    let rec private toDnf = function
        | Disjunction ts ->
            List.collect toDnf ts
        | Conjunction ts ->
            let dnfs = List.map toDnf ts
            let shuffle xss yss =
                List.collect (fun xs -> List.map (List.append xs) yss) xss
            List.reduce shuffle dnfs
        | t -> [[t]]

    let private unguardTerm = function
        | { term = Union gvs } -> List.collect (fun (g, v) -> List.map (withSnd v) (toDnf g)) gvs
        | t -> [[], t]

    let private unguardTerms mapper terms =
        let unguarded = List.map unguardTerm terms
        unguarded |> List.cartesianMap (fun gvs ->
            let gss, vs = List.unzip gvs
            List.concat gss, mapper vs) |> List.ofSeq

    let private unguardRelApp (app : hoRelationalApplication) =
        unguardTerms (fun args -> { app with args = args }) app.args

    let private unguardRelApps mapper (apps : hoRelationalApplication list) =
        match apps with
        | [] -> [mapper ([], [])]
        | _ ->
            let unguardedApps = List.map unguardRelApp apps
            unguardedApps |> List.cartesianMap (fun gapps ->
                List.unzip gapps |> mapper) |> List.ofSeq

    let private constantToSchemaApp constant =
        match constant.term with
        | Constant(_, source, _) ->
            match source with
            | RecursionOutcome(id, state, location, _) ->
                FunctionSummaries.encode constant id state location |> Some
            | LazyInstantiation(_, None, _) -> None
            | LazyInstantiation(_, Some _, _) -> __notImplemented__()
            | _ -> None // __notImplemented__()
        | _ -> __unreachable__()

    let rec private schemasOfTermsRec (acc : ISet<term>) = function
        | [] -> []
        | terms ->
            let folder constant acc = optCons acc (constantToSchemaApp constant)
            let constants = ConstantsOf terms
            constants.ExceptWith acc
            acc.UnionWith constants
            let schemas = Seq.foldBack folder constants []
            let args = List.collect (fun schema -> schema.args |> PersistentHashMap.toSeq |> Seq.map snd |> List.ofSeq) schemas
            let childSchemas = schemasOfTermsRec acc args
            List.append childSchemas schemas

    let private schemasOfTerms terms =
         schemasOfTermsRec (new HashSet<term>()) terms

    let private schemaToRel (schema : SourceSchema) : hoRelation =
        schema.id

    let private schemaToHeadApp (schema : SourceSchema) : hoRelationalApplication =
        let rel = schemaToRel schema
        { symbol = rel; hoRel = None; args = List.append schema.inputs schema.outputs }

    let private schemaAppsToRelApps (apps : SourceSchemaApp list) : hoRelationalApplication list =
        let groups = apps |> List.groupBy (fun app -> app.schema, app.args)
        let groupToRelApp idx ((schema, args), apps) =
            let rel = schemaToRel schema
            let args = List.map (fun input -> PersistentHashMap.find input args) schema.inputs
            let addResutingConst map (app : SourceSchemaApp) =
                let i = app.resultNum
                assert(not <| Map.containsKey i map)
                Map.add i app.resultConst map
            let resultingConstMap = List.fold addResutingConst Map.empty apps
            let resultingConsts = schema.outputs |> List.mapi (fun i t ->
                if Map.containsKey i resultingConstMap then resultingConstMap.[i]
                else Constant (sprintf "res#%i" (i + idx)) (EmptySource()) (TypeOf t))
            { symbol = rel; hoRel = None; args = List.append args resultingConsts }, idx + schema.outputs.Length
        List.mapFold groupToRelApp 0 groups |> fst

    let private encodeSchema key =
        let schema = schemas.[key]
        let head = schemaToHeadApp schema
        let unguardedHeads = unguardRelApp head
        unguardedHeads |> List.collect (fun (gs, head) ->
            head.args
            |> schemasOfTerms
            |> schemaAppsToRelApps
            |> unguardRelApps (fun (gss, body) ->
                { constraints = List.concat (gs::gss); body = body; head = Some head }))

    let encodeQuery (terms : term list) : HOCHCSystem =
        schemas.Clear()
        rules.Clear()
        FunctionSummaries.clear()
        let queries =
            terms
            |> schemasOfTerms
            |> schemaAppsToRelApps
            |> unguardRelApps (fun (gss, body) ->
                { constraints = List.concat (terms::gss); body = body; head = None })
        while schemas.Keys |> Seq.fold (fun acc k -> if rules.ContainsKey k then acc else rules.Add(k, encodeSchema k); true) false do ()
        List.concat (queries::(List.ofSeq rules.Values))
