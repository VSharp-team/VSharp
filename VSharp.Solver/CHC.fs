namespace VSharp

open VSharp.Core

type hoRelation = string
type relation = { id : string; signature : termType list }

type higherOrderArg = hoRelation list

type hoRelationalApplication =
    { symbol : hoRelation; hoRel : higherOrderArg option; args : term list } with
    override x.ToString() =
        let hoRel = Option.map (List.map toString >> join " ⚪ ") x.hoRel
        let args = x.args |> List.map toString |> join ", "
        match hoRel with
        | Some hoRel -> sprintf "%O(%s, %s)" x.symbol hoRel args
        | None -> sprintf "%O(%s)" x.symbol args

type relationalApplication =
    { symbol : relation; args : term list } with
    override x.ToString() =
        let args = x.args |> List.map toString |> join ", "
        sprintf "%s(%s)" x.symbol.id args

type 'a CHC =
    { constraints : term list; body : 'a list; head : 'a option } with
    override x.ToString() =
        let head =
            match x.head with
            | Some app -> toString app
            | None -> "FALSE"
        let constraints = List.map toString x.constraints
        let apps = List.map toString x.body
        let body = List.append constraints apps
        sprintf "%s :- %s" head (join ", " body)

type 'a CHCSystem = 'a list

type HOCHC = hoRelationalApplication CHC
type CHC = relationalApplication CHC
type HOCHCSystem = HOCHC CHCSystem
type CHCSystem = CHC CHCSystem

module CHCs =
    let private rel2FO rel args =
        { id = rel; signature = List.map TypeOf args }

    let private app2FO (hoapp : hoRelationalApplication) : relationalApplication =
        assert(hoapp.hoRel.IsNone)
        { symbol = rel2FO hoapp.symbol hoapp.args; args = hoapp.args }

    let private chc2FO (hochc : HOCHC) : CHC =
        { constraints = hochc.constraints; body = List.map app2FO hochc.body; head = Option.map app2FO hochc.head }

    let toFirstOrder (hoSys : HOCHCSystem) : CHCSystem =
        List.map chc2FO hoSys
