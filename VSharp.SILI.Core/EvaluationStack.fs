namespace VSharp.Core

open VSharp

type evaluationStack = internal { contents : term list list }

module internal EvaluationStack =

    let __corruptedStack__() = raise (System.InvalidProgramException())

    let empty = { contents = List.empty }

    let push (x : term) evaluationStack =
        match evaluationStack.contents with
        | [] -> __corruptedStack__()
        | l :: ls -> { contents = (x :: l) :: ls }

    let pop evaluationStack =
        match evaluationStack.contents with
        | [] | [] :: _ -> __corruptedStack__()
        | (x :: xs) :: ls -> x, { contents = xs :: ls }

    let map f evaluationStack = { contents = List.map (List.map f) evaluationStack.contents }

    let makeSymbolicActiveFrame f evaluationStack =
        match evaluationStack.contents with
        | l :: ls -> {contents = (List.mapi f l) :: ls}
        | [] -> __corruptedStack__()
    let clearActiveFrame evaluationStack =
        match evaluationStack.contents with
        | _ :: ls -> {contents = [] :: ls}
        | _ -> __corruptedStack__()
    let newStackFrame evaluationStack = {contents = [] :: evaluationStack.contents}
    let popStackFrame evaluationStack =
        // TODO: #mb use returnRegister
        match evaluationStack.contents with
        | [] :: ls -> {contents = ls}
        | [_] :: [] -> evaluationStack // res case
        | [res] :: l :: ls -> {contents = (res :: l) :: ls} // call case
        | _ -> __corruptedStack__()

    let filterActiveFrame f evaluationStack =
        match evaluationStack.contents with
        | l :: ls -> { contents = List.filter f l :: ls } // TODO: remove it when CFA module is gone
        | [] -> __corruptedStack__()

    let item i evaluationStack =
        let rec item' i = function
            | l :: ls when List.length l <= i -> item' (i - List.length l) ls
            | l :: _ -> List.item i l
            | [] -> __corruptedStack__()
        item' i evaluationStack.contents

    let length evaluationStack = List.fold (fun acc l -> List.length l + acc) 0 evaluationStack.contents

    let popMany n evaluationStack =
        match evaluationStack.contents with
        | l :: ls ->
            let args, rest = List.splitAt n l
            args, { contents = rest :: ls }
        | [] -> __corruptedStack__()

    let union evaluationStack evaluationStack' =
        { contents = List.append evaluationStack.contents evaluationStack'.contents }

    let toList evaluationStack = List.concat evaluationStack.contents

    let toString evaluationStack = evaluationStack.contents |> List.map toString |> join "\n"
