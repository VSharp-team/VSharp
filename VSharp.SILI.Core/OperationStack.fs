namespace VSharp.Core

open VSharp

type operationStack = internal { contents : term list list }

module internal OperationStack =

    let __corruptedStack__() = raise (System.InvalidProgramException())

    let empty = { contents = List.empty }

    let push (x : term) opStack =
        match opStack.contents with
        | [] -> __corruptedStack__()
        | l :: ls -> { contents = (x :: l) :: ls }

    let pop opStack =
        match opStack.contents with
        | [] | [] :: _ -> __corruptedStack__()
        | (x :: xs) :: ls -> x, { contents = xs :: ls }

    let map f opStack = { contents = List.map (List.map f) opStack.contents }

    let makeSymbolicActiveFrame f opStack =
        match opStack.contents with
        | l :: ls -> {contents = (List.mapi f l) :: ls}
        | [] -> __corruptedStack__()
    let clearActiveFrame opStack =
        match opStack.contents with
        | _ :: ls -> {contents = [] :: ls}
        | _ -> __corruptedStack__()
    let newStackFrame opStack = {contents = [] :: opStack.contents}
    let popStackFrame opStack =
        // TODO: #mb use returnRegister
        match opStack.contents with
        | [] :: ls -> {contents = ls}
        | [_] :: [] -> opStack // res case
        | [res] :: l :: ls -> {contents = (res :: l) :: ls} // call case
        | _ -> __corruptedStack__()

    let filterActiveFrame f opStack =
        match opStack.contents with
        | l :: ls -> { contents = List.filter f l :: ls } // TODO: remove it when CFA module is gone
        | [] -> __corruptedStack__()

    let item i opStack =
        let rec item' i = function
            | l :: ls when List.length l <= i -> item' (i - List.length l) ls
            | l :: _ -> List.item i l
            | [] -> __corruptedStack__()
        item' i opStack.contents

    let length opStack = List.fold (fun acc l -> List.length l + acc) 0 opStack.contents

    let popMany n opStack =
        match opStack.contents with
        | l :: ls ->
            let args, rest = List.splitAt n l
            args, { contents = rest :: ls }
        | [] -> __corruptedStack__()

    let union opStack opStack' =
        { contents = List.append opStack.contents opStack'.contents }

    let toList opStack = List.concat opStack.contents

    let toString opStack = opStack.contents |> List.map toString |> join "\n"
