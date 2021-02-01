namespace VSharp.Core

open VSharp

type operationStack = private { contents : term list }

module internal OperationStack =

    let __corruptedStack__() = raise (System.InvalidProgramException())

    let empty = { contents = List.empty }

    let push x opStack =
        { contents = x :: opStack.contents }

    let pop opStack =
        match opStack.contents with
        | x :: xs -> x, {contents = xs}
        | _ -> __corruptedStack__()

    let map f opStack = { contents = List.map f opStack.contents }

    let mapi f opStack = { contents = List.mapi f opStack.contents }

    let filter f opStack = { contents = List.filter f opStack.contents } // TODO: remove it when CFA module is gone

    let item i opStack = List.item i opStack.contents

    let length opStack = List.length opStack.contents

    let popMany n opStack =
        let args, contents = List.splitAt n opStack.contents
        args, { contents = contents }

    let union opStack opStack' =
        { contents = List.append opStack.contents opStack'.contents }

    let toList opStack = opStack.contents

    let toString opStack = opStack.contents |> List.map toString |> join "\n"
