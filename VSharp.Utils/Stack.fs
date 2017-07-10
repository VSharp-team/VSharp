namespace VSharp

module public Stack =
    type 'a stack = 'a list

    let peak = function
        | [] -> failwith "Attempt to peak head of an empty stack"
        | hd::tl -> hd

    let pop = function
        | [] -> failwith "Attempt to pop an empty stack"
        | hd::tl -> tl

    let push stack element = element::stack

    let updateHead stack newHd = push (pop stack) newHd

    let updateMiddle stack idx newVal =
        let rec updateMiddleRec xs idx acc =
            match xs with
            | [] -> acc
            | x::xs' when idx = 0 -> updateMiddleRec xs' (idx - 1) (newVal::acc)
            | x::xs' -> updateMiddleRec xs' (idx - 1) (x::acc)
        updateMiddleRec stack ((List.length stack) - idx - 1) [] |> List.rev

    let empty = List.empty

    let singleton = List.singleton

    let isEmpty = List.isEmpty

    let middle idx stack = List.item ((List.length stack) - idx - 1) stack

    let size = List.length

    let tryFindBottom = List.tryFindBack
