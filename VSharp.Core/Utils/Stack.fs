namespace VSharp.Core.Utils

module public Stack =
    type 'a stack =
        | EmptyStack
        | StackNode of 'a * 'a stack
        override this.ToString() =
            let rec toString = function
                | EmptyStack -> ""
                | StackNode(hd, EmptyStack) -> hd.ToString()
                | StackNode(hd, tl) -> hd.ToString() + " -> " + toString tl
            "{ " + toString this + " }"

    let peak = function
        | EmptyStack -> failwith "Empty stack"
        | StackNode(hd, tl) -> hd

    let pop = function
        | EmptyStack -> failwith "Empty stack"
        | StackNode(hd, tl) -> tl

    let push stack element = StackNode(element, stack)

    let empty = EmptyStack

    let singleton x = push empty x

    let isEmpty = function
        | EmptyStack -> true
        | _ -> false
