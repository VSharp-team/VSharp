namespace VSharp

type 'a persistent(defaultValue, copy) =
    let cache = new System.Collections.Generic.Stack<'a ref>()
    member x.Value with get() = !cache.Peek()
    member x.Mutate(v) = cache.Peek() := v
    member x.Reset() = cache.Push(ref (defaultValue()))
    member x.Save() = cache.Push(ref (copy x.Value))
    member x.Restore() = cache.Pop() |> ignore
