namespace VSharp

type Persistent<'T>(defaultValue, copy) =
    let cache = new System.Collections.Generic.Stack<'T ref>()
    member x.Value with get() = x.Read()
    member x.Mutate(v) = cache.Peek() := v
    member x.Read() = !cache.Peek()
    member x.Reset() = cache.Push(ref (defaultValue()))
    member x.Save() = cache.Push(ref (copy (x.Read())))
    member x.Restore() = cache.Pop() |> ignore
