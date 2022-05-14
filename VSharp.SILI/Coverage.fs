namespace VSharp.Interpreter.IL

open System.Reflection

module Coverage =

    let modules = System.Collections.Generic.List<Module>()


    // Lightweight types for representing coverage information
    [<Struct>]
    type location = {moduleToken : int; methodToken : int; offset : int; threadId : int}

    // Represents code path in reverse order: adding new location into such path is constant-time by using List.cons
    type path = location list

    let resolveModule moduleToken = modules.[moduleToken]
    let resolveMethod moduleToken methodToken =
        (resolveModule moduleToken).ResolveMethod methodToken

    let moduleToken (m : Module) =
    // TODO: this is slow!
        match Seq.tryFindIndex ((=)m) modules with
        | Some idx -> idx
        | None ->
            modules.Add m
            modules.Count - 1
