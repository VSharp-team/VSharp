namespace VSharp.Interpreter.IL

open System.Reflection
open VSharp
open VSharp.Concolic

module Coverage =

    let modules = System.Collections.Generic.List<Module>()

    // Represents code path in reverse order: adding new location into such path is constant-time by using List.cons
    type path = coverageLocation list

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

    let dump (path : path) =
        path |> List.rev |> List.map toString |> join " => "
