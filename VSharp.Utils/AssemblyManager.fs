namespace VSharp

open System.Collections.Generic
open System.Reflection

module AssemblyManager =
    let assemblySearchPaths = List<string>()
    let knownAssemblies = HashSet<AssemblyName>()
    let resolvedAssemblies = Dictionary<AssemblyName, Assembly>()
    let unresolvedAssemblies = HashSet<AssemblyName>()

    let () =
        System.AppDomain.CurrentDomain.add_AssemblyResolve <| System.ResolveEventHandler(fun sender args ->
            assemblySearchPaths |> Seq.tryPick (fun path ->
                let assemblyPath = System.IO.Path.Combine(path, AssemblyName(args.Name).Name + ".dll")
                if System.IO.File.Exists assemblyPath then
                    Assembly.LoadFrom assemblyPath |> Some
                else None)
            |> Option.defaultValue null)

    let rec private tryResolve depth (name : AssemblyName) =
        if knownAssemblies.Add name then
            try
                let assembly = Assembly.Load(name)
                resolvedAssemblies.Add(name, assembly)
                resolveDependencies assembly (depth - 1)
            with _ ->
                let added = unresolvedAssemblies.Add name
                assert added

    and private resolveDependencies (assembly : Assembly) depth =
        if depth > 0 then
            let dependencies = assembly.GetReferencedAssemblies()
            dependencies |> Array.iter (tryResolve depth)

    let load dependenciesDepth (assembly : Assembly) =
        let name = assembly.GetName()
        if knownAssemblies.Add name then
            resolvedAssemblies.Add(name, assembly)
            resolveDependencies assembly dependenciesDepth

    let reset() =
        knownAssemblies.Clear()
        resolvedAssemblies.Clear()
        unresolvedAssemblies.Clear()

    let assemblies () =
        resolvedAssemblies.Values :> Assembly seq
