namespace VSharp

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Runtime.Loader
open Microsoft.Extensions.DependencyModel
open Microsoft.Extensions.DependencyModel.Resolution

type internal CurrentDirectoryAssemblyResolver(assemblyPath : string) =
    interface ICompilationAssemblyResolver with
        member x.TryResolveAssemblyPaths(library : CompilationLibrary, assemblies : List<string>) =
            let path = Path.Combine(assemblyPath, library.Name + ".dll")
            if File.Exists path then
                assemblies.Add path
                true
            else false

[<AllowNullLiteral>]
type internal AssemblyResolveContext(assembly : Assembly) as this =
    let assemblyDir = Path.GetDirectoryName assembly.Location
    let depsContext = DependencyContext.Load assembly
    let resolver : ICompilationAssemblyResolver = CompositeCompilationAssemblyResolver [|
                CurrentDirectoryAssemblyResolver assemblyDir;
                AppBaseCompilationAssemblyResolver assemblyDir :> ICompilationAssemblyResolver;
                ReferenceAssemblyPathResolver() :> ICompilationAssemblyResolver;
                PackageCompilationAssemblyResolver() :> ICompilationAssemblyResolver |] :> ICompilationAssemblyResolver
    let assemblyContext = AssemblyLoadContext.GetLoadContext assembly
    let resolvingHandler = Func<_,_,_> this.OnResolving
    let resolvedAssemblies = ResizeArray<Assembly>(seq {assembly})

    let () =
        assemblyContext.add_Resolving resolvingHandler

    new(assemblyPath : string) =
        new AssemblyResolveContext(Assembly.LoadFile(assemblyPath))

    member private x.OnResolving (_ : AssemblyLoadContext) (assemblyName : AssemblyName) : Assembly =
        let compLib = x.TryGetFromCompilationLibs(assemblyName)
        let compLib =
            match compLib with
            | None -> x.TryGetFromRuntimeLibs(assemblyName)
            | _ -> compLib

        let resolved =
            match compLib with
            | Some compLib ->
                x.LoadLibrary compLib
            | None ->
                x.LoadFromSharedLibrary assemblyName
        if resolved <> null then
            resolvedAssemblies.Add resolved
        resolved

    member private x.LoadFromSharedLibrary(assemblyName : AssemblyName) =
        let dllPath = Path.Combine(assemblyDir, $"%s{(assemblyName.Name.Split(',')).[0]}.dll");
        try
            assemblyContext.LoadFromAssemblyPath dllPath
        with ex ->
            Logger.error $"[AssemblyManager] Assembly resolution failed: {ex}"
            null

    member x.TryGetFromCompilationLibs(assemblyName : AssemblyName) : CompilationLibrary option =
        depsContext.CompileLibraries |> Seq.tryFind (fun e -> e.Name.Equals(assemblyName.Name, StringComparison.OrdinalIgnoreCase))

    member private x.TryGetFromRuntimeLibs(assemblyName : AssemblyName) : CompilationLibrary option =
        match depsContext.RuntimeLibraries |> Seq.tryFind (fun e -> e.Name.Equals(assemblyName.Name, StringComparison.OrdinalIgnoreCase)) with
        | Some runLib ->
            CompilationLibrary(
                runLib.Type,
                runLib.Name,
                runLib.Version,
                runLib.Hash,
                runLib.RuntimeAssemblyGroups |> Seq.collect (fun g -> g.AssetPaths),
                runLib.Dependencies,
                runLib.Serviceable) |> Some
        | None -> None

    member private x.LoadLibrary(compLib : CompilationLibrary) =
        try
            let assemblies = List<string>();
            if resolver.TryResolveAssemblyPaths(compLib, assemblies) then
                assemblyContext.LoadFromAssemblyPath(assemblies.[0])
            else null
        with ex ->
            Logger.error "[AssemblyManager] Assembly resolution failed: %O" ex
            null

    member x.ResolvedAssemblies with get() = resolvedAssemblies
    member x.Assembly with get() = assembly

    interface IDisposable with
        override x.Dispose() =
            assemblyContext.remove_Resolving resolvingHandler

module AssemblyManager =
    let mutable private currentResolver : AssemblyResolveContext = null

    let Resolve (assemblyPath : string) =
        if currentResolver <> null then
            (currentResolver :> IDisposable).Dispose()
        currentResolver <- new AssemblyResolveContext(assemblyPath)
        currentResolver.Assembly

    let Load (assembly : Assembly) =
        if currentResolver <> null then
            (currentResolver :> IDisposable).Dispose()
        currentResolver <- new AssemblyResolveContext(assembly)

    let Assemblies() =
        if currentResolver <> null then currentResolver.ResolvedAssemblies else null
