namespace VSharp

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Runtime.Loader
open Microsoft.Extensions.DependencyModel
open Microsoft.Extensions.DependencyModel.Resolution

type internal AssemblyResolveContext(assemblyPath : string) as this =
    let assembly = Assembly.LoadFile(assemblyPath)
    let depsContext = DependencyContext.Load assembly
    let resolver : ICompilationAssemblyResolver = CompositeCompilationAssemblyResolver [|
                AppBaseCompilationAssemblyResolver(Path.GetDirectoryName(assemblyPath)) :> ICompilationAssemblyResolver;
                ReferenceAssemblyPathResolver() :> ICompilationAssemblyResolver;
                PackageCompilationAssemblyResolver() :> ICompilationAssemblyResolver |] :> ICompilationAssemblyResolver
    let assemblyContext = AssemblyLoadContext.GetLoadContext assembly
    let resolvingHandler = Func<_,_,_> this.OnResolving

    let () =
        assemblyContext.add_Resolving resolvingHandler

    member private x.OnResolving (_ : AssemblyLoadContext) (assemblyName : AssemblyName) : Assembly =
        let compLib = x.TryGetFromCompilationLibs(assemblyName)
        let compLib =
            match compLib with
            | None -> x.TryGetFromRuntimeLibs(assemblyName)
            | _ -> compLib

        match compLib with
        | Some compLib ->
            x.LoadLibrary compLib
        | None ->
            x.LoadFromSharedLibrary assemblyName

    member private x.LoadFromSharedLibrary(assemblyName : AssemblyName) =
        let dllPath = Path.Combine(Path.GetDirectoryName(assemblyPath), sprintf "%s.dll" (assemblyName.Name.Split(',')).[0]);
        try
            assemblyContext.LoadFromAssemblyPath dllPath
        with ex ->
            Logger.error "%O" ex
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
            Logger.error "%O" ex
            null

    member x.Assembly with get() = assembly

    interface IDisposable with
        override x.Dispose() =
            assemblyContext.remove_Resolving resolvingHandler

module AssemblyManager =
    let assemblies = List<Assembly>()
    let Resolve assemblyPath =
        let resolver = new AssemblyResolveContext(assemblyPath)
        assemblies.Clear()
        assemblies.Add resolver.Assembly
        // TODO: add dependencies?
        resolver.Assembly

    let Assemblies() = assemblies
