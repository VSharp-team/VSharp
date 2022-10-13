namespace VSharp

open System
open System.IO
open System.Reflection
open System.Runtime.Loader
open Microsoft.Extensions.DependencyModel
open Microsoft.Extensions.DependencyModel.Resolution
open VSharp.CSharpUtils.AssemblyResolving

[<AllowNullLiteral>]
type internal AssemblyResolveContext(assembly : Assembly) as this =
    let assemblyDir = Path.GetDirectoryName assembly.Location
    let depsContext = DependencyContext.Load assembly
    let assemblyContext = AssemblyLoadContext.GetLoadContext assembly
    let resolvingHandler = Func<_,_,_> this.OnResolving
    let resolvedAssemblies = ResizeArray<Assembly>(seq {assembly})

    let assemblyResolver = CompositeAssemblyResolver(
        CurrentDirectoryAssemblyResolver assemblyDir, // Try to get a dll from the directory of the base assembly
        MicrosoftDependencyModelAssemblyResolver( // Try Microsoft API ways (they don't work for base assemblies prior .NET Core, depsContext is just null)
            depsContext,
            [|
                AppBaseCompilationAssemblyResolver assemblyDir :> ICompilationAssemblyResolver;
                ReferenceAssemblyPathResolver() :> ICompilationAssemblyResolver;
                PackageCompilationAssemblyResolver() :> ICompilationAssemblyResolver |]
        ),
        NuGetPackageNameMatchAssemblyResolver(), // Try to get a dll from the NuGet package directory with name matching assembly name
        NuGetGraphAssemblyResolver assembly // If the base assembly is in NuGet package directory, get it's NuGet package dependencies and search transitively in their dirs
    )

    let () =
        assemblyContext.add_Resolving resolvingHandler

    new(assemblyPath : string) =
        new AssemblyResolveContext(Assembly.LoadFile(assemblyPath))

    member private x.OnResolving (ctx : AssemblyLoadContext) (assemblyName : AssemblyName) : Assembly =
        let resolved =
            try
                assemblyResolver.Resolve assemblyName |> ctx.LoadFromAssemblyPath
            with ex -> null

        if resolved <> null then
            resolvedAssemblies.Add resolved
        else
            Logger.error $"[AssemblyManager] Cannot resolve: {assemblyName.FullName}"
        resolved

    member x.ResolvedAssemblies with get() = ResizeArray(resolvedAssemblies)
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
        if currentResolver <> null && currentResolver.Assembly <> assembly then
            (currentResolver :> IDisposable).Dispose()
        currentResolver <- new AssemblyResolveContext(assembly)

    let Assemblies() =
        if currentResolver <> null then currentResolver.ResolvedAssemblies else null
