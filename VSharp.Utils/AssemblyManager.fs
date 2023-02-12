namespace VSharp

open System
open System.Collections.Generic
open System.Reflection
open VSharp.CSharpUtils

module AssemblyManager =
    let mutable private alc = VSharpAssemblyLoadContext("vsharp_alc_0")
    
    let mutable private alcVersion = 0

    let GetAssemblies() =
        alc.Assemblies
        
    let SetDependenciesDirs (dirs : IEnumerable<string>) =
        alc.DependenciesDirs = dirs
    
    let LoadFromAssemblyPath (assemblyPath : string) =
        alc.LoadFromAssemblyPath assemblyPath

    let LoadCopy (assembly : Assembly) =
        alc.LoadFromAssemblyPath(assembly.Location)

    let LoadFromAssemblyName (assemblyName : string) =
        alc.LoadFromAssemblyName(AssemblyName(assemblyName))

    let NormalizeType (t : Type) =
        alc.NormalizeType(t)

    let NormalizeMethod (m : MethodInfo) =
        alc.NormalizeMethod(m)

    // Used in tests to reset the state. For example, in tests Veritas.
    // A more correct approach is to inject a VSharpAssemblyLoadContext instance
    // into all places of use via the constructor.
    // But there are no resources for this approach.
    let Reset() =
        alcVersion <- alcVersion + 1
        alc <- VSharpAssemblyLoadContext("vsharp_alc_" + alcVersion.ToString())
