namespace VSharp

open System.Collections.Generic

module internal DecompilerServices =
    let private assemblyLoader = new JetBrains.Metadata.Reader.API.MetadataLoader(JetBrains.Metadata.Access.MetadataProviderFactory.DefaultProvider)
    let private assemblies = new Dictionary<string, JetBrains.Metadata.Reader.API.IMetadataAssembly>()
    let private decompilers = new Dictionary<string, JetBrains.Decompiler.ClassDecompiler>()

    let private loadAssembly (path : JetBrains.Util.FileSystemPath) =
        getDictValueOrUpdate assemblies (path.ToString()) (fun () -> assemblyLoader.LoadFrom(path, fun _ -> true))

    let public decompile qualifiedTypeName methodName assemblyPath =
        let metadataAssembly = getDictValueOrUpdate assemblies (assemblyPath.ToString()) (fun () -> assemblyLoader.LoadFrom(assemblyPath, fun _ -> true)) in
        let parentPath = JetBrains.Util.FileSystemPath.Parse(typeof<System.String>.Assembly.Location).Parent
        metadataAssembly.ReferencedAssembliesNames |> Seq.iter (fun name -> loadAssembly (JetBrains.Metadata.Utils.AssemblyNameMetadataExtensions.FindAssemblyFile(parentPath, name)) |> ignore)
        let metadataTypeInfo = metadataAssembly.GetTypeInfoFromQualifiedName(qualifiedTypeName, false)
        let metadataMethod = metadataTypeInfo.GetMethods() |> Seq.tryPick (fun m -> if m.Name.Equals(methodName) then Some(m) else None) in
        match metadataMethod with
        | None -> None
        | Some metadataMethod ->
            let decompiler = getDictValueOrUpdate decompilers (assemblyPath.ToString()) (fun () ->
                let lifetime = JetBrains.DataFlow.Lifetimes.Define()
                let methodCollector = new JetBrains.Metadata.Utils.MethodCollectorStub()
                let options = new JetBrains.Decompiler.ClassDecompilerOptions(true)
                new JetBrains.Decompiler.ClassDecompiler(lifetime.Lifetime, metadataAssembly, options, methodCollector))
            Some (decompiler.Decompile(metadataTypeInfo, metadataMethod))

    let public resolveType (typ : System.Type) =
        let assembly = loadAssembly (JetBrains.Util.FileSystemPath.Parse(typ.Assembly.Location)) in
        if assembly = null then null
        else assembly.GetTypeFromQualifiedName(typ.AssemblyQualifiedName, false)
