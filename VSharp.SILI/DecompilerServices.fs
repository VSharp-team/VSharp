namespace VSharp

open JetBrains.Metadata.Reader.API
open System.Collections.Generic

module internal DecompilerServices =
    let private assemblyLoader = new JetBrains.Metadata.Reader.API.MetadataLoader(JetBrains.Metadata.Access.MetadataProviderFactory.DefaultProvider)
    let private assemblies = new Dictionary<string, JetBrains.Metadata.Reader.API.IMetadataAssembly>()
    let private decompilers = new Dictionary<string, JetBrains.Decompiler.ClassDecompiler>()
    let private decompiledClasses = new Dictionary<string, JetBrains.Decompiler.Ast.IDecompiledClass>()

    let private loadAssembly (path : JetBrains.Util.FileSystemPath) =
        getDictValueOrUpdate assemblies (path.ToString()) (fun () -> assemblyLoader.LoadFrom(path, fun _ -> true))

    let public decompileClass assemblyPath qualifiedTypeName =
        getDictValueOrUpdate decompiledClasses qualifiedTypeName (fun () ->
            let metadataAssembly = loadAssembly assemblyPath in
            let parentPath = JetBrains.Util.FileSystemPath.Parse(typeof<System.String>.Assembly.Location).Parent in
            metadataAssembly.ReferencedAssembliesNames |> Seq.iter (fun name -> loadAssembly (JetBrains.Metadata.Utils.AssemblyNameMetadataExtensions.FindAssemblyFile(parentPath, name)) |> ignore)
            let metadataTypeInfo = metadataAssembly.GetTypeInfoFromQualifiedName(qualifiedTypeName, false) in
            let decompiler = getDictValueOrUpdate decompilers (assemblyPath.ToString()) (fun () ->
                let lifetime = JetBrains.DataFlow.Lifetimes.Define()
                let methodCollector = new JetBrains.Metadata.Utils.MethodCollectorStub()
                let options = new JetBrains.Decompiler.ClassDecompilerOptions(true)
                new JetBrains.Decompiler.ClassDecompiler(lifetime.Lifetime, metadataAssembly, options, methodCollector))
            decompiler.Decompile(metadataTypeInfo, JetBrains.Application.Progress.NullProgressIndicator.Instance))

    let public decompileMethod assemblyPath qualifiedTypeName (methodInfo : IMetadataMethod) =
        let decompiledClass = decompileClass assemblyPath qualifiedTypeName in
        Seq.tryPick (fun (m : JetBrains.Decompiler.Ast.IDecompiledMethod) -> if m.MetadataMethod = methodInfo then Some(m) else None) decompiledClass.Methods

    let public resolveType (typ : System.Type) =
        let assembly = loadAssembly (JetBrains.Util.FileSystemPath.Parse(typ.Assembly.Location)) in
        if assembly = null then null
        else assembly.GetTypeFromQualifiedName(typ.AssemblyQualifiedName, false)

    let public getPropertyOfNode (node : JetBrains.Decompiler.Ast.INode) key defaultValue =
        // node.Data.TryGetValue is poorly implemented (it checks reference equality of keys), so searching manually...
        let option = node.Data |> Seq.tryPick (fun keyValue -> if (keyValue.Key.ToString() = key) then Some(keyValue.Value) else None) in
        match option with
        | Some t -> t
        | None -> defaultValue

    let public locationOfType qualifiedTypeName =
        let typ = System.Type.GetType(qualifiedTypeName) in
        if typ = null then __notImplemented__()
        JetBrains.Util.FileSystemPath.Parse(typ.Assembly.Location)

    let public getDefaultFieldValuesOf isStatic qualifiedTypeName =
        let assemblyPath = locationOfType qualifiedTypeName in
        let decompiledClass = decompileClass assemblyPath qualifiedTypeName in
        let extractFieldInfo (f : JetBrains.Decompiler.Ast.IDecompiledField) =
            (f.MetadataField.Name, (f.MetadataField.Type, f.Initializer))
        let isFieldStatic required (f : JetBrains.Decompiler.Ast.IDecompiledField) =
            f.MetadataField.IsStatic = required
        in
        decompiledClass.Fields |> Seq.filter (isFieldStatic isStatic) |> Seq.map extractFieldInfo |> List.ofSeq

    let public getStaticConstructorOf qualifiedTypeName =
        let assemblyPath = locationOfType qualifiedTypeName in
        let decompiledClass = decompileClass assemblyPath qualifiedTypeName in
        decompiledClass.Methods |> Seq.tryFind (fun (m : JetBrains.Decompiler.Ast.IDecompiledMethod) -> m.MetadataMethod.IsStatic && m.MetadataMethod.Name = ".cctor")

    let public methodInfoToMetadataMethod assemblyPath qualifiedTypeName (methodInfo : System.Reflection.MethodInfo) =
        let assembly = loadAssembly assemblyPath in
        let typ = assembly.GetTypeInfoFromQualifiedName(qualifiedTypeName, false) in
        typ.GetMethods() |> Array.tryPick (fun m -> if m.Token.Value = uint32 methodInfo.MetadataToken then Some(m) else None)
