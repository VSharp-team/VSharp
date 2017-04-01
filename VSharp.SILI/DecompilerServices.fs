namespace VSharp

open JetBrains.Metadata.Reader.API
open JetBrains.Decompiler.Ast
open System.Collections.Generic

module internal DecompilerServices =
    let private assemblyLoader = new JetBrains.Metadata.Reader.API.MetadataLoader(JetBrains.Metadata.Access.MetadataProviderFactory.DefaultProvider)
    let private assemblies = new Dictionary<string, JetBrains.Metadata.Reader.API.IMetadataAssembly>()
    let private decompilers = new Dictionary<string, JetBrains.Decompiler.ClassDecompiler>()
    let private decompiledClasses = new Dictionary<string, IDecompiledClass>()

    let private loadAssembly (path : JetBrains.Util.FileSystemPath) =
        getDictValueOrUpdate assemblies (path.ToString()) (fun () -> assemblyLoader.LoadFrom(path, fun _ -> true))

    let public getPropertyOfNode (node : INode) key defaultValue =
        // node.Data.TryGetValue is poorly implemented (it checks reference equality of keys), so searching manually...
        let option = node.Data |> Seq.tryPick (fun keyValue -> if (keyValue.Key.ToString() = key) then Some(keyValue.Value) else None) in
        match option with
        | Some t -> t
        | None -> defaultValue

    let public getTypeOfNode (node : INode) =
        getPropertyOfNode node "Type" null :?> JetBrains.Metadata.Reader.API.IMetadataType

    let public setTypeOfNode (node : INode) (t : JetBrains.Metadata.Reader.API.IMetadataType) =
        node.Data.SetValue(JetBrains.Decompiler.Utils.DataKey<JetBrains.Metadata.Reader.API.IMetadataType>("Type"), t)

    let public copyTypeTo (src : INode) (dst : INode) =
        setTypeOfNode dst (getTypeOfNode src)

    let private createThisOf (typ : IMetadataTypeInfo) =
        let this = AstFactory.CreateThisReference(null) in
        if typ.IsClass || typ.IsInterface 
        then AstFactory.CreateDeref(this, null, true) :> IExpression
        else this :> IExpression

    let private embodyGetter (property : IDecompiledProperty) =
        if property.Getter <> null && property.IsAuto && property.Getter.Body = null then
            let fieldSpecification = new FieldSpecification(property.BackingField) in
            // TODO: deref?
            let this = if property.Getter.MetadataMethod.IsStatic then null else createThisOf property.OwnerClass.TypeInfo in
            let fieldReference = AstFactory.CreateFieldAccess(this, fieldSpecification, null) in
            let returnStatement = AstFactory.CreateReturn(fieldReference :> IExpression, null) in
            let blockStatement = AstFactory.CreateBlockStatement([returnStatement]) in
            property.Getter.Body <- blockStatement
        property.Getter

    let private embodySetter (property : IDecompiledProperty) =
        if property.Setter <> null && property.IsAuto && property.Setter.Body = null then
            let fieldSpecification = new FieldSpecification(property.BackingField) in
            // TODO: deref?
            let this = if property.Getter.MetadataMethod.IsStatic then null else AstFactory.CreateThisReference(null) in
            let fieldReference = AstFactory.CreateFieldAccess(this, fieldSpecification, null) in
            let valueParameter = property.Setter.Signature.Parameters.Item(0) in
            let valueParameterReference = AstFactory.CreateParameterReference(valueParameter, null) in
            let assignment = AstFactory.CreateBinaryOperation(OperationType.Assignment, fieldReference, valueParameterReference, null) in
            setTypeOfNode assignment valueParameter.Type
            let assignmentStatement = AstFactory.CreateExpressionStatement(assignment, null) in
            let blockStatement = AstFactory.CreateBlockStatement([assignmentStatement]) in
            property.Setter.Body <- blockStatement
        property.Setter

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
        // TODO: this list can be memorized for one time, implement it after indexer expressions
        let methods =
            List.append
                (List.ofSeq decompiledClass.Methods)
                (List.collect (fun (prop : IDecompiledProperty) -> List.filter ((<>) null) [embodyGetter prop; embodySetter prop]) (List.ofSeq decompiledClass.Properties))
        methods |> List.tryPick (fun (m : IDecompiledMethod) -> if m.MetadataMethod = methodInfo then Some(m) else None)

    let public resolveType (typ : System.Type) =
        let assembly = loadAssembly (JetBrains.Util.FileSystemPath.Parse(typ.Assembly.Location)) in
        if assembly = null then null
        else assembly.GetTypeFromQualifiedName(typ.AssemblyQualifiedName, false)

    let public locationOfType qualifiedTypeName =
        let typ = System.Type.GetType(qualifiedTypeName) in
        if typ = null then __notImplemented__()
        JetBrains.Util.FileSystemPath.Parse(typ.Assembly.Location)

    let public getDefaultFieldValuesOf isStatic qualifiedTypeName =
        let assemblyPath = locationOfType qualifiedTypeName in
        let decompiledClass = decompileClass assemblyPath qualifiedTypeName in
        let extractDecompiledFieldInfo (f : IDecompiledField) =
            (f.MetadataField.Name, (f.MetadataField.Type, f.Initializer))
        let isDecompiledFieldStatic required (f : IDecompiledField) =
            f.MetadataField.IsStatic = required
        let extractBackingFieldInfo (f : IDecompiledProperty) =
            (f.BackingField.Name, (f.BackingField.Type, null))
        let isStaticBackingField required (p : IDecompiledProperty) =
            p.IsAuto && p.BackingField.IsStatic = required
        in
        let regularFields = decompiledClass.Fields |> Seq.filter (isDecompiledFieldStatic isStatic) |> Seq.map extractDecompiledFieldInfo in
        let backingFields = decompiledClass.Properties |> Seq.filter (isStaticBackingField isStatic) |> Seq.map extractBackingFieldInfo in
        Seq.append regularFields backingFields |> List.ofSeq

    let public getStaticConstructorOf qualifiedTypeName =
        let assemblyPath = locationOfType qualifiedTypeName in
        let decompiledClass = decompileClass assemblyPath qualifiedTypeName in
        decompiledClass.Methods |> Seq.tryFind (fun (m : IDecompiledMethod) -> m.MetadataMethod.IsStatic && m.MetadataMethod.Name = ".cctor")

    let public methodInfoToMetadataMethod assemblyPath qualifiedTypeName (methodInfo : System.Reflection.MethodInfo) =
        let assembly = loadAssembly assemblyPath in
        let typ = assembly.GetTypeInfoFromQualifiedName(qualifiedTypeName, false) in
        typ.GetMethods() |> Array.tryPick (fun m -> if m.Token.Value = uint32 methodInfo.MetadataToken then Some(m) else None)
