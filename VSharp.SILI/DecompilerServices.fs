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

    let public setPropertyOfNode (node : INode) property value =
        node.Data.SetValue(JetBrains.Decompiler.Utils.DataKey<obj>(property), value :> obj)

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

    // For some reason DotPeek poorly handles Mono-compiled auto-properties. Backing field is not initialized,
    // the property is not an auto-one, but body is still null!
    let private hackBuggyAutoProperty (property : IDecompiledProperty) embodier =
        printfn "Warning: hacking buggy auto-property %s.%s" property.OwnerClass.TypeInfo.FullyQualifiedName property.MetadataProperty.Name
        let fieldNameIs name (field : IMetadataField) = field.Name = name
        let backingField =
            property.OwnerClass.TypeInfo.GetFields()
                |> Array.tryFind (fieldNameIs (sprintf "<%s>k__BackingField" property.MetadataProperty.Name))
        in
        match backingField with
        | Some field ->
            embodier property field
        | None -> ()

    let private embodyAutoGetter (property : IDecompiledProperty) (backingField : IMetadataField) =
        let fieldSpecification = new FieldSpecification(backingField) in
        let this = if property.Getter.MetadataMethod.IsStatic then null else createThisOf property.OwnerClass.TypeInfo in
        let fieldReference = AstFactory.CreateFieldAccess(this, fieldSpecification, null) in
        let returnStatement = AstFactory.CreateReturn(fieldReference :> IExpression, null) in
        let blockStatement = AstFactory.CreateBlockStatement([returnStatement]) in
        property.Getter.Body <- blockStatement

    let private embodyAutoSetter (property : IDecompiledProperty) (backingField : IMetadataField) =
        let fieldSpecification = new FieldSpecification(backingField) in
        let this = if property.Getter.MetadataMethod.IsStatic then null else AstFactory.CreateThisReference(null) in
        let fieldReference = AstFactory.CreateFieldAccess(this, fieldSpecification, null) in
        let valueParameter = property.Setter.Signature.Parameters.Item(0) in
        let valueParameterReference = AstFactory.CreateParameterReference(valueParameter, null) in
        let assignment = AstFactory.CreateBinaryOperation(OperationType.Assignment, fieldReference, valueParameterReference, null) in
        setTypeOfNode assignment valueParameter.Type
        let assignmentStatement = AstFactory.CreateExpressionStatement(assignment, null) in
        let blockStatement = AstFactory.CreateBlockStatement([assignmentStatement]) in
        property.Setter.Body <- blockStatement

    let private embodyGetter (property : IDecompiledProperty) =
        if property.Getter <> null && not property.IsAuto && property.Getter.Body = null then
            hackBuggyAutoProperty property embodyAutoGetter
        else if property.Getter <> null && property.IsAuto && property.Getter.Body = null then
            embodyAutoGetter property property.BackingField
        property.Getter

    let private embodySetter (property : IDecompiledProperty) =
        if property.Setter <> null && not property.IsAuto && property.Setter.Body = null then
            hackBuggyAutoProperty property embodyAutoSetter
        else if property.Setter <> null && property.IsAuto && property.Setter.Body = null then
            embodyAutoSetter property property.BackingField
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

    let idOfMetadataField (field : IMetadataField) =
        sprintf "%s.%s" field.DeclaringType.FullyQualifiedName field.Name

    let rec getDefaultFieldValuesOf isStatic qualifiedTypeName =
        let assemblyPath = locationOfType qualifiedTypeName in
        let decompiledClass = decompileClass assemblyPath qualifiedTypeName in
        let extractDecompiledFieldInfo (f : IDecompiledField) =
            (idOfMetadataField f.MetadataField, (f.MetadataField.Type, f.Initializer))
        let isDecompiledFieldStatic required (f : IDecompiledField) =
            f.MetadataField.IsStatic = required
        let extractBackingFieldInfo (f : IDecompiledProperty) =
            (idOfMetadataField f.BackingField, (f.BackingField.Type, f.Initializer))
        let isStaticBackingField required (p : IDecompiledProperty) =
            p.IsAuto && p.BackingField.IsStatic = required
        in
        let regularFields = decompiledClass.Fields |> Seq.filter (isDecompiledFieldStatic isStatic) |> Seq.map extractDecompiledFieldInfo |> List.ofSeq in
        let backingFields = decompiledClass.Properties |> Seq.filter (isStaticBackingField isStatic) |> Seq.map extractBackingFieldInfo |> List.ofSeq in
        let parentFields =
            if isStatic || decompiledClass.TypeInfo.Base = null then []
            else getDefaultFieldValuesOf false decompiledClass.TypeInfo.Base.AssemblyQualifiedName
        List.concat [regularFields; backingFields; parentFields]

    let public getStaticConstructorOf qualifiedTypeName =
        let assemblyPath = locationOfType qualifiedTypeName in
        let decompiledClass = decompileClass assemblyPath qualifiedTypeName in
        decompiledClass.Methods |> Seq.tryFind (fun (m : IDecompiledMethod) -> m.MetadataMethod.IsStatic && m.MetadataMethod.Name = ".cctor")

    let public methodInfoToMetadataMethod assemblyPath qualifiedTypeName (methodInfo : System.Reflection.MethodInfo) =
        let assembly = loadAssembly assemblyPath in
        let typ = assembly.GetTypeInfoFromQualifiedName(qualifiedTypeName, false) in
        typ.GetMethods() |> Array.tryPick (fun m -> if m.Token.Value = uint32 methodInfo.MetadataToken then Some(m) else None)

    let rec private baseClassesChainAcc acc = function
        | null -> acc
        | (t : IMetadataType) -> baseClassesChainAcc (t::acc) (t.GetBaseType())

    let public baseClassesChain (t : IMetadataType) =
        baseClassesChainAcc [] t
