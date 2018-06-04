namespace VSharp.Interpreter

open VSharp
open JetBrains.Metadata.Reader.API
open JetBrains.Decompiler.Ast
open System.Collections.Generic

module internal DecompilerServices =
    let private assemblyLoader = new JetBrains.Metadata.Reader.API.MetadataLoader(JetBrains.Metadata.Access.MetadataProviderFactory.DefaultProvider)
    let private assemblies = new Dictionary<string, JetBrains.Metadata.Reader.API.IMetadataAssembly>()
    let private decompilers = new Dictionary<string, JetBrains.Decompiler.ClassDecompiler>()
    let private decompiledClasses = new Dictionary<string, IDecompiledClass>()

    let internal jetBrainsFileSystemPath path = JetBrains.Util.FileSystemPath.Parse(path)

    let rec internal loadAssemblyByName (name : string) =
        let path = System.Reflection.Assembly.Load(name).Location
        if not(assemblies.ContainsKey(path)) then
            let jbPath = JetBrains.Util.FileSystemPath.Parse(path)
            loadAssembly jbPath |> ignore

    and internal loadAssembly (path : JetBrains.Util.FileSystemPath) =
        let assembly = Dict.getValueOrUpdate assemblies (path.ToString()) (fun () -> assemblyLoader.LoadFrom(path, fun _ -> true))
        assembly.ReferencedAssembliesNames |> Seq.iter (fun reference -> loadAssemblyByName reference.FullName)
        assembly

    let internal getPropertyOfNode (node : INode) key defaultValue =
        // node.Data.TryGetValue is poorly implemented (it checks for reference equality of keys), so searching manually...
        node.Data |> Seq.tryPick (fun keyValue -> if (keyValue.Key.ToString() = key) then Some(keyValue.Value) else None) |?? defaultValue

    let internal setPropertyOfNode (node : INode) property value =
        node.Data.SetValue(JetBrains.Decompiler.Utils.DataKey<obj>(property), box value)

    let internal getTypeOfNode (node : INode) =
        getPropertyOfNode node "Type" null :?> JetBrains.Metadata.Reader.API.IMetadataType

    let public setTypeOfNode (node : INode) (t : JetBrains.Metadata.Reader.API.IMetadataType) =
        node.Data.SetValue(JetBrains.Decompiler.Utils.DataKey<JetBrains.Metadata.Reader.API.IMetadataType>("Type"), t)

    let public copyTypeTo (src : INode) (dst : INode) =
        setTypeOfNode dst (getTypeOfNode src)

    let private createThisOf (typ : IMetadataTypeInfo) =
        let this = AstFactory.CreateThisReference(null)
        if typ.IsClass || typ.IsInterface
        then AstFactory.CreateDeref(this, null, true) :> IExpression
        else this :> IExpression

    // For some reason DotPeek poorly handles Mono-compiled auto-properties. Backing field is not initialized,
    // the property is not an auto-one, but body is still null!
    let private hackBuggyAutoProperty (property : IDecompiledProperty) embodier =
//        printfn "Warning: hacking buggy auto-property %s.%s" property.OwnerClass.TypeInfo.FullyQualifiedName property.MetadataProperty.Name
        let fieldNameIs name (field : IMetadataField) = field.Name = name
        let backingField =
            property.OwnerClass.TypeInfo.GetFields()
                |> Array.tryFind (fieldNameIs (sprintf "<%s>k__BackingField" property.MetadataProperty.Name))
        match backingField with
        | Some field ->
            embodier property field
        | None -> ()

    let private embodyAutoGetter (property : IDecompiledProperty) (backingField : IMetadataField) =
        let fieldSpecification = new FieldSpecification(backingField)
        let this = if property.Getter.MetadataMethod.IsStatic then null else createThisOf property.OwnerClass.TypeInfo
        let fieldReference = AstFactory.CreateFieldAccess(this, fieldSpecification, null)
        let returnStatement = AstFactory.CreateReturn(fieldReference :> IExpression, null)
        let blockStatement = AstFactory.CreateBlockStatement([returnStatement])
        property.Getter.Body <- blockStatement

    let private embodyAutoSetter (property : IDecompiledProperty) (backingField : IMetadataField) =
        let fieldSpecification = new FieldSpecification(backingField)
        let this = if property.Getter.MetadataMethod.IsStatic then null else AstFactory.CreateThisReference(null)
        let fieldReference = AstFactory.CreateFieldAccess(this, fieldSpecification, null)
        let valueParameter = property.Setter.Signature.Parameters.Item(0)
        let valueParameterReference = AstFactory.CreateParameterReference(valueParameter, null)
        let assignment = AstFactory.CreateBinaryOperation(OperationType.Assignment, fieldReference, valueParameterReference, null)
        setTypeOfNode assignment valueParameter.Type
        let assignmentStatement = AstFactory.CreateExpressionStatement(assignment, null)
        let blockStatement = AstFactory.CreateBlockStatement([assignmentStatement])
        property.Setter.Body <- blockStatement

    let private embodyGetter (property : IDecompiledProperty) =
        if property.Getter <> null && not property.IsAuto && property.Getter.Body = null then
            hackBuggyAutoProperty property embodyAutoGetter
        elif property.Getter <> null && property.IsAuto && property.Getter.Body = null then
            embodyAutoGetter property property.BackingField
        property.Getter

    let private embodySetter (property : IDecompiledProperty) =
        if property.Setter <> null && not property.IsAuto && property.Setter.Body = null then
            hackBuggyAutoProperty property embodyAutoSetter
        elif property.Setter <> null && property.IsAuto && property.Setter.Body = null then
            embodyAutoSetter property property.BackingField
        property.Setter

    let public decompileClass assemblyPath qualifiedTypeName =
        Dict.getValueOrUpdate decompiledClasses qualifiedTypeName (fun () ->
            let metadataAssembly = loadAssembly assemblyPath
            let possiblyUnresolvedMetadataTypeInfo = metadataAssembly.GetTypeInfoFromQualifiedName(qualifiedTypeName, false)
            let metadataTypeInfo =
                if possiblyUnresolvedMetadataTypeInfo.IsResolved then possiblyUnresolvedMetadataTypeInfo
                else metadataAssembly.GetTypeInfoFromQualifiedName(qualifiedTypeName.Substring(0, qualifiedTypeName.IndexOf(",")), false)
            let decompiler = Dict.getValueOrUpdate decompilers (assemblyPath.ToString()) (fun () ->
                let lifetime = JetBrains.DataFlow.Lifetimes.Define()
                let methodCollector = new JetBrains.Metadata.Utils.MethodCollectorStub()
                let options = new JetBrains.Decompiler.ClassDecompilerOptions(true)
                new JetBrains.Decompiler.ClassDecompiler(lifetime.Lifetime, metadataAssembly, options, methodCollector))
            decompiler.Decompile(metadataTypeInfo, JetBrains.Application.Progress.NullProgressIndicator.Instance))

    type DecompilationResult =
        | MethodWithExplicitInitializer of IDecompiledMethod
        | MethodWithImplicitInitializer of IDecompiledMethod
        | MethodWithoutInitializer of IDecompiledMethod
        | ObjectConstuctor of IDecompiledMethod
        | DefaultConstuctor
        | DecompilationError

    let public isConstructor (m : IMetadataMethod) =
        m.Name = ".ctor"

    let public decomplieSuitableMethod methodComparator (decompiledClass : IDecompiledClass) (methodInfo : IMetadataMethod) =
        // TODO: this list can be memorized for one time, implement it after indexer expressions
        List.append
            (List.ofSeq decompiledClass.Methods)
            (List.collect (fun (prop : IDecompiledProperty) -> List.filter ((<>) null) [embodyGetter prop; embodySetter prop]) (List.ofSeq decompiledClass.Properties))
        |> List.tryPick (fun (m : IDecompiledMethod) -> if methodComparator methodInfo m.MetadataMethod then Some(m) else None)
        |> function
        | Some m when m.MetadataMethod.DeclaringType.AssemblyQualifiedName = typeof<obj>.AssemblyQualifiedName -> ObjectConstuctor m
        | Some m when isConstructor methodInfo ->
            if m.Initializer = null then MethodWithImplicitInitializer m else MethodWithExplicitInitializer m
        | Some m -> MethodWithoutInitializer m
        | _ when isConstructor methodInfo -> DefaultConstuctor
        | _ -> DecompilationError

    let public decompileMethod assemblyPath qualifiedTypeName (methodInfo : IMetadataMethod) =
        let decompiledClass = decompileClass assemblyPath qualifiedTypeName
        decomplieSuitableMethod (fun l r -> l.Equals r) decompiledClass (methodInfo : IMetadataMethod)

    let public resolveType (typ : System.Type) =
        let assembly = loadAssembly (JetBrains.Util.FileSystemPath.Parse(typ.Assembly.Location))
        if assembly = null then null
        else assembly.GetTypeFromQualifiedName(typ.AssemblyQualifiedName, false)

    let public resolveTypeInfo (typ : System.Type) =
        let assembly = loadAssembly (JetBrains.Util.FileSystemPath.Parse(typ.Assembly.Location))
        if assembly = null then null
        else assembly.GetTypeInfoFromQualifiedName(typ.AssemblyQualifiedName, false)

    let public locationOfType qualifiedTypeName =
        let typ = System.Type.GetType(qualifiedTypeName)
        if typ = null then __notImplemented__()
        JetBrains.Util.FileSystemPath.Parse(typ.Assembly.Location)

    let idOfMetadataField (field : IMetadataField) =
        sprintf "%s.%s" field.DeclaringType.FullyQualifiedName field.Name

    let private initializerOf (f : IDecompiledField) =
        let mf = f.MetadataField
        if mf.IsLiteral
        then
            let literal = AstFactory.CreateLiteral(Constant.FromValueAndType(mf.GetLiteralValue(), mf.Type), null) :> IExpression
            setTypeOfNode literal mf.Type
            literal
        else f.Initializer

    let rec getDefaultFieldValuesOf isStatic withParent qualifiedTypeName =
        let assemblyPath = locationOfType qualifiedTypeName
        let decompiledClass = decompileClass assemblyPath qualifiedTypeName
        let extractDecompiledFieldInfo (f : IDecompiledField) =
            (idOfMetadataField f.MetadataField, (f.MetadataField.Type, initializerOf f))
        let isDecompiledFieldStatic required (f : IDecompiledField) =
            f.MetadataField.IsStatic = required
        let extractBackingFieldInfo (f : IDecompiledProperty) =
            (idOfMetadataField f.BackingField, (f.BackingField.Type, f.Initializer))
        let isStaticBackingField required (p : IDecompiledProperty) =
            p.IsAuto && p.BackingField.IsStatic = required
        let regularFields = decompiledClass.Fields |> Seq.filter (isDecompiledFieldStatic isStatic) |> Seq.map extractDecompiledFieldInfo |> List.ofSeq
        let backingFields = decompiledClass.Properties |> Seq.filter (isStaticBackingField isStatic) |> Seq.map extractBackingFieldInfo |> List.ofSeq
        let parentFields =
            if not withParent || isStatic || decompiledClass.TypeInfo.Base = null then []
            else getDefaultFieldValuesOf false withParent decompiledClass.TypeInfo.Base.Type.AssemblyQualifiedName
        List.append3 regularFields backingFields parentFields

    let public getStaticConstructorOf qualifiedTypeName =
        let assemblyPath = locationOfType qualifiedTypeName
        let decompiledClass = decompileClass assemblyPath qualifiedTypeName
        decompiledClass.Methods |> Seq.tryFind (fun (m : IDecompiledMethod) -> m.MetadataMethod.IsStatic && m.MetadataMethod.Name = ".cctor")

    let public metadataMethodToString (m : IMetadataMethod) =
        let parameters = m.Parameters |> Array.map (fun p -> p.Type.FullName) |> List.ofArray
        let parametersAndThis = if m.Signature.HasThis then "this"::parameters else parameters
        sprintf "%s %s.%s(%s)"
            m.Signature.ReturnType.FullName
            m.DeclaringType.FullyQualifiedName
            m.Name
            (join ", " parametersAndThis)

    let public methodInfoToMetadataMethod assemblyPath qualifiedTypeName (methodInfo : System.Reflection.MethodInfo) =
        let assembly = loadAssembly assemblyPath
        let typ = assembly.GetTypeInfoFromQualifiedName(qualifiedTypeName, false)
        typ.GetMethods() |> Array.tryPick (fun m -> if m.Token.Value = uint32 methodInfo.MetadataToken then Some(m) else None)

    let internal getBaseCtorWithoutArgs qualifiedTypeName =
        let assemblyPath = locationOfType qualifiedTypeName
        let decompiledClass = decompileClass assemblyPath qualifiedTypeName
        let ctors = decompiledClass.TypeInfo.GetMethods() |> Array.filter (fun (m : IMetadataMethod) -> isConstructor m && Array.length m.Parameters = 0 && not m.IsStatic)
        assert(Array.length ctors = 1)
        ctors.[0]

    let internal resolveAdd argTypes : IMetadataType -> IMetadataMethod = function
        | :? IMetadataClassType as t ->
            let argsCount = Seq.length argTypes
            let overloads =
                t.Type.GetMethods()
                    |> Array.filter (fun m -> m.Name = "Add" && m.Parameters.Length = argsCount)
                    |> List.ofArray
            match overloads with
            | [] -> internalfail "suitable overload of Add not found in collection!"
            | [x] -> x
            | _ -> __notImplemented__() // TODO: args should be matched typewise
        | _ -> __notImplemented__()

    let internal assemblyQualifiedName : IMetadataType -> string = function
        | :? IMetadataClassType as t -> t.Type.AssemblyQualifiedName
        | t -> t.AssemblyQualifiedName

    let internal getTokenBy (node : Choice<IMethodParameter, ILocalVariable>) =
        match node with
        | Choice1Of2 node -> sprintf "MethodParameter:%i" (Microsoft.FSharp.Core.LanguagePrimitives.PhysicalHash(node))
        | Choice2Of2 node -> sprintf "LocalVariable:%i" (node.DeclarationScope.ToString().GetHashCode())

    let rec internal getThisTokenBy (node : INode) =
        match node with
        | :? IDecompiledMethod as m -> m.MetadataMethod.Token.ToString()
        | _ -> getThisTokenBy node.Parent

    let internal isOperationAssignment = function
        | OperationType.AssignmentAdd
        | OperationType.AssignmentDivide
        | OperationType.AssignmentLogicalAnd
        | OperationType.AssignmentLogicalOr
        | OperationType.AssignmentLogicalXor
        | OperationType.AssignmentMultiply
        | OperationType.AssignmentRemainder
        | OperationType.AssignmentShiftLeft
        | OperationType.AssignmentShiftRight
        | OperationType.AssignmentSubtract
        | OperationType.PostfixIncrement
        | OperationType.PostfixDecrement
        | OperationType.PrefixIncrement
        | OperationType.PrefixDecrement -> true
        | _ -> false

    let internal getAssignmentOperation = function
        | OperationType.AssignmentAdd -> Core.OperationType.Add
        | OperationType.AssignmentDivide -> Core.OperationType.Divide
        | OperationType.AssignmentLogicalAnd -> Core.OperationType.LogicalAnd
        | OperationType.AssignmentLogicalOr -> Core.OperationType.LogicalOr
        | OperationType.AssignmentLogicalXor -> Core.OperationType.LogicalXor
        | OperationType.AssignmentMultiply -> Core.OperationType.Multiply
        | OperationType.AssignmentRemainder -> Core.OperationType.Remainder
        | OperationType.AssignmentShiftLeft -> Core.OperationType.ShiftLeft
        | OperationType.AssignmentShiftRight -> Core.OperationType.ShiftRight
        | OperationType.AssignmentSubtract -> Core.OperationType.Subtract
        | op -> internalfailf "%O is not an assignment operation" op

    let internal convertOperation = function
        | OperationType.Add -> Core.OperationType.Add
        | OperationType.Divide -> Core.OperationType.Divide
        | OperationType.Equal -> Core.OperationType.Equal
        | OperationType.NotEqual -> Core.OperationType.NotEqual
        | OperationType.Greater -> Core.OperationType.Greater
        | OperationType.GreaterOrEqual -> Core.OperationType.GreaterOrEqual
        | OperationType.Less -> Core.OperationType.Less
        | OperationType.LessOrEqual -> Core.OperationType.LessOrEqual
        | OperationType.LogicalAnd -> Core.OperationType.LogicalAnd
        | OperationType.LogicalOr -> Core.OperationType.LogicalOr
        | OperationType.LogicalNeg -> Core.OperationType.LogicalNeg
        | OperationType.LogicalXor -> Core.OperationType.LogicalXor
        | OperationType.Multiply -> Core.OperationType.Multiply
        | OperationType.Not -> Core.OperationType.Not
        | OperationType.Remainder -> Core.OperationType.Remainder
        | OperationType.ShiftLeft -> Core.OperationType.ShiftLeft
        | OperationType.ShiftRight -> Core.OperationType.ShiftRight
        | OperationType.Subtract -> Core.OperationType.Subtract
        | OperationType.UnaryMinus -> Core.OperationType.UnaryMinus
        | _ -> __notImplemented__()

    let internal isConditionalOperation = function
        | OperationType.ConditionalAnd
        | OperationType.ConditionalOr -> true
        | _ -> false
