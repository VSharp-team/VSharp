namespace VSharp

open System.Collections.Generic
open System.Reflection

module public SVM =

    let private init = lazy(
        State.activator <- new Activator()
        Functions.Explorer.interpreter <- new SymbolicInterpreter())

    let private prepareAndInvoke (dictionary : System.Collections.IDictionary) assemblyPath (m : MethodInfo) invoke =
        init.Force()
        let qualifiedTypeName = m.DeclaringType.AssemblyQualifiedName
        let metadataMethodOption = DecompilerServices.methodInfoToMetadataMethod assemblyPath qualifiedTypeName m
        match metadataMethodOption with
        | None ->
            printfn "WARNING: metadata method for %s.%s not found!" qualifiedTypeName m.Name
        | Some metadataMethod ->
            dictionary.Add(m, (Nop, State.empty))
            invoke (MetadataMethodIdentifier metadataMethod) (fun (result, state) ->
            System.Console.WriteLine("For {0}.{1} got {2}!", m.DeclaringType.Name, m.Name, ControlFlow.resultToTerm result)
            dictionary.[m] <- (ControlFlow.resultToTerm result, state))

    let private interpretEntryPoint (dictionary : System.Collections.IDictionary) assemblyPath (m : MethodInfo) =
        assert(m.IsStatic)
        prepareAndInvoke dictionary assemblyPath m (fun id k ->
        let metadata = Metadata.empty
        let initialState = State.emptyRestricted
        Interpreter.initialize initialState (fun newState ->
        match id with
        | MetadataMethodIdentifier mm ->
            Interpreter.initializeStaticMembersIfNeed null newState mm.DeclaringType.AssemblyQualifiedName (fun (_, state) ->
            Functions.Explorer.invoke id state None k)
        | _ -> __unreachable__()))

    let private explore (dictionary : System.Collections.IDictionary) assemblyPath (m : MethodInfo) =
        prepareAndInvoke dictionary assemblyPath m Functions.Explorer.explore

    let private exploreType ignoreList ep dictionary assemblyPath (t : System.Type) =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        let bindingFlags = BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.DeclaredOnly
        if List.forall (fun keyword -> not(t.AssemblyQualifiedName.Contains(keyword))) ignoreList && t.IsPublic then
            t.GetMethods(bindingFlags) |> FSharp.Collections.Array.iter (fun m -> if m <> ep && not m.IsAbstract then explore dictionary assemblyPath m)

    let private replaceLambdaLines str =
        System.Text.RegularExpressions.Regex.Replace(str, @"@\d+(\+|\-)\d*\[Microsoft.FSharp.Core.Unit\]", "")

    let private resultToString (kvp : KeyValuePair<_, _>) =
        let term, state = kvp.Value
        sprintf "%s\nHEAP:\n%s" (toString term) (replaceLambdaLines (State.dumpMemory state))

    let public Run (assembly : Assembly) (ignoreList : List<_>) =
        let ignoreList = List.ofSeq ignoreList
        let dictionary = new Dictionary<MethodInfo, Term * State.state>()
        let path = JetBrains.Util.FileSystemPath.Parse(assembly.Location)
        let ep = assembly.EntryPoint
        assembly.GetTypes() |> FSharp.Collections.Array.iter (fun elem -> exploreType ignoreList ep dictionary path elem)
        if ep <> null then interpretEntryPoint dictionary path ep
        System.Linq.Enumerable.ToDictionary(dictionary :> IEnumerable<_>, (fun kvp -> kvp.Key), resultToString) :> IDictionary<_, _>
