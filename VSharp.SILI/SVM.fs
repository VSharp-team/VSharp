namespace VSharp

open System.Collections.Generic
open System.Reflection

module public SVM =

    let private reset () =
        IdGenerator.reset()
        State.activator <- new Activator()
        Functions.UnboundedRecursionExplorer.interpreter <- new SymbolicInterpreter()
        Memory.reset()

    let private interpret (dictionary : System.Collections.IDictionary) assemblyPath (m : MethodInfo) =
        if m.IsAbstract then ()
        else
            reset()
            let qualifiedTypeName = m.DeclaringType.AssemblyQualifiedName in
            let metadataMethodOption = DecompilerServices.methodInfoToMetadataMethod assemblyPath qualifiedTypeName m in
            match metadataMethodOption with
            | None ->
                printfn "WARNING: metadata method for %s.%s not found!" qualifiedTypeName m.Name
            | Some metadataMethod ->
                Functions.UnboundedRecursionExplorer.explore (MetadataMethodIdentifier metadataMethod) (fun (result, state) ->
                System.Console.WriteLine("For {0}.{1} got {2}!", m.DeclaringType.Name, m.Name, ControlFlow.resultToTerm result)
                dictionary.Add(m, (ControlFlow.resultToTerm result, state)))

    let private runType ignoreList dictionary assemblyPath (t : System.Type) =
        if List.forall (fun keyword -> not(t.AssemblyQualifiedName.Contains(keyword))) ignoreList then
            t.GetMethods() |> Array.iter (interpret dictionary assemblyPath)

    let private replaceLambdaLines str =
        System.Text.RegularExpressions.Regex.Replace(str, @"@\d+(\+|\-)\d*\[Microsoft.FSharp.Core.Unit\]", "")

    let private resultToString (kvp : KeyValuePair<_, _>) =
        let term, state = kvp.Value in
        sprintf "%s\nHEAP:\n%s" (toString term) (replaceLambdaLines (State.dumpMemory state))

    let public Run (assembly : Assembly) (ignoreList : List<_>) =
        let ignoreList = List.ofSeq ignoreList
        let dictionary = new Dictionary<MethodInfo, Term * State.state>()
        let path = JetBrains.Util.FileSystemPath.Parse(assembly.Location) in
        assembly.GetTypes() |> Array.iter (fun elem -> runType ignoreList dictionary path elem) |> ignore
        System.Linq.Enumerable.ToDictionary(dictionary :> IEnumerable<_>, (fun kvp -> kvp.Key), resultToString) :> IDictionary<_, _>
