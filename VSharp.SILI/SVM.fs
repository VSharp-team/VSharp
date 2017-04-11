namespace VSharp

open System.Collections.Generic
open System.Reflection

module public SVM =

    let private interpret (dictionary : System.Collections.IDictionary) assemblyPath qualifiedTypeName (m : MethodInfo) =
        let state = State.empty in
        let declaringType = Types.FromDotNetType(m.DeclaringType) in
        let metadataMethodOption = DecompilerServices.methodInfoToMetadataMethod assemblyPath qualifiedTypeName m
        Interpreter.initializeStaticMembersIfNeed state m.DeclaringType.AssemblyQualifiedName (fun state ->
        match metadataMethodOption with
        | None -> printfn "WARNING: metadata method for %s.%s not found!" qualifiedTypeName m.Name
        | Some metadataMethod ->
            let this, state =
                match m with
                | _ when m.IsStatic -> (Concrete(null, declaringType), state)
                | _ ->
                    let instance, state = Memory.allocateSymbolicInstance false "" state declaringType in
                    if Terms.IsHeapRef instance then (instance, state)
                    else
                        let key = "external data" in
                        let state = State.push state [(key, instance)] in
                        (Memory.referenceToVariable state key true, state)
            Memory.resetHeap()
            Interpreter.decompileAndReduceMethod state this [] qualifiedTypeName metadataMethod assemblyPath (fun (result, state) ->
            dictionary.Add(m, (ControlFlow.resultToTerm result, state))))

    let private runType ignoreList dictionary assemblyPath (t : System.Type) =
        let qualifiedTypeName = t.FullName in
        if List.forall (fun keyword -> not(qualifiedTypeName.Contains(keyword))) ignoreList then
            t.GetMethods() |> Array.iter (interpret dictionary assemblyPath qualifiedTypeName)

    let private resultToString (kvp : KeyValuePair<_, _>) =
        let term, state = kvp.Value in
        sprintf "%s\nHEAP:\n%s" (toString term) (State.dumpHeap state)

    let public Run (assembly : Assembly) (ignoreList : List<_>) =
        let ignoreList = List.ofSeq ignoreList
        let dictionary = new Dictionary<MethodInfo, Term * State.state>()
        let path = JetBrains.Util.FileSystemPath.Parse(assembly.Location) in
        assembly.GetTypes() |> Array.iter (fun elem -> runType ignoreList dictionary path elem) |> ignore
        System.Linq.Enumerable.ToDictionary(dictionary :> IEnumerable<_>, (fun kvp -> kvp.Key), resultToString) :> IDictionary<_, _>
