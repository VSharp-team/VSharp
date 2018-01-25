namespace VSharp.Interpreter

open VSharp
open VSharp.Core
open System.Collections.Generic
open System.Reflection

module public SVM =

    let private init = lazy(Core.API.Configure (new Activator()) (new SymbolicInterpreter()))

    let private prepareAndInvoke (dictionary : System.Collections.IDictionary) assemblyPath (m : MethodInfo) invoke =
        init.Force()
        let qualifiedTypeName = m.DeclaringType.AssemblyQualifiedName
        let metadataMethodOption = DecompilerServices.methodInfoToMetadataMethod assemblyPath qualifiedTypeName m
        match metadataMethodOption with
        | None ->
            printfn "WARNING: metadata method for %s.%s not found!" qualifiedTypeName m.Name
        | Some metadataMethod ->
            dictionary.Add(m, null)
            invoke ({ metadataMethod = metadataMethod }) (fun (result, state) ->
            System.Console.WriteLine("For {0}.{1} got {2}!", m.DeclaringType.Name, m.Name, ControlFlow.ResultToTerm result)
            dictionary.[m] <- (ControlFlow.ResultToTerm result, state))

    let private interpretEntryPoint (dictionary : System.Collections.IDictionary) assemblyPath (m : MethodInfo) =
        assert(m.IsStatic)
        prepareAndInvoke dictionary assemblyPath m InterpretEntryPoint

    let private explore (dictionary : System.Collections.IDictionary) assemblyPath (m : MethodInfo) =
        prepareAndInvoke dictionary assemblyPath m Explore

    let private exploreType ignoreList ep dictionary assemblyPath (t : System.Type) =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        let bindingFlags = BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.DeclaredOnly
        if List.forall (fun keyword -> not(t.AssemblyQualifiedName.Contains(keyword))) ignoreList && t.IsPublic then
            t.GetMethods(bindingFlags) |> FSharp.Collections.Array.iter (fun m -> if m <> ep && not m.IsAbstract then explore dictionary assemblyPath m)

    let private replaceLambdaLines str =
        System.Text.RegularExpressions.Regex.Replace(str, @"@\d+(\+|\-)\d*\[Microsoft.FSharp.Core.Unit\]", "")

    let private resultToString (kvp : KeyValuePair<_, _>) =
        let term, state = kvp.Value
        sprintf "%O\nHEAP:\n%s" term (state |> Memory.Dump |> replaceLambdaLines)

    let public Run (assembly : Assembly) (ignoreList : List<_>) =
        let ignoreList = List.ofSeq ignoreList
        let dictionary = new Dictionary<MethodInfo, term * state>()
        let path = JetBrains.Util.FileSystemPath.Parse(assembly.Location)
        let ep = assembly.EntryPoint
        assembly.GetTypes() |> FSharp.Collections.Array.iter (fun elem -> exploreType ignoreList ep dictionary path elem)
        if ep <> null then interpretEntryPoint dictionary path ep
        System.Linq.Enumerable.ToDictionary(dictionary :> IEnumerable<_>, (fun kvp -> kvp.Key), resultToString) :> IDictionary<_, _>
