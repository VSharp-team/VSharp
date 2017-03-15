namespace VSharp

open System.Reflection

module public SVM =

    let private interpret assemblyPath qualifiedTypeName (m : MethodInfo) =
        let res = new System.Text.StringBuilder((sprintf "=========== Interpreting %s.%s: ===========\n" qualifiedTypeName m.Name))
        let state = State.empty in
        let declaringType = Types.FromDotNetType(m.DeclaringType) in
        let metadataMethodOption = DecompilerServices.methodInfoToMetadataMethod assemblyPath qualifiedTypeName m
        match metadataMethodOption with
        | None -> res.Append(sprintf "WARNING: metadata method for %s.%s not found!" qualifiedTypeName m.Name).ToString()
        | Some metadataMethod ->
            let this, state =
                match m with
                | _ when m.IsStatic -> (Concrete(null, declaringType), state)
                | _ ->
                    let instance, state = Memory.allocateSymbolicInstance "" state declaringType in
                    if Terms.IsHeapRef instance then (instance, state)
                    else
                        let key = "external data" in
                        let state = State.push state [(key, instance)] in
                        (Memory.referenceToVariable state key true, state)
            Interpreter.decompileAndReduceMethod state this [] qualifiedTypeName metadataMethod assemblyPath (fun (term, state) ->
            res.Append((sprintf "=========== Results: ===========\nSVM result: %s\nSVM environment: %s" (toString term) (toString state))).ToString())

    let private runType assemblyPath (t : System.Type) =
        let res = new System.Text.StringBuilder()
        let qualifiedTypeName = t.FullName in
        let disabledTests = [
            "Calculator";
//            "Logics";
//            "Conditional";
//            "Arithmetics";
//            "ClassesSimple"
//            "Fibonacci";
//            "Lambdas";
//            "GCD"
            ] in
        if List.forall (fun keyword -> not(qualifiedTypeName.Contains(keyword))) disabledTests then
            t.GetMethods() |> Array.fold (fun (acc : System.Text.StringBuilder) elem -> acc.AppendLine((interpret assemblyPath qualifiedTypeName elem))) res
            |> fun (sb : System.Text.StringBuilder) -> sb.ToString()
        else ""

    let public Run (assembly : Assembly) =
        let res = new System.Text.StringBuilder((sprintf "Running assembly %s...\n" assembly.FullName))
        let path = JetBrains.Util.FileSystemPath.Parse(assembly.Location) in
        assembly.GetTypes() |> Array.fold (fun (acc : System.Text.StringBuilder) elem -> acc.Append((runType path elem))) res
        |> fun (sb : System.Text.StringBuilder) -> sb.ToString()
