namespace VSharp

open System.Reflection

module public SVM =

    let private interpret assemblyPath qualifiedTypeName (m : MethodInfo) =
        printfn "=========== Interpreting %s.%s: ===========" qualifiedTypeName m.Name
        let state = State.empty in
        let declaringType = Types.FromDotNetType(m.DeclaringType) in
        let metadataMethodOption = DecompilerServices.methodInfoToMetadataMethod assemblyPath qualifiedTypeName m
        Interpreter.initializeStaticMembersIfNeed state m.DeclaringType.AssemblyQualifiedName (fun state ->
        match metadataMethodOption with
        | None -> printfn "WARNING: metadata method for %s.%s not found!" qualifiedTypeName m.Name
        | Some metadataMethod ->
            let this, state =
                match m with
                | _ when m.IsStatic ->
                (Concrete(null, declaringType), state)
                | _ ->
                    let instance, state = Memory.allocateSymbolicInstance false "" state declaringType in
                    if Terms.IsHeapRef instance then (instance, state)
                    else
                        let key = "external data" in
                        let state = State.push state [(key, instance)] in
                        (Memory.referenceToVariable state key true, state)
            Interpreter.decompileAndReduceMethod state this [] qualifiedTypeName metadataMethod assemblyPath (fun (term, state) ->
            printfn "=========== Results: ==========="
            printfn "SVM result: %s" (toString term)
            printfn "SVM environment: %s" (toString state)))

    let private runType assemblyPath (t : System.Type) =
        let qualifiedTypeName = t.FullName in
        let disabledTests = [
            "Calculator";
//            "Logics";
//            "Conditional";
//            "Arithmetics";
//            "Fibonacci";
//            "Lambdas";
//            "GCD";
//            "ClassesSimple";
            //"StaticMembers"
            ] in
        if t.IsPublic && List.forall (fun keyword -> not(qualifiedTypeName.Contains(keyword))) disabledTests then
            t.GetMethods() |> Array.iter (interpret assemblyPath qualifiedTypeName)

    let public Run (assembly : Assembly) =
        printfn "Running assembly %s..." assembly.FullName
        let path = JetBrains.Util.FileSystemPath.Parse(assembly.Location) in
        assembly.GetTypes() |> Array.iter (runType path)
