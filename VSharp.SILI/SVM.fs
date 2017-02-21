namespace VSharp

open System.Reflection

module public SVM =

    let private interpret assemblyPath qualifiedTypeName (m : MethodInfo) =
        printfn "=========== Interpreting %s.%s: ===========" qualifiedTypeName m.Name
        Interpreter.decompileAndReduceMethod State.empty [] qualifiedTypeName m.Name assemblyPath (fun (term, state) ->
        printfn "=========== Results: ==========="
        printfn "SVM result: %s" (toString term)
        printfn "SVM environment: %s" (toString state))

    let private runType assemblyPath (t : System.Type) =
        let qualifiedTypeName = t.FullName in
        let disabledTests = ["Calculator"; "Arithmetics"; "Fibonacci"; "GCD"(*; "Branching"; "Logics" *)] in
        if List.forall (fun keyword -> not(qualifiedTypeName.Contains(keyword))) disabledTests then
            t.GetMethods() |> Array.iter (interpret assemblyPath qualifiedTypeName)

    let public Run (assembly : Assembly) =
        printfn "Running assembly %s..." assembly.FullName
        let path = JetBrains.Util.FileSystemPath.Parse(assembly.Location) in
        assembly.GetTypes() |> Array.iter (runType path)
