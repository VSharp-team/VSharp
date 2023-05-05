namespace VSharp

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open System.Text
open System.Xml

// NOTE: if 'expectedCoverage' is null 'TestResultsChecker' runs all tests and checks results equality
//       otherwise, it compares expected coverage to computed coverage of tests
type TestResultsChecker(testDir : DirectoryInfo, runnerDir : DirectoryInfo, expectedCoverage : Nullable<int>) =
    let expectedCoverage = None
    // NOTE: if 'TestResultsChecker' succeeds, 'resultMessage' is empty, otherwise, it contains failure message
    let mutable resultMessage = ""
    let runDotnet args =
        let output = StringBuilder()
        let error = StringBuilder()
        let info = ProcessStartInfo()
        info.WorkingDirectory <- testDir.FullName
        info.FileName <- "dotnet"
        info.Arguments <- args
        info.UseShellExecute <- false
        info.RedirectStandardOutput <- true
        info.RedirectStandardError <- true
        let proc = Process.Start info
        proc.OutputDataReceived.Add <| fun args -> output.Append args.Data |> ignore
        proc.ErrorDataReceived.Add <| fun args -> error.Append args.Data |> ignore
        proc.BeginOutputReadLine()
        proc.BeginErrorReadLine()
        proc.WaitForExit()
        proc.ExitCode, output.ToString(), error.ToString()

    let runDotCover globalArg runTests =
        let filters = ["-:module=Microsoft.*"; "-:module=FSharp.*"; "-:class=VSharp.*"; "-:module=VSharp.Utils"]
        let command = sprintf "dotcover --dcFilters=\"%s\" %s --dcReportType=DetailedXML %s" (filters |> join ";") runTests globalArg
        let code, _, error = runDotnet command
        if code <> 0 then Logger.error $"Non zero dotcover return code: {code}"
        code = 0, error

    let runnerWithArgs (directory : DirectoryInfo) =
        $"%s{runnerDir.FullName}%c{Path.DirectorySeparatorChar}VSharp.TestRunner.dll {directory.FullName}"

    let run (methodInfo : MethodInfo) testDir =
        match expectedCoverage with
        | Some _ ->
            let _, localTools, _ = runDotnet "tool list"
            let globalArg =
                if localTools.Contains "dotcover" then ""
                else
                    let _, globalTools, _ = runDotnet "tool list -g"
                    if globalTools.Contains "dotcover" then "-g"
                    else raise (InvalidOperationException "JetBrains.DotCover tool not found! Either install it locally by running 'dotnet tool restore' in build directory, or globally by running 'dotnet tool install JetBrains.dotCover.GlobalTool -g'")
            // TODO: try to run DotCover from IntegrationTests in same process
            runDotCover globalArg (runnerWithArgs testDir)
        | None ->
            let exitCode, error = __notImplemented__()
            exitCode = 0, error
            // TODO: run TestRunner from IntegrationTests in same process
            // let code, _, error = runDotnet (runnerWithArgs testDir)
            // code = 0, error

    let rec typeNameForDotCover (typ : Type) =
        if typ.IsGenericType then
            let args = typ.GetGenericArguments()
            let name = typ.GetGenericTypeDefinition().FullName.Replace("+", ".")
            let index = name.IndexOf("`")
            let trimmedName = if index >= 0 then name.Substring(0, index) else name
            sprintf "%s<%s>" trimmedName (args |> Array.map typeNameForDotCover |> join ",")
        elif typ.IsGenericParameter then typ.Name
        elif typ.IsByRef then
            typeNameForDotCover (typ.GetElementType())
        else
            let name = if typ.FullName <> null then typ.FullName else typ.Name
            name.Replace("+", ".") // Replace + in inner class names with dots

    let splitTypeName (typeName : string) =
        // NOTE: splitting inner classes
        let name = typeName.Split([|'+'|])
        // NOTE: deleting namespace from first class
        name.[0] <- Array.last (name.[0].Split([|'.'|]))
        name

    let rec declaringTypeName4Dotcover (typ : Type) =
        if typ.IsGenericType then
            let args = typ.GetGenericArguments()
            let names = typ.GetGenericTypeDefinition().FullName |> splitTypeName
            let handleGenericsInName (name : string) =
                let index = name.IndexOf("`")
                let trimmedName = if index >= 0 then name.Substring(0, index) else name
                sprintf "%s<%s>" trimmedName (args |> Array.map typeNameForDotCover |> join ",")
            names.[names.Length - 1] <- handleGenericsInName names.[names.Length - 1]
            names
        else typ.FullName |> splitTypeName

    let getCoverage (m : MethodInfo) =
        assert(Option.isSome expectedCoverage)
        if String.IsNullOrEmpty m.DeclaringType.Namespace then
            __notImplemented__() // TODO: does coverage report in this case omit the namespace?
        // TODO: we might also have inner classes and properties
        let assemblyName = m.Module.Assembly.GetName().Name
        let namespaceName = m.DeclaringType.Namespace
        let typeNames = declaringTypeName4Dotcover m.DeclaringType
        let typeSection = Array.map (fun t -> sprintf "/Type[@Name='%s']" t) typeNames |> join ""
        let nameOfParameter (p : ParameterInfo) = typeNameForDotCover p.ParameterType
        let parametersTypes = m.GetParameters() |> Seq.map nameOfParameter |> join ","
        let returnType = m.ReturnType |> typeNameForDotCover
        let methodName = sprintf "%s(%s):%s" m.Name parametersTypes returnType
        let xpath = sprintf "/Root/Assembly[@Name='%s']/Namespace[@Name='%s']%s/Method[@Name='%s']/@CoveragePercent" assemblyName namespaceName typeSection methodName
        let doc = XmlDocument()
        let docPath = sprintf "%s%cdotCover.Output.xml" testDir.FullName Path.DirectorySeparatorChar
        assert(File.Exists(docPath))
        doc.Load(docPath)
        let nodes = doc.DocumentElement.SelectNodes(xpath)
        match nodes.Count with
        | 0 -> raise (InvalidOperationException <| sprintf "Coverage results for %O not found!" m)
        | 1 ->
            let elem = nodes.[0]
            (elem :?> XmlAttribute).Value |> Int32.Parse
        | _ -> raise (InvalidOperationException <| sprintf "Invalid query of coverage results for %O!" m)

    member x.Check(methodInfo : MethodInfo, [<Out>] actualCoverage : Nullable<uint> byref) : bool =
        let success, error = run methodInfo testDir
        Logger.error "error output %O" error
        if not success then
            raise <| InvalidOperationException ("Running test results checker failed: " + error)
        match expectedCoverage with
        | Some expectedCoverage ->
            let coverage = getCoverage methodInfo
            actualCoverage <- Nullable(uint coverage)
            if coverage = expectedCoverage then true
            else
                resultMessage <- sprintf "Incomplete coverage! Expected %d, but got %d" expectedCoverage coverage
                false
        | None ->
            actualCoverage <- Nullable()
            true

    member x.ResultMessage = resultMessage
