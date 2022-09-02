namespace VSharp

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open System.Text
open System.Xml

// NOTE: if 'expectedCoverage' is null 'TestResultsChecker' runs all tests and checks results equality
//       otherwise, it compares expected coverage to computed coverage of tests
type TestResultsChecker(testDir : DirectoryInfo, runnerDir : string, expectedCoverage : Nullable<int>) =
    let expectedCoverage = Option.ofNullable expectedCoverage
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

    let run =
        match expectedCoverage with
        | Some _ ->
            let _, localTools, _ = runDotnet "tool list"
            let globalArg =
                if localTools.Contains "dotcover" then ""
                else
                    let _, globalTools, _ = runDotnet "tool list -g"
                    if globalTools.Contains "dotcover" then "-g"
                    else raise (InvalidOperationException "JetBrains.DotCover tool not found! Either install it locally by running 'dotnet tool restore' in build directory, or globally by running 'dotnet tool install JetBrains.dotCover.GlobalTool -g'")
            fun (directory : DirectoryInfo) ->
                let filters = ["-:module=Microsoft.*"; "-:module=FSharp.*"; "-:class=VSharp.*"; "-:module=VSharp.Utils"]
                let code, _, error = runDotnet <| sprintf "dotcover --dcFilters=\"%s\" %s%cVSharp.TestRunner.dll %s --dcReportType=DetailedXML %s" (filters |> join ";") runnerDir Path.DirectorySeparatorChar directory.FullName globalArg
                code = 0, error
        | None ->
            fun (directory : DirectoryInfo) ->
                let code, _, error = runDotnet <| sprintf " %s%cVSharp.TestRunner.dll %s" runnerDir Path.DirectorySeparatorChar directory.FullName
                code = 0, error

    let rec typeName4Dotcover (typ : Type) =
        if typ.IsGenericType then
            let args = typ.GetGenericArguments()
            let name = typ.GetGenericTypeDefinition().FullName.Replace("+", ".")
            let index = name.IndexOf("`")
            let trimmedName = if index >= 0 then name.Substring(0, index) else name
            sprintf "%s<%s>" trimmedName (args |> Array.map typeName4Dotcover |> join ",")
        elif typ.IsGenericParameter then typ.Name
        else typ.FullName.Replace("+", ".") // Replace + in inner class names with dots

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
                sprintf "%s<%s>" trimmedName (args |> Array.map typeName4Dotcover |> join ",")
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
        let nameOfParameter (p : ParameterInfo) = typeName4Dotcover p.ParameterType
        let parametersTypes = m.GetParameters() |> Seq.map nameOfParameter |> join ","
        let returnType = m.ReturnType |> typeName4Dotcover
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
        let success, error = run testDir
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
