using System.CommandLine;
using System.CommandLine.Invocation;

namespace VSharp.TestRenderer;

internal static class RendererProgram
{
    public static int RenderTests(string path, DirectoryInfo outputDir, bool wrapErrors)
    {
        if (Directory.Exists(path))
        {
            var testsDir = new DirectoryInfo(path);
            var tests = testsDir.EnumerateFiles("*.vst");
            var testsList = tests.ToList();
            if (testsList.Count > 0)
            {
                Renderer.Render(testsList, wrapErrors, outputDir:outputDir);
                return 0;
            }
            Console.Error.WriteLine("No *.vst tests found in {0}", testsDir.FullName);
            return 1;
        }

        if (File.Exists(path))
        {
            var file = new FileInfo(path);
            Renderer.Render(new[] {file}, wrapErrors, outputDir:outputDir);
            return 0;
        }

        return 1;
    }

    public static int Main(string[] args)
    {
        var testPathArgument =
            new Argument<string>("test-path", description: "Path to the tests (.vst)");
        var wrapErrorsOption =
            new System.CommandLine.Option("--wrap-errors", description: "Enables exception handling in error suites");
        var outputOption =
            new System.CommandLine.Option<DirectoryInfo>(aliases: new[] { "--output", "-o" },
                () => new DirectoryInfo(Directory.GetCurrentDirectory()),
                "Path where NUnit tests will be generated");

        var rootCommand = new RootCommand();

        rootCommand.AddArgument(testPathArgument);
        rootCommand.AddGlobalOption(outputOption);
        rootCommand.AddGlobalOption(wrapErrorsOption);

        rootCommand.Description = "V# test rendering tool. Accepts unit test in *.vst format, generates NUnit tests.";

        rootCommand.Handler = CommandHandler.Create<string, DirectoryInfo, bool>((testPath, output, wrapErrors) =>
        {
            RenderTests(testPath, output, wrapErrors);
        });

        return rootCommand.InvokeAsync(args).Result;
    }
}
