using System.Diagnostics;

namespace VSharp.TestRenderer;

internal static class RendererProgram
{
  public static int Main(string[] args)
    {
        Debug.Assert(args.Length == 1);
        string path = args[0];
        if (Directory.Exists(path))
        {
            var testsDir = new DirectoryInfo(path);
            var tests = testsDir.EnumerateFiles("*.vst");
            var testsList = tests.ToList();
            if (testsList.Count > 0)
            {
                Renderer.RenderTests(testsList);
                return 0;
            }
            Console.Error.WriteLine("No *.vst tests found in {0}", testsDir.FullName);
            // throw new ArgumentException();
            return 1;
        }

        if (File.Exists(path))
        {
            var file = new FileInfo(path);
            Renderer.RenderTests(new[] {file});
            return 0;
        }

        // throw new ArgumentException();
        return 1;
    }
}
