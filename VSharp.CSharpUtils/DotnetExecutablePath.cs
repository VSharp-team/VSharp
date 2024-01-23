namespace VSharp.CSharpUtils;

public static class DotnetExecutablePath
{
    public static string ExecutablePath { get; private set; } = "dotnet";

    public static void OverridePath(string newPath)
    {
        ExecutablePath = newPath;
    }
}
