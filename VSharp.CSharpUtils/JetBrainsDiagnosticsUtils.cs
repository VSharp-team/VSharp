namespace VSharp.CSharpUtils;

public static class JetBrainsDiagnosticsUtils
{
    [Implements("System.Void JetBrains.Diagnostics.Log..cctor()")]
    public static void LogStaticConstructor() { }
}
