namespace VSharp.CSharpUtils;

public static class JetBrainsDiagnosticsUtils
{
    [Implements("System.Void JetBrains.Diagnostics.Log..cctor()")]
    public static void LogStaticConstructor() { }

    [Implements("System.Boolean JetBrains.Diagnostics.LogEx.IsTraceEnabled(JetBrains.Diagnostics.ILog)")]
    public static bool LogIsTraceEnabled()
    {
        return false;
    }
}
