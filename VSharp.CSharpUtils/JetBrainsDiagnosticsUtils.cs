namespace VSharp.CSharpUtils;

public static class JetBrainsDiagnosticsUtils
{

    [Implements("System.Boolean JetBrains.Diagnostics.LogEx.IsTraceEnabled(JetBrains.Diagnostics.ILog)")]
    public static bool LogIsTraceEnabled()
    {
        return false;
    }
}
