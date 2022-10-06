namespace VSharp.CSharpUtils;

public class DiagnosticsUtils
{
    [Implements("System.Boolean System.Diagnostics.Debugger.get_IsAttached()")]
    private static bool DebuggerIsAttached()
    {
        return false;
    }
}
