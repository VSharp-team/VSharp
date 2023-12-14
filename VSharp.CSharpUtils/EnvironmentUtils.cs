namespace VSharp.CSharpUtils;

public static class EnvironmentUtils
{
    [Implements("System.Void System.Environment..cctor()")]
    public static void EnvironmentStaticConstructor() { }

    [Implements("System.String System.IO.Path.GetFullPathInternal(System.String)")]
    public static string GetFullPathInternal(string s)
    {
        // TODO: implement fully
        return s;
    }
}
