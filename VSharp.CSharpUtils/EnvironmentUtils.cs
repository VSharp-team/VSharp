namespace VSharp.CSharpUtils;

public static class EnvironmentUtils
{
    [Implements("System.Void System.Environment..cctor()")]
    public static void EnvironmentStaticConstructor() { }
}
