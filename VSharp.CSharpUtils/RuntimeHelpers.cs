namespace VSharp.CSharpUtils
{
    public static class RuntimeHelpersUtils
    {
        [Implements("System.Void System.Runtime.CompilerServices.RuntimeHelpers._RunClassConstructor(System.RuntimeType)")]
        public static void _RunClassConstructor(System.Type t) {} // TODO: need to initialize statics of t #do
    }
}
