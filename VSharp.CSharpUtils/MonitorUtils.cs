namespace VSharp.CSharpUtils
{
    public static class Monitor
    {
        [Implements("System.Void System.Threading.Monitor.Exit(System.Object)")]
        public static void Exit(object o) { }
    }
}
