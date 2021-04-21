namespace VSharp.CSharpUtils
{
    public static class Monitor
    {
        [Implements("System.Void System.Threading.Monitor.ReliableEnter(System.Object, System.Boolean&)")]
        public static void ReliableEnter(object o, out bool success)
        {
            success = true;
        }
    }
}