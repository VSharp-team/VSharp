namespace VSharp.CSharpUtils
{
    public static class ThreadUtils
    {
        [Implements("System.Void System.Threading.Thread..cctor()")]
        public static void ThreadStaticConstructor() {}
    }
}
