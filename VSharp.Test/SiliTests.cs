using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace VSharp.Test
{
    [TestClass]
    public sealed class SiliTests
    {
        [TestMethod]
        public void RunCSharpTests()
        {
            SVM.Run(System.Reflection.Assembly.Load("VSharp.CSharpUtils"));
        }
    }
}
