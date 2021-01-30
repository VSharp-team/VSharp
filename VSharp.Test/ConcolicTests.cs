using System.IO;
using System.Reflection;
using NUnit.Framework;
using VSharp.Concolic;

namespace VSharp.Test
{
    [TestFixture]
    public class CommunicationTest
    {
        [Test]
        public void TestHandshake()
        {
            var writer = new StreamWriter("dotnet_lastrun", false);
            writer.AutoFlush = false;
            Logger.current_text_writer = writer;
            Assembly assembly = Assembly.LoadFile("/home/dvvrd/dev/vsharp/VSharp.ClrInteraction/TestProject.dll");
            var machine = new ClientMachine(assembly, null);
            Assert.IsTrue(machine.Spawn());
            while (machine.ExecCommand()) { }
        }
    }
}