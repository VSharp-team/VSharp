using System;
using System.IO;
using System.Linq;
using System.Reflection;
using NUnit.Framework;
using VSharp.Concolic;
using VSharp.Core;
using VSharp.Interpreter.IL;

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
            string path = "/home/dvvrd/dev/vsharp/VSharp.ClrInteraction/TestProject.dll";
            string moduleName = path;
            Assembly assembly = Assembly.LoadFile(path);
            Module module = assembly.Modules.FirstOrDefault(m => m.FullyQualifiedName == moduleName);
            if (module == null)
                throw new InvalidOperationException("Could not resolve module!");
            int methodToken = 0x6000001;
            MethodBase method = module.ResolveMethod(methodToken);
            if (method == null)
                throw new InvalidOperationException("Could not resolve method!");

            var searcher = new CFASearcher();
            var interpreter = new MethodInterpreter(searcher);
            var machine = new ClientMachine(method, interpreter, API.Memory.EmptyState);
            Assert.IsTrue(machine.Spawn());
            while (machine.ExecCommand()) { }
        }
    }
}