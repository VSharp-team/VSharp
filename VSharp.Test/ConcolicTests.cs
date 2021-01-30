using System;
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
            Logger.current_text_writer = TestContext.Progress;
            var communicator = new Communicator(0);
            Assert.IsTrue(communicator.Connect());
        }
    }
}