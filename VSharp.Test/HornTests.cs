using JetBrains.ReSharper.FeaturesTestFramework.Daemon;
using NUnit.Framework;

namespace VSharp.Test
{
    [TestFixture]
    public sealed class HornTests : CSharpHighlightingTestBase
    {
        protected override string RelativeTestDataPath => "Horn";

        [Test]
        public void Test01()
        {
            // Just to load VSharp.Integration
            // TODO: Understand how to do it correctly
            new ZoneMarker();
            DoNamedTest();
        }
    }
}
