using System.Collections.Generic;
using System.Linq;
using JetBrains.Application.platforms;
using JetBrains.ReSharper.FeaturesTestFramework.Daemon;
using JetBrains.Util;
using NUnit.Framework;

namespace VSharp.Test
{
    [TestFixture]
    public sealed class HornTests : CSharpHighlightingTestBase
    {
        protected override string RelativeTestDataPath => "Horn";

        protected override IEnumerable<string> GetReferencedAssemblies(PlatformID platformId)
        {
            var result = base.GetReferencedAssemblies(platformId).Concat(System.Reflection.Assembly.Load("VSharp.CSharpUtils").Location).ToList();
            System.Console.WriteLine("RESULT IS: " + string.Join("\n", result));
            return result;
        }

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
