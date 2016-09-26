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
            var result = base.GetReferencedAssemblies(platformId).Concat(new[] { "VSharp.CSharpUtils" }).ToList();
            System.Console.WriteLine("RESULT IS: " + string.Join("\n", result));
            return result;
        }

        [Test]
        public void Test01()
        {
            // Just to load VSharp.Integration
            // TODO: Understand how to do it correctly
            new ZoneMarker();
            CSharpUtils.Tests.Arithmetics.Add7(35, 0);
            DoNamedTest();
        }
    }
}
