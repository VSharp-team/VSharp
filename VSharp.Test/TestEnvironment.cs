using JetBrains.Application.BuildScript.Application.Zones;
using JetBrains.ReSharper.TestFramework;
using JetBrains.TestFramework;
using JetBrains.TestFramework.Application.Zones;
using NUnit.Framework;

[assembly: RequiresSTA]
[assembly: TestDataPathBase("data")]

namespace VSharp.Test
{
    [ZoneDefinition]
    public class TestEnvironmentZone : ITestsZone, IRequire<PsiFeatureTestZone>
    {
    }

    [SetUpFixture]
    public class ReSharperTestEnvironmentAssembly : ExtensionTestEnvironmentAssembly<TestEnvironmentZone>
    {
    }
}