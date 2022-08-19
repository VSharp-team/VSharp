using System;
using System.IO;
using System.Linq;
using LoanExam;
using NUnit.Framework;

namespace VSharp.Test.Tests.LoanExam;

public class TestGeneratorTests
{
    [Test]
    public void A()
    {
        TestGenerator.Cover(typeof(CreditCalculationService), 10);
    }

    [Test]
    public void B()
    {
        var di = new DirectoryInfo("./VSharp.tests.last");
        var exists = di.Exists;
        Assert.True(exists);

        var vsts = di.GetFiles("*.vst");
        Assert.Greater(vsts.Length, 0);
        var tis = vsts
            .Select(x => UnitTest.Deserialize(x.FullName))
            .ToList();

        Assert.AreEqual(vsts.Length, tis.Count);
    }
}