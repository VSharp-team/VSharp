using System.Collections;

namespace VSharp.UnitTestStructureProposal;

public abstract class UnitTestData : IEnumerable<object[]>
{
    public abstract IEnumerable<UnitTest> UnitTests { get; }
    public IEnumerator<object[]> GetEnumerator()
    {
        var unitTests = UnitTests;

        foreach (var ut in unitTests)
        {
            if (ut.Args == null || ut.Exception != null)
            {
                continue;
            }
            
            yield return CreateArgs(ut);
        }
    }

    private object[] CreateArgs(UnitTest unitTest)
    {
        return unitTest
            .Args
            .Append(unitTest.Expected)
            .ToArray();
    }

    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
}