namespace VSharp.UnitTestStructureProposal;

public abstract class DirectoryUnitTestData: UnitTestData
{
    public abstract DirectoryInfo Directory {get;}

    private IEnumerable<UnitTest>? _unitTests;
    
    public override IEnumerable<UnitTest> UnitTests
    {
        get
        {
            if (_unitTests != null)
            {
                return _unitTests;
            }

            var di = Directory;
            var exists = di.Exists;
            Assert.True(exists);

            var vsts = di.GetFiles("*.vst");
            _unitTests = vsts
                .Select(x => UnitTest.Deserialize(x.FullName))
                .ToList();
            
            Assert.NotEmpty(_unitTests);

            return _unitTests;
        }
    }
}