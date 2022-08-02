namespace IntegrationTests;

using VSharp.Test;

internal interface IDependence
{
    int F();
}

[TestSvmFixture]
internal class Mocking
{
    private IDependence _dependence = null;

    public Mocking(IDependence dep)
    {
        _dependence = dep;
    }

    [TestSvm(100)]
    public int ComputeWithDependence()
    {
        var x = _dependence.F();
        var y = _dependence.F();
        if (x > y)
            return 1;
        return 2;
    }
}
