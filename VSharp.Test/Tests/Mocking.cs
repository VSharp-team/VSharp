using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using VSharp.Test;

namespace IntegrationTests;

public interface IDependence
{
    int F();
}

public interface IDependence2
{
    int X { get; set; }
}

public interface INetwork
{
    int Read();
}

public interface IFoo
{
    int Foo();
}

public interface IBar : IFoo
{
    int Bar();
}

[TestSvmFixture]
public class Mocking
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

    [TestSvm(100)]
    public int ComputeWithDependence2(IDependence2 input)
    {
        if (input.X == 0)
            return 0;

        if (input.X > 0)
            throw new NullReferenceException("explicit");

        input.X = -input.X;

        return input.X;
    }

    [TestSvm(100)]
    public int ReadAllFromNetwork([DisallowNull] byte[] buffer, [DisallowNull] INetwork network)
    {
        int next;
        int count = 0;
        while ((next = network.Read()) >= 0)
        {
            buffer[count++] = (byte)next;
        }

        return count;
    }

    [TestSvm(100)]
    public bool Enumerable(IEnumerable<int> enumerable)
    {
        var enumerator = enumerable.GetEnumerator();
        return enumerator.MoveNext();
    }

    [TestSvm(100)]
    public bool InterfaceInheritance1(IBar iBar)
    {
        if (iBar.Foo() > 0)
        {
            return true;
        }

        return false;
    }

    [TestSvm(100)]
    public bool InterfaceInheritance2(IBar iBar)
    {
        if (iBar.Bar() > 0)
        {
            return true;
        }

        return false;
    }
}
