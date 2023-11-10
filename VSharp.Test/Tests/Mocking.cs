using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using NUnit.Framework;
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

public interface INum
{
    int GetNum();
}

public interface INum2 : INum
{
    int GetNum2();
}

public class NumImpl1 : INum
{
    public int GetNum()
    {
        return 1;
    }
}

public class NumImpl2 : INum
{
    public int GetNum()
    {
        return 2;
    }
}

public class NumImpl3 : INum2
{
    public int GetNum()
    {
        return 2;
    }

    public int GetNum2()
    {
        return 4;
    }
}

public class A
{
    public virtual int GetNum()
    {
        return 2;
    }
}

public class B : A
{
    public override int GetNum()
    {
        return 3;
    }
}

public class C : B
{
    public override int GetNum()
    {
        return 4;
    }
}

public enum MockEnum
{
    Foo,
    Bar,
    Fizz,
    Buzz
}

public interface IEnumMock
{
    MockEnum Get();
}

public interface IIntPtrMock
{
    IntPtr Get();
}

public interface IEnumPtrMock
{
    unsafe MockEnum* Get();
}

public interface IInterface
{
    int GetInt() => 42;

    object GetObj() => GetInt();
}

public class Derived : IInterface
{
    public int X;
    public object GetObj()
    {
        return "string";
    }
}

public interface IOutMock
{
    void Get(out int i);
}

public interface IOutMock1
{
    void Get(out Derived i);
}

public struct MyStruct
{
    public int X;
}

public interface IOutMock2
{
    void Get(out MyStruct i);
}

public interface IOutReturnMock
{
    int Get(out int i);
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
    public int ComputeWithDependence3()
    {
        if (!ReferenceEquals(this, _dependence))
            return 3;

        var x = _dependence.F();
        var y = _dependence.F();
        if (x > y)
            return 1;
        return 2;
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
    public bool? Enumerable2(IEnumerable<int> enumerable)
    {
        var enumerator = enumerable.GetEnumerator();
        if (ReferenceEquals(enumerable, enumerator))
            return null;
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

    [TestSvm(100)]
    public int BranchInterface(INum num)
    {
        var n = num.GetNum();

        if (num is NumImpl1)
        {
            n += 11;
            return n;
        }

        if (num is NumImpl2)
        {
            n += 12;
            return n;
        }

        if (n == 199)
        {
            n += 11;
        }

        return n;
    }

    [TestSvm(100)]
    public int ItemMock(IList<int> list)
    {
        if (list[1] == 0)
        {
            return 1;
        }
        return 0;
    }

    [Ignore("multiple mocks with same method are not supported")]
    public int BranchInterface2(INum num, INum num1)
    {
        var n = num.GetNum();
        var n1 = num1.GetNum();

        if (num is INum2 num2 && num1 is INum2 num3)
        {
            n += num2.GetNum2();
            n1 += num3.GetNum2();
            n += num2.GetNum();
            n1 += num3.GetNum();
            if (num == num1)
                return n * n1;
            if (n > n1)
                return n - n1;
            return n + n1;
        }

        if (num is INum2 num4)
        {
            n += num4.GetNum2();
            return n;
        }

        if (num1 is INum2 num5)
        {
            n1 += num5.GetNum2();
            return n1;
        }

        return n;
    }

    [Ignore("multiple mocks with same method are not supported")]
    public int BranchInterface3(INum num, INum num1)
    {
        if (num != num1)
        {
            var n = num.GetNum();
            var n1 = num1.GetNum();

            if (num is INum2 num2 && num1 is INum2 num3)
            {
                n += num2.GetNum2();
                n1 += num3.GetNum2();
                n += num2.GetNum();
                n1 += num3.GetNum();
                if (n > n1)
                    return n - n1;
                return n + n1;
            }

            if (num is INum2 num4)
            {
                n += num4.GetNum2();
                return n;
            }

            if (num1 is INum2 num5)
            {
                n1 += num5.GetNum2();
                return n1;
            }

            return n;
        }

        return 0;
    }

    [Ignore("multiple mocks with same method are not supported")]
    public static int MultipleMocks(A a, A b)
    {
        if (a != b)
        {
            if (a.GetNum() > 10 && b.GetNum() == 14)
                return 1;
        }

        return 0;
    }

    [Ignore("multiple mocks with same method are not supported")]
    public static int MultipleMocks2(A a, A b)
    {
        if (a != b && a is B)
        {
            if (a.GetNum() > 10 && b.GetNum() == 14)
                return 1;
        }

        return 0;
    }

    [TestSvm(100)]
    public int EnumMock(IEnumMock enumMock)
    {
        if (enumMock.Get() == MockEnum.Bar)
        {
            return 1;
        }

        if (enumMock.Get() == MockEnum.Foo)
        {
            return 2;
        }

        if (enumMock.Get() == MockEnum.Fizz)
        {
            return 3;
        }

        return 4;
    }

    [TestSvm(100)]
    public int IntPtrMock(IIntPtrMock mock)
    {
        var value = mock.Get();

        if (value == IntPtr.MaxValue)
        {
            return 1;
        }

        return 2;
    }

    [TestSvm(100)]
    public unsafe int EnumPtrMock(IEnumPtrMock mock)
    {
        var value = mock.Get();

        if (value == (MockEnum*)5)
        {
            return 1;
        }

        return 2;
    }

    [TestSvm(100)]
    public int DefaultImplTest(IInterface i)
    {
        var value = i.GetObj();

        if (i is Derived)
        {
            if ((string)value == "string")
                return 2;
            return -1;
        }
        if (value is int n)
        {
            return n;
        }

        if ((string)value == "string")
        {
            return 1;
        }

        return 0;
    }

    [TestSvm(100)]
    public int OutMock(IOutMock mock)
    {
        var i = 322;
        mock.Get(out i);

        return i;
    }

    [TestSvm(100)]
    public Derived OutMock1(IOutMock1 mock)
    {
        var i = new Derived
        {
            X = 19
        };
        mock.Get(out i);

        return i;
    }

    [TestSvm(100)]
    public MyStruct OutMock2(IOutMock2 mock)
    {
        var i = new MyStruct
        {
            X = 19
        };
        mock.Get(out i);

        return i;
    }

    [TestSvm]
    public int OutMockWithReturn(IOutReturnMock mock)
    {
        var i = 16;
        var j = mock.Get(out i);

        if (j != 0)
            return 0;
        return i;
    }
}
