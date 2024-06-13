using System;
using System.Collections.Generic;
using NUnit.Framework;
using VSharp.Test;

namespace IntegrationTests;

[TestSvmFixture]
class SplittingTest
{
    [TestSvm(94)]
    public static int TestSolvingCopyOverwrittenValueUnreachable1(string[] a, string[] b)
    {
        if (a != null && b != null && a.Length > b.Length)
        {
            a[0] = "42";
            b[0] = "4";
            Array.Copy(a, 0, b, 0, b.Length);
            if (b[0] != "42") // unreachable
            {
                return -1;
            }

            return 0;
        }

        return 3;
    }

    [TestSvm(95)]
    public static int TestSolvingCopyOverwrittenValueUnreachable2(string[] a, int i, string[] b)
    {
        if (a != null && b != null && a.Length > b.Length)
        {
            b[i] = "500";
            Array.Copy(a, 0, b, 0, b.Length);
            if (b.Length > 0 && a[i] != "500" && b[i] == "500") // unreachable
            {
                return -1;
            }

            return 0;
        }

        return 3;
    }

    [TestSvm(94)]
    public static int TestSolvingCopy8(object[] a, object[] b, int i)
    {
        if (a.Length > b.Length && 0 <= i && i < b.Length)
        {
            Array.Fill(a, "abc");
            Array.Copy(a, b, b.Length);

            if (b[i] == b[i + 1])
                return 42;
            return 10;
        }

        return 3;
    }

    [TestSvm(88)]
    public static int LastRecordReachability(string[] a, string[] b, int i, string s)
    {
        a[i] = "1";
        b[1] = s;
        if (b[1] != s)
        {
            // unreachable
            return -1;
        }

        return 0;
    }

    [TestSvm(100)]
    public static int ConcreteDictionaryTest1(int a, string b)
    {
        var d = new Dictionary<int, List<string>>();
        d.Add(1, new List<string> { "2", "3" });
        d.Add(4, new List<string> { "5", "6" });
        if (d.TryGetValue(a, out var res))
        {
            if (res.Contains(b))
                return 1;
            return 0;
        }

        return 2;
    }

    public static int IrrelevantRecordsAreUnreachable(int i, string s)
    {
        var a = new string[] { "a", "b", "c", "d" };
        a[2] = "323";
        a[i] = s;
        a[1] = "1";
        if (a[i] != "1" && a[i] != s)
        {
            return -1;
        }

        return 1;
    }

    public class Person
    {
        public string FirstName { get; set; }
        public string LastName { get; init; }
        public override int GetHashCode()
        {
            return 800;
        }
    };
        
    [TestSvm(95)]
    public static int IteKeyWrite(int i)
    {
        var a = new Person[4];
        a[0] = new Person() {FirstName = "asd", LastName = "qwe"};
        a[3] = new Person() {FirstName = "zxc", LastName = "vbn"};
        var p = a[i];
        p.FirstName = "323";
        if (i == 0 && a[3].FirstName == "323")
        {
            // unreachable
            return -1;
        }

        return 1;
    }
    
    [TestSvm(96)]
    public static int IteValueWrite(int i, int j)
    {
        var a = new string[] { "a", "b", "c", "d" };
        a[j] = a[i];
        if (a[j] == "a") return 0;
        if (a[j] == "b") return 1;
        if (a[j] == "c") return 2;
        if (a[j] == "d") return 3;
        // unreachable
        return -1;
    }
    
    [TestSvm(95)]
    public static int TestSplittingWithCopy(int srcI, int dstI, int len, int i)
    {
        string[] arr = {"a", "b", "c", "d", "e"};
        var a = new string[5];
        Array.Copy(arr, srcI, a, dstI, len);
        if (a[i].Length != 1)
            return -1;
        return 1;
    }
    [TestSvm(91)]
    public static int TestSplittingWithCopyRegionsImportance(string[] a, int i)
    {
        a[i] = "a";
        a[1] = "b";
        var b = new string[a.Length];
        Array.Copy(a, 0, b, 0, a.Length);
        if (i == 1 && b[i] == "a") // unreachable
            // record b[i], "a" exists only if i != 1
            return -1;
        if (i != 1 && b[i] != "a") // unreachable
        {
            return -2;
        }

        return 1;
    }
    
    [TestSvm(93)]
    public static int ExistingKeyReading(string s1, string s2, int i)
    {
        var a = new string[10];
        a[i] = s1;
        a[2] = s2;
        if (a[i] == s2) return 1;
        if (a[i] == s1) return 0;
        return -1;
    }

    [Ignore("Implement branching with solver interaction on callVirt")]
    public static int CallVirtIte(int i)
    {
        var a = new object[] { "aasd", 500, new Person() };
        var r = a[i].GetHashCode();
        if (r == 500) return 1;
        if (r == 800) return 2;
        return 0;
    }
}
