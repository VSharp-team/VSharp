using NUnit.Framework;
using VSharp.Test;

namespace VSharp.ML.GameMaps;

[TestSvmFixture, Category("Dataset")]
public class ControlFlow
{
    [TestSvm(100,serialize:"BinarySearch"), Category("Dataset")]
    public static int BinarySearch(int[] a, int x, int lo, int hi)
    {
        if (a == null) throw new ArgumentException("a == null");

        if (lo < 0) throw new ArgumentException("lo < 0");
        if (lo > hi) throw new ArgumentException("lo > hi");

        var m = lo + (hi - lo) / 2;

        while (lo < hi)
            if (a[m] == x)
                return m;
            else if (a[m] > x)
                hi = m;
            else
                lo = m + 1;

        return -1;
    }
    
    public static int Switches1(int x, int y, int z)
    {
        var sum = 0;
        switch (x % 10)
        {
            case 0:
                sum += 1;
                break;
            case 1:
                sum += 1;
                break;
            case 2:
                sum += 1;
                break;
            case 3:
                sum += 1;
                break;
            case 4:
                sum += 1;
                break;
            case 5:
                sum += 2;
                break;
            case 6:
                sum += 2;
                break;
            case 7:
                sum += 2;
                break;
            case 8:
                sum += 2;
                break;
            case 9:
                sum += 2;
                break;
        }
        
        switch (y % 10)
        {
            case 0:
                sum += 1;
                break;
            case 1:
                sum += 1;
                break;
            case 2:
                sum += 1;
                break;
            case 3:
                sum += 1;
                break;
            case 4:
                sum += 1;
                break;
            case 5:
                sum += 2;
                break;
            case 6:
                sum += 2;
                break;
            case 7:
                sum += 2;
                break;
            case 8:
                sum += 2;
                break;
            case 9:
                sum += 2;
                break;
        }
        
        switch (z % 10)
        {
            case 0:
                sum += 1;
                break;
            case 1:
                sum += 1;
                break;
            case 2:
                sum += 1;
                break;
            case 3:
                sum += 1;
                break;
            case 4:
                sum += 1;
                break;
            case 5:
                sum += 2;
                break;
            case 6:
                sum += 2;
                break;
            case 7:
                sum += 2;
                break;
            case 8:
                sum += 2;
                break;
            case 9:
                sum += 2;
                break;
        }
        
        return sum;
    }

    public static string Switches2(int x)
    {
        List<string> numbers = new List<string>();
        int val = x;
        while (val != 0)
        {
            switch (val % 10)
            {
                case 0:
                    numbers.Add("Zero");
                    break;
                case 1:
                    numbers.Add("One");
                    break;
                case 2:
                    numbers.Add("Two");
                    break;
                case 3:
                    numbers.Add("Three");
                    break;
                case 4:
                    numbers.Add("Four");
                    break;
                case 5:
                    numbers.Add("Five");
                    break;
                case 6:
                    numbers.Add("Six");
                    break;
                case 7:
                    numbers.Add("Seven");
                    break;
                case 8:
                    numbers.Add("Eight");
                    break;
                case 9:
                    numbers.Add("Nine");
                    break;
            }

            numbers.Add("; ");
            val = val / 10;
        }

        return String.Concat(numbers) ;
    }

    [TestSvm(100,serialize:"NestedFors"), Category("Dataset")]
    public static int NestedFors(int x)
    {
        int sum = 0;
        for (int i = 1; i <= x; i++)
        {
            for (int j = 1; j <= i; j++)
            {
                for (int k = 1; k <= j; k++)
                {
                    sum += k;
                }
            }
        }

        return sum;
    }
}