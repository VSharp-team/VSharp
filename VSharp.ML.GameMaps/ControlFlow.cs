namespace VSharp.ML.GameMaps;

public class ControlFlow
{
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
}