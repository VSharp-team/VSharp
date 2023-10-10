using NUnit.Framework;
using VSharp.Test;

[TestSvmFixture, Category("Dataset")]
class GFG {

    // return the maximum
    // element from the array
    static int getMax(int[] arr, int n)
    {
        int max = int.MinValue;
        for (int i = 0; i < n; i++)
            if (arr[i] > max)
                max = arr[i];
        return max;
    }

    // return the sum of the
    // elements in the array
    static int getSum(int[] arr, int n)
    {
        int total = 0;
        for (int i = 0; i < n; i++)
            total += arr[i];
        return total;
    }

    // find minimum required
    // painters for given
    // maxlen which is the
    // maximum length a painter
    // can paint
    static int numberOfPainters(int[] arr,
        int n, int maxLen)
    {
        int total = 0, numPainters = 1;

        for (int i = 0; i < n; i++) {
            total += arr[i];

            if (total > maxLen) {

                // for next count
                total = arr[i];
                numPainters++;
            }
        }

        return numPainters;
    }
    
    [TestSvm(20,serialize:"bsPartition"), Category("Dataset")]
    static public int bsPartition(int[] arr,
        int n, int k)
    {
        int lo = getMax(arr, n);
        int hi = getSum(arr, n);

        while (lo < hi) {
            int mid = lo + (hi - lo) / 2;
            int requiredPainters = numberOfPainters(arr, n, mid);

            // find better optimum in lower
            // half here mid is included
            // because we may not get
            // anything better
            if (requiredPainters <= k)
                hi = mid;

            // find better optimum in upper
            // half here mid is excluded
            // because it gives required
            // Painters > k, which is invalid
            else
                lo = mid + 1;
        }

        // required
        return lo;
    }
    
    static public int BinSearchMain(int[] arr, int k)
    {
        int n = arr.Length;
        return (bsPartition(arr, n, k));
    }
}