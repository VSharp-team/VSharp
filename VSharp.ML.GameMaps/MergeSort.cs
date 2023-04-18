// C# program for Merge Sort

using NUnit.Framework;
using VSharp.Test;

[TestSvmFixture, Category("Dataset")]
class MergeSort {

    // Merges two subarrays of []arr.
    // First subarray is arr[l..m]
    // Second subarray is arr[m+1..r]
    void merge(int[] arr, uint l, uint m, uint r)
    {
        // Find sizes of two
        // subarrays to be merged
        uint n1 = m - l + 1;
        uint n2 = r - m;

        // Create temp arrays
        int[] L = new int[n1];
        int[] R = new int[n2];
        int i, j;

        // Copy data to temp arrays
        for (i = 0; i < n1; ++i)
            L[i] = arr[l + i];
        for (j = 0; j < n2; ++j)
            R[j] = arr[m + 1 + j];

        // Merge the temp arrays

        // Initial indexes of first
        // and second subarrays
        i = 0;
        j = 0;

        // Initial index of merged
        // subarray array
        uint k = l;
        while (i < n1 && j < n2) {
            if (L[i] <= R[j]) {
                arr[k] = L[i];
                i++;
            }
            else {
                arr[k] = R[j];
                j++;
            }
            k++;
        }

        // Copy remaining elements
        // of L[] if any
        while (i < n1) {
            arr[k] = L[i];
            i++;
            k++;
        }

        // Copy remaining elements
        // of R[] if any
        while (j < n2) {
            arr[k] = R[j];
            j++;
            k++;
        }
    }

    // Main function that
    // sorts arr[l..r] using
    // merge()
    [TestSvm(100,serialize:"mergeSort"), Category("Dataset")]
    public void mergeSort(int[] arr, uint l, uint r)
    {
        if (l < r) {
            // Find the middle
            // point
            uint m = l + (r - l) / 2;

            // Sort first and
            // second halves
            mergeSort(arr, l, m);
            mergeSort(arr, m + 1, r);

            // Merge the sorted halves
            merge(arr, l, m, r);
        }
    }

    
    public static int[] MergeSortMain(int[] arr)
    {
        MergeSort ob = new MergeSort();
        ob.mergeSort(arr, 0, (uint)(arr.Length - 1));
        return arr;
    }
}
