using System.Collections.Generic;
//Added to maps
namespace Advanced.Algorithms.Combinatorics;

/// <summary>
///     Combination generator (nCr).
/// </summary>
public class Combination
{
    public static List<List<T>> FindCombination<T>(List<T> n, int r, bool withRepetition = false)
    {
        var result = new List<List<T>>();

        FindCombinationRecurse(n, r, withRepetition, 0, new List<T>(), new HashSet<int>(), result);

        return result;
    }

    public static void FindCombinationRecurse<T>(List<T> n, int r, bool withRepetition,
        int k, List<T> prefix, HashSet<int> prefixIndices,
        List<List<T>> result)
    {
        if (prefix.Count == r)
        {
            result.Add(new List<T>(prefix));
            return;
        }

        for (var j = k; j < n.Count; j++)
        {
            if (prefixIndices.Contains(j) && !withRepetition) continue;

            prefix.Add(n[j]);
            prefixIndices.Add(j);

            FindCombinationRecurse(n, r, withRepetition, j, prefix, prefixIndices, result);

            prefix.RemoveAt(prefix.Count - 1);
            prefixIndices.Remove(j);
        }
    }
}