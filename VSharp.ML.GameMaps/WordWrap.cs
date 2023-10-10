// C# program for space
// optimized solution of
// Word Wrap problem.
using System;
using System.Text;
using NUnit.Framework;
using VSharp.Test;

[TestSvmFixture, Category("Dataset")]
class WordWrap
{

// Function to find space optimized
// solution of Word Wrap problem.
	[TestSvm(20,serialize:"solveWordWrap"), Category("Dataset")]
	public static List<Tuple<int, int>> solveWordWrap(int[] arr, int n, int k)
{
	int i, j;

	// Variable to store number of
	// characters in given line.
	int currlen;

	// Variable to store possible
	// minimum cost of line.
	int cost;

	// DP table in which dp[i]
	// represents cost of line
	// starting with word arr[i].
	int[] dp = new int[n];

	// Array in which ans[i] store
	// index of last word in line
	// starting with word arr[i].
	int[] ans = new int[n];

	// If only one word is present
	// then only one line is required.
	// Cost of last line is zero.
	// Hence cost of this line is zero.
	// Ending point is also n-1 as
	// single word is present.
	dp[n - 1] = 0;
	ans[n - 1] = n - 1;

	// Make each word first
	// word of line by iterating
	// over each index in arr.
	for (i = n - 2; i >= 0; i--)
	{
		currlen = -1;
		dp[i] = int.MaxValue;

		// Keep on adding words in
		// current line by iterating
		// from starting word upto
		// last word in arr.
		for (j = i; j < n; j++)
		{

			// Update number of characters
			// in current line. arr[j] is
			// number of characters in
			// current word and 1
			// represents space character
			// between two words.
			currlen += (arr[j] + 1);

			// If limit of characters
			// is violated then no more
			// words can be added to
			// current line.
			if (currlen > k)
			{
				break;
			}

			// If current word that is
			// added to line is last
			// word of arr then current
			// line is last line. Cost of
			// last line is 0. Else cost
			// is square of extra spaces
			// plus cost of putting line
			// breaks in rest of words
			// from j+1 to n-1.
			if (j == n - 1)
			{
				cost = 0;
			}
			else
			{
				cost = (k - currlen) *
					(k - currlen) + dp[j + 1];
			}

			// Check if this arrangement
			// gives minimum cost for
			// line starting with word
			// arr[i].
			if (cost < dp[i])
			{
				dp[i] = cost;
				ans[i] = j;
			}
		}
	}

	// Print starting index
	// and ending index of
	// words present in each line.
	List<Tuple<int, int>> result = new List<Tuple<int, int>>();
	i = 0;
	while (i < n)
	{
		result.Add(new Tuple<int, int>(i+1,ans[i] + 1));
		i = ans[i] + 1;
	}

	return result;
}

public static List<Tuple<int, int>> WordWrapMain(int[] arr, int M)
{
	int n = arr.Length;
	return solveWordWrap(arr, n, M);
}
}


[TestSvmFixture]
public class BaseConversion
{
    /// <summary>
    ///     Converts base of given number to the target base.
    /// </summary>
    /// <param name="srcNumber">Input number in source base system.</param>
    /// <param name="srcBaseChars">Source base system characters in increasing order. For example 0123456789 for base 10.</param>
    /// <param name="dstBaseChars">Destination base system characters in increasing order. For example 01 for base 2.</param>
    /// <param name="precision">Required precision when dealing with fractions. Defaults to 32 places.</param>
    /// <returns>The result in target base as a string.</returns>
    [TestSvm()]
    public static string BaseConvert(string srcNumber,
        string srcBaseChars,
        string dstBaseChars, int precision = 32)
    {
        srcNumber = srcNumber.Trim();
        if (srcNumber.Contains("."))
        {
            var tmp = srcNumber.Split('.');
            var whole = tmp[0].TrimEnd();
            var fraction = tmp[1].TrimStart();

            return ConvertWhole(whole, srcBaseChars, dstBaseChars) +
                   "." + ConvertFraction(fraction, srcBaseChars, dstBaseChars, precision);
        }

        return ConvertWhole(srcNumber, srcBaseChars, dstBaseChars);
    }

    /// <summary>
    ///     Converts the whole part of source number.
    /// </summary>
    private static string ConvertWhole(string srcNumber,
        string srcBaseChars,
        string dstBaseChars)
    {
        if (string.IsNullOrEmpty(srcNumber)) return string.Empty;

        var srcBase = srcBaseChars.Length;
        var dstBase = dstBaseChars.Length;

        if (srcBase <= 1) throw new Exception("Invalid source base length.");

        if (dstBase <= 1) throw new Exception("Invalid destination base length.");

        long base10Result = 0;
        var j = 0;
        //convert to base 10
        //move from least to most significant numbers
        for (var i = srcNumber.Length - 1; i >= 0; i--)
        {
            //eg. 1 * 2^0 
            base10Result += srcBaseChars.IndexOf(srcNumber[i])
                            * (long)Math.Pow(srcBase, j);
            j++;
        }

        var result = new StringBuilder();
        //now convert to target base
        while (base10Result != 0)
        {
            var rem = (int)base10Result % dstBase;
            result.Insert(0, dstBaseChars[rem]);
            base10Result = base10Result / dstBase;
        }

        return result.ToString();
    }

    /// <summary>
    ///     Converts the fractional part of source number.
    /// </summary>
    private static string ConvertFraction(string srcNumber,
        string srcBaseChars,
        string dstBaseChars, int maxPrecision)
    {
        if (string.IsNullOrEmpty(srcNumber)) return string.Empty;

        var srcBase = srcBaseChars.Length;
        var dstBase = dstBaseChars.Length;

        if (srcBase <= 1) throw new Exception("Invalid source base length.");

        if (dstBase <= 1) throw new Exception("Invalid destination base length.");

        decimal base10Result = 0;
        //convert to base 10
        //move from most significant numbers to least
        for (var i = 0; i < srcNumber.Length; i++)
            //eg. 1 * 1/(2^1) 
            base10Result += srcBaseChars.IndexOf(srcNumber[i])
                            * (decimal)(1 / Math.Pow(srcBase, i + 1));

        var result = new StringBuilder();
        //now convert to target base
        while (base10Result != 0 && maxPrecision > 0)
        {
            base10Result = base10Result * dstBase;
            result.Append(dstBaseChars[(int)Math.Floor(base10Result)]);
            base10Result -= Math.Floor(base10Result);
            maxPrecision--;
        }

        return result.ToString();
    }

    [TestSvmFixture]
    public class ManachersPalindrome
    {
	    [TestSvm()]
	    public int FindLongestPalindrome(string input)
	    {
		    if (input.Length <= 1) throw new ArgumentException("Invalid input");

		    if (input.Contains("$")) throw new Exception("Input contain sentinel character $.");

		    //for even length palindrome
		    //we need to do this hack with $
		    var array = input.ToCharArray();
		    var modifiedInput = new StringBuilder();

		    foreach (var item in array)
		    {
			    modifiedInput.Append("$");
			    modifiedInput.Append(item.ToString());
		    }

		    modifiedInput.Append("$");

		    var result = FindLongestPalindromeR(modifiedInput.ToString());

		    //remove length of $ sentinel
		    return result / 2;
	    }

	    /// <summary>
	    ///     Find the longest palindrome in linear time.
	    /// </summary>
	    private int FindLongestPalindromeR(string input)
	    {
		    var palindromeLengths = new int[input.Length];

		    int left = -1, right = 1;
		    var length = 1;

		    var i = 0;
		    //loop through each char
		    while (i < input.Length)
		    {
			    //terminate if end of input
			    while (left >= 0 && right < input.Length)
				    if (input[left] == input[right])
				    {
					    left--;
					    right++;
					    length += 2;
				    }
				    else
				    {
					    //end of current palindrome
					    break;
				    }

			    var @continue = false;

			    //set length of current palindrome
			    palindromeLengths[i] = length;

			    //use mirror values on left side of palindrome
			    //to fill palindrome lengths on right side of palindrome
			    //so that we can save computations
			    if (right > i + 2)
			    {
				    var l = i - 1;
				    var r = i + 1;

				    //start from current palindrome center
				    //all the way to right end of current palindrome
				    while (r < right)
				    {
					    //find mirror char palindrome length
					    var mirrorLength = palindromeLengths[l];

					    //mirror palindrome left end exceeds
					    //current palindrom left end
					    if (l - mirrorLength / 2 < left + 1)
					    {
						    //set length equals to maximum
						    //we can reach and then continue exploring
						    palindromeLengths[r] = 2 * (l - (left + 1)) + 1;
						    r++;
						    l--;
					    }
					    //mirror palindrome is totally contained
					    //in our current palindrome
					    else if (l - mirrorLength / 2 > left + 1
					             && r + mirrorLength / 2 < right - 1)
					    {
						    //so just set the value and continue exploring
						    palindromeLengths[r] = palindromeLengths[l];
						    r++;
						    l--;
					    }
					    //mirror palindrome exactly fits inside right side
					    //of current palindrome
					    else
					    {
						    //set length equals to maximum
						    //and then continue exploring in main loop
						    length = palindromeLengths[l];

						    //continue to main loop
						    //update state values to skip
						    //already computed values
						    i = r;
						    left = i - length / 2 - 1;
						    right = i + length / 2 + 1;

						    @continue = true;
						    break;
					    }
				    }

				    //already computed until i-1 by now
				    i = r;
			    }

			    //continue to main loop
			    //states values are already set
			    if (@continue) continue;

			    //reset as usual
			    left = i;
			    right = i + 2;
			    length = 1;

			    i++;
		    }

		    return FindMax(palindromeLengths);
	    }

	    /// <summary>
	    ///     Returns the max index in given int[] array.
	    /// </summary>
	    private int FindMax(int[] palindromeLengths)
	    {
		    return palindromeLengths.Concat(new[] { int.MinValue }).Max();
	    }
    }
}