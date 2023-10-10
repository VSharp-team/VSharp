// C# program to get number of ways
// to increase LCS by 1
using System;
using System.Collections.Generic;
using NUnit.Framework;
using VSharp.Test;

[TestSvmFixture, Category("Dataset")]
class LCS{
	
static int M = 26;

// Method returns total ways to increase
// LCS length by 1
	[TestSvm(5,serialize:"waysToIncreaseLCSBy1"), Category("Dataset")]
	public static int waysToIncreaseLCSBy1(String str1,
								String str2)
{
	int m = str1.Length, n = str2.Length;

	// Fill positions of each character in vector
	List<int>[] position = new List<int>[M];
	for(int i = 0; i < M; i++)
		position[i] = new List<int>();

	for(int i = 1; i <= n; i++)
		position[str2[i - 1] - 'a'].Add(i);

	int[,] lcsl = new int[m + 2, n + 2];
	int[,] lcsr = new int[m + 2, n + 2];

	// Initializing 2D array by 0 values
	for(int i = 0; i <= m + 1; i++)
		for(int j = 0; j <= n + 1; j++)
			lcsl[i, j] = lcsr[i, j] = 0;

	// Filling LCS array for prefix substrings
	for(int i = 1; i <= m; i++)
	{
		for(int j = 1; j <= n; j++)
		{
			if (str1[i - 1] == str2[j - 1])
				lcsl[i, j] = 1 + lcsl[i - 1, j - 1];
			else
				lcsl[i, j] = Math.Max(lcsl[i - 1, j],
									lcsl[i, j - 1]);
		}
	}

	// Filling LCS array for suffix substrings
	for(int i = m; i >= 1; i--)
	{
		for(int j = n; j >= 1; j--)
		{
			if (str1[i - 1] == str2[j - 1])
				lcsr[i, j] = 1 + lcsr[i + 1, j + 1];
			else
				lcsr[i, j] = Math.Max(lcsr[i + 1, j],
									lcsr[i, j + 1]);
		}
	}

	// Looping for all possible insertion
	// positions in first string
	int ways = 0;
	for(int i = 0; i <= m; i++)
	{
		
		// Trying all possible lower
		// case characters
		for(int d = 0; d < 26; d++)
		{
			
			// Now for each character, loop over same
			// character positions in second string
			for(int j = 0; j < position[d].Count; j++)
			{
				int p = position[d][j];

				// If both, left and right substrings make
				// total LCS then increase result by 1
				if (lcsl[i, p - 1] +
					lcsr[i + 1, p + 1] == lcsl[m, n])
					ways++;
			}
		}
	}
	return ways;
}

public static int LCSMain(String str1, String str2)
{
	return (waysToIncreaseLCSBy1(str1, str2));
}
}
