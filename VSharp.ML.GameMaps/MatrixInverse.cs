// C# program to find adjoint and inverse of a matrix
using System;
using System.Collections.Generic;
using NUnit.Framework;
using VSharp.Test;

[TestSvmFixture, Category("Dataset")]
class MatrixInverse
{
	
static readonly int N = 4;

// Function to get cofactor of A[p,q] in [,]temp. n is current
// dimension of [,]A
public static void getCofactor(int [,]A, int [,]temp, int p, int q, int n)
{
	int i = 0, j = 0;

	// Looping for each element of the matrix
	for (int row = 0; row < n; row++)
	{
		for (int col = 0; col < n; col++)
		{
			// Copying into temporary matrix only those element
			// which are not in given row and column
			if (row != p && col != q)
			{
				temp[i, j++] = A[row, col];

				// Row is filled, so increase row index and
				// reset col index
				if (j == n - 1)
				{
					j = 0;
					i++;
				}
			}
		}
	}
}

/* Recursive function for finding determinant of matrix.
n is current dimension of [,]A. */
	[TestSvm(50,serialize:"determinant"), Category("Dataset")]
	public static int determinant(int [,]A, int n)
{
	int D = 0; // Initialize result

	// Base case : if matrix contains single element
	if (n == 1)
		return A[0, 0];

	int [,]temp = new int[N, N]; // To store cofactors

	int sign = 1; // To store sign multiplier

	// Iterate for each element of first row
	for (int f = 0; f < n; f++)
	{
		// Getting Cofactor of A[0,f]
		getCofactor(A, temp, 0, f, n);
		D += sign * A[0, f] * determinant(temp, n - 1);

		// terms are to be added with alternate sign
		sign = -sign;
	}
	return D;
}

// Function to get adjoint of A[N,N] in adj[N,N].
	[TestSvm(50,serialize:"adjoint"), Category("Dataset")]
	public static void adjoint(int [,]A, int [,]adj)
{
	if (N == 1)
	{
		adj[0, 0] = 1;
		return;
	}

	// temp is used to store cofactors of [,]A
	int sign = 1;
	int [,]temp = new int[N, N];

	for (int i = 0; i < N; i++)
	{
		for (int j = 0; j < N; j++)
		{
			// Get cofactor of A[i,j]
			getCofactor(A, temp, i, j, N);

			// sign of adj[j,i] positive if sum of row
			// and column indexes is even.
			sign = ((i + j) % 2 == 0)? 1: -1;

			// Interchanging rows and columns to get the
			// transpose of the cofactor matrix
			adj[j, i] = (sign) * (determinant(temp, N - 1));
		}
	}
}

// Function to calculate and store inverse, returns false if
// matrix is singular
	[TestSvm(5,serialize:"matrixInverse"), Category("Dataset")]
	public static bool matrixInverse(int [,]A, float [,]inverse)
{
	// Find determinant of [,]A
	int det = determinant(A, N);
	if (det == 0)
	{
		return false;
	}

	// Find adjoint
	int [,]adj = new int[N, N];
	adjoint(A, adj);

	// Find Inverse using formula "inverse(A) = adj(A)/det(A)"
	for (int i = 0; i < N; i++)
		for (int j = 0; j < N; j++)
			inverse[i, j] = adj[i, j]/(float)det;

	return true;
}

public static Tuple<int[,],float[,]> MatrixInverseMain(int[,] A)
{
	int [,]adj = new int[N, N]; // To store adjoint of [,]A

	float [,]inv = new float[N, N]; // To store inverse of [,]A

	adjoint(A, adj);

	if (matrixInverse(A, inv))
		return new Tuple<int[,], float[,]>(adj, inv);
	return null;
}
}
