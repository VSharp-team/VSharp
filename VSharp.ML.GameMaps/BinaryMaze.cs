// C# program to find the shortest
// path between a given source cell
// to a destination cell.
using System;
using System.Collections.Generic;
using NUnit.Framework;
using VSharp.Test;

[TestSvmFixture, Category("Dataset")]
class BinaryMaze1
{
static int ROW = 9;
static int COL = 10;

// To store matrix cell coordinates
public class Point
{
	public int x;
	public int y;

	public Point(int x, int y)
	{
		this.x = x;
		this.y = y;
	}
};

// A Data Structure for queue used in BFS
public class queueNode
{
	// The coordinates of a cell
	public Point pt;
	
	// cell's distance of from the source
	public int dist;

	public queueNode(Point pt, int dist)
	{
		this.pt = pt;
		this.dist = dist;
	}
};

// check whether given cell (row, col)
// is a valid cell or not.
static bool isValid(int row, int col)
{
	// return true if row number and
	// column number is in range
	return (row >= 0) && (row < ROW) &&
		(col >= 0) && (col < COL);
}

// These arrays are used to get row and column
// numbers of 4 neighbours of a given cell
static int []rowNum = {-1, 0, 0, 1};
static int []colNum = {0, -1, 1, 0};

// function to find the shortest path between
// a given source cell to a destination cell.
	[TestSvm(70,serialize:"BinaryMaze1BFS"), Category("Dataset")]
	public static int BinaryMaze1BFS(int [,]mat, Point src,
						Point dest)
{
	// check source and destination cell
	// of the matrix have value 1
	if (mat[src.x, src.y] != 1 ||
		mat[dest.x, dest.y] != 1)
		return -1;

	bool [,]visited = new bool[ROW, COL];
	
	// Mark the source cell as visited
	visited[src.x, src.y] = true;

	// Create a queue for BFS
	Queue<queueNode> q = new Queue<queueNode>();
	
	// Distance of source cell is 0
	queueNode s = new queueNode(src, 0);
	q.Enqueue(s); // Enqueue source cell

	// Do a BFS starting from source cell
	while (q.Count != 0)
	{
		queueNode curr = q.Peek();
		Point pt = curr.pt;

		// If we have reached the destination cell,
		// we are done
		if (pt.x == dest.x && pt.y == dest.y)
			return curr.dist;

		// Otherwise dequeue the front cell
		// in the queue and enqueue
		// its adjacent cells
		q.Dequeue();

		for (int i = 0; i < 4; i++)
		{
			int row = pt.x + rowNum[i];
			int col = pt.y + colNum[i];
			
			// if adjacent cell is valid, has path
			// and not visited yet, enqueue it.
			if (isValid(row, col) &&
					mat[row, col] == 1 &&
			!visited[row, col])
			{
				// mark cell as visited and enqueue it
				visited[row, col] = true;
				queueNode Adjcell = new queueNode
						(new Point(row, col),
								curr.dist + 1 );
				q.Enqueue(Adjcell);
			}
		}
	}

	// Return -1 if destination cannot be reached
	return -1;
}

public static int BinaryMaze1Main(int[,] mat)
{
	Point source = new Point(0, 0);
	Point dest = new Point(3, 4);

	int dist = BinaryMaze1BFS(mat, source, dest);

	return dist;
}
}

// C# implementation of the code

[TestSvmFixture, Category("Dataset")]
class BinaryMaze2 {

	static bool[, ] visited;

	// Check if it is possible to go to (x, y) from the
	// current position. The function returns false if the
	// cell has value 0 or already visited
	static bool isSafe(int[, ] mat, bool[, ] visited, int x,
					int y)
	{
		return (x >= 0 && x < mat.GetLength(0) && y >= 0
				&& y < mat.GetLength(1) && mat[x, y] == 1
				&& !visited[x, y]);
	}

	static int findShortestPath(int[, ] mat, int i, int j,
								int x, int y, int min_dist,
								int dist)
	{

		if (i == x && j == y) {
			min_dist = Math.Min(dist, min_dist);
			return min_dist;
		}

		// set (i, j) cell as visited
		visited[i, j] = true;
		// go to the bottom cell
		if (isSafe(mat, visited, i + 1, j)) {
			min_dist = findShortestPath(mat, i + 1, j, x, y,
										min_dist, dist + 1);
		}
		// go to the right cell
		if (isSafe(mat, visited, i, j + 1)) {
			min_dist = findShortestPath(mat, i, j + 1, x, y,
										min_dist, dist + 1);
		}
		// go to the top cell
		if (isSafe(mat, visited, i - 1, j)) {
			min_dist = findShortestPath(mat, i - 1, j, x, y,
										min_dist, dist + 1);
		}
		// go to the left cell
		if (isSafe(mat, visited, i, j - 1)) {
			min_dist = findShortestPath(mat, i, j - 1, x, y,
										min_dist, dist + 1);
		}
		// backtrack: remove (i, j) from the visited matrix
		visited[i, j] = false;
		return min_dist;
	}

	// Wrapper over findShortestPath() function
	[TestSvm(50,serialize:"findShortestPathLength"), Category("Dataset")]
	public static int findShortestPathLength(int[, ] mat,
									int[] src, int[] dest)
	{
		if (mat.GetLength(0) == 0
			|| mat[src[0], src[1]] == 0
			|| mat[dest[0], dest[1]] == 0)
			return -1;

		int row = mat.GetLength(0);
		int col = mat.GetLength(1);

		// construct an `M × N` matrix to keep track of
		// visited cells
		visited = new bool[row, col];
		for (int i = 0; i < row; i++) {
			for (int j = 0; j < col; j++)
				visited[i, j] = false;
		}

		int dist = Int32.MaxValue;
		dist = findShortestPath(mat, src[0], src[1],
								dest[0], dest[1], dist, 0);

		if (dist != Int32.MaxValue)
			return dist;
		return -1;
	}
	
	[TestSvm(100,serialize:"BinaryMaze2Main"), Category("Dataset")]
	public static int BinaryMaze2Main(int[,] mat)
	{
		int[] src = { 0, 0 };
		int[] dest = { 3, 4 };
		int dist = findShortestPathLength(mat, src, dest);
		return dist;
	}
}



