// A C# program to check if a given
// directed graph is Eulerian or not

// A class that represents an
// undirected graph
using System;
using System.Collections.Generic;
using NUnit.Framework;
using VSharp.Test;

// This class represents a directed
// graph using adjacency list
[TestSvmFixture, Category("Dataset")]
class EulerGraph{
	
// No. of vertices
public int V;

// Adjacency List
public List<int> []adj;

// Maintaining in degree
public int []init;		

// Constructor
	EulerGraph(int v)
{
	V = v;
	adj = new List<int>[v];
	init = new int[V];
	
	for(int i = 0; i < v; ++i)
	{
		adj[i] = new List<int>();
		init[i] = 0;
	}
}

// Function to add an edge into the graph
void addEdge(int v, int w)
{
	adj[v].Add(w);
	init[w]++;
}

// A recursive function to print DFS
// starting from v
void DFSUtil(int v, Boolean []visited)
{
	
	// Mark the current node as visited
	visited[v] = true;

	// Recur for all the vertices
	// adjacent to this vertex
	foreach(int i in adj[v])
	{
		
		if (!visited[i])
			DFSUtil(i, visited);
	}
}

// Function that returns reverse
// (or transpose) of this graph
	EulerGraph getTranspose()
{
	EulerGraph g = new EulerGraph(V);
	for(int v = 0; v < V; v++)
	{
		
		// Recur for all the vertices
		// adjacent to this vertex
		foreach(int i in adj[v])
		{
			g.adj[i].Add(v);
			(g.init[v])++;
		}
	}
	return g;
}

// The main function that returns
// true if graph is strongly connected
Boolean isSC()
{
	
	// Step 1: Mark all the vertices
	// as not visited (For first DFS)
	Boolean []visited = new Boolean[V];
	for(int i = 0; i < V; i++)
		visited[i] = false;

	// Step 2: Do DFS traversal starting
	// from the first vertex.
	DFSUtil(0, visited);

	// If DFS traversal doesn't visit
	// all vertices, then return false.
	for(int i = 0; i < V; i++)
		if (visited[i] == false)
			return false;

	// Step 3: Create a reversed graph
	EulerGraph gr = getTranspose();

	// Step 4: Mark all the vertices as
	// not visited (For second DFS)
	for(int i = 0; i < V; i++)
		visited[i] = false;

	// Step 5: Do DFS for reversed graph
	// starting from first vertex.
	// Starting Vertex must be same
	// starting point of first DFS
	gr.DFSUtil(0, visited);

	// If all vertices are not visited
	// in second DFS, then return false
	for(int i = 0; i < V; i++)
		if (visited[i] == false)
			return false;

	return true;
}

// This function returns true if the
// directed graph has a eulerian
// cycle, otherwise returns false
Boolean isEulerianCycle()
{
	
	// Check if all non-zero degree
	// vertices are connected
	if (isSC() == false)
		return false;

	// Check if in degree and out
	// degree of every vertex is same
	for(int i = 0; i < V; i++)
		if (adj[i].Count != init[i])
			return false;

	return true;
}

// Driver code
[TestSvm(expectedCoverage:50, serialize:"EulerMain"), Category("Dataset")]
public static bool EulerMain(EulerGraph g)
{
	return g.isEulerianCycle();
}
}