// A C# program to find bridges 
// in a given undirected graph 
using System;
using System.Collections.Generic;
using NUnit.Framework;
using VSharp.Test;

// This class represents a undirected graph  
// using adjacency list representation 
[TestSvmFixture, Category("Dataset")]
public class BridgesGraph 
{ 
    private int V; // No. of vertices 
    private List<Tuple<int, int>> bridges; 

    // Array of lists for Adjacency List Representation 
    private List<int> []adj; 
    int time = 0; 
    static readonly int NIL = -1; 

    // Constructor 
    BridgesGraph(int v) 
    { 
        V = v; 
        adj = new List<int>[v]; 
        for (int i = 0; i < v; ++i) 
            adj[i] = new List<int>(); 
    } 

    // Function to add an edge into the graph 
    void addEdge(int v, int w) 
    { 
        adj[v].Add(w); // Add w to v's list. 
        adj[w].Add(v); //Add v to w's list 
    } 

    // A recursive function that finds and prints bridges 
    // using DFS traversal 
    // u --> The vertex to be visited next 
    // visited[] --> keeps track of visited vertices 
    // disc[] --> Stores discovery times of visited vertices 
    // parent[] --> Stores parent vertices in DFS tree 
    void bridgeUtil(int u, bool []visited, int []disc, 
                    int []low, int []parent) 
    { 

        // Mark the current node as visited 
        visited[u] = true; 

        // Initialize discovery time and low value 
        disc[u] = low[u] = ++time; 

        // Go through all vertices adjacent to this 
        foreach(int i in adj[u]) 
        { 
            int v = i; // v is current adjacent of u 

            // If v is not visited yet, then make it a child 
            // of u in DFS tree and recur for it. 
            // If v is not visited yet, then recur for it 
            if (!visited[v]) 
            { 
                parent[v] = u; 
                bridgeUtil(v, visited, disc, low, parent); 

                // Check if the subtree rooted with v has a 
                // connection to one of the ancestors of u 
                low[u] = Math.Min(low[u], low[v]); 

                // If the lowest vertex reachable from subtree 
                // under v is below u in DFS tree, then u-v is 
                // a bridge 
                if (low[v] > disc[u]) 
                    bridges.Add (new Tuple<int,int> (u, v)); 
            } 

            // Update low value of u for parent function calls. 
            else if (v != parent[u]) 
                low[u] = Math.Min(low[u], disc[v]); 
        } 
    } 


    // DFS based function to find all bridges. It uses recursive 
    // function bridgeUtil() 
    [TestSvm(expectedCoverage:50, serialize:"BridgesGraph"), Category("Dataset")]
    public void bridge() 
    { 
        // Mark all the vertices as not visited 
        bool []visited = new bool[V]; 
        int []disc = new int[V]; 
        int []low = new int[V]; 
        int []parent = new int[V]; 


        // Initialize parent and visited,  
        // and ap(articulation point) arrays 
        for (int i = 0; i < V; i++) 
        { 
            parent[i] = NIL; 
            visited[i] = false; 
        } 

        // Call the recursive helper function to find Bridges 
        // in DFS tree rooted with vertex 'i' 
        for (int i = 0; i < V; i++) 
            if (visited[i] == false) 
                bridgeUtil(i, visited, disc, low, parent); 
    } 

    // Driver code
    public List<Tuple<int,int>> BridgesMain(BridgesGraph g) 
    {
        g.bridge();
        return bridges;
    } 
} 
