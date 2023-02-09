using System;
using System.Collections.Generic;
using VSharp.Test;

namespace IntegrationTests;


[TestSvmFixture]
public class Graph
{
    class Edge {
        public int src, dest, weight;
        public Edge() { src = dest = weight = 0; }
    };
 
    int V, E;
    Edge[] edge;
 
    // Creates a graph with V vertices and E edges
    Graph(int v, int e)
    {
        V = v;
        E = e;
        edge = new Edge[e];
        for (int i = 0; i < e; ++i)
            edge[i] = new Edge();
    }
 
    // The main function that finds shortest distances from
    // src to all other vertices using Bellman-Ford
    // algorithm. The function also detects negative weight
    // cycle
    [TestSvm(100,timeout:200,serialize:"BellmanFord_guided:true_coverage_zone:class")]
    //[TestSvm(100)]
    public int[] BellmanFord(Graph graph, int src)
    {
        int V = graph.V, E = graph.E;
        int[] dist = new int[V];
 
        // Step 1: Initialize distances from src to all
        // other vertices as INFINITE
        for (int i = 0; i < V; ++i)
            dist[i] = int.MaxValue;
        dist[src] = 0;
 
        // Step 2: Relax all edges |V| - 1 times. A simple
        // shortest path from src to any other vertex can
        // have at-most |V| - 1 edges
        for (int i = 1; i < V; ++i) {
            for (int j = 0; j < E; ++j) {
                int u = graph.edge[j].src;
                int v = graph.edge[j].dest;
                int weight = graph.edge[j].weight;
                if (dist[u] != int.MaxValue
                    && dist[u] + weight < dist[v])
                    dist[v] = dist[u] + weight;
            }
        }
 
        // Step 3: check for negative-weight cycles. The
        // above step guarantees shortest distances if graph
        // doesn't contain negative weight cycle. If we get
        // a shorter path, then there is a cycle.
        for (int j = 0; j < E; ++j) {
            int u = graph.edge[j].src;
            int v = graph.edge[j].dest;
            int weight = graph.edge[j].weight;
            if (dist[u] != int.MaxValue
                && dist[u] + weight < dist[v]) {
                // Graph contains negative weight cycle
                return null;
            }
        }
        return dist;
    }
 
}

[TestSvmFixture]
public class AhoCorasick
{
    // Max number of states in the matching
    // machine. Should be equal to the sum
    // of the length of all keywords.
    static int MAXS = 500;
     
    // Maximum number of characters
    // in input alphabet
    static int MAXC = 26;
     
    // OUTPUT FUNCTION IS IMPLEMENTED USING out[]
    // Bit i in this mask is one if the word with
    // index i appears when the machine enters
    // this state.
    static int[] outt = new int[MAXS];
 
    // FAILURE FUNCTION IS IMPLEMENTED USING f[]
    static int[] f = new int[MAXS];
 
    // GOTO FUNCTION (OR TRIE) IS
    // IMPLEMENTED USING g[,]
    static int[,] g = new int[MAXS, MAXC];
     
    // Builds the String matching machine.
    // arr -   array of words. The index of each keyword is
    // important:
    //         "out[state] & (1 << i)" is > 0 if we just
    //         found word[i] in the text.
    // Returns the number of states that the built machine
    // has. States are numbered 0 up to the return value -
    // 1, inclusive.
    static int buildMatchingMachine(String[] arr, int k)
    {
         
        // Initialize all values in output function as 0.
        for(int i = 0; i < outt.Length; i++)
            outt[i] = 0;
     
        // Initialize all values in goto function as -1.
        for(int i = 0; i < MAXS; i++)
            for(int j = 0; j < MAXC; j++)
                g[i, j] = -1;
     
        // Initially, we just have the 0 state
        int states = 1;
     
        // Convalues for goto function, i.e., fill g[,]
        // This is same as building a Trie for []arr
        for(int i = 0; i < k; ++i)
        {
            String word = arr[i];
            int currentState = 0;
     
            // Insert all characters of current
            // word in []arr
            for(int j = 0; j < word.Length; ++j)
            {
                int ch = word[j] - 'a';
     
                // Allocate a new node (create a new state)
                // if a node for ch doesn't exist.
                if (g[currentState, ch] == -1)
                    g[currentState, ch] = states++;
     
                currentState = g[currentState, ch];
            }
     
            // Add current word in output function
            outt[currentState] |= (1 << i);
        }
     
        // For all characters which don't have
        // an edge from root (or state 0) in Trie,
        // add a goto edge to state 0 itself
        for(int ch = 0; ch < MAXC; ++ch)
            if (g[0, ch] == -1)
                g[0, ch] = 0;
     
        // Now, let's build the failure function
        // Initialize values in fail function
        for(int i = 0; i < MAXC; i++)
            f[i] = 0;
     
        // Failure function is computed in
        // breadth first order
        // using a queue
        Queue<int> q = new Queue<int>();
     
        // Iterate over every possible input
        for(int ch = 0; ch < MAXC; ++ch)
        {
             
            // All nodes of depth 1 have failure
            // function value as 0. For example,
            // in above diagram we move to 0
            // from states 1 and 3.
            if (g[0, ch] != 0)
            {
                f[g[0, ch]] = 0;
                q.Enqueue(g[0, ch]);
            }
        }
     
        // Now queue has states 1 and 3
        while (q.Count != 0)
        {
             
            // Remove the front state from queue
            int state = q.Peek();
            q.Dequeue();
     
            // For the removed state, find failure
            // function for all those characters
            // for which goto function is
            // not defined.
            for(int ch = 0; ch < MAXC; ++ch)
            {
                 
                // If goto function is defined for
                // character 'ch' and 'state'
                if (g[state, ch] != -1)
                {
                     
                    // Find failure state of removed state
                    int failure = f[state];
     
                    // Find the deepest node labeled by
                    // proper suffix of String from root to
                    // current state.
                    while (g[failure, ch] == -1)
                        failure = f[failure];
     
                    failure = g[failure, ch];
                    f[g[state, ch]] = failure;
     
                    // Merge output values
                    outt[g[state, ch]] |= outt[failure];
     
                    // Insert the next level node
                    // (of Trie) in Queue
                    q.Enqueue(g[state, ch]);
                }
            }
        }
        return states;
    }
     
    // Returns the next state the machine will transition to
    // using goto and failure functions. currentState - The
    // current state of the machine. Must be between
    //                0 and the number of states - 1,
    //                inclusive.
    // nextInput - The next character that enters into the
    // machine.
    static int findNextState(int currentState,
                             char nextInput)
    {
        int answer = currentState;
        int ch = nextInput - 'a';
     
        // If goto is not defined, use
        // failure function
        while (g[answer, ch] == -1)
            answer = f[answer];
     
        return g[answer, ch];
    }
     
    // This function finds all occurrences of
    // all array words in text.
    [TestSvm(100,timeout:200, coverageZone:CoverageZone.Method, serialize:"AhoCorasick.searchWords_true_coverage_zone:method")]
    public static List<Tuple<string, int, int>> searchWords(String[] arr, int k,
                            String text)
    {
        List<Tuple<string, int, int>> result = new List<Tuple<string, int, int>>();
     
        // Preprocess patterns.
        // Build machine with goto, failure
        // and output functions
        buildMatchingMachine(arr, k);
     
        // Initialize current state
        int currentState = 0;
     
        // Traverse the text through the
        // built machine to find all
        // occurrences of words in []arr
        for(int i = 0; i < text.Length; ++i)
        {
            currentState = findNextState(currentState,
                                         text[i]);
     
            // If match not found, move to next state
            if (outt[currentState] == 0)
                continue;
     
            // Match found, print all matching
            // words of []arr
            // using output function.
            for(int j = 0; j < k; ++j)
            {
                if ((outt[currentState] & (1 << j)) > 0)
                {
                    result.Add(new Tuple<string,int,int>(arr[j],(i - arr[j].Length + 1),i));
                }
            }
        }

        return result;
    }
     
    // Driver code
    //[TestSvm(100, guidedMode: true, coverageZone:CoverageZone.Method)]
    [TestSvm(100)]
    public static List<Tuple<string, int, int>> AhoCorasickMain(string[] words, string text)
    {
        int k = words.Length;
     
        return searchWords(words, k, text);
    }
}

[TestSvmFixture]
public class KMPSearch
{
    [TestSvm(100, guidedMode: false, coverageZone:CoverageZone.Method, serialize:"KMPSearchSearch_guided:false_coverage_zone:method")]
    //[TestSvm(100)]
    public static List<int> Search(string pat, string txt)
    {
        List<int> result = new List<int>();
        int M = pat.Length;
        int N = txt.Length;
 
        // create lps[] that will hold the longest
        // prefix suffix values for pattern
        int[] lps = new int[M];
        int j = 0; // index for pat[]
 
        // Preprocess the pattern (calculate lps[]
        // array)
        computeLPSArray(pat, M, lps);
 
        int i = 0; // index for txt[]
        while (i < N) {
            if (pat[j] == txt[i]) {
                j++;
                i++;
            }
            if (j == M) {
                result.Add(i - j);
                j = lps[j - 1];
            }
 
            // mismatch after j matches
            else if (i < N && pat[j] != txt[i]) {
                // Do not match lps[0..lps[j-1]] characters,
                // they will match anyway
                if (j != 0)
                    j = lps[j - 1];
                else
                    i = i + 1;
            }
        }

        return result;
    }
 
    static void computeLPSArray(string pat, int M, int[] lps)
    {
        // length of the previous longest prefix suffix
        int len = 0;
        int i = 1;
        lps[0] = 0; // lps[0] is always 0
 
        // the loop calculates lps[i] for i = 1 to M-1
        while (i < M) {
            if (pat[i] == pat[len]) {
                len++;
                lps[i] = len;
                i++;
            }
            else // (pat[i] != pat[len])
            {
                // This is tricky. Consider the example.
                // AAACAAAA and i = 7. The idea is similar
                // to search step.
                if (len != 0) {
                    len = lps[len - 1];
 
                    // Also, note that we do not increment
                    // i here
                }
                else // if (len == 0)
                {
                    lps[i] = len;
                    i++;
                }
            }
        }
    }
 
    // Driver program to test above function
    //[TestSvm(100, guidedMode: false, coverageZone:CoverageZone.Method, serialize:"KMPSearchMain_guided:false_coverage_zone:method")]
    public static List<int> KMPSearchMain(string txt, string pattern)
    {
        var result = Search(pattern, txt);
        return result;
    }
}

[TestSvmFixture]
public class KruskalGraph
{
    class Edge : IComparable<Edge> {
        public int src, dest, weight;
 
        // Comparator function used for sorting edges
        // based on their weight
        public int CompareTo(Edge compareEdge)
        {
            return this.weight - compareEdge.weight;
        }
    }
 
    // A class to represent
    // a subset for union-find
    public class subset {
        public int parent, rank;
    };
 
    int V, E; // V-> no. of vertices & E->no.of edges
    Edge[] edge; // collection of all edges
 
    // Creates a graph with V vertices and E edges
    KruskalGraph(int v, int e)
    {
        V = v;
        E = e;
        edge = new Edge[E];
        for (int i = 0; i < e; ++i)
            edge[i] = new Edge();
    }
 
    // A utility function to find set of an element i
    // (uses path compression technique)
    int find(subset[] subsets, int i)
    {
        // find root and make root as
        // parent of i (path compression)
        if (subsets[i].parent != i)
            subsets[i].parent
                = find(subsets, subsets[i].parent);
 
        return subsets[i].parent;
    }
 
    // A function that does union of
    // two sets of x and y (uses union by rank)
    void Union(subset[] subsets, int x, int y)
    {
        int xroot = find(subsets, x);
        int yroot = find(subsets, y);
 
        // Attach smaller rank tree under root of
        // high rank tree (Union by Rank)
        if (subsets[xroot].rank < subsets[yroot].rank)
            subsets[xroot].parent = yroot;
        else if (subsets[xroot].rank > subsets[yroot].rank)
            subsets[yroot].parent = xroot;
 
        // If ranks are same, then make one as root
        // and increment its rank by one
        else {
            subsets[yroot].parent = xroot;
            subsets[xroot].rank++;
        }
    }
 
    // The main function to construct MST
    // using Kruskal's algorithm
    [TestSvm(100)]
    public Tuple<int,List<Tuple<int,int,int>>> KruskalMST()
    {  
        List<Tuple<int, int, int>> resultEdges = new List<Tuple<int, int, int>>(); 
        // This will store the
        // resultant MST
        Edge[] result = new Edge[V];
        int e = 0; // An index variable, used for result[]
        int i
            = 0; // An index variable, used for sorted edges
        for (i = 0; i < V; ++i)
            result[i] = new Edge();
 
        // Step 1: Sort all the edges in non-decreasing
        // order of their weight. If we are not allowed
        // to change the given graph, we can create
        // a copy of array of edges
        Array.Sort(edge);
 
        // Allocate memory for creating V subsets
        subset[] subsets = new subset[V];
        for (i = 0; i < V; ++i)
            subsets[i] = new subset();
 
        // Create V subsets with single elements
        for (int v = 0; v < V; ++v) {
            subsets[v].parent = v;
            subsets[v].rank = 0;
        }
 
        i = 0; // Index used to pick next edge
 
        // Number of edges to be taken is equal to V-1
        while (e < V - 1) {
            // Step 2: Pick the smallest edge. And increment
            // the index for next iteration
            Edge next_edge = new Edge();
            next_edge = edge[i++];
 
            int x = find(subsets, next_edge.src);
            int y = find(subsets, next_edge.dest);
 
            // If including this edge doesn't cause cycle,
            // include it in result and increment the index
            // of result for next edge
            if (x != y) {
                result[e++] = next_edge;
                Union(subsets, x, y);
            }
            // Else discard the next_edge
        }
        
        int minimumCost = 0;
        for (i = 0; i < e; ++i) {
            resultEdges.Add(new Tuple<int,int,int>(result[i].src, result[i].dest, result[i].weight));
            minimumCost += result[i].weight;
        }

        return new Tuple<int, List<Tuple<int, int, int>>>(minimumCost, resultEdges);
    }
}


[TestSvmFixture]
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

    [TestSvm(100)]
    static public int partition(int[] arr,
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

    // Driver code
    [TestSvm(100)]
    static public int BinarySearchMain(int[] arr, int k)
    {
        int n = arr.Length;
        return (partition(arr, n, k));
    }
}

[TestSvmFixture]
public static class ConvexHull {
    static HashSet<List<int> > hull
        = new HashSet<List<int> >();
    // Stores the result (points of convex hull)
 
    // Returns the side of point p with respect to line
    // joining points p1 and p2.
    public static int findSide(List<int> p1, List<int> p2,
                               List<int> p)
    {
        int val = (p[1] - p1[1]) * (p2[0] - p1[0])
                  - (p2[1] - p1[1]) * (p[0] - p1[0]);
 
        if (val > 0) {
            return 1;
        }
        if (val < 0) {
            return -1;
        }
        return 0;
    }
 
    // returns a value proportional to the distance
    // between the point p and the line joining the
    // points p1 and p2
    public static int lineDist(List<int> p1, List<int> p2,
                               List<int> p)
    {
        return Math.Abs((p[1] - p1[1]) * (p2[0] - p1[0])
                        - (p2[1] - p1[1]) * (p[0] - p1[0]));
    }
 
    // End points of line L are p1 and p2. side can have
    // value 1 or -1 specifying each of the parts made by
    // the line L
    public static void quickHull(List<List<int> > a, int n,
                                 List<int> p1, List<int> p2,
                                 int side)
    {
        int ind = -1;
        int max_dist = 0;
 
        // finding the point with maximum distance
        // from L and also on the specified side of L.
        for (int i = 0; i < n; i++) {
            int temp = lineDist(p1, p2, a[i]);
            if (findSide(p1, p2, a[i]) == side
                && temp > max_dist) {
                ind = i;
                max_dist = temp;
            }
        }
 
        // If no point is found, add the end points
        // of L to the convex hull.
        if (ind == -1) {
            hull.Add(p1);
            hull.Add(p2);
            return;
        }
 
        // Recur for the two parts divided by a[ind]
        quickHull(a, n, a[ind], p1,
                  -findSide(a[ind], p1, p2));
        quickHull(a, n, a[ind], p2,
                  -findSide(a[ind], p2, p1));
    }
 
    public static void computeHull(List<List<int> > a, int n)
    {
        // a[i].second -> y-coordinate of the ith point
        if (n < 3)
        {
            throw new Exception("Convex hull not possible for given set of points.");
        }
 
        // Finding the point with minimum and
        // maximum x-coordinate
        int min_x = 0;
        int max_x = 0;
        for (int i = 1; i < n; i++) {
            if (a[i][0] < a[min_x][0]) {
                min_x = i;
            }
            if (a[i][0] > a[max_x][0]) {
                max_x = i;
            }
        }
 
        // Recursively find convex hull points on
        // one side of line joining a[min_x] and
        // a[max_x]
        quickHull(a, n, a[min_x], a[max_x], 1);
        quickHull(a, n, a[min_x], a[max_x], -1);
        
    }
 
    // Driver code
    [TestSvm(100)]
    public static HashSet<List<int>> ConvexHullMain(List<List<int> > points)
    {
        int n = points.Count;
        computeHull(points, n);
        return hull;
    }
}

// A C# program to check if a given
// directed graph is Eulerian or not



// This class represents a directed
// graph using adjacency list
[TestSvmFixture]
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
[TestSvm(100)]
public Boolean isEulerianCycle()
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
public static bool EulerMain(EulerGraph g)
{
	return g.isEulerianCycle();
}
}


// This class represents a undirected graph  
// using adjacency list representation 
[TestSvmFixture]
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
    [TestSvm(100,coverageZone:CoverageZone.Method)]
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
    [TestSvm(100,coverageZone:CoverageZone.Method)]
    public List<Tuple<int,int>> BridgesMain(BridgesGraph g) 
    {
        g.bridge();
        return bridges;
    } 
} 

// This class represents an undirected graph
// using adjacency list representation
[TestSvmFixture]
public class CutVerticesGraph {
	private int V; // No. of vertices

	// Array of lists for Adjacency List Representation
	private List<int>[] adj;
	int time = 0;
	static readonly int NIL = -1;

	// Constructor
	CutVerticesGraph(int v)
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
		adj[w].Add(v); // Add v to w's list
	}

	// A recursive function that find articulation points using DFS
	// u --> The vertex to be visited next
	// visited[] --> keeps track of visited vertices
	// disc[] --> Stores discovery times of visited vertices
	// parent[] --> Stores parent vertices in DFS tree
	// ap[] --> Store articulation points
    [TestSvm(100,coverageZone:CoverageZone.Method)]
    public void APUtil(int u, bool[] visited, int[] disc,
				int[] low, int[] parent, bool[] ap)
	{

		// Count of children in DFS Tree
		int children = 0;

		// Mark the current node as visited
		visited[u] = true;

		// Initialize discovery time and low value
		disc[u] = low[u] = ++time;

		// Go through all vertices adjacent to this
		foreach(int i in adj[u])
		{
			int v = i; // v is current adjacent of u

			// If v is not visited yet, then make it a child of u
			// in DFS tree and recur for it
			if (!visited[v]) {
				children++;
				parent[v] = u;
				APUtil(v, visited, disc, low, parent, ap);

				// Check if the subtree rooted with v has
				// a connection to one of the ancestors of u
				low[u] = Math.Min(low[u], low[v]);

				// u is an articulation point in following cases

				// (1) u is root of DFS tree and has two or more children.
				if (parent[u] == NIL && children > 1)
					ap[u] = true;

				// (2) If u is not root and low value of one of its child
				// is more than discovery value of u.
				if (parent[u] != NIL && low[v] >= disc[u])
					ap[u] = true;
			}

			// Update low value of u for parent function calls.
			else if (v != parent[u])
				low[u] = Math.Min(low[u], disc[v]);
		}
	}

	// The function to do DFS traversal.
	// It uses recursive function APUtil()
	[TestSvm(100,coverageZone:CoverageZone.Method)]
    public bool[] AP()
	{
		// Mark all the vertices as not visited
		bool[] visited = new bool[V];
		int[] disc = new int[V];
		int[] low = new int[V];
		int[] parent = new int[V];
		bool[] ap = new bool[V]; // To store articulation points

		// Initialize parent and visited, and
		// ap(articulation point) arrays
		for (int i = 0; i < V; i++) {
			parent[i] = NIL;
			visited[i] = false;
			ap[i] = false;
		}

		// Call the recursive helper function to find articulation
		// points in DFS tree rooted with vertex 'i'
		for (int i = 0; i < V; i++)
			if (visited[i] == false)
				APUtil(i, visited, disc, low, parent, ap);

		// Now ap[] contains articulation points, print them
		return ap;
	}

	// Driver method
	public static bool[] CutVerticesMain(CutVerticesGraph g)
    {
        return g.AP();
    }
}


[TestSvmFixture]
class TravelingSalesmanProblem {

	static int V = 10;

	// implementation of traveling Salesman Problem
	[TestSvm(66,coverageZone:CoverageZone.Method)]
	public static int travllingSalesmanProblem(int[, ] graph,
										int s)
	{
		List<int> vertex = new List<int>();

		for (int i = 0; i < V; i++)
			if (i != s)
				vertex.Add(i);

		// store minimum weight
		// Hamiltonian Cycle.
		int min_path = Int32.MaxValue;

		do {
			// store current Path weight(cost)
			int current_pathweight = 0;
			int k = s;

			// compute current path weight
			for (int i = 0; i < vertex.Count; i++) {
				current_pathweight += graph[k, vertex[i]];
				k = vertex[i];
			}

			current_pathweight += graph[k, s];

			// update minimum
			min_path
				= Math.Min(min_path, current_pathweight);

		} while (findNextPermutation(vertex));

		return min_path;
	}

	// Function to swap the data resent in the left and
	// right indices
	public static List<int> swap(List<int> data, int left,
								int right)
	{
		// Swap the data
		int temp = data[left];
		data[left] = data[right];
		data[right] = temp;

		// Return the updated array
		return data;
	}

	// Function to reverse the sub-array starting from left
	// to the right both inclusive
	public static List<int> reverse(List<int> data,
									int left, int right)
	{
		// Reverse the sub-array
		while (left < right) {
			int temp = data[left];
			data[left++] = data[right];
			data[right--] = temp;
		}

		// Return the updated array
		return data;
	}

	// Function to find the next permutation of the given
	// integer array
	public static bool findNextPermutation(List<int> data)
	{
		// If the given dataset is empty
		// or contains only one element
		// next_permutation is not possible
		if (data.Count <= 1)
			return false;
		int last = data.Count - 2;

		// find the longest non-increasing
		// suffix and find the pivot
		while (last >= 0) {
			if (data[last] < data[last + 1])
				break;
			last--;
		}

		// If there is no increasing pair
		// there is no higher order permutation
		if (last < 0)
			return false;
		int nextGreater = data.Count - 1;

		// Find the rightmost successor
		// to the pivot
		for (int i = data.Count - 1; i > last; i--) {
			if (data[i] > data[last]) {
				nextGreater = i;
				break;
			}
		}

		// Swap the successor and
		// the pivot
		data = swap(data, nextGreater, last);

		// Reverse the suffix
		data = reverse(data, last + 1, data.Count - 1);

		// Return true as the
		// next_permutation is done
		return true;
	}

	// Driver Code
	public static int TSPMain(int[, ] graph, int s)
	{
		return travllingSalesmanProblem(graph, s);
	}
}

// A C# program to find maximal
// Bipartite matching.

[TestSvmFixture]
class BipartiteMatchingGraph
{
	// M is number of applicants
	// and N is number of jobs
	static int M = 6;
	static int N = 6;

	// A DFS based recursive function
	// that returns true if a matching
	// for vertex u is possible
	[TestSvm(100)]
	public bool bpm(bool [,]bpGraph, int u,
			bool []seen, int []matchR)
	{
		// Try every job one by one
		for (int v = 0; v < N; v++)
		{
			// If applicant u is interested
			// in job v and v is not visited
			if (bpGraph[u, v] && !seen[v])
			{
				// Mark v as visited
				seen[v] = true;

				// If job 'v' is not assigned to
				// an applicant OR previously assigned
				// applicant for job v (which is matchR[v])
				// has an alternate job available.
				// Since v is marked as visited in the above
				// line, matchR[v] in the following recursive
				// call will not get job 'v' again
				if (matchR[v] < 0 || bpm(bpGraph, matchR[v],
										seen, matchR))
				{
					matchR[v] = u;
					return true;
				}
			}
		}
		return false;
	}

	// Returns maximum number of
	// matching from M to N
	int maxBPM(bool [,]bpGraph)
	{
		// An array to keep track of the
		// applicants assigned to jobs.
		// The value of matchR[i] is the
		// applicant number assigned to job i,
		// the value -1 indicates nobody is assigned.
		int []matchR = new int[N];

		// Initially all jobs are available
		for(int i = 0; i < N; ++i)
			matchR[i] = -1;

		// Count of jobs assigned to applicants
		int result = 0;
		for (int u = 0; u < M; u++)
		{
			// Mark all jobs as not
			// seen for next applicant.
			bool []seen = new bool[N] ;
			for(int i = 0; i < N; ++i)
				seen[i] = false;

			// Find if the applicant
			// 'u' can get a job
			if (bpm(bpGraph, u, seen, matchR))
				result++;
		}
		return result;
	}

	// Driver Code
	public static int BipartiteMatchingMain (bool[,] bpGraph)
	{
		// Let us create a bpGraph shown
		// in the above example
		
		BipartiteMatchingGraph m = new BipartiteMatchingGraph();
	    return m.maxBPM(bpGraph);
	}
}




	[TestSvmFixture]
	class KnapsackBag : IEnumerable<KnapsackBag.Item>
	{
		List<Item> items;
		const int MaxWeightAllowed = 400;

		public KnapsackBag()
		{
			items = new List<Item>();
		}

		void AddItem(Item i)
		{
			if ((TotalWeight + i.Weight) <= MaxWeightAllowed)
				items.Add(i);
		}

		[TestSvm(100)]
		public void Calculate(List<Item> items)
		{
			foreach (Item i in Sorte(items))
			{
				AddItem(i);
			}
		}

		[TestSvm(100)]
		public List<Item> Sorte(List<Item> inputItems)
		{
			List<Item> choosenItems = new List<Item>();
			for (int i = 0; i < inputItems.Count; i++)
			{
				int j = -1;
				if (i == 0)
				{
					choosenItems.Add(inputItems[i]);
				}

				if (i > 0)
				{
					if (!RecursiveF(inputItems, choosenItems, i, choosenItems.Count - 1, false, ref j))
					{
						choosenItems.Add(inputItems[i]);
					}
				}
			}

			return choosenItems;
		}

		bool RecursiveF(List<Item> knapsackItems, List<Item> choosenItems, int i, int lastBound, bool dec,
			ref int indxToAdd)
		{
			if (!(lastBound < 0))
			{
				if (knapsackItems[i].ResultWV < choosenItems[lastBound].ResultWV)
				{
					indxToAdd = lastBound;
				}

				return RecursiveF(knapsackItems, choosenItems, i, lastBound - 1, true, ref indxToAdd);
			}

			if (indxToAdd > -1)
			{
				choosenItems.Insert(indxToAdd, knapsackItems[i]);
				return true;
			}

			return false;
		}

		#region IEnumerable<Item> Members

		IEnumerator<Item> IEnumerable<Item>.GetEnumerator()
		{
			foreach (Item i in items)
				yield return i;
		}

		#endregion

		#region IEnumerable Members

		System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
		{
			return items.GetEnumerator();
		}

		#endregion

		public int TotalWeight
		{
			get
			{
				var sum = 0;
				foreach (Item i in this)
				{
					sum += i.Weight;
				}

				return sum;
			}
		}

		public class Item
		{
			public string Name { get; set; }
			public int Weight { get; set; }
			public int Value { get; set; }

			public int ResultWV
			{
				get { return Weight - Value; }
			}

			public override string ToString()
			{
				return "Name : " + Name + "        Wieght : " + Weight + "       Value : " + Value +
				       "     ResultWV : " + ResultWV;
			}
		}
	}

	class Knapsack
	{
		static KnapsackBag KnapsackMain(List<KnapsackBag.Item> knapsackItems)
		{
			KnapsackBag b = new KnapsackBag();
			b.Calculate(knapsackItems);
			return b;
		}
	}
