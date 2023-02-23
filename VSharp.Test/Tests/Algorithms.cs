using System;
using System.Collections.Generic;
using VSharp.Test;

namespace IntegrationTests;

[TestSvmFixture]
public class KMPSearch
{
    static List<int> Search(string pat, string txt)
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
    
    [TestSvm(100)]
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
    [TestSvm(50)]
    public static HashSet<List<int>> ConvexHullMain(List<List<int> > points)
    {
        int n = points.Count;
        computeHull(points, n);
        return hull;
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
    static List<Tuple<string, int, int>> searchWords(String[] arr, int k,
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
     
    [TestSvm(100)]
    public static List<Tuple<string, int, int>> AhoCorasickMain(string[] words, string text)
    {
        int k = words.Length;
     
        return searchWords(words, k, text);
    }
}

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
    
    [TestSvm(100)]
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
public static bool EulerMain(EulerGraph g)
{
	return g.isEulerianCycle();
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

[TestSvmFixture]
class A_star
    {
        // Coordinates of a cell - implements the method Equals
        public class Coordinates : IEquatable<Coordinates>
        {
            public int row;
            public int col;

            public Coordinates() { this.row = -1; this.col = -1; }
            public Coordinates(int row, int col) { this.row = row; this.col = col; }

            public Boolean Equals(Coordinates c)
            {
                if (this.row == c.row && this.col == c.col)
                    return true;
                else
                    return false;
            }
        }

        // Class Cell, with the cost to reach it, the values g and f, and the coordinates
        // of the cell that precedes it in a possible path
        public class Cell
        {
            public int cost;
            public int g;
            public int f;
            public Coordinates parent;
        }

        // Class Astar, which finds the shortest path
        public class Astar
        {
            // The array of the cells
            private int[,] walls;
            public Cell[,] cells;
            // The possible path found
            public List<Coordinates> path = new List<Coordinates>();
            // The list of the opened cells
            public List<Coordinates> opened = new List<Coordinates>();
            // The list of the closed cells
            public List<Coordinates> closed = new List<Coordinates>();
            // The start of the searched path
            public Coordinates startCell = new Coordinates(0, 0);
            // The end of the searched path
            public Coordinates finishCell = new Coordinates(7, 7);

            // The constructor
            public Astar(Cell[,] _cells, int [,] _walls)
            {
	            cells = _cells;
	            walls = _walls;
                // Initialization of the cells values
                for (int i = 0; i < _cells.GetLength(0); i++)
                    for (int j = 0; j < _cells.GetLength(1); j++)
                    {
                        cells[i, j] = new Cell();
                        cells[i, j].parent = new Coordinates();
                        if (IsAWall(i, j))
                            cells[i, j].cost = 100;
                        else
                            cells[i, j].cost = 1;
                    }

                // Adding the start cell on the list opened
                opened.Add(startCell);

                // Boolean value which indicates if a path is found
                Boolean pathFound = false;

                // Loop until the list opened is empty or a path is found
                do
                {
                    List<Coordinates> neighbors = new List<Coordinates>();
                    // The next cell analyzed
                    Coordinates currentCell = ShorterExpectedPath();
                    // The list of cells reachable from the actual one
                    neighbors = neighborsCells(currentCell);
                    foreach (Coordinates newCell in neighbors)
                    {
                        // If the cell considered is the final one
                        if (newCell.row == finishCell.row && newCell.col == finishCell.col)
                        {
                            cells[newCell.row, newCell.col].g = cells[currentCell.row,
                                currentCell.col].g + cells[newCell.row, newCell.col].cost;
                            cells[newCell.row, newCell.col].parent.row = currentCell.row;
                            cells[newCell.row, newCell.col].parent.col = currentCell.col;
                            pathFound = true;
                            break;
                        }

                        // If the cell considered is not between the open and closed ones
                        else if (!opened.Contains(newCell) && !closed.Contains(newCell))
                        {
                            cells[newCell.row, newCell.col].g = cells[currentCell.row,
                                currentCell.col].g + cells[newCell.row, newCell.col].cost;
                            cells[newCell.row, newCell.col].f =
                                cells[newCell.row, newCell.col].g + Heuristic(newCell);
                            cells[newCell.row, newCell.col].parent.row = currentCell.row;
                            cells[newCell.row, newCell.col].parent.col = currentCell.col;
                            SetCell(newCell, opened);
                        }

                        // If the cost to reach the considered cell from the actual one is
                        // smaller than the previous one
                        else if (cells[newCell.row, newCell.col].g > cells[currentCell.row,
                            currentCell.col].g + cells[newCell.row, newCell.col].cost)
                        {
                            cells[newCell.row, newCell.col].g = cells[currentCell.row,
                                currentCell.col].g + cells[newCell.row, newCell.col].cost;
                            cells[newCell.row, newCell.col].f =
                                cells[newCell.row, newCell.col].g + Heuristic(newCell);
                            cells[newCell.row, newCell.col].parent.row = currentCell.row;
                            cells[newCell.row, newCell.col].parent.col = currentCell.col;
                            SetCell(newCell, opened);
                            ResetCell(newCell, closed);
                        }
                    }
                    SetCell(currentCell, closed);
                    ResetCell(currentCell, opened);
                } while (opened.Count > 0 && pathFound == false);

                if (pathFound)
                {
                    path.Add(finishCell);
                    Coordinates currentCell = new Coordinates(finishCell.row, finishCell.col);
                    // It reconstructs the path starting from the end
                    while (cells[currentCell.row, currentCell.col].parent.row >= 0)
                    {
                        path.Add(cells[currentCell.row, currentCell.col].parent);
                        int tmp_row = cells[currentCell.row, currentCell.col].parent.row;
                        currentCell.col = cells[currentCell.row, currentCell.col].parent.col;
                        currentCell.row = tmp_row;
                    }

                    // Printing on the screen the 'chessboard' and the path found
                    for (int i = 0; i < 8; i++)
                    {
                        for (int j = 0; j < 8; j++)
                        {
                            // Symbol for a cell that doesn't belong to the path and isn't
                            // a wall
                            char gr = '.';
                            // Symbol for a cell that belongs to the path
                            if (path.Contains(new Coordinates(i, j))) { gr = 'X'; }
                            // Symbol for a cell that is a wall
                            else if (cells[i, j].cost > 1) { gr = '\u2588'; }
                        }
                    }

                    

                }
            }

            // It select the cell between those in the list opened that have the smaller
            // value of f
            public Coordinates ShorterExpectedPath()
            {
                int sep = 0;
                if (opened.Count > 1)
                {
                    for (int i = 1; i < opened.Count; i++)
                    {
                        if (cells[opened[i].row, opened[i].col].f < cells[opened[sep].row,
                            opened[sep].col].f)
                        {
                            sep = i;
                        }
                    }
                }
                return opened[sep];
            }

            // It finds che cells that could be reached from c
            public List<Coordinates> neighborsCells(Coordinates c)
            {
                List<Coordinates> lc = new List<Coordinates>();
                for (int i = -1; i <= 1; i++)
                    for (int j = -1; j <= 1; j++)
                        if (c.row+i >= 0 && c.row+i < 8 && c.col+j >= 0 && c.col+j < 8 &&
                            (i != 0 || j != 0))
                        {
                            lc.Add(new Coordinates(c.row + i, c.col + j));
                        }
                return lc;
            }

            // It determines if the cell with coordinates (row, col) is a wall
            public bool IsAWall(int row, int col)
            {
	            bool found = false;
                for (int i = 0; i < walls.GetLength(0); i++)
                    if (walls[i,0] == row && walls[i,1] == col)
                        found = true;
                return found;
            }

            // The function Heuristic, which determines the shortest path that a 'king' can do
            // This is the maximum value between the orizzontal distance and the vertical one
            public int Heuristic(Coordinates cell)
            {
                int dRow = Math.Abs(finishCell.row - cell.row);
                int dCol = Math.Abs(finishCell.col - cell.col);
                return Math.Max(dRow, dCol);
            }

            // It inserts the coordinates of cell in a list, if it's not already present
            public void SetCell(Coordinates cell, List<Coordinates> coordinatesList)
            {
                if (coordinatesList.Contains(cell) == false)
                {
                    coordinatesList.Add(cell);
                }
            }

            // It removes the coordinates of cell from a list, if it's already present
            public void ResetCell(Coordinates cell, List<Coordinates> coordinatesList)
            {
                if (coordinatesList.Contains(cell))
                {
                    coordinatesList.Remove(cell);
                }
            }
        }

        // The main method
        [TestSvm(51)]
        public static void AStarMain(Cell[,] field, int[,] walls)
        {
            Astar astar = new Astar(field, walls);
        }
    }

[TestSvmFixture]
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
[TestSvm(100)]
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





