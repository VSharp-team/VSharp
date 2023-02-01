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
    [TestSvm(100)]
    public static HashSet<List<int>> ConvexHullMain(List<List<int> > points)
    {
        int n = points.Count;
        computeHull(points, n);
        return hull;
    }
}
