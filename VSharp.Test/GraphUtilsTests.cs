using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;

namespace VSharp.Test;

[TestFixture]
public class GraphUtilsTests
{
    public class Graph
    {
        public class GraphNode : GraphUtils.IGraphNode<GraphNode>
        {
            private readonly Graph _myGraph;
            private readonly int _myIndex;

            public GraphNode(Graph graph, int index)
            {
                _myGraph = graph;
                _myIndex = index;
            }

            public IEnumerable<GraphNode> OutgoingEdges =>
                _myGraph._adjacencyLists[_myIndex].Where(a => a != _myIndex).Select(a => new GraphNode(_myGraph, a));

            public override bool Equals(object obj)
            {
                if (obj is not GraphNode v)
                {
                    return false;
                }

                return _myIndex == v._myIndex;
            }

            public override int GetHashCode() => _myIndex;
        }

        private readonly HashSet<int>[] _adjacencyLists;

        public Graph(int vertexCount)
        {
            VertexCount = vertexCount;
            _adjacencyLists = new HashSet<int>[vertexCount];
            for (var i = 0; i < vertexCount; i++)
            {
                _adjacencyLists[i] = new HashSet<int> { i };
            }
        }

        public int VertexCount { get; }

        public void AddEdge(int one, int another)
        {
            _adjacencyLists[one].Add(another);
            _adjacencyLists[another].Add(one);
        }

        public GraphNode GetVertex(int index) => new(this, index);
    }

    [TestCaseSource(nameof(TestCases))]
    public void IncrementalShortestDistanceBfsTest(Graph graph, int startVertexIndex, IDictionary<int, uint> expected)
    {
        var cache = new Dictionary<Graph.GraphNode, Dictionary<Graph.GraphNode, uint>>();
        var startVertex = graph.GetVertex(startVertexIndex);

        for (var i = 0; i < graph.VertexCount; i++)
        {
            if (i == startVertexIndex)
            {
                continue;
            }

            var vertex = graph.GetVertex(i);

            var currentWithCache = GraphUtils.incrementalSourcedShortestDistanceBfs(vertex, cache);
            var currentWithoutCache = GraphUtils.incrementalSourcedShortestDistanceBfs(vertex,
                new Dictionary<Graph.GraphNode, Dictionary<Graph.GraphNode, uint>>());
            foreach (var (v, d) in currentWithoutCache)
            {
                Assert.That(currentWithCache[v], Is.EqualTo(d));
            }

            cache[vertex] = currentWithCache;
        }

        var actualWithCache = GraphUtils.incrementalSourcedShortestDistanceBfs(startVertex, cache);
        var actualWithoutCache = GraphUtils.incrementalSourcedShortestDistanceBfs(startVertex,
            new Dictionary<Graph.GraphNode, Dictionary<Graph.GraphNode, uint>>());
        foreach (var (v, d) in actualWithoutCache)
        {
            Assert.That(d, Is.EqualTo(actualWithCache[v]));
        }

        foreach (var (v, d) in expected)
        {
            Assert.That(actualWithCache[graph.GetVertex(v)], Is.EqualTo(d));
        }
    }

    private static IEnumerable<object> TestCases()
    {
        var graph1 = new Graph(9);
        graph1.AddEdge(0, 1);
        graph1.AddEdge(0, 7);
        graph1.AddEdge(1, 7);
        graph1.AddEdge(1, 2);
        graph1.AddEdge(7, 6);
        graph1.AddEdge(7, 8);
        graph1.AddEdge(2, 8);
        graph1.AddEdge(8, 6);
        graph1.AddEdge(2, 5);
        graph1.AddEdge(2, 3);
        graph1.AddEdge(6, 5);
        graph1.AddEdge(3, 5);
        graph1.AddEdge(3, 4);
        graph1.AddEdge(5, 4);

        var graph1Expected = new Dictionary<int, uint>
        {
            { 0, 0u },
            { 1, 1u },
            { 2, 2u },
            { 3, 3u },
            { 4, 4u },
            { 5, 3u },
            { 6, 2u },
            { 7, 1u },
            { 8, 2u }
        };

        var graph2 = new Graph(6);
        graph2.AddEdge(0, 1);
        graph2.AddEdge(0, 2);
        graph2.AddEdge(0, 3);
        graph2.AddEdge(2, 4);
        graph2.AddEdge(3, 5);
        graph2.AddEdge(4, 5);

        var graph2Expected = new Dictionary<int, uint>
        {
            { 0, 0u },
            { 1, 1u },
            { 2, 1u },
            { 3, 1u },
            { 4, 2u },
            { 5, 2u }
        };

        yield return new object[] { graph1, 0, graph1Expected };
        yield return new object[] { graph2, 0, graph2Expected };
    }
}
