using System;
using System.Collections.Generic;
using Microsoft.FSharp.Core;
using NUnit.Framework;
using VSharp.Explorer;

namespace VSharp.Test;

[TestFixture]
public class ExecutionTreeTests
{
    private readonly Random _random = new(0);
    private readonly FSharpFunc<Unit, int> _nextRandom;

    public ExecutionTreeTests()
    {
        _nextRandom = FuncConvert.FromFunc(() => _random.Next(0, int.MaxValue));
    }

    [Test]
    [Timeout(5000)]
    public void PickTest()
    {
        var tree = new ExecutionTree<int>(0);
        tree.AddFork(0, new[] { 1 });
        tree.AddFork(0, new int[0]);
        tree.AddFork(0, new[] { 2, 3 });
        tree.AddFork(3, new[] { 4 });
        tree.AddFork(1, new[] { 5, 6 });
        tree.AddFork(6, new[] { 7 });
        tree.AddFork(4, new int[0]);
        tree.AddFork(4, new int[0]);
        tree.AddFork(0, new[] { 8 });
        tree.AddFork(4, new[] { 9, 10 });
        var pickedValues = new HashSet<int>();
        while (pickedValues.Count != 11)
        {
            pickedValues.Add(tree.RandomPick(_nextRandom).Value);
        }

        for (var i = 0; i < 11; i++)
        {
            Assert.True(pickedValues.Contains(i));
        }
    }

    [Test]
    public void RemoveTest1()
    {
        var tree = new ExecutionTree<int>(0);
        tree.AddFork(0, new[] { 1 });
        tree.Remove(1);
        for (var i = 0; i < 100; i++)
        {
            var picked = tree.RandomPick(_nextRandom);
            Assert.That(picked, Is.EqualTo(FSharpOption<int>.Some(0)));
        }
    }

    [Test]
    public void RemoveTest2()
    {
        var tree = new ExecutionTree<int>(0);
        tree.AddFork(0, new[] { 1 });
        tree.AddFork(0, new int[0]);
        tree.AddFork(0, new[] { 2, 3 });
        tree.AddFork(3, new[] { 4 });
        tree.AddFork(1, new[] { 5, 6 });
        tree.AddFork(6, new[] { 7 });
        tree.AddFork(4, new int[0]);
        tree.AddFork(4, new int[0]);
        tree.AddFork(0, new[] { 8 });
        tree.AddFork(4, new[] { 9, 10 });

        for (var j = 0; j <= 10 ; j++)
        {
            tree.Remove(j);
            for (var i = 0; i < 100; i++)
            {
                var picked = tree.RandomPick(_nextRandom);
                Assert.That(picked, Is.Not.EqualTo(FSharpOption<int>.Some(j)));
            }
        }
    }
}
