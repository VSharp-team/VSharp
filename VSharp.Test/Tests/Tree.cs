using NUnit.Framework;
 using System;
using VSharp.Test;

namespace IntegrationTests
{
    public class Tree
    {
        public int Key { get; set; }
        public Tree Left { get; set; }
        public Tree Right { get; set; }

        public static Tree Generate(int depth)
        {
            return depth == 0 ? null : new Tree {Key = 0, Left = Generate(depth - 1), Right = Generate(depth - 1)};
        }

        public int Size => 1 + (Left?.Size ?? 0) + (Left?.Size ?? 0);

        public int Depth => 1 + Math.Max(Left?.Depth ?? 0, Right?.Depth ?? 0);
    }

    [TestSvmFixture]
    public static class TreeTest
    {
        public static bool CheckGeneratedDepthConcrete3()
        {
            Tree t = Tree.Generate(3);
            return t.Depth == 3 && t.Size == 7;
        }

        [TestSvm(100, timeout:2)]
        public static bool CheckGeneratedDepthSymbolic(int d, int s)
        {
            if (d < 0 | d > 10)
                throw new ArgumentException("wrong tree depth");

            Tree t = Tree.Generate(d);

            if (t.Depth != d || t.Size != s)
                return false;

            return true;
        }
    }
}
