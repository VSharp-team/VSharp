using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using VSharp.Test;

namespace OpinionatedCode.Collections
{
    [TestSvmFixture]
    public sealed class RedBlackTree<TKey, TValue>
    {
        private readonly RedBlackTreeNode<TKey, TValue> _leaf = RedBlackTreeNode<TKey, TValue>.CreateLeaf();

        public RedBlackTree()
        {
            Root = _leaf;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public TValue Get(TKey key)
        {
            try
            {
                int hashedKey = key.GetHashCode();
                RedBlackTreeNode<TKey, TValue> node = Root;
                do
                {
                    if (node.HashedKey == hashedKey)
                        return node.Value;
                    node = hashedKey < node.HashedKey ? node.Left : node.Right;
                } while (true);
            }
            catch (NullReferenceException)
            {
                throw new KeyNotFoundException();
            }
        }

        internal RedBlackTreeNode<TKey, TValue> Root { get; private set; }

        public void Add(TKey key, TValue value)
        {
            RedBlackTreeNode<TKey, TValue> newNode = RedBlackTreeNode<TKey, TValue>.CreateNode(key, value, RedBlackTreeNode<TKey, TValue>.ColorEnum.Red, key.GetHashCode());
            RedBlackTreeInsert(newNode);
        }

        [TestSvm()]
        public void RedBlackTreeInsert(RedBlackTreeNode<TKey, TValue> z)
        {
            var y = _leaf;
            var x = Root;
            while (x != _leaf)
            {
                y = x;
                x = z.HashedKey < x.HashedKey ? x.Left : x.Right;
            }

            z.Parent = y;
            if (y == _leaf)
            {
                Root = z;
            }
            else if (z.HashedKey < y.HashedKey)
            {
                y.Left = z;
            }
            else
            {
                y.Right = z;
            }

            z.Left = _leaf;
            z.Right = _leaf;
            z.Color = RedBlackTreeNode<TKey, TValue>.ColorEnum.Red;
            InsertFixup(z);
        }

        private void InsertFixup(RedBlackTreeNode<TKey, TValue> z)
        {
            while (z.Parent.Color == RedBlackTreeNode<TKey, TValue>.ColorEnum.Red)
            {
                if (z.Parent == z.Parent.Parent.Left)
                {
                    var y = z.Parent.Parent.Right;
                    if (y.Color == RedBlackTreeNode<TKey, TValue>.ColorEnum.Red)
                    {
                        z.Parent.Color = RedBlackTreeNode<TKey, TValue>.ColorEnum.Black;
                        y.Color = RedBlackTreeNode<TKey, TValue>.ColorEnum.Black;
                        z.Parent.Parent.Color = RedBlackTreeNode<TKey, TValue>.ColorEnum.Red;
                        z = z.Parent.Parent;
                    }
                    else {
                        if (z == z.Parent.Right)
                        {
                            z = z.Parent;
                            RotateLeft(z);
                        }

                        z.Parent.Color = RedBlackTreeNode<TKey, TValue>.ColorEnum.Black;
                        z.Parent.Parent.Color = RedBlackTreeNode<TKey, TValue>.ColorEnum.Red;
                        RotateRight(z.Parent.Parent);
                    }
                }
                else
                {
                    var y = z.Parent.Parent.Left;
                    if (y.Color == RedBlackTreeNode<TKey, TValue>.ColorEnum.Red)
                    {
                        z.Parent.Color = RedBlackTreeNode<TKey, TValue>.ColorEnum.Black;
                        y.Color = RedBlackTreeNode<TKey, TValue>.ColorEnum.Black;
                        z.Parent.Parent.Color = RedBlackTreeNode<TKey, TValue>.ColorEnum.Red;
                        z = z.Parent.Parent;
                    }
                    else
                    {
                        if (z == z.Parent.Left)
                        {
                            z = z.Parent;
                            RotateRight(z);
                        }

                        z.Parent.Color = RedBlackTreeNode<TKey, TValue>.ColorEnum.Black;
                        z.Parent.Parent.Color = RedBlackTreeNode<TKey, TValue>.ColorEnum.Red;
                        RotateLeft(z.Parent.Parent);
                    }
                }
            }

            Root.Color = RedBlackTreeNode<TKey, TValue>.ColorEnum.Black;
        }

        private void RotateLeft(RedBlackTreeNode<TKey, TValue> x)
        {
            var y = x.Right;
            x.Right = y.Left;
            if (y.Left != _leaf)
            {
                y.Left.Parent = x;
            }

            y.Parent = x.Parent;
            if (x.Parent == _leaf)
            {
                Root = y;
            }
            else if (x == x.Parent.Left)
            {
                x.Parent.Left = y;
            }
            else
            {
                x.Parent.Right = y;
            }

            y.Left = x;
            x.Parent = y;
        }

        private void RotateRight(RedBlackTreeNode<TKey, TValue> x)
        {
            var y = x.Left;
            x.Left = y.Right;
            if (y.Right != _leaf)
            {
                y.Right.Parent = x;
            }
            y.Parent = x.Parent;
            if (x.Parent == _leaf)
            {
                Root = y;
            }
            else if (x == x.Parent.Left)
            {
                x.Parent.Left = y;
            }
            else
            {
                x.Parent.Right = y;
            }

            y.Right = x;
            x.Parent = y;
        }
    }
    public sealed class RedBlackTreeNode<TKey, TValue> 
    {
        public enum ColorEnum
        {
            Red,
            Black
        };

        public readonly TValue Value;

        public readonly TKey Key;

        public readonly bool IsLeaf;

        public readonly int HashedKey;

        public ColorEnum Color;

        public RedBlackTreeNode<TKey, TValue> Parent;

        public RedBlackTreeNode<TKey, TValue> Left;

        public RedBlackTreeNode<TKey, TValue> Right;

        public static RedBlackTreeNode<TKey, TValue> CreateLeaf()
        {
            return new RedBlackTreeNode<TKey, TValue>();
        }

        public static RedBlackTreeNode<TKey, TValue> CreateNode(TKey key, TValue value, ColorEnum color, int hashedKey)
        {
            return new RedBlackTreeNode<TKey, TValue>(key, value, color, hashedKey);
        }

        internal RedBlackTreeNode(TKey key, TValue value, ColorEnum color, int hashedKey)
        {
            Value = value;
            HashedKey = hashedKey;
            Color = color;
            Key = key;
        }

        internal RedBlackTreeNode()
        {
            IsLeaf = true;
            Color = ColorEnum.Black;
            HashedKey = 0;
        }        

        public RedBlackTreeNode<TKey, TValue> Grandparent => Parent?.Parent;

        public RedBlackTreeNode<TKey, TValue> Sibling =>
            Parent == null ? null : Parent.Left == this ? Parent.Right : Parent.Left;

        public RedBlackTreeNode<TKey, TValue> Uncle => Parent?.Sibling;
    }
}