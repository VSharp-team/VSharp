// Copyright (c) 2020 Code Ex Machina, LLC. All rights reserved.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.If not, see <https://www.gnu.org/licenses/>.

using System;
using System.Collections;
using System.Collections.Generic;
using VSharp.Test;

namespace CodeExMachina
{    
    /// <summary>
    /// FreeList represents a free list of btree nodes. By default each
    /// BTree has its own FreeList, but multiple BTrees can share the same
    /// FreeList.
    /// Two Btrees using the same freelist are safe for concurrent write access.
    /// </summary>
    /// <typeparam name="T">The type of elements in the list.</typeparam>
    public class FreeList<T> where T : class
    {
        private const int DefaultFreeListSize = 32;

        private readonly object _mu;
        private readonly List<Node<T>> _freelist;
        private readonly Comparer<T> _comparer;

        /// <summary>
        /// Creates a new free list with default size.
        /// </summary>
        /// <param name="comparer"></param>
        public FreeList(Comparer<T> comparer)
            : this(DefaultFreeListSize, comparer)
        { }

        /// <summary>
        /// Creates a new free list.
        /// </summary>
        /// <param name="size"></param>
        /// <param name="comparer"></param>
        public FreeList(int size, Comparer<T> comparer)
        {
            _mu = new object();
            _freelist = new List<Node<T>>(size);
            _comparer = comparer;
        }

        internal Node<T> NewNode()
        {
            lock (_mu)
            {
                int index = _freelist.Count - 1;

                if (index < 0)
                {
                    return new Node<T>(_comparer);
                }

                Node<T> n = _freelist[index];

                _freelist[index] = null;
                _freelist.RemoveAt(index);

                return n;
            }
        }

        // Adds the given node to the list, returning true if it was added
        // and false if it was discarded.           
        internal bool FreeNode(Node<T> n)
        {
            bool success = false;

            lock (_mu)
            {
                if (_freelist.Count < _freelist.Capacity)
                {
                    _freelist.Add(n);
                    success = true;
                }
            }

            return success;
        }
    }

    /// <summary>
    /// ItemIterator allows callers of Ascend* to iterate in-order over portions of
    /// the tree.  When this function returns false, iteration will stop and the
    /// associated Ascend* function will immediately return.
    /// </summary>
    /// <param name="i"></param>
    public delegate bool ItemIterator<T>(T i) where T : class;

    // Stores items in a node.    
    internal class Items<T> : IEnumerable<T> where T : class
    {
        private readonly List<T> _items = new List<T>();
        private readonly Comparer<T> _comparer;

        public int Length => _items.Count;
        public int Capacity => _items.Capacity;

        public Items(Comparer<T> comparer)
        {
            _comparer = comparer;
        }

        // Inserts a value into the given index, pushing all subsequent values
        // forward.        
        public void InsertAt(int index, T item)
        {
            _items.Insert(index, item);
        }

        // Removes a value at a given index, pulling all subsequent values
        // back.       
        public T RemoveAt(int index)
        {
            T item = _items[index];
            _items.RemoveAt(index);
            return item;
        }

        // Removes and returns the last element in the list.                
        public T Pop()
        {
            int index = _items.Count - 1;
            T item = _items[index];
            _items[index] = null;
            _items.RemoveAt(index);
            return item;
        }

        // Truncates this instance at index so that it contains only the
        // first index items. index must be less than or equal to length.        
        public void Truncate(int index)
        {
            int count = _items.Count - index;
            if (count > 0)
            {
                _items.RemoveRange(index, count);
            }
        }

        // Returns the index where the given item should be inserted into this
        // list.  'found' is true if the item already exists in the list at the given
        // index.        
        public (int, bool) Find(T item)
        {
            int index = _items.BinarySearch(0, _items.Count, item, _comparer);

            bool found = index >= 0;

            if (!found)
            {
                index = ~index;
            }

            return index > 0 && !Less(_items[index - 1], item) ? (index - 1, true) : (index, found);
        }

        public T this[int i]
        {
            get => _items[i];
            set => _items[i] = value;
        }

        public void Append(T item)
        {
            _items.Add(item);
        }

        public void Append(IEnumerable<T> items)
        {
            _items.AddRange(items);
        }

        public List<T> GetRange(int index, int count)
        {
            return _items.GetRange(index, count);
        }

        IEnumerator<T> IEnumerable<T>.GetEnumerator()
        {
            return _items.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return _items.GetEnumerator();
        }

        public override string ToString()
        {
            return string.Join(" ", _items);
        }

        private bool Less(T x, T y)
        {
            return _comparer.Compare(x, y) == -1;
        }
    }

    // Stores child nodes in a node.    
    internal class Children<T> : IEnumerable<Node<T>> where T : class
    {
        private readonly List<Node<T>> _children = new List<Node<T>>();

        public int Length => _children.Count;
        public int Capacity => _children.Capacity;

        // Inserts a value into the given index, pushing all subsequent values
        // forward.        
        public void InsertAt(int index, Node<T> item)
        {
            _children.Insert(index, item);
        }

        // Removes a value at a given index, pulling all subsequent values
        // back.        
        public Node<T> RemoveAt(int index)
        {
            Node<T> n = _children[index];
            _children.RemoveAt(index);
            return n;
        }

        // Removes and returns the last element in the list.        
        public Node<T> Pop()
        {
            int index = _children.Count - 1;
            Node<T> child = _children[index];
            _children[index] = null;
            _children.RemoveAt(index);
            return child;
        }

        // Truncates this instance at index so that it contains only the
        // first index children. index must be less than or equal to length.        
        public void Truncate(int index)
        {
            int count = _children.Count - index;
            if (count > 0)
            {
                _children.RemoveRange(index, count);
            }
        }

        public Node<T> this[int i]
        {
            get => _children[i];
            set => _children[i] = value;
        }
        public void Append(Node<T> node)
        {
            _children.Add(node);
        }

        public void Append(IEnumerable<Node<T>> range)
        {
            _children.AddRange(range);
        }

        public List<Node<T>> GetRange(int index, int count)
        {
            return _children.GetRange(index, count);
        }

        IEnumerator<Node<T>> IEnumerable<Node<T>>.GetEnumerator()
        {
            return _children.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return _children.GetEnumerator();
        }
    }

    // Details what item to remove in a node.remove call.    
    public enum ToRemove
    {
        // removes the given item        
        RemoveItem,

        // removes smallest item in the subtree        
        RemoveMin,

        // removes largest item in the subtree        
        RemoveMax
    }

    public enum Direction
    {
        Descend = -1,
        Ascend = 1
    }

    // node is an internal node in a tree.
    // 
    // It must at all times maintain the invariant that either
    // * len(children) == 0, len(items) unconstrained
    // * len(children) == len(items) + 1    
    [TestSvmFixture]
    public class Node<T> where T : class
    {
        internal Items<T> Items { get; set; }
        internal Children<T> Children { get; set; }
        internal CopyOnWriteContext<T> Cow { get; set; }
        internal Comparer<T> Comparer { get; set; }

        public Node(Comparer<T> comparer)
        {
            Comparer = comparer;
            Items = new Items<T>(comparer);
            Children = new Children<T>();
        }

        public Node<T> MutableFor(CopyOnWriteContext<T> cow)
        {
            if (ReferenceEquals(Cow, cow))
            {
                return this;
            }

            Node<T> node = Cow.NewNode();

            node.Items.Append(Items);
            node.Children.Append(Children);

            return node;
        }

        public Node<T> MutableChild(int i)
        {
            Node<T> c = Children[i].MutableFor(Cow);
            Children[i] = c;
            return c;
        }

        // Splits the given node at the given index.  The current node shrinks,
        // and this function returns the item that existed at that index and a new node
        // containing all items/children after it.        
        public (T item, Node<T> node) Split(int i)
        {
            T item = Items[i];
            Node<T> next = Cow.NewNode();
            next.Items.Append(Items.GetRange(i + 1, Items.Length - (i + 1)));
            Items.Truncate(i);
            if (Children.Length > 0)
            {
                next.Children.Append(Children.GetRange(i + 1, Children.Length - (i + 1)));
                Children.Truncate(i + 1);
            }
            return (item, next);
        }

        // Checks if a child should be split, and if so splits it.
        // Returns whether or not a split occurred.        
        public bool MaybeSplitChild(int i, int maxItems)
        {
            if (Children[i].Items.Length < maxItems)
            {
                return false;
            }
            Node<T> first = MutableChild(i);
            (T item, Node<T> second) = first.Split(maxItems / 2);
            Items.InsertAt(i, item);
            Children.InsertAt(i + 1, second);
            return true;
        }

        // Inserts an item into the subtree rooted at this node, making sure
        // no nodes in the subtree exceed maxItems items.  Should an equivalent item be
        // be found/replaced by insert, it will be returned.        
        [TestSvm()]
        public T Insert(T item, int maxItems)
        {
            (int i, bool found) = Items.Find(item);
            if (found)
            {
                T n = Items[i];
                Items[i] = item;
                return n;
            }
            if (Children.Length == 0)
            {
                Items.InsertAt(i, item);
                return null;
            }
            if (MaybeSplitChild(i, maxItems))
            {
                T inTree = Items[i];
                if (Less(item, inTree))
                {
                    // no change, we want first split node
                }
                else if (Less(inTree, item))
                {
                    i++; // we want second split node
                }
                else
                {
                    T n = Items[i];
                    Items[i] = item;
                    return n;
                }
            }
            return MutableChild(i).Insert(item, maxItems);
        }

        // Finds the given key in the subtree and returns it.        
        public T Get(T key)
        {
            (int i, bool found) = Items.Find(key);
            if (found)
            {
                return Items[i];
            }
            else if (Children.Length > 0)
            {
                return Children[i].Get(key);
            }
            return null;
        }

        // Returns the first item in the subtree.        
        public static T Min(Node<T> n)
        {
            if (n == null)
            {
                return null;
            }
            while (n.Children.Length > 0)
            {
                n = n.Children[0];
            }
            return n.Items.Length == 0 ? null : n.Items[0];
        }

        // Returns the last item in the subtree.        
        public static T Max(Node<T> n)
        {
            if (n == null)
            {
                return null;
            }
            while (n.Children.Length > 0)
            {
                n = n.Children[n.Children.Length - 1];
            }
            return n.Items.Length == 0 ? null : n.Items[n.Items.Length - 1];
        }

        // Removes an item from the subtree rooted at this node.        
        [TestSvm()]
        public T Remove(T item, int minItems, ToRemove typ)
        {
            int i = 0;
            bool found = false;
            switch (typ)
            {
                case ToRemove.RemoveMax:
                    {
                        if (Children.Length == 0)
                        {
                            return Items.Pop();
                        }
                        i = Items.Length;
                    }
                    break;
                case ToRemove.RemoveMin:
                    {
                        if (Children.Length == 0)
                        {
                            return Items.RemoveAt(0);
                        }
                        i = 0;
                    }
                    break;
                case ToRemove.RemoveItem:
                    {
                        (i, found) = Items.Find(item);
                        if (Children.Length == 0)
                        {
                            return found ? Items.RemoveAt(i) : null;
                        }
                    }
                    break;
                default:
                    Environment.FailFast("invalid type");
                    break;
            }
            // If we get to here, we have children.
            if (Children[i].Items.Length <= minItems)
            {
                return GrowChildAndRemove(i, item, minItems, typ);
            }
            Node<T> child = MutableChild(i);
            // Either we had enough items to begin with, or we've done some
            // merging/stealing, because we've got enough now and we're ready to return
            if (found)
            {
                // The item exists at index 'i', and the child we've selected can give us a
                // predecessor, since if we've gotten here it's got > minItems items in it.
                T n = Items[i];
                // We use our special-case 'remove' call with typ=maxItem to pull the
                // predecessor of item i (the rightmost leaf of our immediate left child)
                // and set it into where we pulled the item from.
                Items[i] = child.Remove(null, minItems, ToRemove.RemoveMax);
                return n;
            }
            // Final recursive call.  Once we're here, we know that the item isn't in this
            // node and that the child is big enough to remove from.
            return child.Remove(item, minItems, typ);
        }

        // Grows child 'i' to make sure it's possible to remove an
        // item from it while keeping it at minItems, then calls remove to actually
        // remove it
        // 
        // Most documentation says we have to do two sets of special casing:
        //    1) item is in this node
        //    2) item is in child
        // In both cases, we need to handle the two subcases:
        //    A) node has enough values that it can spare one
        //    B) node doesn't have enough values
        // For the latter, we have to check:
        //    a) left sibling has node to spare
        //    b) right sibling has node to spare
        //    c) we must merge
        // To simplify our code here, we handle cases #1 and #2 the same:
        // If a node doesn't have enough items, we make sure it does (using a,b,c).
        // We then simply redo our remove call, and the second time (regardless of
        // whether we're in case 1 or 2), we'll have enough items and can guarantee
        // that we hit case A.        
        public T GrowChildAndRemove(int i, T item, int minItems, ToRemove typ)
        {
            if (i > 0 && Children[i - 1].Items.Length > minItems)
            {
                // Steal from left child
                Node<T> child = MutableChild(i);
                Node<T> stealFrom = MutableChild(i - 1);
                T stolenItem = stealFrom.Items.Pop();
                child.Items.InsertAt(0, Items[i - 1]);
                Items[i - 1] = stolenItem;
                if (stealFrom.Children.Length > 0)
                {
                    child.Children.InsertAt(0, stealFrom.Children.Pop());
                }
            }
            else if (i < Items.Length && Children[i + 1].Items.Length > minItems)
            {
                // steal from right child
                Node<T> child = MutableChild(i);
                Node<T> stealFrom = MutableChild(i + 1);
                T stolenItem = stealFrom.Items.RemoveAt(0);
                child.Items.Append(Items[i]);
                Items[i] = stolenItem;
                if (stealFrom.Children.Length > 0)
                {
                    child.Children.Append(stealFrom.Children.RemoveAt(0));
                }
            }
            else
            {
                if (i >= Items.Length)
                {
                    i--;
                }
                Node<T> child = MutableChild(i);
                // merge with right child
                T mergeItem = Items.RemoveAt(i);
                Node<T> mergeChild = Children.RemoveAt(i + 1);
                child.Items.Append(mergeItem);
                child.Items.Append(mergeChild.Items);
                child.Children.Append(mergeChild.Children);
                _ = Cow.FreeNode(mergeChild);
            }
            return Remove(item, minItems, typ);
        }

        // Iterate provides a simple method for iterating over elements in the tree.
        // 
        // When ascending, the 'start' should be less than 'stop' and when descending,
        // the 'start' should be greater than 'stop'. Setting 'includeStart' to true
        // will force the iterator to include the first item when it equals 'start',
        // thus creating a "greaterOrEqual" or "lessThanEqual" rather than just a
        // "greaterThan" or "lessThan" queries.        
        [TestSvm()]
        public (bool, bool) Iterate(Direction dir, T start, T stop, bool includeStart, bool hit, ItemIterator<T> iter)
        {
            bool ok, found;
            int index = 0;
            switch (dir)
            {
                case Direction.Ascend:
                    {
                        if (start != null)
                        {
                            (index, _) = Items.Find(start);
                        }
                        for (int i = index; i < Items.Length; i++)
                        {
                            if (Children.Length > 0)
                            {
                                (hit, ok) = Children[i].Iterate(dir, start, stop, includeStart, hit, iter);
                                if (!ok)
                                {
                                    return (hit, false);
                                }
                            }
                            if (!includeStart && !hit && start != null && !Less(start, Items[i]))
                            {
                                hit = true;
                                continue;
                            }
                            hit = true;
                            if (stop != null && !Less(Items[i], stop))
                            {
                                return (hit, false);
                            }
                            if (!iter(Items[i]))
                            {
                                return (hit, false);
                            }
                        }
                        if (Children.Length > 0)
                        {
                            (hit, ok) = Children[Children.Length - 1].Iterate(dir, start, stop, includeStart, hit, iter);
                            if (!ok)
                            {
                                return (hit, false);
                            }
                        }
                    }
                    break;
                case Direction.Descend:
                    {
                        if (start != null)
                        {
                            (index, found) = Items.Find(start);
                            if (!found)
                            {
                                index -= 1;
                            }
                        }
                        else
                        {
                            index = Items.Length - 1;
                        }
                        for (int i = index; i >= 0; i--)
                        {
                            if (start != null && !Less(Items[i], start))
                            {
                                if (!includeStart || hit || Less(start, Items[i]))
                                {
                                    continue;
                                }
                            }
                            if (Children.Length > 0)
                            {
                                (hit, ok) = Children[i + 1].Iterate(dir, start, stop, includeStart, hit, iter);
                                if (!ok)
                                {
                                    return (hit, false);
                                }
                            }
                            if (stop != null && !Less(stop, Items[i]))
                            {
                                return (hit, false);
                            }
                            hit = true;
                            if (!iter(Items[i]))
                            {
                                return (hit, false);
                            }
                        }
                        if (Children.Length > 0)
                        {
                            (hit, ok) = Children[0].Iterate(dir, start, stop, includeStart, hit, iter);
                            if (!ok)
                            {
                                return (hit, false);
                            }
                        }
                    }
                    break;
                default:
                    break;
            }
            return (hit, true);
        }

        // Reset returns a subtree to the freelist.  It breaks out immediately if the
        // freelist is full, since the only benefit of iterating is to fill that
        // freelist up.  Returns true if parent reset call should continue.        
        public bool Reset(CopyOnWriteContext<T> c)
        {
            foreach (Node<T> child in Children)
            {
                if (!child.Reset(c))
                {
                    return false;
                }
            }
            return c.FreeNode(this) != FreeType.ftFreeListFull;
        }

        // Used for testing/debugging purposes.        
        public void Print(System.IO.TextWriter w, int level)
        {
            string repeat = new string(' ', level);
            w.Write($"{repeat}NODE:{Items}\n");
            foreach (Node<T> c in Children)
            {
                c.Print(w, level + 1);
            }
        }

        private bool Less(T x, T y)
        {
            return Comparer.Compare(x, y) == -1;
        }
    }

    /// <summary>
    /// BTree is an implementation of a B-Tree.
    /// 
    /// BTree stores Item instances in an ordered structure, allowing easy insertion,
    /// removal, and iteration.
    /// 
    /// Write operations are not safe for concurrent mutation by multiple
    /// tasks, but Read operations are.
    /// </summary>
    /// <typeparam name="T">The type of elements in the tree.</typeparam>
    public class BTree<T> where T : class
    {
        private int Degree { get; set; }

        /// <summary>
        /// Returns the number of items currently in the tree.
        /// </summary>
        public int Length { get; private set; }

        private Node<T> Root { get; set; }
        private CopyOnWriteContext<T> Cow { get; set; }

        private BTree()
        { }

        /// <summary>
        /// Creates a new B-Tree with the given degree.
        /// 
        /// BTree(2), for example, will create a 2-3-4 tree (each node contains 1-3 items
        /// and 2-4 children).
        /// </summary>
        /// <param name="degree"></param>
        /// <param name="comparer"></param>
        public BTree(int degree, Comparer<T> comparer)
            : this(degree, new FreeList<T>(comparer))
        { }

        /// <summary>
        /// Creates a new B-Tree that uses the given node free list.
        /// </summary>
        /// <param name="degree"></param>
        /// <param name="f"></param>
        public BTree(int degree, FreeList<T> f)
        {
            if (degree <= 1)
            {
                Environment.FailFast("bad degree");
            }
            Degree = degree;
            Cow = new CopyOnWriteContext<T> { FreeList = f };
        }

        /// <summary>
        /// Clone clones the btree, lazily.  Clone should not be called concurrently,
        /// but the original tree (t) and the new tree (t2) can be used concurrently
        /// once the Clone call completes.
        /// 
        /// The internal tree structure of b is marked read-only and shared between t and
        /// t2.  Writes to both t and t2 use copy-on-write logic, creating new nodes
        /// whenever one of b's original nodes would have been modified.  Read operations
        /// should have no performance degredation.  Write operations for both t and t2
        /// will initially experience minor slow-downs caused by additional allocs and
        /// copies due to the aforementioned copy-on-write logic, but should converge to
        /// the original performance characteristics of the original tree.
        /// </summary>        
        public BTree<T> Clone()
        {
            // Create two entirely new copy-on-write contexts.
            // This operation effectively creates three trees:
            //   the original, shared nodes (old b.cow)
            //   the new b.cow nodes
            //   the new out.cow nodes
            CopyOnWriteContext<T> cow1 = new CopyOnWriteContext<T> { FreeList = Cow.FreeList };
            CopyOnWriteContext<T> cow2 = new CopyOnWriteContext<T> { FreeList = Cow.FreeList };
            BTree<T> tree = new BTree<T>
            {
                Degree = Degree,
                Length = Length,
                Root = Root,
                Cow = Cow
            };
            Cow = cow1;
            tree.Cow = cow2;
            return tree;
        }

        /// <summary>
        /// Returns the max number of items to allow per node.
        /// </summary>        
        private int MaxItems()
        {
            return (Degree * 2) - 1;
        }

        /// <summary>
        /// Returns the min number of items to allow per node (ignored for the
        /// root node).
        /// </summary>        
        private int MinItems()
        {
            return Degree - 1;
        }

        /// <summary>
        /// Adds the given item to the tree.  If an item in the tree
        /// already equals the given one, it is removed from the tree and returned.
        /// Otherwise, nil is returned.
        /// 
        /// nil cannot be added to the tree (will panic).
        /// </summary>
        /// <param name="item"></param>
        public T ReplaceOrInsert(T item)
        {
            if (item == null)
            {
                Environment.FailFast("null item being added to BTree");
            }
            if (Root == null)
            {
                Root = Cow.NewNode();
                Root.Items.Append(item);
                Length++;
                return null;
            }
            else
            {
                Root = Root.MutableFor(Cow);
                if (Root.Items.Length >= MaxItems())
                {
                    (T item2, Node<T> second) = Root.Split(MaxItems() / 2);
                    Node<T> oldRoot = Root;
                    Root = Cow.NewNode();
                    Root.Items.Append(item2);
                    Root.Children.Append(oldRoot);
                    Root.Children.Append(second);
                }
            }
            T result = Root.Insert(item, MaxItems());
            if (result == null)
            {
                Length++;
            }
            return result;
        }

        /// <summary>
        /// Removes an item equal to the passed in item from the tree, returning
        /// it.  If no such item exists, returns nil.
        /// </summary>
        /// <param name="item"></param>
        public T Delete(T item)
        {
            return DeleteItem(item, ToRemove.RemoveItem);
        }

        /// <summary>
        /// Removes the smallest item in the tree and returns it.
        /// If no such item exists, returns nil.
        /// </summary>        
        public T DeleteMin()
        {
            return DeleteItem(null, ToRemove.RemoveMin);
        }

        /// <summary>
        /// Removes the largest item in the tree and returns it.
        /// If no such item exists, returns nil.
        /// </summary>        
        public T DeleteMax()
        {
            return DeleteItem(null, ToRemove.RemoveMax);
        }

        private T DeleteItem(T item, ToRemove typ)
        {
            if (Root == null || Root.Items.Length == 0)
            {
                return null;
            }
            Root = Root.MutableFor(Cow);
            T result = Root.Remove(item, MinItems(), typ);
            if (Root.Items.Length == 0 && Root.Children.Length > 0)
            {
                Node<T> oldRoot = Root;
                Root = Root.Children[0];
                _ = Cow.FreeNode(oldRoot);
            }
            if (result != null)
            {
                Length--;
            }
            return result;
        }

        /// <summary>
        /// Calls the iterator for every value in the tree within the range
        /// [greaterOrEqual, lessThan), until iterator returns false.
        /// </summary>
        /// <param name="greaterOrEqual"></param>
        /// <param name="lessThan"></param>
        /// <param name="iterator"></param>
        public void AscendRange(T greaterOrEqual, T lessThan, ItemIterator<T> iterator)
        {
            if (Root == null)
            {
                return;
            }
            _ = Root.Iterate(Direction.Ascend, greaterOrEqual, lessThan, true, false, iterator);
        }

        /// <summary>
        /// Calls the iterator for every value in the tree within the range
        /// [first, pivot), until iterator returns false.
        /// </summary>
        /// <param name="pivot"></param>
        /// <param name="iterator"></param>
        public void AscendLessThan(T pivot, ItemIterator<T> iterator)
        {
            if (Root == null)
            {
                return;
            }
            _ = Root.Iterate(Direction.Ascend, null, pivot, false, false, iterator);
        }

        /// <summary>
        /// Calls the iterator for every value in the tree within
        /// the range [pivot, last], until iterator returns false.
        /// </summary>
        /// <param name="pivot"></param>
        /// <param name="iterator"></param>
        public void AscendGreaterOrEqual(T pivot, ItemIterator<T> iterator)
        {
            if (Root == null)
            {
                return;
            }
            _ = Root.Iterate(Direction.Ascend, pivot, null, true, false, iterator);
        }

        /// <summary>
        /// Calls the iterator for every value in the tree within the range
        /// [first, last], until iterator returns false.
        /// </summary>
        /// <param name="iterator"></param>
        public void Ascend(ItemIterator<T> iterator)
        {
            if (Root == null)
            {
                return;
            }
            _ = Root.Iterate(Direction.Ascend, null, null, false, false, iterator);
        }

        /// <summary>
        /// Calls the iterator for every value in the tree within the range
        /// [lessOrEqual, greaterThan), until iterator returns false.
        /// </summary>
        /// <param name="lessOrEqual"></param>
        /// <param name="greaterThan"></param>
        /// <param name="iterator"></param>
        public void DescendRange(T lessOrEqual, T greaterThan, ItemIterator<T> iterator)
        {
            if (Root == null)
            {
                return;
            }
            _ = Root.Iterate(Direction.Descend, lessOrEqual, greaterThan, true, false, iterator);
        }

        /// <summary>
        /// Calls the iterator for every value in the tree within the range
        /// [pivot, first], until iterator returns false.
        /// </summary>
        /// <param name="pivot"></param>
        /// <param name="iterator"></param>
        public void DescendLessOrEqual(T pivot, ItemIterator<T> iterator)
        {
            if (Root == null)
            {
                return;
            }
            _ = Root.Iterate(Direction.Descend, pivot, null, true, false, iterator);
        }

        /// <summary>
        /// Calls the iterator for every value in the tree within
        /// the range [last, pivot), until iterator returns false.
        /// </summary>
        /// <param name="pivot"></param>
        /// <param name="iterator"></param>
        public void DescendGreaterThan(T pivot, ItemIterator<T> iterator)
        {
            if (Root == null)
            {
                return;
            }
            _ = Root.Iterate(Direction.Descend, null, pivot, false, false, iterator);
        }

        /// <summary>
        /// Calls the iterator for every value in the tree within the range
        /// [last, first], until iterator returns false.
        /// </summary>
        /// <param name="iterator"></param>
        public void Descend(ItemIterator<T> iterator)
        {
            if (Root == null)
            {
                return;
            }
            _ = Root.Iterate(Direction.Descend, null, null, false, false, iterator);
        }

        /// <summary>
        /// Looks for the key item in the tree, returning it.  It returns nil if
        /// unable to find that item.
        /// </summary>
        /// <param name="key"></param>
        public T Get(T key)
        {
            return Root?.Get(key);
        }

        /// <summary>
        /// Returns the smallest item in the tree, or nil if the tree is empty.
        /// </summary>        
        public T Min()
        {
            return Node<T>.Min(Root);
        }

        /// <summary>
        /// Returns the largest item in the tree, or nil if the tree is empty.
        /// </summary>        
        public T Max()
        {
            return Node<T>.Max(Root);
        }

        /// <summary>
        /// Returns true if the given key is in the tree.
        /// </summary>
        /// <param name="key"></param>       
        public bool Has(T key)
        {
            return Get(key) != null;
        }

        /// <summary>
        /// Removes all items from the btree.  If addNodesToFreelist is true,
        /// t's nodes are added to its freelist as part of this call, until the freelist
        /// is full.  Otherwise, the root node is simply dereferenced and the subtree
        /// left to Go's normal GC processes.
        /// 
        /// This can be much faster
        /// than calling Delete on all elements, because that requires finding/removing
        /// each element in the tree and updating the tree accordingly.  It also is
        /// somewhat faster than creating a new tree to replace the old one, because
        /// nodes from the old tree are reclaimed into the freelist for use by the new
        /// one, instead of being lost to the garbage collector.
        /// 
        /// This call takes:
        ///     O(1): when addNodesToFreelist is false, this is a single operation.
        ///     O(1): when the freelist is already full, it breaks out immediately
        ///     O(freelist size):  when the freelist is empty and the nodes are all owned
        ///         by this tree, nodes are added to the freelist until full.
        ///     O(tree size):  when all nodes are owned by another tree, all nodes are
        ///         iterated over looking for nodes to add to the freelist, and due to
        ///         ownership, none are.
        /// </summary>
        /// <param name="addNodesToFreeList"></param>
        public void Clear(bool addNodesToFreeList)
        {
            if (Root != null && addNodesToFreeList)
            {
                _ = Root.Reset(Cow);
            }
            Root = null;
            Length = 0;
        }
    }

    public enum FreeType
    {
        // node was freed (available for GC, not stored in freelist)        
        ftFreeListFull,

        // node was stored in the freelist for later use        
        ftStored,

        // node was ignored by COW, since it's owned by another one        
        ftNotOwned
    }

    // CopyOnWriteContext pointers determine node ownership... a tree with a write
    // context equivalent to a node's write context is allowed to modify that node.
    // A tree whose write context does not match a node's is not allowed to modify
    // it, and must create a new, writable copy (IE: it's a Clone).
    //
    // When doing any write operation, we maintain the invariant that the current
    // node's context is equal to the context of the tree that requested the write.
    // We do this by, before we descend into any node, creating a copy with the
    // correct context if the contexts don't match.
    //
    // Since the node we're currently visiting on any write has the requesting
    // tree's context, that node is modifiable in place.  Children of that node may
    // not share context, but before we descend into them, we'll make a mutable
    // copy.    
    public class CopyOnWriteContext<T> where T : class
    {
        public FreeList<T> FreeList { get; internal set; }

        public Node<T> NewNode()
        {
            Node<T> n = FreeList.NewNode();
            n.Cow = this;
            return n;
        }

        // Frees a node within a given COW context, if it's owned by that
        // context.  It returns what happened to the node (see freeType const
        // documentation).        
        public FreeType FreeNode(Node<T> n)
        {
            if (ReferenceEquals(n.Cow, this))
            {
                // clear to allow GC
                n.Items.Truncate(0);
                n.Children.Truncate(0);
                n.Cow = null;
                return FreeList.FreeNode(n) ? FreeType.ftStored : FreeType.ftFreeListFull;
            }
            else
            {
                return FreeType.ftNotOwned;
            }
        }
    }    

    /// <summary>
    /// Int implements the Item interface for integers.
    /// </summary>
    public class Int : IComparable, IComparable<int>      
    {
        private readonly int _v;

        public Int(int v)
        {
            _v = v;
        }
        
        public override string ToString()
        {
            return _v.ToString();
        }

        public int CompareTo(int other)
        {
            return _v.CompareTo(other);
        }

        public int CompareTo(object obj)
        {
            Int v = (Int)obj;
            return _v.CompareTo(v._v);
        }

        public override bool Equals(object obj)
        {
            return Equals(obj as Int);
        }

        public override int GetHashCode()
        {
            return _v.GetHashCode();
        }

        public bool Equals(Int other)
        {
            if (other is null) return false;
            return _v == other._v;
        }        

        public static bool operator <(Int a, Int b)
        {
            return a._v < b._v;
        }

        public static bool operator >(Int a, Int b)
        {
            return a._v > b._v;
        }

        public static bool operator !=(Int a, Int b)
        {
            return !(a == b);
        }

        public static bool operator ==(Int a, Int b)
        {
            return object.Equals(a, b);
        }        
    }

    /// <summary>
    /// Compare two Ints.
    ///
    /// This must provide a strict weak ordering.
    /// If !(a &lt; b) &amp;&amp; !(b &lt; a), we treat this to mean a == b (i.e. we can only
    /// hold one of either a or b in the tree).
    /// </summary>
    public class IntComparer : Comparer<Int>
    {
        public override int Compare(Int x, Int y)
        {
            return x.CompareTo(y);
        }
    }
}