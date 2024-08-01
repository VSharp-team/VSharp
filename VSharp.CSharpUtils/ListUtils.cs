using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace VSharp.CSharpUtils
{
    public struct ListEnumerator<T> : IEnumerator<T>, IEnumerator
    {
        private readonly List<T> _list;
        private int _index;
        private T? _current;

        internal ListEnumerator(List<T> list)
        {
            _list = list;
            _index = 0;
            _current = default;
        }

        public void Dispose()
        {
        }

        public bool MoveNext()
        {
            if ((uint)_index < (uint)_list.Count)
            {
                _current = _list[_index];
                _index++;
                return true;
            }
            return MoveNextRare();
        }

        private bool MoveNextRare()
        {
            _index = _list.Count + 1;
            _current = default;
            return false;
        }

        public T Current => _current!;

        object? IEnumerator.Current
        {
            get
            {
                if (_index == 0 || _index == _list.Count + 1)
                {
                    throw new InvalidOperationException();
                }
                return Current;
            }
        }

        void IEnumerator.Reset()
        {
            _index = 0;
            _current = default;
        }
    }

    public static class ListUtils
    {
        [Implements("System.Int32 System.Collections.Generic.LinkedList`1[T].IndexOf(this, T)")]
        [Implements("System.Int32 System.Collections.Generic.List`1[T].IndexOf(this, T)")]
        public static int IndexOf<T>(List<T> list, T value)
        {
            var count = list.Count;
            var index = 0;

            while (index < count)
            {
                if (list[index].Equals(value)) return index;

                index++;
            }

            return -1;
        }

        [Implements("System.Boolean System.Collections.Generic.LinkedList`1[T].Remove(this, T)")]
        [Implements("System.Boolean System.Collections.Generic.List`1[T].Remove(this, T)")]
        public static bool Remove<T>(List<T> list, T value)
        {
            var count = list.Count;
            var index = 0;

            while (index < count)
            {
                if (list[index].Equals(value)) break;

                index++;
            }

            if (index == -1)
            {
                return false;
            }

            list.RemoveAt(index);
            return true;
        }

        [Implements("T System.Collections.Generic.LinkedList`1[T].LastOrDefault(this)")]
        [Implements("T System.Collections.Generic.List`1[T].LastOrDefault(this)")]
        public static T LastOrDefault<T>(List<T> list)
        {
            if (list.Count == 0)
            {
                return default(T);
            }

            return list.Last();
        }

        [Implements("System.Boolean System.Collections.Generic.LinkedList`1[T].Contains(this, T)")]
        [Implements("System.Boolean System.Collections.Generic.List`1[T].Contains(this, T)")]
        public static bool Contains<T>(List<T> list, T item)
        {
            var count = list.Count;
            var index = 0;

            while (index < count)
            {
                if (list[index].Equals(item)) return true;

                index++;
            }

            return false;
        }

        [Implements("System.Int32 System.Collections.Generic.LinkedList`1[T].LastIndexOf(this, T)")]
        [Implements("System.Int32 System.Collections.Generic.List`1[T].LastIndexOf(this, T)")]
        public static int LastIndexOf<T>(List<T> list, T item)
        {
            var index = list.Count - 1;

            while (0 <= index)
            {
                if (list[index].Equals(item)) return index;

                index--;
            }

            return -1;
        }

        [Implements(
            "System.Collections.Generic.IEnumerator`1[T] System.Collections.Generic.List`1[T].System.Collections.Generic.IEnumerable<T>.GetEnumerator(this)")]
        public static IEnumerator<T> GetEnumerator<T>(List<T> list)
        {
            return new ListEnumerator<T>(list);
        }
    }
}
