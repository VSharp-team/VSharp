using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace VSharp.CSharpUtils
{
    public sealed class ReferenceEqualityComparer : IEqualityComparer, IEqualityComparer<object>
    {
        private static IEqualityComparer<object> _defaultComparer;

        public static IEqualityComparer<object> Default => _defaultComparer ??= new ReferenceEqualityComparer();
        private ReferenceEqualityComparer() { }

        public new bool Equals(object x, object y)
        {
            return ReferenceEquals(x, y);
        }

        public int GetHashCode(object obj)
        {
            return RuntimeHelpers.GetHashCode(obj);
        }
    }
}
