using System;

namespace VSharp.CSharpUtils
{
    /// <summary>
    /// Calculates the result of applying unary or binary operator to objects with unknown types using 'dynamic's.
    /// Casts the result of each operation to a given type.
    /// </summary>
    public static class Calculator // TODO: use opcode emit for all concrete operations
    {

        /// <summary>
        /// Returns which power of two value <paramref name="x"/> is.
        /// </summary>
        public static uint WhatPowerOf2(object x)
        {
            return x switch
            {
                float s => (uint)Math.Log2(s),
                double d => (uint)Math.Log2(d),
                byte b => (uint)Math.Log2(b),
                sbyte s => (uint)Math.Log2(s),
                short i => (uint)Math.Log2(i),
                ushort i => (uint)Math.Log2(i),
                int i => (uint)Math.Log2(i),
                uint i => (uint)Math.Log2(i),
                long i => (uint)Math.Log2(i),
                ulong i => (uint)Math.Log2(i),
                char c => (uint)Math.Log2(c),
                IntPtr i => (uint)Math.Log2(i.ToInt64()),
                UIntPtr i => (uint)Math.Log2(i.ToUInt64()),
                Enum e => (uint)Math.Log2((double)Convert.ChangeType(e, typeof(double))),
                _ => throw new ArgumentException($"WhatPowerOf2: unexpected argument {x}")
            };
        }

        public static int GetDeterministicHashCode(this string str)
        {
            if (str == null)
                return 0;
            unchecked
            {
                int hash1 = (5381 << 16) + 5381;
                int hash2 = hash1;

                for (int i = 0; i < str.Length; i += 2)
                {
                    hash1 = ((hash1 << 5) + hash1) ^ str[i];
                    if (i == str.Length - 1)
                        break;
                    hash2 = ((hash2 << 5) + hash2) ^ str[i + 1];
                }

                return hash1 + (hash2 * 1566083941);
            }
        }

        public static int GetDeterministicHashCode(this Type typ)
        {
            var method = typ.IsGenericParameter ? typ.DeclaringMethod?.ToString() : "";
            return $"{typ.Assembly.FullName}###{typ.DeclaringType}###{method}###{typ.Name}".GetDeterministicHashCode();
        }
    }
}
