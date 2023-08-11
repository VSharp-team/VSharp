using System;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace VSharp.CSharpUtils
{
    public static unsafe class LayoutUtils
    {
        [StructLayout(LayoutKind.Explicit)]
        private struct FieldDesc
        {
            [FieldOffset(0)] private readonly void* m_pMTOfEnclosingClass;

            // unsigned m_mb                   : 24;
            // unsigned m_isStatic             : 1;
            // unsigned m_isThreadLocal        : 1;
            // unsigned m_isRVA                : 1;
            // unsigned m_prot                 : 3;
            // unsigned m_requiresFullMbValue  : 1;
            [FieldOffset(8)] private readonly uint m_dword1;

            // unsigned m_dwOffset                : 27;
            // unsigned m_type                    : 5;
            [FieldOffset(12)] private readonly uint m_dword2;

            /// <summary>
            ///     Offset in memory
            /// </summary>
            public int Offset => (int) (m_dword2 & 0x7FFFFFF);
        }

        private static FieldDesc* GetFieldDescForFieldInfo(FieldInfo fi)
        {
            if (fi.IsLiteral) {
                throw new Exception("Const field");
            }

            FieldDesc* fd = (FieldDesc*) fi.FieldHandle.Value;
            return fd;
        }

        public static int GetFieldOffset(FieldInfo fi)
        {
            return GetFieldDescForFieldInfo(fi)->Offset;
        }

        // Works correctly only for class (value types are boxed)
        public static int ClassSize(Type t)
        {
            return Marshal.ReadInt32(t.TypeHandle.Value, 4);
        }

        // NOTE: first 16 bytes contain array meta info
        public const int ArrayElementsOffset = 16;
        public static readonly int StringElementsOffset = RuntimeHelpers.OffsetToStringData;
    }
}
