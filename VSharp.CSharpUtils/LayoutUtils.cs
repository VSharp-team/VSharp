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

        public static int ClassSize(Type t)
        {
            return Marshal.ReadInt32(t.TypeHandle.Value, 4);
        }

        public static int ArrayLengthOffset(bool isVector, int dim)
        {
            if (isVector && dim == 0)
                return sizeof(IntPtr);
            throw new NotImplementedException();
        }

        // TODO: research about length bitness
        // NOTE: first 16 bytes contain array meta info: MethodTable ptr * length
        public static readonly int ArrayElementsOffset = sizeof(IntPtr) + sizeof(Int64);

        public static readonly int StringLengthOffset = sizeof(IntPtr);
        public static readonly int StringElementsOffset = RuntimeHelpers.OffsetToStringData;

        public static int MetadataSize(Type t)
        {
            if (t == typeof(String)) return StringElementsOffset;
            if (t.IsArray) return ArrayElementsOffset;
            if (t.IsValueType) return 0;
            return sizeof(IntPtr);
        }
    }
}
