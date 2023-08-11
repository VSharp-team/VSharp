using System;
using System.Runtime.InteropServices;

namespace VSharp.CSharpUtils
{
    public static class GC
    {
        [Implements("System.IntPtr System.RuntimeTypeHandle.GetGCHandle(System.Runtime.CompilerServices.QCallTypeHandle, System.Runtime.InteropServices.GCHandleType)")]
        public static IntPtr GetGCHandle(RuntimeTypeHandle handle, GCHandleType type)
        {
            return IntPtr.Zero;
        }

        [Implements("System.Object System.Runtime.InteropServices.GCHandle.InternalGet(System.IntPtr)")]
        public static object GCHandleInternalGet(IntPtr ptr)
        {
            return null;
        }

        [Implements("System.Void System.GC.KeepAlive(System.Object)")]
        public static void KeepAlive(object ptr)
        {
        }
    }
}
