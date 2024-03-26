using System;
using System.Runtime.InteropServices;

namespace VSharp.CSharpUtils
{
    public static class GC
    {
        [Implements("System.Void System.GC.KeepAlive(System.Object)")]
        public static void KeepAlive(object ptr) { }

        [Implements("System.Void System.GC._SuppressFinalize(System.Object)")]
        public static void SuppressFinalize(object obj) { }
    }
}
