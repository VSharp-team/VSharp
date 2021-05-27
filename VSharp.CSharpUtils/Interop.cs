using System;

namespace VSharp.CSharpUtils
{
    public unsafe class Interop
    {
        [Implements("System.Int32 Interop+Kernel32.LCMapStringEx(System.String, System.UInt32, System.Char*, System.Int32, System.Void*, System.Int32, System.Void*, System.Void*, System.IntPtr)")]
        public static int LCMapStringEx(string a, uint b, char* c, int d, void* e, int f, void* g, void* h, IntPtr i)
        {
            // TODO: for a locale specified by name, maps an input character string to another using a specified transformation, or generates a sort key for the input string
            return 0;
        }

        [Implements("System.Int32 Interop+Globalization.LoadICU()")]
        public static int LoadICU()
        {
            return 1;
        }

        [Implements("System.Boolean System.Globalization.CultureData.GetDefaultLocaleName(System.String&)")]
        public static bool GetDefaultLocaleName(out string name)
        {
            name = "user";
            return true;
        }

        [Implements("System.Boolean System.Globalization.GlobalizationMode.GetInvariantSwitchValue()")]
        public static bool GetInvariantSwitchValue()
        {
            return true;
        }
    }
}
