using System;
using NUnit.Framework;
using VSharp.Test;
using System.Runtime.InteropServices;


namespace IntegrationTests
{
    [TestSvmFixture]
    public sealed class ExternMocks
    {
        [Ignore("Issue #218 NullReferenceException expected on correct DateTime constructor")]
        public static DateTime CallStrDt(int a)
        {
            var dt = new DateTime(a, 12, 22);
            return dt;
        }

        [TestSvm]
        public static DateTime DtNowRet()
        {
            var t1 = DateTime.Now;
            return t1;
        }

        [TestSvm]
        public static int DtNowCompareTo()
        {
            var t1 = DateTime.Now;
            var t2 = DateTime.Now;
            if (t1.CompareTo(t2) > 0)
                return 1;
            return 0;
        }

        [TestSvm]
        public static string ReadLine()
        {
            var str = Console.ReadLine();
            return str;
        }

        [TestSvm]
        public static string ReadLineTwice()
        {
            var str = Console.ReadLine();
            var str2 = Console.ReadLine();
            return str2;
        }

        [Ignore("CFG bug fixed in PR#228")]
        public static string ReadLineLength()
        {
            string s = Console.ReadLine();
            var len = s.Length;
            return s;
        }

        [Ignore("CFG bug fixed in PR#228")]
        public static bool ReadLineCharEq()
        {
            string s = Console.ReadLine();
            return s[1] == 'A';
        }

        [Ignore("String.Equals works only for concrete length strings right now")]
        public static bool ReadLineToUpper()
        {
            string str = Console.ReadLine();
            string upper = str.ToUpperInvariant();
            return upper == str;
        }

        [TestSvm]
        public static int LibcRand()
        {
            var x = libc_rand();
            var y = libc_rand();

            return x;
        }

        [DllImport("libc", EntryPoint = "rand", CallingConvention = CallingConvention.Cdecl)]
        public static extern int libc_rand();
    }
}
