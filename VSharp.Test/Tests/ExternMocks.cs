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

        // [TestSvm(hasExternMocking: true)]
        [Ignore("shims are not supported for .NET 7")]
        public static DateTime DtNowRet()
        {
            var t1 = DateTime.Now;
            return t1;
        }

        [Ignore("Method not implemented")]
        public static int DtNowCompareTo()
        {
            // Something weird
            // Fails Z3 x.Decode System.DateTime (BitVecNum 1)
            var t1 = DateTime.Now;
            var t2 = DateTime.Now;
            if (t1.CompareTo(t2) > 0)
                return 1;
            return 0;
        }

        // [TestSvm(hasExternMocking: true)]
        [Ignore("shims are not supported for .NET 7")]
        public static string ReadLine()
        {
            var str = Console.ReadLine();
            return str;
        }

        // [TestSvm(hasExternMocking: true)]
        [Ignore("shims are not supported for .NET 7")]
        public static string ReadLineTwice()
        {
            var str = Console.ReadLine();
            var str2 = Console.ReadLine();
            return str2;
        }

        // [TestSvm(hasExternMocking: true)]
        [Ignore("shims are not supported for .NET 7")]
        public static string ReadLineLength()
        {
            string s = Console.ReadLine();
            var len = s.Length;
            return s;
        }

        // [TestSvm(hasExternMocking: true)]
        [Ignore("shims are not supported for .NET 7")]
        public static bool ReadLineCharEq()
        {
            string s = Console.ReadLine();
            return s[1] == 'A';
        }

        [Ignore("takes too much time")]
        public static bool ReadLineToUpper()
        {
            string str = Console.ReadLine();
            string upper = str.ToUpperInvariant();
            return upper == str;
        }

        [TestSvm(hasExternMocking: true, supportedOs: OsType.Unix)]
        public static int LibcRand()
        {
            var x = libc_rand();
            var y = libc_rand();

            return x;
        }

        [TestSvm(hasExternMocking: true, supportedOs: OsType.Unix)]
        public static bool LibcRandCmp()
        {
            var x = libc_rand();
            var y = libc_rand();

            return x < y;
        }

        [DllImport("libc", EntryPoint = "rand", CallingConvention = CallingConvention.Cdecl)]
        static extern int libc_rand();

        [TestSvm(hasExternMocking: true, supportedOs: OsType.Windows)]
        public static int MsvcrtRand()
        {
            var x = msvcrt_rand();
            var y = msvcrt_rand();

            return x;
        }

        [TestSvm(hasExternMocking: true, supportedOs: OsType.Windows)]
        public static bool MsvcrtRandCmp()
        {
            var x = msvcrt_rand();
            var y = msvcrt_rand();

            return x < y;
        }

        [DllImport("msvcrt", EntryPoint = "rand", CallingConvention = CallingConvention.Cdecl)]
        public static extern int msvcrt_rand();

        [Ignore("Writing to byref arguments is not implemented")]
        public static int DotnetRand()
        {
            // Extern method with out parameters:
            // GetNonCryptographicallySecureRandomBytes(byte*, Int32)
            var rand = new Random();

            return rand.Next(1000);
        }
    }
}
