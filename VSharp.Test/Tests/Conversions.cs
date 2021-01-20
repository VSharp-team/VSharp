using System;
using NUnit.Framework;
using VSharp.Test.Tests.Methods;

namespace VSharp.Test.Tests
{
    [TestSvmFixture]
    [Ignore("Need exceptions for all tests")]
    public class Conversions
    {
        [TestSvm]
        public static int Conv_Ovf_long_int(long a)
        {
            return checked((int) a);
        }

        [TestSvm]
        public static int Conv_Ovf_short_int(short a)
        {
            return checked((int) a);
        }

        [TestSvm]
        public static int Conv_Ovf_sbyte_int(sbyte a)
        {
            return checked((int) a);
        }

        [TestSvm]
        public static short Conv_Ovf_int_short(int a)
        {
            return checked((short) a);
        }

        [TestSvm]
        public static sbyte Conv_Ovf_int_sbyte(int a)
        {
            return checked((sbyte) a);
        }

        [TestSvm]
        public static sbyte Conv_Ovf_short_sbyte(short a)
        {
            return checked((sbyte) a);
        }

        [Ignore("Exceptions handling")]
        public static sbyte Conv_Ovf_short_sbyte_Overflow1()
        {
            short a = short.MaxValue;
            return Conv_Ovf_short_sbyte(a);
        }

        [TestSvm]
        public static sbyte Conv_Ovf_short_sbyte_No_Overflow1()
        {
            short a = sbyte.MaxValue;
            return Conv_Ovf_short_sbyte(a);
        }

        [TestSvm]
        public static sbyte Conv_Ovf_short_sbyte_No_Overflow2(sbyte a)
        {
            short b = a;
            return Conv_Ovf_short_sbyte(b);
        }

        [TestSvm]
        public static sbyte Conv_Ovf_Un_uint_sbyte(uint a)
        {
            return checked((sbyte) a);
        }

        [TestSvm]
        public static byte Conv_Ovf_Un_uint_byte(uint a)
        {
            return checked((byte) a);
        }

        [TestSvm]
        public static long Conv_Ovf_Un_ulong_long(ulong a)
        {
            return checked((long) a);
        }

        [Ignore("Exceptions handling")]
        public static long Conv_Ovf_Un_ulong_long_Overflow1()
        {
            ulong a = ulong.MaxValue;
            return Conv_Ovf_Un_ulong_long(a);
        }

        [TestSvm]
        public static long Conv_Ovf_Un_ulong_long_No_Overflow1()
        {
            ulong a = 0;
            return Conv_Ovf_Un_ulong_long(a);
        }

        [TestSvm]
        public static ulong Conv_Ovf_long_ulong(long a)
        {
            return checked((ulong) a);
        }

        [Ignore("Exceptions handling")]
        public static ulong Conv_Ovf_long_ulong_Overflow1()
        {
            long a = -45;
            return Conv_Ovf_long_ulong(a);
        }

        [TestSvm]
        public static ulong Conv_Ovf_long_ulong_No_Overflow1()
        {
            long a = long.MaxValue;
            return Conv_Ovf_long_ulong(a);
        }

        [TestSvm]
        public static long Conv_Ovf_Un_ulong_long_No_Overflow2()
        {
            return Conv_Ovf_Un_ulong_long(34U);
        }

        [TestSvm]
        public static IntPtr Conv_Ovf_long_NativeInt(long a)
        {
            return checked((IntPtr) a);
        }


        [TestSvm]
        public static IntPtr Conv_Ovf_int_NativeInt(int a)
        {
            return checked((IntPtr) a);
        }

        [Ignore("Exceptions handling")]
        public static UIntPtr Conv_Ovf_int_UNativeInt(int a)
        {
            return checked((UIntPtr) a);
        }
    }
}
