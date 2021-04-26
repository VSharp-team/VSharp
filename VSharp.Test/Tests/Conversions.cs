using System;
using NUnit.Framework;

namespace VSharp.Test.Tests
{
    [TestSvmFixture]
    [Ignore("Need exceptions for all tests")]
    public class ConversionsOvf
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

    [TestSvmFixture]
    public class Conversions
    {

        // ------------------------ conv.<type> instruction tests ------------------------

        [TestSvm]
        public static uint ConvDoubleToUInt(double n)
        {
            return (uint) n;
        }

        [TestSvm]
        public static uint TestConvDoubleToUInt()
        {
            return ConvDoubleToUInt(Int32.MinValue);
        }

        [TestSvm]
        public static int ConvDoubleToInt(double number) {
            return (int)number;
        }

        [TestSvm]
        public static short ConvDoubleToShort(double number) {
            return (short)number;
        }

        [TestSvm]
        public static ushort ConvOvfDoubleToUshort(double number) {
            return checked((ushort)number);
        }

        [TestSvm]
        public static Single ConvOvfMaxDoubleToSingle()
        {
            var number = Double.MaxValue;
            return (Single)number;
        }

        [TestSvm]
        public static double ConvIntToDouble(int number) {
            return number;
        }

        [TestSvm]
        public static double ConvShortToDouble(short number) {
            return number;
        }

        public interface I {}

        public struct S : I {
            int x;
        }

        public struct SS {
            int x;
        }

        public class C : I {
            int x;
        }

        public class CC : I {
            int x;
        }

        // ------------------------ unbox.any instruction tests ------------------------

        public static U UnboxAny<T, U>(T t) {
            object o = t;
            return (U)o;
        }

        // ============== valid unbox.any ==============

        [TestSvm]
        public static I UnboxAnyStructToInterfaceValid() {
            return UnboxAny<S, I>(new S());
        }

        [TestSvm]
        public static S UnboxAnyInterfaceToStructValid() {
            return UnboxAny<I, S>(new S());
        }

        // ============== invalid unbox.any ==============

        [Ignore("exception handling")]
        public static uint UnboxAnyIntToUint(int number) {
            return UnboxAny<int, uint>(5);
        }

        [Ignore("exception handling")]
        public static double UnboxAnyIntToDouble(int number) {
            return UnboxAny<int, double>(5);
        }

        [Ignore("exception handling")]
        public static S UnboxAnyClassThroughInterfaceToStruct() {
            return UnboxAny<I, S>(new C());
        }

        [Ignore("exception handling")]
        public static I UnboxAnyStructToInterfaceInvalid() {
            return UnboxAny<SS, I>(new SS());
        }

        [Ignore("exception handling")]
        public static SS UnboxAnyInterfaceToStructInvalid() {
            return UnboxAny<I, SS>(new S());
        }

        [Ignore("exception handling")]
        public static CC UnboxAnyClassToClassInvalid() {
            return UnboxAny<C, CC>(new C());
        }

        [Ignore("exception handling")]
        public static SS UnboxAnyStructToStructInvalid() {
            return UnboxAny<S, SS>(new S());
        }

        [Ignore("exception handling")]
        public static C UnboxAnyStructToClass() {
            return UnboxAny<S, C>(new S());
        }

        [Ignore("exception handling")]
        public static S UnboxAnyClassToStruct() {
            return UnboxAny<C, S>(new C());
        }
    }
}
