using System;
using NUnit.Framework;
using VSharp.Test.Tests.Methods;

namespace VSharp.Test.Tests
{
    [TestSvmFixture]
    public class InsInfException
    {

        // #####################################################
        // TESTS THAT SHOULD HANDLE IIE
        private static int GetRank(Array array)
        {
            return array.Rank;
        }

        // expecting 1
        [TestSvm]
        public static int ArrayRank_1()
        {
            int[] array = {7, 10, -56};
            return GetRank(array);
        }

        // expecting 1
        [TestSvm]
        public static int ArrayRank_2()
        {
            int[] array = {7, 10, -56};
            return array.Rank;
        }

        // expecting 2
        [TestSvm]
        public static int ArrayRank_3()
        {
            int[,] array = new int[45,123];
            return GetRank(array);
        }

        // expecting 13
        [TestSvm]
        public static int ArrayRank_4()
        {
            int[,,,,,,,,,,,,] array = new int[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13];
            return GetRank(array);
        }

        // expecting 13
        [TestSvm]
        public static int ArrayRank_5()
        {
            int[,,,,,,,,,,,,] array = new int[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13];
            return array.Rank;
        }

        // expecting 7
        [TestSvm]
        public static int CallVirtualMethod_1()
        {
            VirtualC virtualC = new VirtualC();
            return virtualC.F();
        }

        private static int Call_IVirtual(IVirtual ivirtual)
        {
            return ivirtual.F();
        }

        private static int Call_VirtualB(VirtualB b)
        {
            return b.F();
        }

        // expecting 71
        [TestSvm]
        public static int CallVirtualMethod_2()
        {
            VirtualC virtualC = new VirtualC();
            return Call_IVirtual(virtualC);
        }

        // expecting 8
        [TestSvm]
        public static int CallVirtualMethod_3()
        {
            VirtualG virtualC = new VirtualG();
            return Call_VirtualB(virtualC);
        }


        [TestSvm]
        public static PDR.A CallMethodOfStruct_1()
        {
            PDR.A a = new PDR.A();
            a.SetX(123);
            return a;
        }

        private static PDR.A IncrementX(PDR.A a)
        {
            a.SetX(a.GetX() + 100);
            return a;
        }

        [TestSvm]
        public static PDR.A CallMethodOfStruct_2()
        {
            PDR.A a = new PDR.A();
            PDR.A a1 = IncrementX(a);
            return a1;
        }

        [TestSvm]
        public static object BoxStructType_1()
        {
            PDR.A a = new PDR.A();
            a.SetX(123);
            return (object) a;
        }

        private static object BoxT<T>(T t)
        {
            return t;
        }

        [TestSvm]
        public static object BoxStructType_2()
        {
            PDR.A a = new PDR.A();
            a.SetX(123);
            return BoxT(a);
        }

        [TestSvm]
        public static object BoxReferenceType_1()
        {
            PDR.ClassWithOneField a = new PDR.ClassWithOneField();
            a.x = 123;
            return BoxT(a);
        }

        [TestSvm]
        public static object BoxReferenceType_2(bool f)
        {
            PDR.ClassWithOneField a = null;
            if (f)
            {
                return BoxT(a);
            }

            return new PDR.ClassWithOneField();
        }

        private static T MakeObj<T>() where T: new ()
        {
            return new T();
        }

        private static T MakeDefaultObj<T>()
        {
            return default;
        }

        // [Ignore("Can't execute static cctor of System.Type, because of MemoryRegion.write's __notImplemented__()")]
        [TestSvm]
        public static PDR.A MakeDefault_1()
        {
            return MakeObj<PDR.A>();
        }

        [TestSvm]
        public static PDR.A MakeDefault_2()
        {
            return MakeDefaultObj<PDR.A>();
        }

        [TestSvm]
        public static PDR.ClassWithOneField MakeDefault_3()
        {
            return MakeDefaultObj<PDR.ClassWithOneField>();
        }

        [TestSvm]
        public static object MakeDefault_4()
        {
            return MakeDefaultObj<PDR.ClassWithOneField>();
        }

        private static TR CallDelegate<TA, TR> (Func<TA, TR> f, TA x)
        {
            return f(x);
        }

        private static int Inc(int x) => x + 1;

        //expecting 42
        // [TestSvm]
        public static int CallDelegate_1()
        {
            return CallDelegate(Inc, 41);
        }

        private static T[] EnsureConcreteTypeForArrayCreation<T>(int size)
        {
            return new T[size];
        }

        class GenericClass<T>
        {
            public T _field;
        }
        private static void EnsureConcreteTypeForClassField<T>(GenericClass<T> genericClass, T value)
        {
            if (genericClass == null)
                return;
            genericClass._field = value;
        }

        // [TestSvm]
        [Ignore("Trying to create System.OverflowException fails in Monitor.Enter because of ByRef")]
        public static void CreateIntArray_1()
        {
            int[] array = EnsureConcreteTypeForArrayCreation<int>(10);
            array[9] = 42;
        }


        [TestSvm]
        public static void EnsureConcreteType_2()
        {
            GenericClass<GenericClass<int>> genericClass = new GenericClass<GenericClass<int>>();
            GenericClass<int> value = new GenericClass<int>();
            EnsureConcreteTypeForClassField(value, 42);
            EnsureConcreteTypeForClassField(genericClass, value);
        }



    }
}