using System;
using NUnit.Framework;
using VSharp.Test;

namespace IntegrationTests
{
    [TestSvmFixture]
    public class InsInfException
    {

        // Such situations were not tested:
        // 1) isNullable: execution fails during MakeSymbolicValue
        // 2) __insufficientInformation__ "Cannot process array of unknown dimension!" : could not construct test

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

        // expecting OutOfMemoryException
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
            int[,,,,,,,,,,,,] array = new int[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1];
            return GetRank(array);
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



        private static T MakeObj<T>() where T: new ()
        {
            return new T();
        }

        private static T MakeDefaultObj<T>()
        {
            return default;
        }

        // [TestSvm]
        [Ignore("Barrier: no static cctor initialization during CFA-construction and assumptions-Engine")]
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
        [Ignore("Delegates should be revised")]
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
        private static void EnsureConcreteTypeForClassField<T>(GenericClass<T> genericClass, T value) where T : class
        {
            if (genericClass == null)
                return;
            genericClass._field = value;
        }

        [TestSvm]
        public static void EnsureConcreteType_1()
        {
            int[] array = EnsureConcreteTypeForArrayCreation<int>(10);
            array[9] = 42;
        }

        private static void EnsureConcreteTypeForStelem<T>(T[] array, T value) where T : class
        {
            if (array.Length == 0)
            {
                return;
            }
            array[0] = value;
        }


        [TestSvm]
        public static int EnsureConcreteType_2()
        {
            GenericClass<GenericClass<PDR.ClassWithOneField>> genericClass = new GenericClass<GenericClass<PDR.ClassWithOneField>>();
            GenericClass<PDR.ClassWithOneField> value = new GenericClass<PDR.ClassWithOneField>();
            PDR.ClassWithOneField c = new PDR.ClassWithOneField();
            c.x = 42;
            EnsureConcreteTypeForClassField(value, c);
            EnsureConcreteTypeForClassField(genericClass, value);
            value._field.x = 123;
            return genericClass._field._field.x;
        }

        private static void WriteClassField<T>(GenericClass<T> genericClass, T value)
        {
            if (genericClass == null)
                return;
            genericClass._field = value;
        }

        [TestSvm]
        public static int CanNotInstantiateUnknownType()
        {
            GenericClass<GenericClass<int>> genericClass = new GenericClass<GenericClass<int>>();
            GenericClass<int> value = new GenericClass<int>();
            WriteClassField(value, 42);
            WriteClassField(genericClass, value);
            value._field = 123;
            return genericClass._field._field;
        }


        [TestSvm]
        public static void EnsureConcreteType_3()
        {
            PDR.ClassWithOneField[] array = new PDR.ClassWithOneField[15];
            EnsureConcreteTypeForStelem(array, new PDR.ClassWithOneField());
        }

        private static T BoxTForStructs<T>(object a) where T : struct
        {
            if (a == null) return default;
            return (T)a;
        }

        // [TestSvm]
        [Ignore("Cast should use PC: problem executions branches resulting in StructMerge")]
        public static object UnBoxStruct1()
        {
            PDR.A a = new PDR.A(42);
            return BoxTForStructs<PDR.A>((object)a);
        }


        [TestSvm(100)]
        public static int CallVirtualMethod_IIE_1(VirtualC virtualC)
        {
            if (virtualC == null) return 0;
            return Call_IVirtual(virtualC);
        }


        [TestSvm(57)]
        [IgnoreFuzzer("Need array support (add constraints to mocking)")]
        public static int ArrayRank_IIE_1(Array array)
        {
            if (array == null) return 42;
            return GetRank(array);
        }


        // expecting 71
        [TestSvm]
        public static int CallVirtualMethod_CATCH_IIE_1(VirtualC virtualC)
        {
            VirtualG virtualG = new VirtualG();
            return CallVirtualMethod_IIE_1(virtualG);
        }


        // expecting 3
        [TestSvm]
        [IgnoreFuzzer("Need array support (add constraints to mocking)")]
        public static int ArrayRank_CATCH_IIE_1(Array array)
        {
            int[,,] a = new int[2,3,5];
            return ArrayRank_IIE_1(a);
        }


    }
}
