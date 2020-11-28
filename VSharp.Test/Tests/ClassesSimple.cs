using System;
using System.Collections.Generic;
using NUnit.Framework;

namespace VSharp.Test.Tests
{
    internal class ClassesSimpleA
    {
        private int _intField = 100500;
        private ClassesSimpleB _b = new ClassesSimpleB { _c = new ClassesSimpleC { _n = 13 } };

        public ClassesSimpleA()
        {
            _intField = 100501;
        }

        public ClassesSimpleA(int n)
        {
            _b._c._n = n;
            _b._c.SetN(n);
        }

        public void IncN()
        {
            _b.SetN(_b.GetN() + 1);
        }

        public void DecN()
        {
            _b.SetN(_b.GetN() - 1);
        }

        public int GetN()
        {
            return _b.GetN();
        }
    }

    internal struct ClassesSimpleB
    {
        public ClassesSimpleC _c;

        public int GetN()
        {
            return _c._n;
        }

        public void SetN(int n)
        {
            _c._n = n;
        }
    }

    internal struct ClassesSimpleC
    {
        public int _n;
        public int _m2;

        public int M2
        {
            get { return _m2; }
            set { _m2 = value; }
        }
        public int M1 { get; set; }

        public void SetN(int n)
        {
            _n = n;
            M1 = 50;
            M2 = M1 * 2;
            M2++;
            ++M2;
            M1 -= 8;
        }
    }

    internal static class ClassesSimpleRegistrator
    {
        public static List<string> entries = new List<string>(20);

        public static int RegisterAndReturn(string entry, int value)
        {
            entries.Add(entry);
            return value;
        }
    };

    internal class ClassesSimpleHierarchyA
    {
        private static int staticFieldA = ClassesSimpleRegistrator.RegisterAndReturn("staticFieldA I", 1);

        static ClassesSimpleHierarchyA()
        {
            staticFieldA = ClassesSimpleRegistrator.RegisterAndReturn("staticFieldA II", 2);
        }

        protected int num = ClassesSimpleRegistrator.RegisterAndReturn("field num I", 1);

        public ClassesSimpleHierarchyA()
        {
            num = ClassesSimpleRegistrator.RegisterAndReturn("ClassesSimpleHierarchyA I", 10);
        }

        public ClassesSimpleHierarchyA(int i)
        {
            num = ClassesSimpleRegistrator.RegisterAndReturn("ClassesSimpleHierarchyA(int i) I", i);
        }

        public int GetNum()
        {
            return num;
        }
    }

    internal class ClassesSimpleHierarchyA1 : ClassesSimpleHierarchyA
    {
        private static int staticFieldA1 = ClassesSimpleRegistrator.RegisterAndReturn("staticFieldA1 I", 1);

        static ClassesSimpleHierarchyA1()
        {
            staticFieldA1 = ClassesSimpleRegistrator.RegisterAndReturn("staticFieldA1 II", 2);
        }

        protected int num1 = ClassesSimpleRegistrator.RegisterAndReturn("field num1 I", 1);

        public ClassesSimpleHierarchyA1()
        {
            num1 = ClassesSimpleRegistrator.RegisterAndReturn("ClassesSimpleHierarchyA1 I", 2);
        }

        public ClassesSimpleHierarchyA1(int i) : base(ClassesSimpleRegistrator.RegisterAndReturn("ARG ClassesSimpleHierarchyA1(int i) : base I", i-1))
        {
            num1 = ClassesSimpleRegistrator.RegisterAndReturn("ClassesSimpleHierarchyA1(int i) I", num + 1);
        }
    }

    internal class ClassesSimpleHierarchyA2 : ClassesSimpleHierarchyA1
    {
        private static int staticFieldA2 = ClassesSimpleRegistrator.RegisterAndReturn("staticFieldA2 I", 1);

        static ClassesSimpleHierarchyA2()
        {
            staticFieldA2 = ClassesSimpleRegistrator.RegisterAndReturn("staticFieldA2 II", 2);
        }

        int num2 = ClassesSimpleRegistrator.RegisterAndReturn("field num2 I", 1);

        public ClassesSimpleHierarchyA2()
        {
            num2 = ClassesSimpleRegistrator.RegisterAndReturn("ClassesSimpleHierarchyA2 I", 3);
        }

        public ClassesSimpleHierarchyA2(int i) : base(ClassesSimpleRegistrator.RegisterAndReturn("ARG ClassesSimpleHierarchyA2(int i) : base I", i-1))
        {
            num2 = ClassesSimpleRegistrator.RegisterAndReturn("ClassesSimpleHierarchyA2(int i) I", num1 + 1);
        }

        public ClassesSimpleHierarchyA2(int i, int j) : this(ClassesSimpleRegistrator.RegisterAndReturn("ARG ClassesSimpleHierarchyA2(int i, int j) : this I", i))
        {
            ClassesSimpleRegistrator.RegisterAndReturn("ClassesSimpleHierarchyA2(int i, int j) I", j);
        }

        public new int GetNum()
        {
            return num2;
        }

        public int GetNum2()
        {
            return num1 + num2 + num;
        }
    }

    internal static class ClassesSimpleExceptionInitializer
    {
        public static int Init0(int n)
        {
            throw null;
        }

        public static int Init1(int n)
        {
            if (n > 0)
            {
                throw null;
            }
            return n;
        }
    }

    internal class ClassesSimpleException0
    {
        private static int field0 = ClassesSimpleExceptionInitializer.Init0(24);
    }

    internal class ClassesSimpleException1
    {
        private static int field0 = ClassesSimpleExceptionInitializer.Init1(-24);
        private static int field1 = ClassesSimpleExceptionInitializer.Init1(24);
    }

    [TestSvmFixture]
    public static class ClassesSimple
    {
        [TestSvm]
        public static bool Test1(int n)
        {
            ClassesSimpleA a = new ClassesSimpleA(n);
            a.IncN();
            a.DecN();
            return n == a.GetN();
        }

        struct MyStruct
        {
            public int MyValue;
        }

        [TestSvm]
        public static int SimpleStructureAccess(int newMyValue)
        {
            var x = new MyStruct();
            x.MyValue = newMyValue;
            return x.MyValue;
        }

        [TestSvm]
        public static int ValueTypeMethod(int x, int y)
        {
            return x.CompareTo(y);
        }
    }

    [TestSvmFixture]
    public static class ClassesSimpleException
    {
        [Ignore("Exceptions handling")]
        public static void Test1()
        {
            ClassesSimpleException0 a = new ClassesSimpleException0();
        }

        [Ignore("Exceptions handling")]
        public static void Test2()
        {
            ClassesSimpleException1 a = new ClassesSimpleException1();
        }
    }


    [TestSvmFixture]
    public static class ClassesSimpleHierarchy
    {
        [Ignore("Byref is not implemented")]
        public static List<string> Test1()
        {
            ClassesSimpleHierarchyA2 a = new ClassesSimpleHierarchyA2(123, 42);
            return ClassesSimpleRegistrator.entries;
        }

        [Ignore("Byref is not implemented")]
        public static int Test2()
        {
            ClassesSimpleHierarchyA2 a = new ClassesSimpleHierarchyA2();
            return a.GetNum2();
        }
    }

    [TestSvmFixture]
    public struct SimpleStruct
    {
        public static int X;
        public static int Y;
        public static int Z;

        [Ignore("Insufficient information")]
        public void Set(int x)
        {
            X = x;
        }
    }
    [TestSvmFixture]
    public static class ClassesSimpleMerge
    {
        class A
        {
            public int x;
        }

        class B
        {
            public int x;
        }

        [TestSvm]
        public static object MergingClassesFieldsTest(object o)
        {
            if (o is A)
            {
                var tmp = o as A;
                tmp.x = 2;
            }
            else if (o is B)
            {
                var tmp = o as B;
                tmp.x = 10;
            }
            return o;
        }
    }

    [TestSvmFixture]
    public class ClassesSimplePropertyAccess
    {
        private SimpleStruct _structProperty;

        public struct SimpleStruct
        {
            public int X;

            public void Set(int x)
            {
                X = x;
            }
        }

        public SimpleStruct StructProperty
        {
            get { return _structProperty;}
            set { _structProperty = value; }
        }

        private List<Boolean> SecretProperty { get; set; }

        public int Property
        {
            get { return this.SecretProperty.Count; }
        }

        public ClassesSimplePropertyAccess()
        {
            SecretProperty = new List<bool>();
        }

        [Ignore("_structProperty field is lost #fix")]
        public void TestProperty1()
        {
            var st = new ClassesSimplePropertyAccess();
            st.StructProperty.Set(42);
        }

        [TestSvm]
        public void TestProperty2()
        {
            var st = new SimpleStruct();
            st.Set(42);
        }
    }

    [TestSvmFixture]
    public class ClassesSimplePropertyAccessModify
    {
        public class SimpleContainer
        {
            public int X;

            public void Set(int x)
            {
                X = x;
            }
        }

        public SimpleContainer ContainerProperty
        { get; set; }

        [TestSvm]
        public int TestProperty1(int anyVarName)
        {
            return (new ClassesSimplePropertyAccessModify().ContainerProperty = new SimpleContainer()).X = anyVarName; // anyVarName
        }

        [Ignore("Exceptions handling")]
        public void FirstUseInGuard(SimpleContainer s)
        {
            if (s.X > 5)
                return;
            s.X = 42;
        }

        [Ignore("Exceptions handling")]
        public static void UnionInReference(SimpleContainer a)
        {
            var obj = (object) a;
            var b = (SimpleContainer) obj;
            b.Set(42);
        }

    }
}
