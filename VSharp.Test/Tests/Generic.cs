using System;
using System.Collections;
using System.Collections.Generic;
using IntegrationTests.Typecast;
using NUnit.Framework;
using VSharp.Test;
#pragma warning disable CS0693

namespace IntegrationTests
{
    public interface IKeeper<in T>
    {
        void Keep(T obj);
    }

    public class Bag<T> : IKeeper<T>
    {
        private Queue<T> _queue;

        public Bag()
        {
            _queue = new Queue<T>();
        }

        public void Keep(T obj)
        {
            _queue.Enqueue(obj);
        }
    }

    public interface IGenericInterface1<out T> { }

    public interface IGenericInterface2<out T> { }

    public interface IInterface2<out T, out U> { }

    public class BothGenericInterface<T> : IGenericInterface1<T>, IGenericInterface2<T>
        where T: class
    { }

    public interface IForSpecialConstraints<out T> { }

    public class ForSpecialConstraints<T> : IForSpecialConstraints<T> { }
    public struct StructForSpecialConstraints<T> : IForSpecialConstraints<T> { }

    public sealed class Sealed<T> { }

    [TestSvmFixture]
    public static class GenericInitialize
    {
        [TestSvm]
        public static LinkedList<int> RetDictionary()
        {
            return new LinkedList<int>();
        }

        [TestSvm]
        public static List<double> RetList()
        {
            return new List<double>();
        }
    }

    [TestSvmFixture]
    public static class GenericSubst
    {
        public static class MyList<T>
        {
            public class MyEnumerator<T>
            {
                public class MyCurrent<T>
                {
                    public int Position = 0;
                }
                public MyCurrent<T> Current = new MyCurrent<T>();
            }
        }

        [TestSvm]
        public static int SubstNestedGeneric(MyList<object>.MyEnumerator<double>.MyCurrent<long> current)
        {
            if (current.Position > 0)
                return 0;
            return 42;
        }
    }

    [TestSvmFixture]
    public static class GenericClass<R, V>
        where R : class
        where V : struct
    {
        [TestSvm]
        public static R RetR(R r)
        {
            return r;
        }

        public static object Ret0(R r)
        {
            return 0;
        }

        [TestSvm]
        public static V RetV(V v)
        {
            return v;
        }
    }

    [TestSvmFixture]
    public static class SpecializeGenerics
    {
        [TestSvm]
        public static object RetConstructedRWithInt()
        {
            return GenericClass<object, int>.RetR(0);
        }

        // [TestSvm]
        // public static object RetConstructedRWithInt2()
        // {
        //     return GenericClass<int, int>.RetR(0, 0);
        // }

        [TestSvm]
        public static object RetConstructedR0()
        {
            return GenericClass<object, int>.Ret0(0);
        }

        [TestSvm]
        public static object RetConstructedRWithObject()
        {
            return GenericClass<object, int>.RetR(new object());
        }

        [TestSvm]
        public static int RetConstructedVWithInt()
        {
            return GenericClass<object, int>.RetV(0);
        }

        [TestSvm]
        public static SimpleStruct Test_OnlyGenericMethod_1()
        {
            return GenericMethodInsideGenericType<object, object, int>.OnlyGenericMethod<SimpleStruct>(new SimpleStruct());
        }

        [TestSvm]
        public static object Test_OnlyGenericMethod_2()
        {
            var obj = 1;
            return GenericMethodInsideGenericType<object, object, int>.OnlyGenericMethod<object>(obj);
        }

        [TestSvm]
        public static int Test_MixedGenericParameterAndTypeGenerics_RetW_1()
        {
            int w = 1;
            object t = new object();
            object r = new object();
            int v = 2;
            return GenericMethodInsideGenericType<object, object, int>.MixedGenericParameterAndTypeGenerics_RetW<int>(w, t, r, v);
        }

        [TestSvm]
        public static object Test_MixedGenericParameterAndTypeGenerics_RetT_1()
        {
            int w = 1;
            object t = new object();
            object r = new object();
            int v = 2;
            return GenericMethodInsideGenericType<object, object, int>.MixedGenericParameterAndTypeGenerics_RetT<int>(w, t, r, v);
        }

        [TestSvm]
        public static object Test_MixedGenericParameterAndTypeGenerics_RetT_2()
        {
            int w = 1;
            object t = new object();
            int v = 2;
            return GenericMethodInsideGenericType<object, object, int>.MixedGenericParameterAndTypeGenerics_RetT<int>(w, t, t, v);
        }

        [TestSvm]
        public static int Test_MixedGenericParameterAndTypeGenerics_RetV_1()
        {
            int w = 1;
            object t = new object();
            object r = new object();
            int v = 2;
            return GenericMethodInsideGenericType<object, object, int>.MixedGenericParameterAndTypeGenerics_RetV<int>(w, t, r, v);
        }

        [TestSvm]
        public static int Test_RetDuplicateV_1()
        {
            int v = 1;
            return GenericMethodInsideGenericType<IKeeper<int>, IKeeper<object>, SimpleStruct>.RetDuplicateV<int>(v);
        }
    }

    [TestSvmFixture]
    public static class GenericMethodInsideGenericType<T, R, V>
        where R : class
        where V : struct
    {
        public static W OnlyGenericMethod<W>(W w)
        {
            return w;
        }

        public static W MixedGenericParameterAndTypeGenerics_RetW<W>(W w, T t, R r, V v)
        {
            return w;
        }

        public static T MixedGenericParameterAndTypeGenerics_RetT<W>(W w, T t, R r, V v)
        {
            return t;
        }

        public static T MixedGenericParameterAndTypeGenerics_RetT<W>(W w, V r, T t, V v)
        {
            return t;
        }

        public static V MixedGenericParameterAndTypeGenerics_RetV<W>(W w, T t, R r, V v)
        {
            return v;
        }

        // this type parameter duplication is done intentionally
        public static V RetDuplicateV<V>(V v)
        {
            return v;
        }
    }



    [TestSvmFixture]
    public class Foo<T, U>
    {
        private T _field;

        public Foo()
        {
        }

        [TestSvm]
        public T GetFields()
        {
            return _field;
        }

        [TestSvm]
        public void SetField(T f)
        {
            _field = f;
        }
    }

    [TestSvmFixture]
    public static class GenericMethod
    {
        [TestSvm(100)]
        public static int TestFoo(Foo<int, Piece> f)
        {
            if (f == null) return 0;
            return f.GetFields();
        }
    }

    [TestSvmFixture]
    public static class GenericCandidates
    {
        [TestSvm(100)]
        public static int Subtyping(object o)
        {
            if (o is List<int>)
                return 2;
            if (o is Dictionary<int, int>)
                return 3;
            return 1;
        }

        [TestSvm(100)]
        public static int SubtypingInterface(object o)
        {
            if (o is IEnumerable<IMovable>)
            {
                if (o is List<Knight>)
                    return 3;
                if (o is HashSet<Pawn>)
                    return 4;
                if (o is List<Pawn>)
                    return 5;
                if (o is HashSet<Knight>)
                    return 6;
                if (o is IEnumerable<Piece>)
                    return 7;
                return 2;
            }
            return 1;
        }

        [TestSvm(100)]
        public static int DeepPropagating1(IEnumerable<IMovable> o)
        {
            if (o != null)
            {
                if (o is ICollection<Knight>)
                    return 2;
                return 1;
            }

            return 0;
        }

        [TestSvm(100)]
        public static int DeepPropagating2(object o)
        {
            if (o is IEnumerable<KeyValuePair<Piece, int>>)
            {
                if (o is Dictionary<Piece, int>)
                    return 3;
                return 2;
            }
            return 1;
        }

        [TestSvm(100)]
        public static int DeepPropagating3(IGenericInterface1<IMovable> o)
        {
            if (o is IGenericInterface2<Knight>)
                return 2;
            return 1;
        }

        [TestSvm(100)]
        public static int StandaloneInterface(object o)
        {
            if (o is List<IGenericInterface2<int>>)
                return 2;
            return 1;
        }

        [TestSvm(100)]
        public static int NoSolution1(object o)
        {
            if (o is IEnumerable<int>)
            {
                if (o is List<Knight>)
                    return 3;
                return 2;
            }
            return 1;
        }

        [TestSvm(100)]
        public static int NoSolution2(object o)
        {
            if (o is IInterface2<IMovable, object>)
            {
                if (o is IInterface2<Piece, object>)
                    return 3;
                return 2;
            }
            return 1;
        }

        [TestSvm(100)]
        public static int Nested1(object o)
        {
            if (o is IEnumerable<object>)
            {
                if (o is IEnumerable<IEnumerable<object>>)
                {
                    if (o is IEnumerable<IEnumerable<IEnumerable<object>>>)
                    {
                        if (o is IEnumerable<IEnumerable<IEnumerable<IEnumerable<object>>>>)
                        {
                            if (o is IEnumerable<IEnumerable<IEnumerable<IEnumerable<IEnumerable<object>>>>>)
                            {
                                if (o is List<List<List<List<List<List<List<List<object>>>>>>>>)
                                    return 228;
                                return 6;
                            }
                            return 5;
                        }
                        return 4;
                    }
                    return 3;
                }
                return 2;
            }
            return 1;
        }

        [TestSvm(100)]
        public static int Nested2(object o)
        {
            if (o is Sealed<Sealed<Sealed<Sealed<Sealed<Sealed<int>>>>>>)
                return 2;
            return 1;
        }
    }

    [TestSvmFixture]
    public static class MockRelocation<T, U> where T: unmanaged, IEquatable<U>
    {
        [TestSvm(100)]
        public static bool Contains(List<T> list, U value)
        {
            return true;
        }
    }

    [TestSvmFixture]
    public static class MockRelocation2<T> where T: unmanaged, IComparable<T>
    {
        [TestSvm(100)]
        public static bool Contains(List<T> list)
        {
            return true;
        }
    }

    [TestSvmFixture]
    public static class MockRelocation3<T, U>
        where T : unmanaged, IComparable<T>
        where U : IComparer<T>
    {
        [Ignore("support parameters with cyclic dependencies")]
        public static bool Contains(List<T> list, U value)
        {
            return true;
        }
    }

    [TestSvmFixture]
    public static class MockRelocation4<T, U>
        where T : unmanaged, IComparable<U>
        where U : IComparable<T>
    {
        [Ignore("support parameters with cyclic dependencies")]
        public static bool Contains(List<T> list, U value)
        {
            return true;
        }
    }

    [TestSvmFixture]
    public static class MethodParameters
    {

        [TestSvmFixture]
        public static class MethodParameters1<T>
            where T : IEnumerable<int>
        {
            [TestSvm(100)]
            public static int Method() => 1;
        }

        [TestSvmFixture]
        public static class MethodParameters2<T, U>
            where T : IEnumerable<IEnumerable<U>>
        {
            [TestSvm(100)]
            public static int Method() => 1;
        }

        [TestSvmFixture]
        [IgnoreFuzzer("Need send type-solving result into fuzzer")]
        public static class MethodParameters3<T, U>
            where T : IGenericInterface1<U>, IGenericInterface2<U>
        {
            [TestSvm(100)]
            public static int Method()
            {
                return 1;
            }
        }

        [TestSvmFixture]
        [IgnoreFuzzer("Need send type-solving result into fuzzer")]
        public static class MethodParameters4<T1, T2, U>
            where T1: class, IForSpecialConstraints<U>, new()
            where T2: struct, IForSpecialConstraints<U>
        {
            [TestSvm(100)]
            public static int Method()
            {
                return 1;
            }
        }
    }

    [TestSvmFixture]
    public static class Arrays
    {
        [TestSvm(100)]
        public static int ArraySubtyping1(object o)
        {
            if (o is object[])
            {
                if (o is string[])
                    return 3;

                return 2;
            }

            return 1;
        }

        [TestSvm(100)]
        public static int ArraySubtyping2(Array o)
        {
            if (o is IList)
            {
                if (o is IList<int>)
                    return 3;
                return 2;
            }

            return 1;
        }

        [TestSvm(100)]
        public static int JaggedArray(object o)
        {
            if (o is object[])
            {
                if (o is object[][])
                {
                    if (o is string[][][])
                        return 4;
                    return 3;
                }

                return 2;
            }

            return 1;
        }

        [TestSvm(100)]
        public static int MultidimensionalArray1(object o)
        {
            if (o is object[,,])
            {
                if (o is string[,,])
                    return 3;
                return 2;
            }

            return 1;
        }

        [TestSvm(100)]
        public static int MultidimensionalArray2(object o)
        {
            if (o is object[,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,])
            {
                if (o is string[,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,])
                    return 3;
                return 2;
            }

            return 1;
        }

        [TestSvm(76)]
        public static int MultidimensionalArray3(object o)
        {
            if (o is object[,])
            {
                if (o is object[,,])
                    return 3;
                if (o is string[,,])
                    return 4;
                return 2;
            }

            return 1;
        }

        [TestSvm(100)]
        public static int ArrayWithGeneric1(object o)
        {
            if (o is IEnumerable<int>[])
            {
                if (o is List<int>[])
                    return 3;
                return 2;
            }

            return 1;
        }

        [TestSvm(100)]
        public static int ArrayWithGeneric2(object o)
        {
            if (o is IEnumerable<int>[,])
            {
                if (o is List<int>[,])
                    return 3;
                return 2;
            }

            return 1;
        }

        [TestSvm(100)]
        public static int ArrayWithGeneric3(object o)
        {
            if (o is IEnumerable<object>[,])
            {
                if (o is IEnumerable<object[]>[,])
                {
                    if (o is IEnumerable<int[][,,]>[,])
                        return 4;
                    return 3;
                }
                return 2;
            }

            return 1;
        }

        [TestSvm(100)]
        public static int ArrayWithGeneric4(object o)
        {
            if (o is IForSpecialConstraints<object>[] array)
            {
                if (array[0] is ValueType)
                {
                    return 3;
                }

                return 2;
            }

            return 1;
        }

        [TestSvm(83)]
        public static int ArrayWithGeneric5(object o)
        {
            if (o is ValueType[])
            {
                if (o is IForSpecialConstraints<object>[])
                {
                    return 3;
                }

                return 2;
            }
            return 1;
        }

        [TestSvm(100)]
        public static int ArrayInterfaces1(object o)
        {
            if (o is IEnumerable<int>)
            {
                if (o is int[])
                    return 3;
                return 2;
            }

            return 1;
        }

        [TestSvm(100)]
        public static int ArrayInterfaces2(object o)
        {
            if (o is IEnumerable)
            {
                if (o is int[,])
                    return 3;
                return 2;
            }

            return 1;
        }
    }

    [TestSvmFixture]
    public static class TestUnion
    {
        public static Coord RetCoord(Object obj, Coord coord, int field)
        {
            if (obj is BlackPawn)
            {
                coord.X = 42;
            }
            if (obj is Pawn)
            {
                coord.X += 66;
            }
            return coord;
        }

        public static Object Ret(Object obj)
        {
            var f = obj as BlackPawn;
            if (f != null)
            {
                f.SetNewField(42);
            }
            var a = obj as Pawn;
            if (a != null)
            {
                int b = a.GetNewField();
                a.SetNewField(b + 66);
            }
            return obj;
        }

        [TestSvm(100)]
        public static int RetWorked(Object obj, int a)
        {
            if (obj as BlackPawn != null)
            {
                a = 5;
            }
            if (obj as Pawn != null)
            {
                a = a + 6;
            }
            return a;
        }
    }

    [TestSvmFixture]
    public static class NestedGenerics
    {
        [TestSvm(100)]
        [IgnoreFuzzer("Need recursion constraints in generators")]
        public static int NestedGenericsSmokeTest(List<Bag<int>> list)
        {
            if (list.Count > 0)
            {
                return 0;
            }

            return 1;
        }

        [TestSvm(100)]
        [IgnoreFuzzer("Need recursion constraints in generators")]
        public static int NestedGenericsSmokeTest2(Dictionary<int, Bag<int>> dict)
        {
            if (dict.Count > 0)
            {
                return 0;
            }

            return 1;
        }
    }

//    public static class GenericCast
//    {
//        public static void FilterAndKeep(List<Pawn> listPawn, IKeeper<Pawn> bag)
//        {
//            foreach (var pawn in listPawn)
//            {
//                if (pawn.GetNewField() > 5)
//                {
//                    bag.Keep(pawn);
//                }
//            }
//        }
//
//        public static void TestFilterAndKeep()
//        {
//            IKeeper<Piece> myKeeper = new Bag<Piece>();
//            List<Pawn> myList = new List<Pawn>();
//            FilterAndKeep(myList, myKeeper);
//        }
//
//        public static void TestContrvarianceAndCovariance()
//        {
//            Func<Piece, Pawn> func = (x => new Pawn(x.GetCoord(), x.GetRate()));
//            Func<Pawn, Piece> newFunc = func;
//        }
//    }


//    public interface IFooTest<T, U>
//        where T : IFoo<U>
//        where U : IFoo<T>
//    {
//
//    }
//
//    public class FooFoo<T, U> {}
//
    public interface IFoo<out T> where T : Typecast.Piece {}
//
//    public interface IFooOne : IFoo<IFooTwo> {}
//
//    public interface IFooTwo : IFoo<IFooOne> {}
//
//    public class RecFoo : Foo<RecFoo> {}
//    public class RecFooOne : Foo<RecFooTwo> {}
//    public class RecFooTwo : Foo<RecFooOne> {}
//
//    public class Ret<T, U>
//        where T : Foo<U>, new()
//        where U : Foo<T>
//    {
//TODO: don't parse, need fix
//        public delegate Object MyDelegateOne(String obj);
//
//        public delegate Object MyDelegateTwo(Object obj);
//
//        public T PropT { get; set; }
//        public U PropU { get; set; }
//
//        public bool TestCheckCast(RecFooOne obj1, RecFooTwo obj2)
//        {
//            return obj1 is T && obj2 is U;
//        }
//    }
}
