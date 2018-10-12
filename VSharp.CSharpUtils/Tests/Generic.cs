using System;
using System.Collections.Generic;
using VSharp.CSharpUtils.Tests.Typecast;

namespace VSharp.CSharpUtils.Tests.Generic
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

    public static class GenericInitialize<T, U, P, K, N, Z>
        where T : U
        where U : IKeeper<P>
        where P : struct, IKeeper<T>
        where K : class, IKeeper<U>
        where N : IKeeper<K>
        where Z : List<int>
    {
        public static LinkedList<int> RetDictionary()
        {
            return new LinkedList<int>();;
        }
        
        public static List<double> RetList()
        {
            return new List<double>();
        }
        
        public static T RetT(T t)
        {
            return t;
        }
    }

    public static class GenericTest<T, U, P, K, N, Z>
        where T : U
        where U : IKeeper<P>
        where P : struct, IKeeper<T>
        where K : class, IKeeper<U>
        where N : IKeeper<K>
        where Z : List<int>
    {
        public static T RetT(T t)
        {
            return t;
        }

        public static U RetU(U u)
        {
            return u;
        }

        public static P RetP(P p)
        {
            return p;
        }

        public static K RetK(K k)
        {
            return k;
        }

        public static N RetT(N n)
        {
            return n;
        }

        public static Z RetU(Z z)
        {
            return z;
        }
    }

    public class Foo<T, U>
    {
        private T _filed;

        public Foo()
        {
        }

        public T GetFields()
        {
            return  _filed;
        }

        public void SetField(T f)
        {
            _filed = f;
        }
    }

    public static class GenericMethod
    {
        public static int TestFoo(Foo<int, Piece> f)
        {
            if (f == null) return 0;
            return f.GetFields();
        }

        public static int TestFoo(LinkedList<int> l)
        {
            if (l == null) return 0;
            if (l.First == null) return 1;
            return l.First.Value += 1;
        }
    }

    public static class TetsUnion
    {
//        public static Coord RetCoord(Object obj, Coord coord, int field)
//        {
//            if (obj is BlackPawn)
//            {
//                coord.X = 42;
//            }
//            if (obj is Pawn)
//            {
//                coord.X += 66;
//            }
//            return coord;
//        }

//        public static Object Ret(Object obj)
//        {
//            var f = obj as BlackPawn;
//            if (f != null)
//            {
//                f.SetNewField(42);
//            }
//            var a = obj as Pawn;
//            if (a != null)
//            {
//                int b = a.GetNewField();
//                a.SetNewField(b + 66);
//            }
//            return obj;
//        }

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
