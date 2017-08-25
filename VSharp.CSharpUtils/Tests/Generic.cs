using System;
using System.Collections.Generic;

namespace VSharp.CSharpUtils.Tests
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

    public static class Generic<T, U, P, K, N, Z>
        where T : U
        where U : IKeeper<P>
        where P : struct, IKeeper<T>
        where K : class, IKeeper<U>
        where N : IKeeper<K>
        where Z : List<int>
    {
        public static Dictionary<int, T> RetDictionary()
        {
            return new Dictionary<int, T>();
        }
        
        public static List<double> RetList()
        {
            return new List<double>();
        }
        
        public static T RetT(T t)
        {
            return t;
        }

        public static Dictionary<T, Dictionary<U, Dictionary<List<K>, Dictionary<Queue<N>, Dictionary<Z, double>>>>> GenericParameter1(T t, U u, P p, K k, N n, Z z)
        {
            var a = new Dictionary<T, Dictionary<U, Dictionary<List<K>, Dictionary<Queue<N>, Dictionary<Z, double>>>>>();
            return a;
        }
    }

    public class Foo<T>
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
    public interface IFoo<out T> where T : Piece {}
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
