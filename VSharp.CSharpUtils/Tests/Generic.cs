using System;
using System.Collections.Generic;
using System.Runtime.Remoting.Messaging;

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

    public static class GenericCast
    {
        public static void FilterAndKeep(List<Pawn> listPawn, IKeeper<Pawn> bag)
        {
            foreach (var pawn in listPawn)
            {
                if (pawn.GetNewField() > 5)
                {
                    bag.Keep(pawn);
                }
            }
        }

        public static void TestFilterAndKeep()
        {
            IKeeper<Piece> myKeeper = new Bag<Piece>();
            List<Pawn> myList = new List<Pawn>();
            FilterAndKeep(myList, myKeeper);
        }

        public static void TestContrvarianceAndCovariance()
        {
            Func<Piece, Pawn> func = (x => new Pawn(x.GetCoord(), x.GetRate()));
            Func<Pawn, Piece> newFunc = func;
        }
    }
    
    public static class Generic<T>
    {
        public static Dictionary<int, T> ReDictionary()
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

    
//    public interface IFooTest<T, U>
//        where T : IFoo<U>
//        where U : IFoo<T>
//    {
//        
//    }
//    
//    public class FooFoo<T, U> {}
//    
    public interface IFoo<in T> where T : struct {}
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
//        public T PropT { get; set; }
//        public U PropU { get; set; }
//
//        public bool TestCheckCast(RecFooOne obj1, RecFooTwo obj2)
//        {
//            return obj1 is T && obj2 is U;
//        }
//    }
    
    public static class Function<U, K>
    {
        
//        public static bool RecTypecast1<T>(Foo<U> obj) where T : Foo<U>
//        {
//            return obj is T;
//        } 
        
        public static bool RecTypecast2<T>(Foo<int> obj, T obj1, T[] heh) where T : struct, IFoo<int>
        {
            return obj is T;
        }
        
        public static bool RecTypecast2<T>(Foo<int> obj) where T : struct, IFoo<int>
        {
            return obj is T;
        }

//        public static bool RecTypecast3<T>(Foo<U> obj) where T : Foo<T>
//        {
//            return obj is T;
//        }
        
//TODO: don't parse, need fix        
//        public delegate Object MyDelegateOne(String obj);
//
//        public delegate Object MyDelegateTwo(Object obj);


        //public static Func<int, int> GenFunc()
        //{
        //    return x => x * x;
        //}
        //
        //public static Action<int> GetAction()
        //{
        //    return x => Console.WriteLine(x);
        //}
      
//        public static void TestRet()
//        {
//            var a = new Ret<RecFooOne, RecFooTwo>();
//            var b = a.PropT.GetFields().GetFields().GetFields().GetFields();
//        }
//        public static void TestDelegate()
//        {
//            MyDelegateOne a = str => str;
//            MyDelegateTwo b = obj => obj.ToString();
//        }
    }
}
