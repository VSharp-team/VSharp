using System;
using System.Collections.Generic;

namespace VSharp.CSharpUtils.Tests
{
    public interface IKeeper<in T>
    {
        void Keep(T obj);
    }

    public class Foo<T>
    {
        private T _filed;

        public Foo()
        {
            
        }

        public IEnumerable<T> GetFiels()
        {
            return new List<T> { _filed };
        }

        public void SetField(T f)
        {
            _filed = f;
        }
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

    public static class Generic
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

    public static class Function
    {
        public class Base
        {
            
        }

        public delegate Object MyDelegateOne(String obj);

        public delegate Object MyDelegateTwo(Object obj);
        //public static Func<int, int> GenFunc()
        //{
        //    return x => x * x;
        //}
        //
        //public static Action<int> GetAction()
        //{
        //    return x => Console.WriteLine(x);
        //}

        public static T RetT<T, U>() where T : Foo<U>, ICloneable, new()
        {
            return new T();
        }

        public static void TestDelegate()
        {
            MyDelegateOne a = str => str;
            MyDelegateTwo b = obj => obj.ToString();
        }
    }
}
