using System;
using System.Collections.Generic;

namespace VSharp.CSharpUtils.Tests
{
    interface IKeeper<in T>
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

    static class Generic
    {
        static void FilterAndKeep(List<Pawn> listPawn, IKeeper<Pawn> bag)
        {
            foreach (var pawn in listPawn)
            {
                if (pawn.GetNewField() > 5)
                {
                    bag.Keep(pawn);
                }
            }
        }

        static void TestFilterAndKeep()
        {
            IKeeper<Piece> myKeeper = new Bag<Piece>();
            List<Pawn> myList = new List<Pawn>();
            FilterAndKeep(myList, myKeeper);
        }

        static void TestContrvarianceAndCovariance()
        {
            Func<Piece, Pawn> func = (x => new Pawn(x.GetCoord(), x.GetRate()));
            Func<Pawn, Piece> newFunc = func;
        }
    }
}
