using System;
using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;
using VSharp.Test;

namespace IntegrationTests
{
    public static class Extensions
    {
        public static IEnumerable<T> InterleaveSequenceWith<T>
            (this IEnumerable<T> first, IEnumerable<T> second)
        {
            var firstIter = first.GetEnumerator();
            var secondIter = second.GetEnumerator();

            while (firstIter.MoveNext() && secondIter.MoveNext())
            {
                yield return firstIter.Current;
                yield return secondIter.Current;
            }
        }

        public static bool SequenceEquals<T>
            (this IEnumerable<T> first, IEnumerable<T> second)
        {
            var firstIter = first.GetEnumerator();
            var secondIter = second.GetEnumerator();

            while (firstIter.MoveNext() && secondIter.MoveNext())
            {
                if (!firstIter.Current.Equals(secondIter.Current))
                {
                    return false;
                }
            }

            return true;
        }
    }
    [TestSvmFixture]
    public class LinqTest
    {
        [TestSvm]
        public static int ConcreteLinqTest()
        {
            int[] scores = { 97, 92, 81, 60 };

            IEnumerable<int> scoreQuery =
                from score in scores
                where score > 80 && score % 2 == 0
                select score;

            int result = 0;
            foreach (int i in scoreQuery)
            {
                if (i > 80)
                {
                    result += i;
                }
            }

            return result;
        }

        [TestSvm]
        public static int ConcreteLinqTest1()
        {
            object[] scores = { 97, 92, 81, 60 };

            var intList = scores.Cast<int>().ToList();

            return intList.Max();
        }

        [TestSvm(89)]
        public static int SimpleSymbolicLinqTest(int x, int y, int z)
        {
            int[] scores = { x, y, z, 60 };

            IEnumerable<int> scoreQuery =
                from score in scores
                where score > 80 && score % 2 == 0
                select score;

            int result = 0;
            foreach (int i in scoreQuery)
            {
                if (i > 80)
                {
                    result += i;
                } else if (i > 0)
                {
                    result += 5;
                }
                // else
                // {
                //     result++;
                // }
            }

            if (result != 8)
            {
                return 100;
            }

            return result;
        }

        [TestSvm(100)]
        public static int SymbolicLinqTest2(int x, int y, int z)
        {
            // TODO: use group by and so on
            int[] scores = { x, y, z, 60 };

            IEnumerable<int> scoreQuery =
                from score in scores
                where score > 80 && score % 2 == 0
                select score;

            int left = 0;
            int right = 0;

            foreach (int i in scoreQuery)
            {
                if (i > 90)
                {
                    left++;
                } else
                {
                    right++;
                }
            }

            if (right == 2 && left == 1)
            {
                return 100;
            }

            return left;
        }

        public class Customer
        {
            public int Id { get; set; }
            public long Money { get; set; }
            public int OrderId { get; set; }
        }

        public class Order
        {
            public int Id { get; set; }
            public int Cost { get; set; }
        }

        [TestSvm(100, strat: SearchStrategy.DFS)]
        public static long HardSymbolicLinqTest(int m1, int m2, int id1, int id2, int c1, int c2)
        {
            if (m1 <= 0 | m2 <= 0 | c1 <= 0 | c2 <= 0)
                throw new ArgumentException("wrong arguments");

            var customers = new List<Customer>
            {
                new Customer {Id = 1, Money = m1, OrderId = 1},
                new Customer {Id = 2, Money = m2, OrderId = 2},
            };
            var orders = new List<Order>
            {
                new Order {Id = id1, Cost = c1},
                new Order {Id = id2, Cost = c2},
            };
            var innerJoinQuery =
                from c in customers
                join o in orders on c.OrderId equals o.Id
                select c.Money / o.Cost;

            var validOrdersCount = innerJoinQuery.Count(x => x > 0);
            if (validOrdersCount > 0)
                return innerJoinQuery.Sum();

            return 0;
        }

        static IEnumerable<string> Suits()
        {
            yield return "clubs";
            yield return "diamonds";
            yield return "hearts";
            yield return "spades";
        }

        static IEnumerable<string> Ranks()
        {
            yield return "two";
            yield return "three";
            yield return "four";
            yield return "five";
            yield return "six";
            yield return "seven";
            yield return "eight";
            yield return "nine";
            yield return "ten";
            yield return "jack";
            yield return "queen";
            yield return "king";
            yield return "ace";
        }

        [TestSvm(100)]
        public static IEnumerable<int> SelectTest(string x)
        {
            var newList = Suits().Select(id => id);
            newList.Append(x);
            return newList.Select(i => i.Length);
        }

        [TestSvm(100)]
        public static int OrderByTest(int value)
        {
            var list = new List<Order>();
            var order = new Order { Id = value, Cost = value };
            list.Add(order);
            return list.OrderBy(o => o.Id).First().Id;
        }

        [TestSvm(100)]
        public static int SequenceLinqTest()
        {
            var startingDeck = (from s in Suits()
                    from r in Ranks()
                    select new { Suit = s, Rank = r })
                .ToArray();

            var times = 0;
            var shuffle = startingDeck;

            do {
                shuffle = shuffle.Skip(26)
                    .InterleaveSequenceWith(shuffle.Take(26))
                    .ToArray();
                times++;
            } while (!startingDeck.SequenceEquals(shuffle));

            return times;
        }
    }
}
