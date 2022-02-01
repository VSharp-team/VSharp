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

        [TestSvm(85)]
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

        class Customer
        {
            public int ID { get; set; }
            public char Name { get; set; }
            public char City { get; set; }
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

        class Distributor
        {
            public int ID { get; set; }
            public char Name { get; set; }
            public char City { get; set; }
        }

        // TODO: add this test after concolic will be implemented
        [Ignore("need deep copy for concrete memory or concolic")]
        public static string HardSymbolicLinqTest(int x, int y, int z, int f, int g)
        {
            var customers = new List<Customer>
            {
                new Customer {ID = x, Name = 'J', City = 'R'},
                new Customer {ID = y, Name = 'R', City = 'M'},
                // new Customer {ID = z, Name = "Dima", City = "SPB"},
                // new Customer {ID = f, Name = "Yuri", City = "Novosibirsk"}
            };
            var distributors = new List<Distributor>
            {
                new Distributor {ID = x, Name = 'I', City = 'R'},
                new Distributor {ID = y, Name = 'P', City = 'M'},
                // new Distributor {ID = z, Name = "Olga", City = "SPB"},
                // new Distributor {ID = f, Name = "Lena", City = "Novosibirsk"},
            };
            var innerJoinQuery =
                from cust in customers
                group cust by cust.City into custGroup
                join dist in distributors on custGroup.FirstOrDefault().ID equals dist.ID
                where custGroup.FirstOrDefault().ID > 0 && (dist.Name == 'I' || dist.Name == 'L')
                // orderby custGroup.Key
                select dist;
            var result = "";
            foreach (var dist in innerJoinQuery)
            {
                result += dist.Name + " ";
            }

            var res2 = innerJoinQuery.Aggregate("", (current, dist) => current + (dist.Name + " "));
            return res2;
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

        [Ignore("takes too much time")]
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
