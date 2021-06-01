using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace VSharp.Test.Tests
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
    public class ForKostya
    {
        [TestSvm]
        public static MatchCollection GetMatches()
        {
            // Define a regular expression for repeated words.
            Regex rx = new Regex(@"\b(?<word>\w+)\s+(\k<word>)\b",
                RegexOptions.Compiled | RegexOptions.IgnoreCase);

            // Define a test string.
            string text = "The the quick brown fox  fox jumps over the lazy dog dog.";

            // Find matches.
            MatchCollection matches = rx.Matches(text);
            return matches;
        }

        [TestSvm]
        public static MatchCollection SmallGetMatches()
        {
            // Define a regular expression for repeated words.
            Regex rx = new Regex(@"\b",
                RegexOptions.Compiled | RegexOptions.IgnoreCase);

            // Define a test string.
            string text = "fox  ";

            // Find matches.
            MatchCollection matches = rx.Matches(text);
            return matches;
        }

        [TestSvm]
        public static int LinqTest()
        {
            // TODO: use group by and so on #do
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
        public static int SymbolicLinqTest(int x, int y, int z)
        {
            // TODO: use group by and so on #do
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
                }
            }

            return result;
        }

        class Customer
        {
            public int ID { get; set; }
            public string Name { get; set; }
            public string City { get; set; }
        }

        class Distributor
        {
            public int ID { get; set; }
            public string Name { get; set; }
            public string City { get; set; }
        }

        [TestSvm]
        public static string LinqTest2(int x, int y, int z, int f, int g)
        {
            var customers = new List<Customer>
            {
                new Customer {ID = x, Name = "Jack", City = "Rybinsk"},
                new Customer {ID = y, Name = "John", City = "Moscow"},
                // new Customer {ID = z, Name = "Dima", City = "SPB"},
                // new Customer {ID = f, Name = "Yuri", City = "Novosibirsk"}
            };
            var distributors = new List<Distributor>
            {
                new Distributor {ID = x, Name = "Ivan", City = "Rybinsk"},
                new Distributor {ID = y, Name = "Polina", City = "Moscow"},
                // new Distributor {ID = z, Name = "Olga", City = "SPB"},
                // new Distributor {ID = f, Name = "Lena", City = "Novosibirsk"},
            };
            var innerJoinQuery =
                from cust in customers
                group cust by cust.City into custGroup
                join dist in distributors on custGroup.FirstOrDefault().ID equals dist.ID
                where custGroup.FirstOrDefault().ID > 0 && (dist.Name == "Ivan" || dist.Name == "Lena")
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

        [TestSvm]
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
