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

        class Customer
        {
            public string Name { get; set; }
            public string City { get; set; }
        }

        class Distributor
        {
            public string Name { get; set; }
            public string City { get; set; }
        }

        static IEnumerable<Customer> customers()
        {
            yield return new Customer {Name = "Jack", City = "Rybinsk"};
            yield return new Customer {Name = "John", City = "Moscow"};
            yield return new Customer {Name = "Dima", City = "SPB"};
            yield return new Customer {Name = "Yuri", City = "Novosibirsk"};
        }

        static IEnumerable<Distributor> distributors()
        {
            yield return new Distributor {Name = "Ivan", City = "Rybinsk"};
            yield return new Distributor {Name = "Polina", City = "Moscow"};
            yield return new Distributor {Name = "Olga", City = "SPB"};
            yield return new Distributor {Name = "Lena", City = "Novosibirsk"};
        }

        [TestSvm]
        public static string LinqTest2()
        {
            var innerJoinQuery =
                from cust in customers()
                group cust by cust.City into custGroup
                join dist in distributors() on custGroup.FirstOrDefault().City equals dist.City
                where custGroup.Count() > 0 && (dist.Name == "Ivan" || dist.Name == "Lena")
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
