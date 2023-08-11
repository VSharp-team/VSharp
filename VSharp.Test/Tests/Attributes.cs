using System;
using System.Diagnostics.CodeAnalysis;
using NUnit.Framework;
using VSharp.Test;

namespace IntegrationTests
{
    [TestSvmFixture]
    public class Attributes
    {
        [TestSvm]
        public int DisallowNullTest1([DisallowNull] object obj)
        {
            if (obj == null)
            {
                throw new NullReferenceException();
            }
            return 1;
        }

        [TestSvm]
        public int DisallowNullTest2([DisallowNull] object obj)
        {
            if (obj != null)
            {
                return 1;
            }
            return 0;
        }

        [TestSvm]
        public int DisallowNullTest3([DisallowNull] Typecast.Piece piece, int n)
        {
            if (n == 42)
            {
                return piece.GetRate();
            }
            if (n == 43)
            {
                return piece.GetRate() + n;
            }
            return 1;
        }

        [TestSvm]
        public int NotNullTest1([NotNull] object obj)
        {
            if (obj == null)
            {
                throw new NullReferenceException();
            }
            return 1;
        }

        [TestSvm]
        public int NotNullTest2([NotNull] object obj)
        {
            if (obj != null)
            {
                return 1;
            }
            return 0;
        }

        [TestSvm]
        public int NotNullTest3([NotNull] object obj)
        {
            return 1;
        }

        [TestSvm]
        [return : NotNull]
        public object NotNullTest4([DisallowNull] object obj)
        {
            return obj;
        }

        [TestSvm]
        [return : NotNull]
        public object NotNullTest5(object obj)
        {
            return obj;
        }

        [TestSvm]
        public int NotNullCallsDisallowNullTest1([NotNull] object obj)
        {
            return DisallowNullTest1(obj);
        }

        [TestSvm]
        public int NotNullCallsDisallowNullTest2([NotNull] object obj)
        {
            if (obj == null)
            {
                return DisallowNullTest1(obj);
            }
            return 1;
        }

        [TestSvm]
        public int NotNullCallsDisallowNullTest3([NotNull] object obj, int n)
        {
            if (n > 3)
            {
                return NotNullCallsDisallowNullTest3(obj, 3);
            }
            if (n > 0)
            {
                return NotNullCallsDisallowNullTest3(obj, n - 1);
            }
            if (obj == null)
            {
                return DisallowNullTest1(obj);
            }
            return 1;
        }

        [TestSvm]
        public int DisallowNullCallsNotNullTest([DisallowNull] object obj)
        {
            return NotNullTest1(obj);
        }

        [TestSvm(recThresholdForTest: 0u, strat: SearchStrategy.ShortestDistance)]
        public int ReadAll([NotNull] byte[] buffer)
        {
            int next;
            int count = 0;
            int count2 = 10;
            while (count2 >= 0)
            {
                --count2;
            }

            return count;
        }
    }

    [TestSvmFixture]
    public struct AttributesStruct
    {
        [TestSvm]
        public int DisallowNullTest([DisallowNull] object obj)
        {
            if (obj == null)
            {
                throw new NullReferenceException();
            }
            return 1;
        }

        [TestSvm]
        public int NotNullCallsDisallowNullTest([NotNull] object obj, int n)
        {
            if (n > 3)
            {
                return NotNullCallsDisallowNullTest(obj, 3);
            }
            if (n > 0)
            {
                return NotNullCallsDisallowNullTest(obj, n - 1);
            }
            if (obj == null)
            {
                return DisallowNullTest(obj);
            }
            return 1;
        }
    }
}
