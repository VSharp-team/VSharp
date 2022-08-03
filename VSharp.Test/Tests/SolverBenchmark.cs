using System;
using NUnit.Framework;
using VSharp.Test;

namespace IntegrationTests
{
    [TestSvmFixture]
    public static class SolverBenchmark
    {
        /*
         * Idea with Collatz conjecture is taken from
         * 'On Benchmarking the Capability of Symbolic Execution Tools with Logic Bombs'
         * by Hui Xu et al.
         */
        private static int CollatzStep(int a)
        {
            if (a % 2 == 0)
            {
                return a / 2;
            }

            return 3 * a + 1;
        }

        [TestSvm(100)]
        public static int CollatzLogicBomOmb1(int i)
        {
            if (i <= 0)
            {
                return 0;
            }

            var loopCount = 0;
            var j = i;

            while (j != 1)
            {
                j = CollatzStep(j);
                ++loopCount;
            }

            if (loopCount % 2 == 0)
            {
                return 1;
            }

            return 2;
        }
        
        [TestSvm(100)]
        public static int CollatzLogicBomOmb2(int i)
        {
            if (i <= 0 || i >= 100)
            {
                return 0;
            }

            var loopCount = 0;
            var j = i;

            while (j != 1)
            {
                j = CollatzStep(j);
                ++loopCount;
            }

            // Fails with 17
            if (loopCount == 9)
            {
                return 1;
            }

            return 2;
        }

        private static int CustomCollatzStep(int a, int b)
        {
            if (a % 2 == 0)
            {
                return a / 2;
            }

            return b * a + 1;
        }
        
        // Fails with j == 11 and loopCount == 14
        [TestSvm(100)]
        public static int CollatzLogicBomOmb3(int i)
        {
            var j = 12;
            var loopCount = 0;

            while (j != 1)
            {
                j = CustomCollatzStep(j, i);
                ++loopCount;
            }

            if (loopCount == 9)
            {
                return 1;
            }

            return 2;
        }
        
        [TestSvm(100)]
        public static int CollatzLogicBomOmb4(int i, int j)
        {
            var loopCount = 0;
            
            while (i != 1)
            {
                if (j == 1)
                {
                    break;
                }

                i = CollatzStep(i);
                j = CollatzStep(j);
                
                ++loopCount;
            }
            
            if (loopCount > 3)
            {
                return 1;
            }

            return 2;
        }
        
        [TestSvm(100)]
        public static int CollatzLogicBomOmb5(int i, int j, int k)
        {
            var loopCount = 0;
            
            while (i != 1)
            {
                if (j == 1)
                {
                    break;
                }

                if (k == 1)
                {
                    break;
                }

                i = CollatzStep(i);
                j = CollatzStep(j);
                k = CollatzStep(k);
                
                ++loopCount;
            }
            
            if (loopCount > 2)
            {
                return 1;
            }

            return 2;
        }
        
        [TestSvm(100)]
        public static int CollatzLogicBomOmb6(int i, int j)
        {
            var loopCount = 0;

            while (i + j != 2)
            {
                i = CollatzStep(i);
                j = CollatzStep(j);
                
                ++loopCount;
            }
            
            if (loopCount > 3)
            {
                return 1;
            }

            return 2;
        }
        
        [TestSvm(100)]
        public static int CollatzLogicBomOmb7(int i)
        {
            for (var j = 1; j < 100; ++j)
            {
                var k = j;
                
                while (k != i)
                {
                    k = CollatzStep(k);
                }
            }

            return 0;
        }
        
        [TestSvm(100)]
        public static int CollatzLogicBomOmb8(int i, int j)
        {
            for (var k = 1; k < 17; ++k)
            {
                var l = k;
                
                while (l != i)
                {
                    l = CustomCollatzStep(l, j);
                }
            }

            return 0;
        }

        private static int[,] sudoku =
        {
            { 0, 2, 0, 0 },
            { 3, 0, 0, 0 },
            { 2, 0, 0, 0 },
            { 0, 3, 0, 4 }
        };
        
        [Ignore("Fails with 'key not found' in getOpCode")]
        public static bool SudokuSolution(int[] values)
        {
            for (var i = 0; i < values.Length; ++i)
            {
                if (values[i] < 1 || values[i] > 4)
                {
                    return false;
                }
            }

            var zeroCount = 0;
            
            for (var i = 0; i < 4; ++i)
            {
                for (var j = 0; j < 4; ++j)
                {
                    if (sudoku[i, j] == 0)
                    {
                        ++zeroCount;
                    }
                }
            }

            if (zeroCount != values.Length)
            {
                return false;
            }

            var currentZero = 0;
            
            for (var i = 0; i < 4; ++i)
            {
                for (var j = 0; j < 4; ++j)
                {
                    if (sudoku[i, j] == 0)
                    {
                        sudoku[i, j] = values[currentZero];
                        ++currentZero;
                    }
                }
            }

            for (var i = 0; i < 4; ++i)
            {
                var filled = new bool[4];
                for (var j = 0; j < 4; ++j)
                {
                    filled[sudoku[i, j] - 1] = true;
                }
                for (var j = 0; j < 4; ++j)
                {
                    if (!filled[j])
                    {
                        return false;
                    }
                }
            }
            
            for (var i = 0; i < 4; ++i)
            {
                var filled = new bool[4];
                for (var j = 0; j < 4; ++j)
                {
                    filled[sudoku[j, i] - 1] = true;
                }
                for (var j = 0; j < 4; ++j)
                {
                    if (!filled[j])
                    {
                        return false;
                    }
                }
            }
            
            for (var i = 0; i < 4; i += 2)
            {
                for (var j = 0; j < 4; j += 2)
                {
                    var filled = new bool[4];
                    
                    for (var k = 0; k < 2; ++k)
                    {
                        for (var l = 0; l < 2; ++l)
                        {
                            filled[sudoku[i + k, j + l] - 1] = true;
                        }
                    }
                    
                    if (!filled[j])
                    {
                        return false;
                    }
                }
            }

            return true;
        }
    }
}
