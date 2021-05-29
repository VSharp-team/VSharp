using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.InteropServices.ComTypes;
using System.Text;
using NUnit.Framework;
using VSharp.Interpreter.IL;

namespace VSharp.Test.Tests.Pobs
{



    // [TestPobsFixture]
    public class SmokeTest
    {
        // [TestPobs]
        public static int EntryMethod(int x)
        {
            x++;
            throw null;
        }
    }




    public class UnsatCases
    {
        public static void EntryMethod(int x)
        {
            if (x > 0)
            {
                if (x <= 0)
                {
                    throw null;
                }
            }
        }
    }


    // [TestPobsFixture]
    public class SwitchWithSequentialCases1
    {
        // [TestPobs]
        public static int EntryMethod(int x)
        {
            switch (x)
            {
                case 0:
                    return 1;
                case 1:
                    return 11;
                case 2:
                    return 101;
                case 3:
                    return 1001;
                case 4:
                    throw null; // go here
                default:
                    return -1;
            }
        }
    }

    // [TestPobsFixture]
    public class CallsTest
    {
        public static int F()
        {
            int sum = 0;
            for (int i = 0; i < 10; i++)
            {
                sum += i;
            }

            return sum;
        }

        public static int G(int v)
        {
            throw null;
        }


        // [TestPobs]
        public static int EntryMethod2(int x)
        {
            if (x > 5)
            {
                return F();
            }

            return G(x);
        }
    }

    // [TestPobsFixture]
    public class LotsOfIfs
    {
        // [TestPobs]
        public static int EntryMethod(bool f1, bool f2, bool f3, bool f4, int x)
        {
            int res = 0;
            if (f1)
            {
                if (f2)
                {
                    if (f3)
                    {
                        if (f4)
                        {
                            return res = x;
                        }
                        else
                        {
                            return res = x + 1;
                        }
                    }
                    else
                    {
                        if (f4)
                        {
                            return res  = x + 2;
                        }
                        else
                        {
                            return res = x + 3;
                        }
                    }
                }
                else
                {
                    if (f3)
                    {
                        if (f4)
                        {
                            return res = x + 4;
                        }
                        else
                        {
                            return res = x + 5;
                        }
                    }
                    else
                    {
                        if (f4)
                        {
                            return res  = x + 6;
                        }
                        else
                        {
                            return res = x + 7;
                        }
                    }
                }
            }
            else
            {
                if (f2)
                {
                    if (f3)
                    {
                        if (f4)
                        {
                            return res = x + 8;
                        }
                        else
                        {
                            throw null;
                        }
                    }
                    else
                    {
                        if (f4)
                        {
                            return res  = x + 10;
                        }
                        else
                        {
                            return res = x + 11;
                        }
                    }
                }
                else
                {
                    if (f3)
                    {
                        if (f4)
                        {
                            return res = x + 12;
                        }
                        else
                        {
                            return res = x + 13;
                        }
                    }
                    else
                    {
                        if (f4)
                        {
                            return res  = x + 14;
                        }
                        else
                        {
                            return res = x + 15;
                        }
                    }
                }
            }
        }
    }
}
