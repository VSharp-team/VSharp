using System;
using System.Collections.Generic;
using System.Reflection;
using System.Diagnostics;
using System.Linq;

using NUnit.Framework;
using VSharp.Core;
using VSharp.Interpreter.IL;
using VSharp.Test.Tests;
using VSharp.Test.Tests.Pobs;

namespace VSharp.Test
{

    public class PobsSetup
    {

        static BindingFlags All =
            BindingFlags.IgnoreCase | BindingFlags.DeclaredOnly | BindingFlags.Instance |
            BindingFlags.Static | BindingFlags.NonPublic | BindingFlags.Public;

        public enum DesiredStatus
        {
            Witnessed,
            Unreachable,
            Unknown
        }

        public class CodeLocationProxy
        {
            public CodeLocationProxy(int offset, Type t, string methodName, DesiredStatus desiredStatus)
            {
                DesiredStatus = desiredStatus.ToString();
                Offset = offset;

                Method = t.GetMethod(methodName, All);
                Assert.IsNotNull(Method);
            }

            public int Offset { get; private set; }
            public string DesiredStatus { get; private set; }
            public MethodBase Method { get; private set; }
        }



        private static bool AnswerPobsForSearcher(MethodInfo entry, CodeLocationProxy[] proxies, IBidirectionalSearcher searcher, uint maxBound)
        {
            List<codeLocation> codeLocations = new List<codeLocation>();
            foreach (var p in proxies)
            {
                codeLocations.Add(new codeLocation(p.Offset, p.Method));
            }

            var svm = new SVM(new PobsInterpreter(searcher));
            svm.ConfigureSolver();
            // SVM.ConfigureSimplifier(new Z3Simplifier()); can be used to enable Z3-based simplification (not recommended)

            var stopWatch = new Stopwatch();
            stopWatch.Start();
            var (dict, testResult) = svm.AnswerPobs(entry, codeLocations);
            stopWatch.Stop();

            Console.WriteLine($"Searcher = {searcher.GetType()}, ElapsedTime = {stopWatch.Elapsed}");

            bool allWitnessed = true;
            foreach (var p in proxies)
            {
                var loc = new Core.codeLocation(p.Offset, p.Method);

                if (p.DesiredStatus != dict[loc])
                {
                    allWitnessed = false;
                    Console.WriteLine($"Searcher {searcher.GetType()} could not witness loc = {loc}");
                }
                // Assert.AreEqual(p.DesiredStatus, dict[loc], $"Checking location {loc}");
            }

            return allWitnessed;
        }

        [TestCaseSource(nameof(PobsCases))]
        public static void AnswerPobs(Type t, string mainName, CodeLocationProxy[] proxies)
        {
            uint maxBound = 20u;
            var entryMethod = t.GetMethod(mainName, All);
            var searchers = new IBidirectionalSearcher[]
            {
                // new TargetedSearcher()
                // new BFSSearcher()

                // , new TargetedSearcher()
                new BidirectionalSearcher(new DFSSearcher(maxBound), new BackwardSearcher(), new TargetedSearcher.DummyTargetedSearcher())
                , new BidirectionalSearcher(new BFSSearcher(maxBound), new BackwardSearcher(), new TargetedSearcher.DummyTargetedSearcher())
            };

            bool allWitnessed = true;
            foreach (var s in searchers)
            {
                allWitnessed &= AnswerPobsForSearcher(entryMethod, proxies, s, maxBound);
            }

            Assert.True(allWitnessed);
        }

        public static object[] PobsCases =
        {
            // first test case
            new object[]
            {
                typeof(SwitchWithSequentialCases1), nameof(SwitchWithSequentialCases1.EntryMethod),
                new[]
                {
                    new CodeLocationProxy(0x2A, typeof(SwitchWithSequentialCases1),
                        nameof(SwitchWithSequentialCases1.EntryMethod), DesiredStatus.Witnessed),
                }
            }
            // second testCase
            ,
            new object[]
            {
                typeof(CallsTest), nameof(CallsTest.EntryMethod2),
                new[]
                {
                    new CodeLocationProxy(0x06, typeof(CallsTest), nameof(CallsTest.G), DesiredStatus.Witnessed)
                }
            },
            new object[]
            {
                typeof(LotsOfIfs), nameof(LotsOfIfs.EntryMethod),
                new[]
                {
                    new CodeLocationProxy(0x4C, typeof(LotsOfIfs), nameof(LotsOfIfs.EntryMethod),
                        DesiredStatus.Witnessed),
                }
            },
            new object[]
            {
                typeof(UnsatCases), nameof(UnsatCases.EntryMethod),
                new[]
                {
                    new CodeLocationProxy(0x08, typeof(UnsatCases), nameof(UnsatCases.EntryMethod),
                        DesiredStatus.Unknown),
                }
            },
            new object[]
            {
                typeof(Calls.Recursion), nameof(Calls.Recursion.F),
                new[]
                {
                    new CodeLocationProxy(0x9, typeof(Calls.Recursion), nameof(Calls.Recursion.G),
                        DesiredStatus.Witnessed),
                }
            },
            new object[]
            {
                typeof(Calls.Recursion), nameof(Calls.Recursion.TrickyCycle),
                new[]
                {
                    new CodeLocationProxy(0x9, typeof(Calls.Recursion), nameof(Calls.Recursion.G),
                        DesiredStatus.Witnessed),
                }
            },
            new object[]
            {
                typeof(SwitchWithSequentialCases1),
                nameof(SwitchWithSequentialCases1.SwitchWithExpensiveCalculations),
                new[]
                {
                    new CodeLocationProxy(0x3c, typeof(SwitchWithSequentialCases1),
                        nameof(SwitchWithSequentialCases1.SwitchWithExpensiveCalculations),
                        DesiredStatus.Witnessed),
                }
            },
            new object[]
            {
                typeof(SwitchWithSequentialCases1), nameof(SwitchWithSequentialCases1.TestForDFS),
                new[]
                {
                    new CodeLocationProxy(0x3F, typeof(SwitchWithSequentialCases1),
                        nameof(SwitchWithSequentialCases1.TestForDFS), DesiredStatus.Witnessed),
                }
            },
            new object[]
            {
                typeof(SwitchWithSequentialCases1),
                nameof(SwitchWithSequentialCases1.TrickyTestForTargetedSearcher),
                new[]
                {
                    new CodeLocationProxy(0x12, typeof(SwitchWithSequentialCases1),
                        nameof(SwitchWithSequentialCases1.LittleExpensiveCalculations), DesiredStatus.Witnessed),
                }
            },
            new object[]
            {
                typeof(SwitchWithSequentialCases1), nameof(SwitchWithSequentialCases1.BoundTest),
                new[]
                {
                    new CodeLocationProxy(0x16, typeof(SwitchWithSequentialCases1),
                        nameof(SwitchWithSequentialCases1.BoundTest), DesiredStatus.Witnessed),
                }
            },
            new object[]
            {
                typeof(LinqTest), nameof(LinqTest.SimpleSymbolicLinqTest),
                new[]
                {
                    // new CodeLocationProxy(0x009d, typeof(ForKostya), nameof(ForKostya.SymbolicLinqTest2), DesiredStatus.Witnessed),
                    new CodeLocationProxy(0x007b, typeof(LinqTest), nameof(LinqTest.SimpleSymbolicLinqTest),
                        DesiredStatus.Witnessed),
                    new CodeLocationProxy(0x0077, typeof(LinqTest), nameof(LinqTest.SimpleSymbolicLinqTest),
                        DesiredStatus.Witnessed),
                }
            },
            new object[]
            {
                typeof(RegExTest), nameof(RegExTest.OwnImplementationTest),
                new[]
                {
                    new CodeLocationProxy(0x15, typeof(RegExImplementation), nameof(RegExImplementation.MatchHere),
                        DesiredStatus.Witnessed),
                    new CodeLocationProxy(0xD, typeof(RegExImplementation), nameof(RegExImplementation.MatchStar),
                        DesiredStatus.Witnessed),
                    new CodeLocationProxy(0x0095, typeof(RegExImplementation), nameof(RegExImplementation.MatchHere),
                        DesiredStatus.Witnessed),
                    new CodeLocationProxy(0x0063, typeof(RegExImplementation), nameof(RegExImplementation.MatchHere),
                        DesiredStatus.Witnessed),
                }
            },
            new object[]
            {
                // TODO: investigate problem with encoding addresses, after fill holes
                typeof(RegExTest), nameof(RegExTest.OwnImplementationTest2),
                new[]
                {
                    new CodeLocationProxy(0x0040, typeof(RegExImplementation), nameof(RegExImplementation.MatchHere),
                        DesiredStatus.Witnessed),
                    new CodeLocationProxy(0xD, typeof(RegExImplementation), nameof(RegExImplementation.MatchStar),
                        DesiredStatus.Witnessed),
                    new CodeLocationProxy(0x0095, typeof(RegExImplementation), nameof(RegExImplementation.MatchHere),
                        DesiredStatus.Witnessed),
                    new CodeLocationProxy(0x0063, typeof(RegExImplementation), nameof(RegExImplementation.MatchHere),
                        DesiredStatus.Witnessed),
                }
            }

            , new object[]
            {
                typeof(CallsTest), nameof(CallsTest.SomeCallChain),
                new []
                {
                    new CodeLocationProxy(0x13, typeof(CallsTest), nameof(CallsTest.F), DesiredStatus.Witnessed),
                    new CodeLocationProxy(0x6, typeof(CallsTest), nameof(CallsTest.G3), DesiredStatus.Witnessed),
                }
            }

            , new object[]
            {
                typeof(RegExTest), nameof(RegExTest.OwnImplementationTest),
                new []
                {
                    new CodeLocationProxy(0x0023, typeof(RegExImplementation), nameof(RegExImplementation.Match), DesiredStatus.Witnessed),
                    new CodeLocationProxy(0x0032, typeof(RegExImplementation), nameof(RegExImplementation.Match), DesiredStatus.Witnessed),
                    new CodeLocationProxy(0x000d, typeof(RegExImplementation), nameof(RegExImplementation.MatchStar), DesiredStatus.Witnessed),
                    new CodeLocationProxy(0x004f, typeof(RegExTest), nameof(RegExTest.OwnImplementationTest), DesiredStatus.Witnessed),
                    new CodeLocationProxy(0x006a, typeof(RegExTest), nameof(RegExTest.OwnImplementationTest), DesiredStatus.Witnessed),
                    new CodeLocationProxy(0x006c, typeof(RegExTest), nameof(RegExTest.OwnImplementationTest), DesiredStatus.Witnessed),
                    // new CodeLocationProxy(0x002e, typeof(RegExImplementation), nameof(RegExImplementation.MatchStar), DesiredStatus.Witnessed),
                    // new CodeLocationProxy(0x0095, typeof(RegExImplementation), nameof(RegExImplementation.MatchHere), DesiredStatus.Witnessed),
                    // new CodeLocationProxy(0x0063, typeof(RegExImplementation), nameof(RegExImplementation.MatchHere), DesiredStatus.Witnessed),
                    // new CodeLocationProxy(0x0077, typeof(LinqTest), nameof(LinqTest.SimpleSymbolicLinqTest), DesiredStatus.Witnessed),
                }
            }

            , new object[]
            {
                typeof(RegExTest), nameof(RegExTest.OwnImplementationTest3),
                new []
                {
                    new CodeLocationProxy(0x004a, typeof(RegExTest), nameof(RegExTest.OwnImplementationTest3), DesiredStatus.Witnessed),
                    new CodeLocationProxy(0x004d, typeof(RegExTest), nameof(RegExTest.OwnImplementationTest3), DesiredStatus.Witnessed),
                    new CodeLocationProxy(0x004f, typeof(RegExTest), nameof(RegExTest.OwnImplementationTest3), DesiredStatus.Witnessed),
                }
            }

            , new object[]
            {
                typeof(Calls.NestedCalls), nameof(Calls.NestedCalls.F1),
                new []
                {
                    new CodeLocationProxy(0x7, typeof(Calls.NestedCalls), nameof(Calls.NestedCalls.F4), DesiredStatus.Witnessed),
                }
            }

        };
    }
}
