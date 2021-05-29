using System;
using System.IO;
using System.Reflection;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading;
using NUnit.Framework;
using NUnit.Framework.Interfaces;
using NUnit.Framework.Internal;
using NUnit.Framework.Internal.Builders;
using NUnit.Framework.Internal.Commands;
using VSharp.Interpreter.IL;

namespace VSharp.Test
{

    public class PobsSetup
    {
        public enum DesiredStatus
        {
            Witnessed
            , Unreachable
            , Unknown
        }
        public class CodeLocationProxy
        {
            public CodeLocationProxy(int offset, Type t, string methodName, DesiredStatus desiredStatus)
            {
                DesiredStatus = desiredStatus.ToString();
                Offset = offset;
                BindingFlags all =
                    BindingFlags.IgnoreCase | BindingFlags.DeclaredOnly | BindingFlags.Instance |
                    BindingFlags.Static | BindingFlags.NonPublic | BindingFlags.Public;
                Method = t.GetMethod(methodName, all);
                Assert.IsNotNull(Method);
            }

            public int Offset { get; private set; }
            public string DesiredStatus { get; private set; }
            public MethodBase Method { get; private set; }
        }


        [TestCaseSource(nameof(DivideCases))]
        public static void AnswerPobs(Type t, string mainName, CodeLocationProxy[] proxies)
        {
            Core.codeLocation[] codeLocations = proxies.Select(p => new Core.codeLocation(p.Offset, p.Method)).ToArray();

            var svm = new SVM(new PobsInterpreter(new DummySearcher()));
            svm.ConfigureSolver();
            // SVM.ConfigureSimplifier(new Z3Simplifier()); can be used to enable Z3-based simplification (not recommended)

            var dict = svm.AnswerPobs(t.GetMethod(mainName), codeLocations);
            // var msg = new StringBuilder();
            // foreach (var kvp in dict)
            // {
            //     msg.Append("Status = " + kvp.Value).AppendLine();
            //     msg.Append("Pob = " + kvp.Key).AppendLine().AppendLine();
            // }

            foreach (var p in proxies)
            {
                var loc = new Core.codeLocation(p.Offset, p.Method);
                Assert.AreEqual(p.DesiredStatus, dict[loc], $"Checking location: {loc}");
            }

        }

        public static object[] DivideCases =
        {
            // // first test case
            // new object[]
            // {
            //     typeof(SwitchWithSequentialCases1), nameof(SwitchWithSequentialCases1.EntryMethod),
            //     new []
            //     {
            //         new CodeLocationProxy(56, typeof(SwitchWithSequentialCases1), nameof(SwitchWithSequentialCases1.EntryMethod), DesiredStatus.Witnessed),
            //     }
            // }
            // // second testCase
            // , new object[]
            // {
            //     typeof(CallsTest), nameof(CallsTest.EntryMethod2),
            //     new []
            //     {
            //         new CodeLocationProxy(2, typeof(CallsTest), nameof(CallsTest.G), DesiredStatus.Witnessed)
            //     }
            // }
            // , new object[]
            // {
            //     typeof(LotsOfIfs), nameof(LotsOfIfs.EntryMethod),
            //     new []
            //     {
            //         new CodeLocationProxy(210, typeof(LotsOfIfs), nameof(LotsOfIfs.EntryMethod), DesiredStatus.Witnessed),
            //     }
            // }
            // , new object[]
            // {
            //     typeof(UnsatCases), nameof(UnsatCases.EntryMethod),
            //     new []
            //     {
            //         new CodeLocationProxy(23, typeof(UnsatCases), nameof(UnsatCases.EntryMethod), DesiredStatus.Unknown),
            //     }
            // }
        };

    }



}
