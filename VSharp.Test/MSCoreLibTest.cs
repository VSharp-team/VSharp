using System;
using System.IO;
using System.Reflection;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;
using NUnit.Framework;
using NUnit.Framework.Interfaces;
using NUnit.Framework.Internal;
using NUnit.Framework.Internal.Builders;
using NUnit.Framework.Internal.Commands;
using VSharp.Interpreter.IL;
using ChessDotNet.Pieces;

namespace VSharp.Test
{
    [TestFixture]
    public class MSCoreLibTest
    {
        [Test]
        public static void Test()
        {
            var svm = new SVM(new StepInterpreter());
            // SVM.ConfigureSimplifier(new Z3Simplifier()); can be used to enable Z3-based simplification (not recommended)
//            svm.ConfigureSolver(new SmtSolverWrapper<Microsoft.Z3.AST>(new Z3Solver()));
            var assembly = typeof(int).Assembly;
            svm.Run(assembly, new List<string>());
        }

        private static void ExploreMSCorLib()
        {
            string t = AppDomain.CurrentDomain.BaseDirectory;
        }
    }



}