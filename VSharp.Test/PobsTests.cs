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
    public class TestPobsFixtureAttribute : NUnitAttribute, IFixtureBuilder
    {
        public IEnumerable<TestSuite> BuildFrom(ITypeInfo typeInfo)
        {
            var typ = new Utils.DummyTypeInfo(typeInfo.Type);
            yield return new NUnitTestFixtureBuilder().BuildFrom(typ, new Utils.DummyFilter());
        }
    }

    public class TestPobsAttribute : TestSvmAttribute
    {
        public override TestCommand Wrap(TestCommand command)
        {
            return new TestPobsCommand(command);
        }

        private class TestPobsCommand : DelegatingTestCommand
        {
            public TestPobsCommand(TestCommand innerCommand) : base(innerCommand) {}

            public override TestResult Execute(TestExecutionContext context)
            {
                var methodInfo = innerCommand.Test.Method.MethodInfo;

                var dict = _svm.AnswerPobs(methodInfo);
                var msg = new StringBuilder();
                foreach (var kvp in dict)
                {
                    msg.Append("Status = " + kvp.Value).AppendLine();
                    msg.Append("Pob = " + kvp.Key).AppendLine().AppendLine();

                }

                context.CurrentResult.SetResult(ResultState.Success, msg.ToString());
                return context.CurrentResult;
            }
        }
    }
}
