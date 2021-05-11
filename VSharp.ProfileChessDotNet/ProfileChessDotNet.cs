using System;
using System.Reflection;
using VSharp.Analyzer;
using VSharp.Interpreter.IL;
using VSharp.Test;

namespace VSharp.ProfileChessDotNet
{
    class Program
    {
        static void Main(string[] args)
        {
            var svm = new SVM(new MethodInterpreter(new ExceptionsExplorationSearcher()));
            var chessDotNetClass = new Test.Tests.ChessDotNet();
            var chessDotNetType = chessDotNetClass.GetType();
            var chessDotNetMethod = chessDotNetType.GetMethod("CreateGame7");
            svm.ExploreOne(chessDotNetMethod);
        }
    }
}
