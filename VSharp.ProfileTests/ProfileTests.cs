using VSharp.Interpreter.IL;
using VSharp.Test;

namespace VSharp.ProfileTests
{
    class Program
    {
        static void Main(string[] args)
        {
            var svm = new SVM(new MethodInterpreter(10, new ExceptionsExplorationSearcher()));
            svm.ConfigureSolver();
            var testingMethodType = typeof(Test.Tests.PDR);
            var testingMethod = testingMethodType.GetMethod("BreakCallSitesCompositionRecursion");
            svm.ExploreOne(testingMethod);
        }
    }
}
