using NUnit.Framework;

namespace VSharp.Test
{
    [TestFixture]
    public sealed class SolverTests
    {
        [Test]
        public void SmokeTest()
        {
            IZ3Solver solver = new Z3Solver();
            solver.Encode(Core.API.Terms.Nop);
        }
    }
}
