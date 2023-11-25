using System.Runtime.InteropServices;
using VSharp.Test;

namespace IntegrationTests;

[TestSvmFixture]
public class InternalCalls
{
    [TestSvm]
    public static int LastPInvokeErrorTest(int i)
    {
        if (i < 100)
        {
            return 2;
        }

        Marshal.SetLastPInvokeError(i);

        if (Marshal.GetLastPInvokeError() != i)
        {
            return -1;
        }

        return 1;
    }
}
