# V# Symbolic Execution Engine

V# is a symbolic execution engine for .NET binaries, performing completely automated and unassisted test generation for .NET assemblies. It is cross-platform and supports .NET, .NET Core and .NET Framework assemblies.

## Requirements

.NET 6 or greater

## Testing a small function

Create an empty NUnit test project `DemoProject` and insert the following code:

```csharp
using System;
using NUnit.Framework;

namespace DemoProject
{
    public static class DemoClass
    {
        public static int Abs(int x)
        {
            int y = x;
            if (x < 0)
                y = -x;

            if (y < 0)
                throw new Exception("What?");

            return y;
        }
    }

    public class Tests
    {
        [Test]
        public void Test1()
        {
            var success = VSharp.TestGenerator.CoverAndRun(typeof(DemoClass));
            Assert.IsTrue(success);
        }

    }
}
```

The test will generate two unit tests for `Abs` function and run all the tests. You will sequentially see one value that gets into the `then` branch first of the first condition and `INT_MIN` value which takes the `Abs` function throwing the exception.

Run the test coverage measurement tool to be sure the exhaustiveness of the generated test coverage. The generated tests can be found in `DemoProject` working directory, in `VSharp.tests.0` subfolder.

## Current state

The project is currently in active development stage. If you encounter the problem, consider [submitting the issue](https://github.com/VSharp-team/VSharp/issues).

## License

The project is licensed under the [Apache License Version 2.0](https://www.apache.org/licenses/LICENSE-2.0)