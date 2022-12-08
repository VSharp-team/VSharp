using System;
using VSharp.Test;

namespace IntegrationTests;

[TestSvmFixture]
public static class Environment
{
    [TestSvm]
    public static void ConsoleWriteLineString()
    {
        Console.WriteLine("Some string");
    }
}
