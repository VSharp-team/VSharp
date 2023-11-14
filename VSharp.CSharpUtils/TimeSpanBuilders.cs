using System;

namespace VSharp.CSharpUtils;

public static class TimeSpanBuilders
{
    public static TimeSpan Infinite = TimeSpan.FromMilliseconds(-1);

    public static TimeSpan FromMilliseconds(int milliseconds)
    {
        return new TimeSpan(days: 0, hours: 0, minutes: 0, seconds: 0, milliseconds: milliseconds);
    }

    public static TimeSpan FromSeconds(int seconds)
    {
        return new TimeSpan(days: 0, hours: 0, minutes: 0, seconds: seconds, milliseconds: 0);
    }
}
