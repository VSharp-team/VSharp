using System;

namespace VSharp.CSharpUtils
{
    public static class DelegateUtils
    {
        [Implements("System.Delegate System.Delegate.Combine(System.Delegate, System.Delegate)")]
        public static Delegate Combine(Delegate one, Delegate another)
        {
            return one;
        }

        [Implements("System.Delegate System.Delegate.Remove(System.Delegate, System.Delegate)")]
        public static Delegate Remove(Delegate one, Delegate another)
        {
            return one;
        }
    }
}
