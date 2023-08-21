using System;
using System.Runtime.InteropServices;
using VSharp.Test;

namespace IntegrationTests
{
    [TestSvmFixture]
    public static class Delegates
    {
        [TestSvm(100)]
        public static void DelegateParameter1(Action action)
        {
            action();
        }

        [TestSvm(100)]
        public static int DelegateParameter2(Func<int, int> func, int n)
        {
            return func(n);
        }

        [TestSvm(100)]
        public static bool DelegateParameter3(Func<int, int> func, int n)
        {
            if (func(n) > 0)
            {
                return true;
            }

            return false;
        }

        [TestSvm(100)]
        public static int DelegateParameter4(Func<int, int> func, int n, int m)
        {
            if (func(n) > 0)
            {
                if (func(m) < 10)
                {
                    return 0;
                }

                return 1;
            }

            return 2;
        }

        [TestSvm(100)]
        public static int DelegateParameter5(Func<int, int> func, int n)
        {
            if (func == null)
            {
                return 1;
            }

            return 2;
        }

        [TestSvm]
        public static bool InnerDelegate(int n)
        {
            var b = 0;

            Action a = () =>
            {
                b += 5;
            };

            a();

            if (n > 0)
            {
                return true;
            }
            else
            {
                return false;
            }
        }
    }

    public class SomeData
    {
        public int Value1 { get; set; }
        public char Value2 { get; set; }
    }

    [TestSvmFixture]
    public class Events
    {
        public struct IRQContext
        {
            public uint UserESP;
        }

        private int _x = 0;

        public delegate int IRQDelegate(ref IRQContext aContext);

        public event IRQDelegate Interrupt30;

        public int? RaiseHandler(ref IRQContext aContext)
        {
            if (aContext.UserESP > 0)
            {
                return Interrupt30?.Invoke(ref aContext);
            }

            // return Interrupt30?.Invoke(ref aContext).ToString().Length;
            return Interrupt30?.Invoke(ref aContext) + 10;
        }

        [TestSvm(100)]
        public int AddEventHandler(IRQDelegate interrupt1, IRQDelegate interrupt2, IRQContext aContext)
        {
            var d = new IRQDelegate((ref IRQContext context) =>
            {
                _x += 10;
                return (int)context.UserESP;
            });
            var d1 = new IRQDelegate((ref IRQContext context) =>
            {
                _x *= 10;
                return (int)context.UserESP + _x;
            });
            Interrupt30 += d;
            Interrupt30 += interrupt2;
            Interrupt30 += interrupt1;
            Interrupt30 += d1;
            var a = RaiseHandler(ref aContext);
            return a.Value;
        }

        [TestSvm(100)]
        public int? RemoveEventHandler(IRQDelegate interrupt1, IRQDelegate interrupt2, IRQContext aContext)
        {
            var d = new IRQDelegate((ref IRQContext context) =>
            {
                _x += 10;
                return (int)context.UserESP;
            });
            var d1 = new IRQDelegate((ref IRQContext context) =>
            {
                _x *= 10;
                return (int)context.UserESP + _x;
            });
            Interrupt30 += d;
            Interrupt30 += interrupt2;
            Interrupt30 -= interrupt1;
            Interrupt30 += d;
            Interrupt30 += d1;
            Interrupt30 -= (IRQDelegate) Delegate.Combine(interrupt2, d);
            return Interrupt30.Invoke(ref aContext);
        }

        [TestSvm(100)]
        public int ConcreteEventHandler(IRQContext aContext)
        {
            Interrupt30 = null;
            var i = 0;
            var d = new IRQDelegate((ref IRQContext context) =>
            {
                i += 1;
                _x += 10;
                return (int)context.UserESP;
            });
            var d1 = new IRQDelegate((ref IRQContext context) =>
            {
                if (i == 2)
                    return _x;
                _x *= 10;
                return (int)context.UserESP + _x;
            });
            Interrupt30 += d;
            Interrupt30 += d;
            Interrupt30 -= d1;
            Interrupt30 += d1;
            Interrupt30 += d;
            Interrupt30 += d;
            Interrupt30 -= (IRQDelegate) Delegate.Combine(d, d);
            var a = RaiseHandler(ref aContext);
            return a.Value;
        }

        [TestSvm(100)]
        public int ConcreteEventHandler1()
        {
            Interrupt30 = null;
            var ctx = new IRQContext();
            var i = 0;
            var x = 0;
            var d = new IRQDelegate((ref IRQContext context) =>
            {
                i += 1;
                x += 10;
                return (int)context.UserESP;
            });
            var d1 = new IRQDelegate((ref IRQContext context) =>
            {
                if (i == 2)
                    return x;
                x *= 10;
                return (int)context.UserESP + x;
            });
            Interrupt30 += d;
            Interrupt30 += d;
            Interrupt30 -= d1;
            Interrupt30 += d1;
            Interrupt30 += d;
            Interrupt30 += d;
            Interrupt30 -= (IRQDelegate) Delegate.Combine(d, d);
            var a = RaiseHandler(ref ctx);
            return a.Value;
        }
    }

    [TestSvmFixture]
    public class ClassWithDelegates1
    {
        private Func<SomeData, int> _delegate;

        [TestSvm(100)]
        public int DelegateField1(SomeData data)
        {
            if (_delegate(data) == 42)
            {
                return 1;
            }

            if (_delegate(data) == 73)
            {
                return 2;
            }

            if (_delegate(data) == 0)
            {
                return 3;
            }

            return 4;
        }
    }

    [TestSvmFixture]
    public class ClassWithDelegates2
    {
        private Func<int, SomeData> _delegate;

        [TestSvm(100)]
        public int DelegateField2(int n)
        {
            if (_delegate(n) == null)
            {
                return 1;
            }

            if (_delegate(n).Value1 == 73)
            {
                return 2;
            }

            if (_delegate(n).Value2 == 'x')
            {
                return 3;
            }

            return 4;
        }
    }

    [TestSvmFixture]
    public class ClassWithDelegates3<T> where T : class
    {
        private Func<T, int> _delegate;

        [TestSvm(100)]
        public bool DelegateField3(T v)
        {
            if (v != null && _delegate(v) == 42)
            {
                return true;
            }

            return false;
        }
    }

    [TestSvmFixture]
    public class ClassWithDelegates4
    {
        private Func<int> _delegate;

        [TestSvm(100)]
        public bool DelegateField4()
        {
            if (_delegate != null)
            {
                return true;
            }

            return false;
        }
    }

    [TestSvmFixture]
    public class ClassWithEvent
    {
        private event Action<int> EventDelegate;

        [TestSvm(100)]
        public void AddListener(Action<int> listener)
        {
            if (listener != null && EventDelegate != null)
            {
                EventDelegate += listener;
            }
        }

        [TestSvm(100)]
        public void RemoveListener(Action<int> listener)
        {
            if (listener != null && EventDelegate != null)
            {
                EventDelegate -= listener;
            }
        }

        [TestSvm(100)]
        public bool FireEvent(int n)
        {
            if (n > 0 && EventDelegate != null)
            {
                EventDelegate.Invoke(n);
                return true;
            }

            return false;
        }
    }
}
