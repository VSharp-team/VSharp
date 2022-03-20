using System;

namespace VSharp.CSharpUtils
{
    public static class Exceptions
    {
        [Implements("System.Void System.InvalidCastException..ctor(this, System.String)")]
        public static void CreateInvalidCastException(string msg)
        {
            throw new InvalidCastException(msg);
        }

        [Implements("System.Void System.NullReferenceException..ctor(this)")]
        public static void CreateNullReferenceException()
        {
            throw new NullReferenceException();
        }

        [Implements("System.Void System.OverflowException..ctor(this)")]
        public static void CreateOverflowException()
        {
            throw new OverflowException();
        }

        [Implements("System.Void System.IndexOutOfRangeException..ctor(this)")]
        public static void CreateIndexOutOfRangeException()
        {
            throw new IndexOutOfRangeException();
        }

        [Implements("System.Void System.ArgumentOutOfRangeException..ctor(this)")]
        public static void ArgumentOutOfRangeException()
        {
            throw new ArgumentOutOfRangeException();
        }

        [Implements("System.Void System.ArgumentException..ctor(this)")]
        public static void ArgumentException()
        {
            throw new ArgumentException();
        }

        [Implements("System.Void System.DivideByZeroException..ctor(this)")]
        public static void DivideByZeroException()
        {
            throw new ArgumentException();
        }

        [Implements("System.Void System.ArithmeticException..ctor(this)")]
        public static void ArithmeticException()
        {
            throw new ArgumentException();
        }

        [Implements("System.Void System.ArrayTypeMismatchException..ctor(this)")]
        public static void CreateArrayTypeMismatchException()
        {
            throw new ArrayTypeMismatchException();
        }

        [Implements("System.Void System.ArgumentNullException..ctor(this)")]
        public static void CreateArgumentNullException()
        {
            throw new ArgumentNullException();
        }

        [Implements("System.Void System.OutOfMemoryException..ctor(this)")]
        public static void CreateOutOfMemoryException()
        {
            throw new OutOfMemoryException();
        }
    }
}
