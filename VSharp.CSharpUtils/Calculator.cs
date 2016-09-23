using System;

namespace VSharp.CSharpUtils
{
    /// <summary>
    /// Calculates the result of applying unary or binary operator to objects with unknown types using 'dynamic's.
    /// Casts the result of each operation to a given type.
    /// </summary>
    public static class Calculator
    {
        /// <summary>
        /// Calculates <arg>x</arg> + <arg>y</arg> casted to <arg>targetType</arg>
        /// </summary>
        public static object Add(object x, object y, Type targetType)
        {
            return Convert.ChangeType((dynamic)x + (dynamic)y, targetType);
        }

        /// <summary>
        /// Calculates <arg>x</arg> - <arg>y</arg> casted to <arg>targetType</arg>
        /// </summary>
        public static object Sub(object x, object y, Type targetType)
        {
            return Convert.ChangeType((dynamic)x - (dynamic)y, targetType);
        }

        /// <summary>
        /// Calculates <arg>x</arg> * <arg>y</arg> casted to <arg>targetType</arg>
        /// </summary>
        public static object Mul(object x, object y, Type targetType)
        {
            return Convert.ChangeType((dynamic)x * (dynamic)y, targetType);
        }

        /// <summary>
        /// Calculates <arg>x</arg> / <arg>y</arg> casted to <arg>targetType</arg>
        /// </summary>
        public static object Div(object x, object y, Type targetType)
        {
            return Convert.ChangeType((dynamic)x / (dynamic)y, targetType);
        }

        /// <summary>
        /// Calculates <arg>x</arg> % <arg>y</arg> casted to <arg>targetType</arg>
        /// </summary>
        public static object Rem(object x, object y, Type targetType)
        {
            return Convert.ChangeType((dynamic)x % (dynamic)y, targetType);
        }
    }
}
