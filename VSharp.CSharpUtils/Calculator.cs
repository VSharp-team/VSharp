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
        /// Calculates <arg>x</arg> + <arg>y</arg> casted to <arg>targetType</arg>.
        /// </summary>
        public static object Add(object x, object y, Type targetType)
        {
            return Convert.ChangeType((dynamic) x + (dynamic) y, targetType);
        }

        /// <summary>
        /// Calculates <arg>x</arg> + <arg>y</arg> casted to <arg>targetType</arg> and checks for overflow.
        /// <param name="success">If false then overflow happened during calculation process.</param>
        /// </summary>
        public static object AddChecked(object x, object y, Type targetType, out bool success)
        {
            success = true;
            try
            {
                checked { return Convert.ChangeType((dynamic)x + (dynamic)y, targetType); }
            }
            catch (OverflowException e)
            {
                success = false;
                return e;
            }
        }

        /// <summary>
        /// Calculates <arg>x</arg> - <arg>y</arg> casted to <arg>targetType</arg>.
        /// </summary>
        public static object Sub(object x, object y, Type targetType)
        {
            return Convert.ChangeType((dynamic) x - (dynamic) y, targetType);
        }

        /// <summary>
        /// Calculates <arg>x</arg> - <arg>y</arg> casted to <arg>targetType</arg> and checks for overflow.
        /// <param name="success">If false then overflow happened during calculation process.</param>
        /// </summary>
        public static object SubChecked(object x, object y, Type targetType, out bool success)
        {
            success = true;
            try
            {
                checked { return Convert.ChangeType((dynamic)x - (dynamic)y, targetType); }
            }
            catch (OverflowException e)
            {
                success = false;
                return e;
            }
        }

        /// <summary>
        /// Calculates <arg>x</arg> * <arg>y</arg> casted to <arg>targetType</arg>.
        /// </summary>
        public static object Mul(object x, object y, Type targetType)
        {
            return Convert.ChangeType((dynamic) x * (dynamic) y, targetType);
        }

        /// <summary>
        /// Calculates <arg>x</arg> * <arg>y</arg> casted to <arg>targetType</arg> and checks for overflow.
        /// <param name="success">If false then overflow happened during calculation process.</param>
        /// </summary>
        public static object MulChecked(object x, object y, Type targetType, out bool success)
        {
            success = true;
            try
            {
                checked { return Convert.ChangeType((dynamic)x * (dynamic)y, targetType); }
            }
            catch (OverflowException e)
            {
                success = false;
                return e;
            }
        }

        /// <summary>
        /// Calculates <arg>x</arg> / <arg>y</arg> casted to <arg>targetType</arg>.
        /// </summary>
        public static object Div(object x, object y, Type targetType)
        {
            return Convert.ChangeType((dynamic) x / (dynamic) y, targetType);
        }

        /// <summary>
        /// Calculates <arg>x</arg> / <arg>y</arg> casted to <arg>targetType</arg> and checks for overflow.
        /// <param name="success">If false then overflow happened during calculation process.</param>
        /// </summary>
        public static object DivChecked(object x, object y, Type targetType, out bool success)
        {
            success = true;
            try
            {
                checked { return Convert.ChangeType((dynamic)x / (dynamic)y, targetType); }
            }
            catch (OverflowException e)
            {
                success = false;
                return e;
            }
        }

        /// <summary>
        /// Calculates <arg>x</arg> % <arg>y</arg> casted to <arg>targetType</arg>.
        /// </summary>
        public static object Rem(object x, object y, Type targetType)
        {
            return Convert.ChangeType((dynamic) x % (dynamic) y, targetType);
        }

        /// <summary>
        /// Calculates <arg>x</arg> % <arg>y</arg> casted to <arg>targetType</arg> and checks for overflow.
        /// <param name="success">If false then overflow happened during calculation process.</param>
        /// </summary>
        public static object RemChecked(object x, object y, Type targetType, out bool success)
        {
            success = true;
            try
            {
                checked { return Convert.ChangeType((dynamic)x % (dynamic)y, targetType); }
            }
            catch (OverflowException e)
            {
                success = false;
                return e;
            }
        }

        /// <summary>
        /// Calculates -<arg>x</arg> casted to <arg>targetType</arg>.
        /// </summary>
        public static object UnaryMinus(object x, Type targetType)
        {
            return Convert.ChangeType(-(dynamic) x, targetType);
        }

        /// <summary>
        /// Calculates -<arg>x</arg> casted to <arg>targetType</arg> and checks for overflow.
        /// <param name="success">If false then overflow happened during calculation process.</param>
        /// </summary>
        public static object UnaryMinusChecked(object x, Type targetType, out bool success)
        {
            success = true;
            try
            {
                checked { return Convert.ChangeType(-(dynamic)x, targetType); }
            }
            catch (OverflowException e)
            {
                success = false;
                return e;
            }
        }

    }
}
