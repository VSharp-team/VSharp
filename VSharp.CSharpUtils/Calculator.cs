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
        /// Returns true if <paramref name="x"/> is fuzzy equal to zero with the given precision <paramref name="eps"/>.
        /// </summary>
        public static bool IsZero(object x, double eps = 1e-8)
        {
            dynamic val = x;
            return val < eps && val > -eps;
        }

        /// <summary>
        /// Returns true if numeric value <paramref name="x"/> is power of two.
        /// </summary>
        public static bool IsPowOfTwo(object x)
        {
            return !IsZero(x) & (((dynamic) x & ((dynamic) x - 1)) == 0);
        }

        /// <summary>
        /// Returns which power of two value <paramref name="x"/> is.
        /// </summary>
        public static uint WhatPowerOf2(object x)
        {
            dynamic val = x;
            uint i = 0;
            while (val > 1)
            {
                val >>= 1;
                i++;
            }
            return i;
        }

        /// <summary>
        /// Returns true if numeric value <paramref name="x"/> is fuzzy equal to numeric value <paramref name="y"/> with the given precision <paramref name="eps"/>.
        /// </summary>
        public static bool FuzzyEqual(object x, object y, double eps = 1e-8)
        {
            return IsZero((dynamic) x - (dynamic) y, eps);
        }

        /// <summary>
        /// Calculates <paramref name="x"/> + <paramref name="y"/> casted to <paramref name="targetType"/>.
        /// </summary>
        public static object Add(object x, object y, Type targetType)
        {
            return Convert.ChangeType((dynamic) x + (dynamic) y, targetType);
        }

        /// <summary>
        /// Calculates <paramref name="x"/> + <paramref name="y"/> casted to <paramref name="targetType"/> and checks for overflow.
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
        /// Calculates <paramref name="x"/> - <paramref name="y"/> casted to <paramref name="targetType"/>.
        /// </summary>
        public static object Sub(object x, object y, Type targetType)
        {
            return Convert.ChangeType((dynamic) x - (dynamic) y, targetType);
        }

        /// <summary>
        /// Calculates <paramref name="x"/> - <paramref name="y"/> casted to <paramref name="targetType"/> and checks for overflow.
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
        /// Calculates <paramref name="x"/> * <paramref name="y"/> casted to <paramref name="targetType"/>.
        /// </summary>
        public static object Mul(object x, object y, Type targetType)
        {
            return Convert.ChangeType((dynamic) x * (dynamic) y, targetType);
        }

        /// <summary>
        /// Calculates <paramref name="x"/> * <paramref name="y"/> casted to <paramref name="targetType"/> and checks for overflow.
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
        /// Calculates <paramref name="x"/> / <paramref name="y"/> casted to <paramref name="targetType"/>.
        /// May return exception object if division by zero occured.
        /// </summary>
        public static object Div(object x, object y, Type targetType, out bool success)
        {
            success = true;
            try
            {
                return Convert.ChangeType((dynamic) x/(dynamic) y, targetType);
            }
            catch (DivideByZeroException e)
            {
                success = false;
                return e;
            }
        }

        /// <summary>
        /// Calculates <paramref name="x"/> / <paramref name="y"/> casted to <paramref name="targetType"/> and checks for overflow.
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
            catch (DivideByZeroException e)
            {
                success = false;
                return e;
            }
        }

        /// <summary>
        /// Calculates <paramref name="x"/> % <paramref name="y"/> casted to <paramref name="targetType"/>.
        /// </summary>
        public static object Rem(object x, object y, Type targetType, out bool success)
        {
            success = true;
            try
            {
                return Convert.ChangeType((dynamic) x%(dynamic) y, targetType);
            }
            catch (DivideByZeroException e)
            {
                success = false;
                return e;
            }
        }

        /// <summary>
        /// Calculates <paramref name="x"/> % <paramref name="y"/> casted to <paramref name="targetType"/> and checks for overflow.
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
        /// Calculates <paramref name="x"/> << <paramref name="y"/> casted to <paramref name="targetType"/>.
        /// </summary>
        public static object ShiftLeft(object x, object y, Type targetType)
        {
            return Convert.ChangeType((dynamic)x << (dynamic)y, targetType);
        }

        /// <summary>
        /// Calculates <paramref name="x"/> << <paramref name="y"/> casted to <paramref name="targetType"/>.
        /// </summary>
        public static object ShiftRight(object x, object y, Type targetType)
        {
            return Convert.ChangeType((dynamic)x >> (dynamic)y, targetType);
        }

        /// <summary>
        /// Calculates -<paramref name="x"/> casted to <paramref name="targetType"/>.
        /// </summary>
        public static object UnaryMinus(object x, Type targetType)
        {
            return Convert.ChangeType(-(dynamic) x, targetType);
        }

        /// <summary>
        /// Calculates -<paramref name="x"/> casted to <paramref name="targetType"/> and checks for overflow.
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
        /// <summary>
        /// Calculates <paramref name="x"/> & <paramref name="y"/> casted to <paramref name="targetType"/>.
        /// </summary>
        public static object BitwiseAnd(object x, object y, Type targetType)
        {
            return Convert.ChangeType((dynamic)x & (dynamic)y, targetType);
        }
        /// <summary>
        /// Calculates <paramref name="x"/> | <paramref name="y"/> casted to <paramref name="targetType"/>.
        /// </summary>
        public static object BitwiseOr(object x, object y, Type targetType)
        {
            return Convert.ChangeType((dynamic)x | (dynamic)y, targetType);
        }
        /// <summary>
        /// Calculates <paramref name="x"/> ^ <paramref name="y"/> casted to <paramref name="targetType"/>.
        /// </summary>
        public static object BitwiseXor(object x, object y, Type targetType)
        {
            return Convert.ChangeType((dynamic)x ^ (dynamic)y, targetType);
        }
        public static int Compare(object x, object y)
        {
            if ((dynamic) x == (dynamic) y)
            {
                return 0;
            }
            if ((dynamic)x < (dynamic)y)
            {
                return -1;
            }
            return 1;
        }

        public static int GetDeterministicHashCode(this string str)
        {
            if (str == null)
                return 0;
            unchecked
            {
                int hash1 = (5381 << 16) + 5381;
                int hash2 = hash1;

                for (int i = 0; i < str.Length; i += 2)
                {
                    hash1 = ((hash1 << 5) + hash1) ^ str[i];
                    if (i == str.Length - 1)
                        break;
                    hash2 = ((hash2 << 5) + hash2) ^ str[i + 1];
                }

                return hash1 + (hash2 * 1566083941);
            }
        }

        public static int GetDeterministicHashCode(this Type typ)
        {
            var method = typ.IsGenericParameter ? typ.DeclaringMethod?.ToString() : "";
            return $"{typ.Assembly.FullName}###{typ.DeclaringType}###{method}###{typ.Name}".GetDeterministicHashCode();
        }
    }
}
