using System;

namespace VSharp.CSharpUtils
{
    public class StringUtils
    {
        [Implements("System.Int32 System.String.GetHashCode(this)")]
        public static int GetHashCode(string str)
        {
            return str.GetDeterministicHashCode();
        }

        [Implements("System.Int32 System.CultureAwareComparer.GetHashCode(this, System.String)")]
        public static int CultureAwareComparerGetHashCode(object _, string str)
        {
            return str.GetDeterministicHashCode();
        }

        [Implements("System.Boolean System.String.EqualsHelper(System.String, System.String)")]
        [Implements("System.Boolean System.String.Equals(System.String, System.String)")]
        [Implements("System.Boolean System.String.Equals(this, System.String)")]
        public static bool Equals(string str1, string str2)
        {
            if (str1 is null && str2 is null)
                return true;

            if (str1 is null)
                return false;

            if (str2 is null)
                return false;

            if (str1.Length != str2.Length)
                return false;

            for (var i = 0; i < str1.Length; i++)
            {
                if (str1[i] != str2[i])
                    return false;
            }

            return true;
        }

        [Implements("System.Boolean System.String.Equals(System.String, System.String, System.StringComparison)")]
        [Implements("System.Boolean System.String.Equals(this, System.String, System.StringComparison)")]
        public static bool EqualsWithComparison(string str1, string str2, System.StringComparison comparison)
        {
            return Equals(str1, str2);
        }

        [Implements("System.Boolean System.String.StartsWith(this, System.String, System.StringComparison)")]
        public static bool StartsWith(string str1, string str2)
        {
            // TODO: works only for InvariantCulture, implement fully

            if (str1 == null)
                throw new NullReferenceException();
            if (str2 == null)
                throw new ArgumentNullException();

            var i = 0;
            var j = 0;

            var len1 = str1.Length;
            var len2 = str2.Length;

            while (j < len2)
            {
                if (str2[j] == '\0')
                {
                    j++;
                    continue;
                }

                if (i < len1 && str1[i] == '\0')
                {
                    i++;
                    continue;
                }

                if (i >= len1 || str1[i] != str2[j])
                    return false;
                i++;
                j++;
            }

            return true;
        }
    }
}
