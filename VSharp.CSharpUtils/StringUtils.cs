using System.Linq;

namespace VSharp.CSharpUtils
{
    public class StringUtils
    {
        [Implements("System.Int32 System.String.GetHashCode(this)")]
        public static int GetHashCode(string str)
        {
            return str.GetDeterministicHashCode();
        }
    }
}
