namespace VSharp.CSharpUtils
{
    public static class CLRConfig
    {
        [Implements("System.Boolean System.CLRConfig.GetConfigBoolValue(System.String, System.Boolean&)")]
        public static bool GetConfigBoolValue(object _, out bool b)
        {
            b = true;
            return false;
        }
    }
}
