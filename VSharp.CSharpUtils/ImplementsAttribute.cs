using System;

namespace VSharp.CSharpUtils
{
    [AttributeUsage(AttributeTargets.All)]
    public class ImplementsAttribute : Attribute
    {
        public readonly string Name;

        public ImplementsAttribute(string name)
        {
            Name = name;
        }
    }
}