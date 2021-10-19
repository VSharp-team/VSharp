using System;
using System.Reflection;
using NUnit.Framework.Interfaces;
using NUnit.Framework.Internal;

namespace VSharp.Test.Utils
{
    public class DummyFilter : IPreFilter
    {
        /* Filter for exploring all possible methods */
        public bool IsMatch(Type type)
        {
            return true;
        }

        public bool IsMatch(Type type, MethodInfo method)
        {
            return true;
        }
    }

    public class DummyTypeInfo : ITypeInfo
    {
        /*
         * This class is mostly a hack to bypass NUnit test-class validation checks
         * (NUnit doesn't allow test class to be generic with not specified parameters
         * However, we want to keep generic classes generic, e.g. in ``Tests/Generic.cs")
         * It's a copy-paste of NUnit.Framework.Internal.TypeWrapper with certain modifications
         * (e.g. ``ContainsGenericParameters" always returns ``false")
         * For NUnit validation checks see:
         * NUnit.Framework.Internal.Builders.NUnitTestFixtureBuilder.CheckTestFixtureIsValid
         */
        public Type Type { get; }

        public DummyTypeInfo(Type type)
        {
            Type = type;
        }

        public ITypeInfo BaseType
        {
            get
            {
                var baseType = Type.GetTypeInfo().BaseType;

                return baseType != null
                    ? new TypeWrapper(baseType)
                    : null;
            }
        }

        public string Name => Type.Name;

        public string FullName => Type.FullName;

        public Assembly Assembly => Type.GetTypeInfo().Assembly;

        public string Namespace => Type.Namespace;

        public bool IsAbstract => Type.GetTypeInfo().IsAbstract;

        public bool IsGenericType => false;

        public bool IsType(Type type)
        {
            return Type == type;
        }

        public bool ContainsGenericParameters => false;

        public bool IsGenericTypeDefinition => false;

        public bool IsSealed => Type.GetTypeInfo().IsSealed;

        public bool IsStaticClass => true;

        public string GetDisplayName()
        {
            return TypeHelper.GetDisplayName(Type);
        }

        public string GetDisplayName(object[] args)
        {
            return TypeHelper.GetDisplayName(Type, args);
        }

        public ITypeInfo MakeGenericType(Type[] typeArgs)
        {
            return new TypeWrapper(Type.MakeGenericType(typeArgs));
        }

        public Type GetGenericTypeDefinition()
        {
            return Type.GetGenericTypeDefinition();
        }

        public T[] GetCustomAttributes<T>(bool inherit) where T : class
        {
            return (T[]) ((ICustomAttributeProvider) Type.GetTypeInfo()).GetCustomAttributes(typeof(T), inherit);
        }

        public bool IsDefined<T>(bool inherit) where T : class
        {
            return ((ICustomAttributeProvider) Type.GetTypeInfo()).IsDefined(typeof(T), inherit);
        }

        public bool HasMethodWithAttribute(Type attributeType)
        {
            return Reflect.HasMethodWithAttribute(Type, attributeType);
        }

        public IMethodInfo[] GetMethods(BindingFlags flags)
        {
            var methods = Type.GetMethods(flags);
            var result = new MethodWrapper[methods.Length];

            for (int i = 0; i < methods.Length; i++)
                result[i] = new MethodWrapper(Type, methods[i]);

            return result;
        }

        public ConstructorInfo GetConstructor(Type[] argTypes)
        {
            return Type.GetConstructor(argTypes);
        }

        public bool HasConstructor(Type[] argTypes)
        {
            return GetConstructor(argTypes) != null;
        }

        public object Construct(object[] args)
        {
            return null;
        }

        public override string ToString()
        {
            return Type.ToString();
        }
    }
}