using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Runtime.Loader;
using NUnit.Framework;
using VSharp.CSharpUtils;

namespace UnitTests
{
    [TestFixture]
    public class VSharpAssemblyLoadContextTests
    {
        [Test]
        public void TestIdempotentLoading()
        {
            var alc = new VSharpAssemblyLoadContext("test_ctx");
            var assemblyPathForTest = GetType().Assembly.Location;
            
            var asm1 = alc.LoadFromAssemblyPath(assemblyPathForTest);
            var asmCount1 = alc.Assemblies.Count();
            
            var asm2 = alc.LoadFromAssemblyPath(asm1.Location);
            var asmCount2 = alc.Assemblies.Count();
            Assert.AreEqual(asmCount1, asmCount2);
            Assert.AreEqual(asm1, asm2);
        }
        
        [Test]
        public void TestLoadingByName()
        {
            var alc = new VSharpAssemblyLoadContext("test_ctx");
            var assemblyNameForTest = GetType().Assembly.FullName;

            var asm = alc.LoadFromAssemblyName(new AssemblyName(assemblyNameForTest));

            Assert.AreEqual(assemblyNameForTest, asm.FullName);
        }
        
        [Test]
        public void TestTypeNormalization()
        {
            var alc = new VSharpAssemblyLoadContext("test_ctx");
            var testType = new List<VSharpAssemblyLoadContextTests>().GetType();
            var parameterType1 = testType.GetGenericArguments().First();
            var ctxName1 = AssemblyLoadContext.GetLoadContext(parameterType1.Assembly).Name;
            Assert.AreEqual("Default", ctxName1);
            
            alc.LoadFromAssemblyPath(GetType().Assembly.Location);
            var normalizedType = alc.NormalizeType(testType);
            Assert.AreNotEqual(testType, normalizedType);

            var parameterType2 = normalizedType.GetGenericArguments().First();
            var ctxName2 = AssemblyLoadContext.GetLoadContext(parameterType2.Assembly).Name;
            Assert.AreEqual(alc.Name, ctxName2);
        }
        
        [Test]
        public void TestMethodNormalization()
        {
            var alc = new VSharpAssemblyLoadContext("test_ctx");
            var testMethod = GetType().GetMethods().First();
            var ctxName1 = AssemblyLoadContext.GetLoadContext(testMethod.Module.Assembly).Name;
            Assert.AreEqual("Default", ctxName1);
            
            alc.LoadFromAssemblyPath(GetType().Assembly.Location);
            var normalizedMethod = alc.NormalizeMethod(testMethod);
            Assert.AreNotEqual(testMethod, normalizedMethod);

            var ctxName2 = AssemblyLoadContext.GetLoadContext(normalizedMethod.Module.Assembly).Name;
            Assert.AreEqual(alc.Name, ctxName2);
        }
    }
}
