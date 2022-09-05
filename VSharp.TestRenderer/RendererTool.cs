using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace VSharp.TestRenderer;

using static CodeRenderer;

using BlockCreator = Func<IBlock, BlockSyntax>;
using BlockCreatorWithVar = Func<IBlock, IdentifierNameSyntax, BlockSyntax>;
using ExprCreator = Func<IdentifierNameSyntax, ExpressionSyntax>;

public static class Renderer
{
    private static IEnumerable<string>? _extraAssemblyLoadDirs;

    // TODO: create class 'Expression' with operators?

    private static ExpressionSyntax RenderNotNullTypesEqual(ExpressionSyntax x, ExpressionSyntax y)
    {
        return
            RenderAnd(
                RenderNotNull(x), RenderNotNull(y),
                RenderEq(RenderGetType(x), RenderGetType(y))
            );
    }

    private static void RenderStructurallyEqual(
        MethodRenderer structEq,
        IdentifierNameSyntax compareObjects)
    {
        var mainBlock = structEq.Body;
        var (expected, got) = structEq.GetTwoArgs();

        mainBlock.AddAssert(RenderNotNullTypesEqual(expected, got));
        var typeId = RenderGetType(expected);
        var bindingFlags = MethodRenderer.BindingFlags ?? BindingFlags;
        var getFields = RenderCall(typeId, "GetFields", bindingFlags);
        var fieldsId = mainBlock.AddDecl("fields", null, getFields);

        var forEachBlockCreator = new BlockCreatorWithVar((foreachBlock, fieldId) =>
        {
            var fieldType = RenderMemberAccess(fieldId, "FieldType");
            var multicastDelegateType = TypeOfExpression(MulticastDelegate);
            var isDelegate = RenderSubType(fieldType, multicastDelegateType);
            var isDelegateId = foreachBlock.AddDecl("isDelegate", null, isDelegate);
            var ignoreCase = RenderMemberAccess("StringComparison", "OrdinalIgnoreCase");
            var nameContainsId = RenderMemberAccess(fieldId, "Name", "Contains");
            var containsThread = RenderCall(nameContainsId, RenderLiteral("threadid"), ignoreCase);
            var containsThreadId = foreachBlock.AddDecl("containsThread", null, containsThread);
            var expectedField = RenderCall(fieldId, "GetValue", expected);
            var gotField = RenderCall(fieldId, "GetValue", got);
            var compareFields = RenderCall(compareObjects, expectedField, gotField);
            var fieldsEqId = foreachBlock.AddDecl("fieldsEq", null, compareFields);
            var condition = RenderAnd(RenderNot(isDelegateId), RenderNot(containsThreadId), RenderNot(fieldsEqId));
            foreachBlock.AddIf(condition, ReturnStatement(False));
            return foreachBlock.GetBlock();
        });

        mainBlock.AddForEach(null, "field", fieldsId, forEachBlockCreator);
        mainBlock.AddReturn(True);
    }

    private static void RenderContentwiseEqual(
        MethodRenderer contentwiseEq,
        IdentifierNameSyntax compareObjects)
    {
        var mainBlock = contentwiseEq.Body;
        var (expected, got) = contentwiseEq.GetTwoArgs();

        mainBlock.AddAssert(RenderNotNullTypesEqual(expected, got));

        var expectedRank = RenderMemberAccess(expected, "Rank");
        var gotRank = RenderMemberAccess(got, "Rank");
        var rankNotEq = RenderNotEq(expectedRank, gotRank);
        mainBlock.AddIf(rankNotEq, ReturnStatement(False));

        var forBlockCreator = new BlockCreatorWithVar((forBlock, iteratorId) =>
        {
            var expectedLength = RenderCall(expected, "GetLength", iteratorId);
            var gotLength = RenderCall(got, "GetLength", iteratorId);
            var expectedLb = RenderCall(expected, "GetLowerBound", iteratorId);
            var gotLb = RenderCall(got, "GetLowerBound", iteratorId);
            var condition =
                RenderOr(
                    RenderNotEq(expectedLength, gotLength),
                    RenderNotEq(expectedLb, gotLb)
                );
            forBlock.AddIf(condition, ReturnStatement(False));
            return forBlock.GetBlock();
        });
        var increment = new ExprCreator(iteratorId =>
            PrefixUnaryExpression(SyntaxKind.PreIncrementExpression, iteratorId));
        var conditionCreator = new ExprCreator(iteratorId =>
            BinaryExpression(SyntaxKind.LessThanExpression, iteratorId, expectedRank));
        mainBlock.AddFor(null, "i", conditionCreator, increment, forBlockCreator);

        var expectedEnumerator = RenderCall(expected, "GetEnumerator");
        var expectedEnumId = mainBlock.AddDecl("expectedEnum", null, expectedEnumerator);
        var gotEnumerator = RenderCall(got, "GetEnumerator");
        var gotEnumId = mainBlock.AddDecl("gotEnum", null, gotEnumerator);
        var whileBlockCreator = new BlockCreator(whileBlock =>
        {
            var expectedCurrent = RenderMemberAccess(expectedEnumId, "Current");
            var gotCurrent = RenderMemberAccess(gotEnumId, "Current");
            var compareElems = RenderCall(compareObjects, expectedCurrent, gotCurrent);
            whileBlock.AddIf(RenderNot(compareElems), ReturnStatement(False));
            return whileBlock.GetBlock();
        });
        var expectedMoveNext = RenderCall(expectedEnumId, "MoveNext");
        var gotMoveNext = RenderCall(gotEnumId, "MoveNext");
        var condition = RenderAnd(expectedMoveNext, gotMoveNext);
        mainBlock.AddWhile(condition, whileBlockCreator);
        mainBlock.AddReturn(True);
    }

    private static void RenderCompareObjects(
        MethodRenderer compareObjects,
        IdentifierNameSyntax structEq,
        IdentifierNameSyntax contentwiseEq)
    {
        var mainBlock = compareObjects.Body;
        var (expected, got) = compareObjects.GetTwoArgs();

        mainBlock.AddIf(RenderIsNull(expected), ReturnStatement(RenderIsNull(got)));
        mainBlock.AddIf(RenderIsNull(got), ReturnStatement(False));
        var expectedTypeId = mainBlock.AddDecl("type", null, RenderGetType(expected));
        var typesNotEq = RenderNotEq(expectedTypeId, RenderGetType(got));
        mainBlock.AddIf(typesNotEq, ReturnStatement(False));
        var expectedIsPrimitive = RenderMemberAccess(expectedTypeId, "IsPrimitive");
        var expectedIsEnum = RenderMemberAccess(expectedTypeId, "IsEnum");
        var condition =
            RenderOr(
                expectedIsPrimitive,
                RenderIsType(expected, StringType),
                expectedIsEnum
            );
        var equals = RenderCall(got, "Equals", expected);
        mainBlock.AddIf(condition, ReturnStatement(equals));

        var gotAsArray = RenderAsType(got, SystemArray);
        var thenBranchCreator = new BlockCreatorWithVar((block, arrayId) =>
        {
            var arrayCase = RenderCall(contentwiseEq, arrayId, gotAsArray);
            block.AddReturn(arrayCase);
            return block.GetBlock();
        });
        mainBlock.AddIfIsPattern(expected, SystemArray, "array", thenBranchCreator);
        mainBlock.AddReturn(RenderCall(structEq, expected, got));
    }

    private static ExpressionSyntax RenderExceptionCondition(IdentifierNameSyntax caughtExObj, IdentifierNameSyntax expectedExType)
    {
        var innerException = RenderMemberAccess(caughtExObj, "InnerException");
        var notNull = RenderNotNull(innerException);
        var exGetType = RenderGetType(innerException);
        var eq = RenderEq(exGetType, expectedExType);
        return RenderAnd(notNull, eq);
    }
    private static void RenderAssemblyLoader(MethodRenderer assemblyLoader)
    {
        var mainBlock = assemblyLoader.Body;
        var (path, name) = assemblyLoader.GetTwoArgs();
        var tryBlock = new BlockCreator(block =>
        {
            var assembly =
                RenderCall(
                    IdentifierName("Assembly"), "LoadFrom",
                    path
                );
            block.AddReturn(assembly);
            return block.GetBlock();
        });
        var catchBlock = new BlockCreatorWithVar((block, _) =>
        {
            var assembly =
                RenderCall(
                    IdentifierName("Assembly"), "Load",
                    name
                );
            block.AddReturn(assembly);
            return block.GetBlock();
        });
        mainBlock.AddTryCatch(tryBlock, ExceptionType, "ex", catchBlock);
    }

    private static void RenderTest(
        MethodRenderer test,
        IdentifierNameSyntax compareObjects,
        MethodBase method,
        IEnumerable<object> args,
        object? thisArg,
        Type? ex,
        object expected)
    {
        var mainBlock = test.Body;

        // NOTE: declaring assembly and module of testing method
        var methodModule = method.Module;
        var moduleId = mainBlock.AddModuleDecl(methodModule);
        var methodId = mainBlock.AddMethodDecl(method, moduleId);

        // NOTE: declaring arguments and 'this' of testing method
        var createArray = RenderArrayCreation(VectorOfObjects, args.Select(mainBlock.RenderObject));
        var argsId = mainBlock.AddDecl("args", VectorOfObjects, createArray);
        var thisArgId = mainBlock.AddDecl("thisArg", ObjectType, mainBlock.RenderObject(thisArg));

        // NOTE: calling testing method
        var invokeMethod =
            RenderCall(
                methodId, "Invoke",
                thisArgId, argsId
            );

        if (ex == null)
        {
            var resultId = mainBlock.AddDecl("result", ObjectType, invokeMethod);
            var condition =
                RenderCall(
                    compareObjects,
                    resultId,
                    mainBlock.RenderObject(expected)
                );
            mainBlock.AddAssert(condition);
        }
        else
        {
            // NOTE: handling exceptions
            // TODO: use Assert.Throws instead of catch clause
            var tryBlockCreator = new BlockCreator(tryBlock =>
            {
                tryBlock.AddDecl("result", ObjectType, invokeMethod);
                return tryBlock.GetBlock();
            });
            var catchBlockCreator = new BlockCreatorWithVar((catchBlock, exceptionId) =>
            {
                var expectedExType = catchBlock.AddTypeDecl(ex);
                catchBlock.AddAssert(RenderExceptionCondition(exceptionId, expectedExType));
                return catchBlock.GetBlock();
            });
            mainBlock.AddTryCatch(tryBlockCreator, TargetInvocationExceptionType, "ex", catchBlockCreator);
        }
    }

    private static Assembly TryLoadAssemblyFrom(object sender, ResolveEventArgs args)
    {
        var existingInstance = AppDomain.CurrentDomain.GetAssemblies().FirstOrDefault(assembly => assembly.FullName == args.Name);
        if (existingInstance != null)
        {
            return existingInstance;
        }
        foreach (string path in _extraAssemblyLoadDirs)
        {
            string assemblyPath = Path.Combine(path, new AssemblyName(args.Name).Name + ".dll");
            if (!File.Exists(assemblyPath))
                return null;
            Assembly assembly = Assembly.LoadFrom(assemblyPath);
            return assembly;
        }

        return null;
    }

    public static void RenderTests(IEnumerable<FileInfo> tests)
    {
        AppDomain.CurrentDomain.AssemblyResolve += TryLoadAssemblyFrom;

        // Creating class renderer
        var generatedClass =
            new ClassRenderer(
                "GeneratedClass",
                RenderAttributeList("TestFixture"),
                null
            );

        // Rendering 'AssemblyLoader' method
        MethodRenderer assemblyLoader =
            generatedClass.AddMethod(
                "LoadAssembly",
                null,
                new[] { Private, Static },
                AssemblyType,
                (StringType, "path"), (StringType, "name")
            );
        RenderAssemblyLoader(assemblyLoader);

        // Configuring 'AssemblyLoader' method, which will be called for loading assembly
        MethodRenderer.AssemblyLoader = assemblyLoader.MethodId;

        // Configuring 'BindingFlags' static field
        var bindingFlags =
            generatedClass.AddField(
                BindingFlagsType,
                "bindingFlags",
                new[] { Private, Static },
                BindingFlags
            );
        MethodRenderer.BindingFlags = bindingFlags;

        // Creating and rendering methods for equality check
        MethodRenderer structurallyEqual =
            generatedClass.AddMethod(
                "StructurallyEqual",
                null,
                new[] { Private, Static },
                BoolType,
                (ObjectType, "expected"), (ObjectType, "got")
            );
        MethodRenderer contentwiseEqual =
            generatedClass.AddMethod(
                "ContentwiseEqual",
                null,
                new[] { Private, Static },
                BoolType,
                (SystemArray, "expected"), (SystemArray, "got")
            );
        MethodRenderer compareObjects =
            generatedClass.AddMethod(
                "CompareObjects",
                null,
                new[] { Private, Static },
                BoolType,
                (ObjectType, "expected"), (ObjectType, "got")
            );
        RenderStructurallyEqual(structurallyEqual, compareObjects.MethodId);
        RenderContentwiseEqual(contentwiseEqual, compareObjects.MethodId);
        RenderCompareObjects(compareObjects, structurallyEqual.MethodId, contentwiseEqual.MethodId);

        foreach (FileInfo fi in tests)
        {
            testInfo ti;
            using (FileStream stream = new FileStream(fi.FullName, FileMode.Open, FileAccess.Read))
            {
                ti = UnitTest.DeserializeTestInfo(stream);
            }

            _extraAssemblyLoadDirs = ti.extraAssemblyLoadDirs;
            UnitTest test = UnitTest.DeserializeFromTestInfo(ti);
            // _extraAssemblyLoadDirs = test.ExtraAssemblyLoadDirs;

            var method = test.Method;

            // Console.Out.WriteLine("Starting rendering test for method {0}", method);
            object[] parameters = test.Args ?? method.GetParameters()
                .Select(t => Reflection.defaultOf(t.ParameterType)).ToArray();
            object thisArg = test.ThisArg;
            if (thisArg == null && !method.IsStatic)
                thisArg = Reflection.createObject(method.DeclaringType);
            var testRenderer = generatedClass.AddMethod(
                $"{method.Name}Test",
                RenderAttributeList("Test"),
                new[] { Public, Static },
                VoidType,
                System.Array.Empty<(TypeSyntax, string)>()
            );
            RenderTest(
                testRenderer,
                compareObjects.MethodId,
                method,
                parameters,
                thisArg,
                test.Exception,
                test.Expected
            );
        }

        string[] usings =
        {
            "System",
            "System.Reflection",
            "System.Runtime.Serialization",
            "System.Diagnostics",
            "NUnit.Framework"
        };

        var comp =
            RenderProgram(
                usings,
                "GeneratedNamespace",
                generatedClass.Render()
            );

        // comp.NormalizeWhitespace().WriteTo(Console.Out);
        var dir = $"{Directory.GetCurrentDirectory()}{Path.DirectorySeparatorChar}generated.cs";
        using var streamWriter = new StreamWriter(File.Create(dir));
        comp.NormalizeWhitespace().WriteTo(streamWriter);
    }
}
