using System.Diagnostics;
using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace VSharp.TestRenderer;

using static CodeRenderer;

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

    // TODO: write prerendered methods in C# and parse via Roslyn AST
    private static void RenderStructurallyEqual(
        MethodRenderer structEq,
        SimpleNameSyntax compareObjects,
        IdentifierNameSyntax? bindingFlagsField = null)
    {
        var mainBlock = structEq.Body;
        var (expected, got) = structEq.GetTwoArgs();

        mainBlock.AddAssert(RenderNotNullTypesEqual(expected, got));
        var typeId = RenderGetType(expected);
        var bindingFlags = bindingFlagsField ?? BindingFlags;
        var getFields = RenderCall(typeId, "GetFields", bindingFlags);
        var fieldsId = mainBlock.AddDecl("fields", null, getFields);

        var foreachBlock = mainBlock.NewBlock();
        var fieldId = foreachBlock.NewIdentifier("field");
        var fieldType = RenderMemberAccess(fieldId, "FieldType");
        var multicastDelegateType = TypeOfExpression(MulticastDelegate);
        var isDelegate = RenderSubType(fieldType, multicastDelegateType);
        var isDelegateId = foreachBlock.AddDecl("isDelegate", null, isDelegate);
        var ignoreCase = RenderEnum(StringComparison.OrdinalIgnoreCase);
        var nameContainsId = RenderMemberAccess(fieldId, "Name", "Contains");
        var containsThread = RenderCall(nameContainsId, RenderLiteral("threadid"), ignoreCase);
        var containsThreadId = foreachBlock.AddDecl("containsThread", null, containsThread);
        var expectedField = RenderCall(fieldId, "GetValue", expected);
        var gotField = RenderCall(fieldId, "GetValue", got);
        var compareFields = RenderCall(compareObjects, expectedField, gotField);
        var fieldsEqId = foreachBlock.AddDecl("fieldsEq", null, compareFields);
        var condition = RenderAnd(RenderNot(isDelegateId), RenderNot(containsThreadId), RenderNot(fieldsEqId));
        foreachBlock.AddIf(condition, ReturnStatement(False));

        mainBlock.AddForEach(null, fieldId, fieldsId, foreachBlock.Render());
        mainBlock.AddReturn(True);
    }

    private static void RenderContentwiseEqual(
        MethodRenderer contentwiseEq,
        SimpleNameSyntax compareObjects)
    {
        var mainBlock = contentwiseEq.Body;
        var (expected, got) = contentwiseEq.GetTwoArgs();

        mainBlock.AddAssert(RenderNotNullTypesEqual(expected, got));

        var expectedRank = RenderMemberAccess(expected, "Rank");
        var gotRank = RenderMemberAccess(got, "Rank");
        var rankNotEq = RenderNotEq(expectedRank, gotRank);
        mainBlock.AddIf(rankNotEq, ReturnStatement(False));

        var forBlock = mainBlock.NewBlock();
        var iteratorId = forBlock.NewIdentifier("i");
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
        mainBlock.AddFor(null, iteratorId, expectedRank, forBlock.Render());

        var expectedEnumerator = RenderCall(expected, "GetEnumerator");
        var expectedEnumId = mainBlock.AddDecl("expectedEnum", null, expectedEnumerator);
        var gotEnumerator = RenderCall(got, "GetEnumerator");
        var gotEnumId = mainBlock.AddDecl("gotEnum", null, gotEnumerator);
        var whileBlock = mainBlock.NewBlock();
        var expectedCurrent = RenderMemberAccess(expectedEnumId, "Current");
        var gotCurrent = RenderMemberAccess(gotEnumId, "Current");
        var compareElems = RenderCall(compareObjects, expectedCurrent, gotCurrent);
        whileBlock.AddIf(RenderNot(compareElems), ReturnStatement(False));
        var expectedMoveNext = RenderCall(expectedEnumId, "MoveNext");
        var gotMoveNext = RenderCall(gotEnumId, "MoveNext");
        condition = RenderAnd(expectedMoveNext, gotMoveNext);
        mainBlock.AddWhile(condition, whileBlock.Render());
        mainBlock.AddReturn(True);
    }

    private static void RenderCompareObjects(
        MethodRenderer compareObjects,
        SimpleNameSyntax structEq,
        SimpleNameSyntax contentwiseEq)
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
        var arrayId = mainBlock.NewIdentifier("array");
        condition = RenderIsType(expected, SystemArray, arrayId.Identifier);
        var thenBlock = mainBlock.NewBlock();
        var arrayCase = RenderCall(contentwiseEq, arrayId, gotAsArray);
        thenBlock.AddReturn(arrayCase);
        mainBlock.AddIf(condition, thenBlock.Render());
        mainBlock.AddReturn(RenderCall(structEq, expected, got));
    }

    private static ExpressionSyntax RenderExceptionCondition(IdentifierNameSyntax caughtExObj, ExpressionSyntax expectedExType)
    {
        var innerException = RenderMemberAccess(caughtExObj, "InnerException");
        var notNull = RenderNotNull(innerException);
        var exGetType = RenderGetType(innerException);
        var eq = RenderEq(exGetType, expectedExType);
        return RenderAnd(notNull, eq);
    }

    private static void RenderTest(
        MethodRenderer test,
        GenericNameSyntax fieldsAllocator,
        SimpleNameSyntax compareObjects,
        MethodBase method,
        IEnumerable<object> args,
        object? thisArg,
        Type? ex,
        object expected)
    {
        var mainBlock = test.Body;

        // Declaring arguments and 'this' of testing method
        var renderedArgs = args.Select(arg => mainBlock.RenderObject(arg, fieldsAllocator));
        IdentifierNameSyntax thisArgId = null!;
        if (thisArg != null)
        {
            Debug.Assert(Reflection.hasThis(method));
            thisArgId = mainBlock.AddDecl("thisArg", ObjectType, mainBlock.RenderObject(thisArg, fieldsAllocator));
        }

        // Calling testing method
        var callMethod = RenderCall(thisArgId, method, renderedArgs.ToArray());

        if (ex == null)
        {
            var resultId = mainBlock.AddDecl("result", ObjectType, callMethod);
            var condition =
                RenderCall(
                    compareObjects,
                    resultId,
                    mainBlock.RenderObject(expected, fieldsAllocator)
                );
            mainBlock.AddAssert(condition);
        }
        else
        {
            // Handling exceptions
            var delegateExpr = ParenthesizedLambdaExpression(callMethod);
            var assertThrows =
                // TODO: use parsing of AST to get names? (instead of strings)
                RenderCall(
                    "Assert", "Throws",
                    new []{ RenderType(ex) },
                    delegateExpr
                );
            mainBlock.AddCall(assertThrows);
        }
    }

    private static void RenderInitializer(
        MethodRenderer initializer,
        TypeSyntax typeToAllocate,
        IdentifierNameSyntax? bindingFlagsField = null)
    {
        var fields = initializer.GetOneArg();
        var mainBlock = initializer.Body;
        var type = RenderTypeOf(typeToAllocate);
        var allocate =
            RenderCall("GetUninitializedObject", type);
        var obj = mainBlock.AddDecl("obj", null, allocate);
        var fieldName = mainBlock.NewIdentifier("fieldName");
        var fieldValue = mainBlock.NewIdentifier("fieldValue");

        var forEachBlock = mainBlock.NewBlock();
        var bindingFlags = bindingFlagsField ?? BindingFlags;

        var getField = RenderCall(type, "GetField", fieldName, bindingFlags);
        var field = forEachBlock.AddDecl("field", null, getField);
        var setField = RenderCall(field, "SetValue", obj, fieldValue);
        forEachBlock.AddCall(setField);

        mainBlock.AddForEach(null, new []{ fieldName, fieldValue}, fields, forEachBlock.Render());
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

        // Creating helper class
        var objectsManager =
            new ClassRenderer(
                "ObjectsManager",
                null,
                null
            );

        // Creating 'BindingFlags' static field
        var bindingFlagsField =
            objectsManager.AddField(
                BindingFlagsType,
                "bindingFlags",
                new[] { Private, Static },
                BindingFlags
            );

        // Creating and rendering methods for equality check
        MethodRenderer structurallyEqual =
            objectsManager.AddMethod(
                "StructurallyEqual",
                null,
                new[] { Private, Static },
                BoolType,
                (ObjectType, "expected"), (ObjectType, "got")
            );
        MethodRenderer contentwiseEqual =
            objectsManager.AddMethod(
                "ContentwiseEqual",
                null,
                new[] { Private, Static },
                BoolType,
                (SystemArray, "expected"), (SystemArray, "got")
            );
        MethodRenderer compareObjects =
            objectsManager.AddMethod(
                "CompareObjects",
                null,
                new[] { Private, Static },
                BoolType,
                (ObjectType, "expected"), (ObjectType, "got")
            );
        RenderStructurallyEqual(structurallyEqual, compareObjects.MethodId, bindingFlagsField);
        RenderContentwiseEqual(contentwiseEqual, compareObjects.MethodId);
        RenderCompareObjects(compareObjects, structurallyEqual.MethodId, contentwiseEqual.MethodId);

        var typeToAllocate = objectsManager.NewGenericParameter("T");
        var initializer =
            objectsManager.AddMethod(
                "Initialize",
                null,
                new [] {Public, Static},
                typeToAllocate,
                new []{typeToAllocate},
                (FieldsMapType, "fields")
            );
        RenderInitializer(initializer, typeToAllocate, bindingFlagsField);

        // Creating main class for generating tests
        var generatedClass =
            new ClassRenderer(
                "GeneratedClass",
                RenderAttributeList("TestFixture"),
                null
            );

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
                (GenericNameSyntax) initializer.MethodId,
                compareObjects.MethodId,
                method,
                parameters,
                thisArg,
                test.Exception,
                test.Expected
            );
        }

        var comp =
            RenderProgram(
                "GeneratedNamespace",
                objectsManager.Render(),
                generatedClass.Render()
            );

        // comp.NormalizeWhitespace().WriteTo(Console.Out);
        var dir = $"{Directory.GetCurrentDirectory()}{Path.DirectorySeparatorChar}generated.cs";
        using var streamWriter = new StreamWriter(File.Create(dir));
        comp.NormalizeWhitespace().WriteTo(streamWriter);
    }
}
