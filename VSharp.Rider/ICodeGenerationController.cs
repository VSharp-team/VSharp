namespace VSharp;

public interface ICodeGenerationController
{
    void GenerateTests(GenerateTestsModel model, Map<PsiClass, List<UtMethodTestSet>> testSetsByClass);
}
