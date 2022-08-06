namespace VSharp;

public interface IRunConfigurationHelper
{
    void RunTestsWithCoverage(GenerateTestsModel model, MutableList<PsiFile> testFiles);
}