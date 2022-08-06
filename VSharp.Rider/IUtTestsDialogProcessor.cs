namespace VSharp;

public interface IUtTestsDialogProcessor
{
    void CreateDialogAndGenerateTests(Project project, Set<PsiClass> srcClasses, Nullable<MemberInfo> focusedMethod);
}