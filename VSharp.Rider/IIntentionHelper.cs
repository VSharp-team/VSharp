namespace VSharp;

public interface IIntentionHelper
{
    void ApplyIntentions(Project project, Editor editor, PsiFile testFile);
}