namespace VSharp;

public interface IPsiElementHandler
{
    PsiElementHandler MakePsiElementHandler(PsiFile file);

    /**
     * Check if the action to create tests is available for the provided PsiElement.
     */
    bool IsCreateTestActionAvailable(PsiElement element);

    /**
     * Get the containing PsiClass for the PsiElement.
     */
    Nullable<PsiClass> ContainingClass(PsiElement element);

    /**
     * Cast PsiElement to the provided class.
     *
     * It is required to abstract transition from other syntax trees(Kt tree) to Psi tree.
     * For instance, we can't cast KtNamedFunction to PsiMethod, but we can transition it.
     */
    T ToPsi<T>(PsiElement element, T clazz);
}