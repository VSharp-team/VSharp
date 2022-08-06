namespace VSharp;

public interface IIntelliJApiHelper
{
    enum Target
    {
        ThreadPool,
        ReadAction,
        WriteAction,
        EdtLater
    };

    void Run(Target target, Runnable runnable);

    bool IsAndroidStudio();

    Nullable<String> AndroidGradleSdk(Project project);
}