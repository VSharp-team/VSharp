using System.Threading.Tasks;

namespace VSharp.CSharpUtils;

public static class TaskExtensions
{
    private static readonly System.Collections.Generic.List<Task> ForgottenTask = new();

    /// <summary>
    /// Observes the task to avoid the UnobservedTaskException event to be raised.
    /// </summary>
    public static void Forget(this Task task)
    {
        // note: this code is inspired by a tweet from Ben Adams: https://twitter.com/ben_a_adams/status/1045060828700037125
        // Only care about tasks that may fault (not completed) or are faulted,
        // so fast-path for SuccessfullyCompleted and Canceled tasks.
        if (!task.IsCompleted || task.IsFaulted)
        {
            // use "_" (Discard operation) to remove the warning IDE0058: Because this call is not awaited, execution of the current method continues before the call is completed
            // https://learn.microsoft.com/en-us/dotnet/csharp/fundamentals/functional/discards?WT.mc_id=DT-MVP-5003978#a-standalone-discard
            _ = ForgetAwaited(task);
        }

        // Allocate the async/await state machine only when needed for performance reasons.
        // More info about the state machine: https://blogs.msdn.microsoft.com/seteplia/2017/11/30/dissecting-the-async-methods-in-c/?WT.mc_id=DT-MVP-5003978
        static async Task ForgetAwaited(Task task)
        {
            try
            {
                // No need to resume on the original SynchronizationContext, so use ConfigureAwait(false)
                await task.ConfigureAwait(false);
            }
            catch
            {
                // Nothing to do here
            }
        }
    }

    public static void ForgetUntilExecutionRequested(this Task task)
    {
        ForgottenTask.Add(task);
        task.Forget();
    }

    public static void RequestForgottenTaskExecution()
    {
        foreach (var task in ForgottenTask)
        {
            task.Wait();
        }
    }
}
