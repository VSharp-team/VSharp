using System.Threading.Tasks;

namespace VSharp.CSharpUtils
{
    public static class ThreadUtils
    {
        // [Implements("System.Void System.Threading.Thread..cctor()")]
        public static void ThreadStaticConstructor() {}

        [Implements("System.Threading.Tasks.Task System.Threading.Tasks.Task.Run(System.Action)")]
        public static Task TaskRun(System.Action action)
        {
            var task = new Task(action);
            task.RunSynchronously();
            return task;
        }

        [Implements("System.Void System.Threading.Tasks.Task.Wait(this)")]
        public static void TaskWait(Task task)
        {
            if (!task.IsCompleted)
                throw new System.InvalidProgramException();
        }
    }
}
