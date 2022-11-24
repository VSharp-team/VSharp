namespace VSharp.Interpreter.IL

type internal ConcolicSearcher(baseSearcher : IForwardSearcher) =

    interface IForwardSearcher with
        override x.Init states = baseSearcher.Init states
        override x.Pick() = baseSearcher.Pick (fun s -> not s.suspended)
        override x.Pick selector = baseSearcher.Pick (fun s -> not s.suspended && selector s)
        override x.Update(parent, newStates) = baseSearcher.Update(parent, newStates)
        override x.States() = baseSearcher.States()
        override x.Reset() = baseSearcher.Reset()
        override x.Remove cilState = baseSearcher.Remove cilState
        override x.StatesCount with get() = baseSearcher.StatesCount
