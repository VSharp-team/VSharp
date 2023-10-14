namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.Environment -------------------------------

module internal Environment =

    let GetResourceFromDefault (state : state) (_ : term list) =
        Memory.AllocateString "Getting resource strings currently not supported!" state

    let GetCurrentManagedThreadId  (_ : state) (_ : term list) =
        MakeNumber 0

    let GetManagedThreadId  (_ : state) (_ : term list) =
        MakeNumber 0

    let WriteLine (_ : state) (args : term list) =
        assert(List.length args = 1)
        Nop()

    let Get_IsOutputRedirected (_ : state) (args : term list) =
        assert(List.length args = 0)
        MakeBool false

    let ConsoleClear (_ : state) (args : term list) =
        assert(List.length args = 0)
        Nop()

    let CreateDirectory (state : state) (args : term list) =
        assert(List.length args = 1)
        let name = args[0]
        let t = typeof<System.IO.DirectoryInfo>
        let dir = Memory.AllocateDefaultClass state t
        let fields = Reflection.fieldsOf false t
        let nameField = fields |> Array.find (fun (f, _) -> f.name = "_name") |> fst
        let states = Memory.WriteClassField state dir nameField name
        assert(List.length states = 1)
        dir

    let FileExists (_ : state) (args : term list) =
        assert(List.length args = 1)
        False()

    let IsBuiltInComSupportedInternal (_ : state) (args : term list) =
        assert(List.length args = 0)
        False()

    let GetLinkTarget (_ : state) (args : term list) =
        assert(List.length args = 1)
        NullRef typeof<string>

    let GetEnvironmentVariable (_ : state) (args : term list) =
        assert(List.length args = 1)
        NullRef typeof<string>
