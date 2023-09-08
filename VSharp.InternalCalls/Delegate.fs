namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilStateOperations

module internal Delegate =

    let DelegateCombine (i : IInterpreter) cilState (args : term list) =
        assert(List.length args = 2)
        let d1 = args[0]
        let d2 = args[1]
        assert(IsReference d1 && IsReference d2)
        let combine t cilState k =
            let d = Memory.CombineDelegates cilState.state args t
            push d cilState
            List.singleton cilState |> k
        let typesCheck cilState k =
            let state = cilState.state
            let d1Type = MostConcreteTypeOfRef state d1
            let d2Type = MostConcreteTypeOfRef state d2
            let t = TypeUtils.mostConcreteType d1Type d2Type
            let secondCheck cilState k =
                StatedConditionalExecutionCIL cilState
                    (fun state k -> k (Types.RefIsType state d2 t, state))
                    (combine t)
                    (i.Raise i.ArgumentException)
                    k
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (Types.RefIsType state d1 t, state))
                secondCheck
                (i.Raise i.ArgumentException)
                k
        let nullCheck cilState k =
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (IsNullReference d2, state))
                (fun cilState k -> push d1 cilState; List.singleton cilState |> k)
                typesCheck
                k
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (IsNullReference d1, state))
            (fun cilState k -> push d2 cilState; List.singleton cilState |> k)
            nullCheck
            id

    let DelegateRemove (i : IInterpreter) cilState (args : term list) =
        assert(List.length args = 2)
        let source = args[0]
        let toRemove = args[1]
        assert(IsReference source && IsReference toRemove)
        let remove t cilState k =
            let d = Memory.RemoveDelegate cilState.state source toRemove t
            push d cilState
            List.singleton cilState |> k
        let typesCheck cilState k =
            let state = cilState.state
            let sourceType = MostConcreteTypeOfRef cilState.state source
            let toRemoveType = MostConcreteTypeOfRef state toRemove
            let t = TypeUtils.mostConcreteType sourceType toRemoveType
            let secondCheck cilState k =
                StatedConditionalExecutionCIL cilState
                    (fun state k -> k (Types.RefIsType state toRemove t, state))
                    (remove t)
                    (i.Raise i.ArgumentException)
                    k
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (Types.RefIsType state source t, state))
                secondCheck
                (i.Raise i.ArgumentException)
                k
        let nullCheck cilState k =
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (IsNullReference source, state))
                (fun cilState k -> push (TypeOf source |> NullRef) cilState; List.singleton cilState |> k)
                typesCheck
                k
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (IsNullReference toRemove, state))
            (fun cilState k -> push source cilState; List.singleton cilState |> k)
            nullCheck
            id
