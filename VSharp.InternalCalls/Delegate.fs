namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState

module internal Delegate =

    let DelegateCombine (i : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 2)
        let d1 = args[0]
        let d2 = args[1]
        assert(IsReference d1 && IsReference d2)
        let combine t (cilState : cilState) k =
            let d = Memory.CombineDelegates cilState.state args t
            cilState.Push d
            List.singleton cilState |> k
        let typesCheck (cilState : cilState) k =
            let state = cilState.state
            let d1Type = MostConcreteTypeOfRef state d1
            let d2Type = MostConcreteTypeOfRef state d2
            let t = TypeUtils.mostConcreteType d1Type d2Type
            let secondCheck (cilState : cilState) k =
                cilState.StatedConditionalExecutionCIL
                    (fun state k -> k (Types.RefIsType state d2 t, state))
                    (combine t)
                    (i.Raise i.ArgumentException)
                    k
            cilState.StatedConditionalExecutionCIL
                (fun state k -> k (Types.RefIsType state d1 t, state))
                secondCheck
                (i.Raise i.ArgumentException)
                k
        let nullCheck (cilState : cilState) k =
            cilState.StatedConditionalExecutionCIL
                (fun state k -> k (IsNullReference d2, state))
                (fun cilState k -> cilState.Push d1; List.singleton cilState |> k)
                typesCheck
                k
        cilState.StatedConditionalExecutionCIL
            (fun state k -> k (IsNullReference d1, state))
            (fun cilState k -> cilState.Push d2; List.singleton cilState |> k)
            nullCheck
            id

    let DelegateRemove (i : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 2)
        let source = args[0]
        let toRemove = args[1]
        assert(IsReference source && IsReference toRemove)
        let remove t (cilState : cilState) k =
            let d = Memory.RemoveDelegate cilState.state source toRemove t
            cilState.Push d
            List.singleton cilState |> k
        let typesCheck (cilState : cilState) k =
            let state = cilState.state
            let sourceType = MostConcreteTypeOfRef cilState.state source
            let toRemoveType = MostConcreteTypeOfRef state toRemove
            let t = TypeUtils.mostConcreteType sourceType toRemoveType
            let secondCheck (cilState : cilState) k =
                cilState.StatedConditionalExecutionCIL
                    (fun state k -> k (Types.RefIsType state toRemove t, state))
                    (remove t)
                    (i.Raise i.ArgumentException)
                    k
            cilState.StatedConditionalExecutionCIL
                (fun state k -> k (Types.RefIsType state source t, state))
                secondCheck
                (i.Raise i.ArgumentException)
                k
        let nullCheck (cilState : cilState) k =
            cilState.StatedConditionalExecutionCIL
                (fun state k -> k (IsNullReference source, state))
                (fun cilState k -> cilState.Push (TypeOf source |> NullRef); List.singleton cilState |> k)
                typesCheck
                k
        cilState.StatedConditionalExecutionCIL
            (fun state k -> k (IsNullReference toRemove, state))
            (fun cilState k -> cilState.Push source; List.singleton cilState |> k)
            nullCheck
            id
