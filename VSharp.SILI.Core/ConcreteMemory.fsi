namespace VSharp.Core

open System
open VSharp

type public ConcreteMemory =
    internal new : unit -> ConcreteMemory
    member internal Copy : unit -> ConcreteMemory
    member public Contains : concreteHeapAddress -> bool
    member public TryFullyConcrete : concreteHeapAddress -> obj option
    member public VirtToPhys : concreteHeapAddress -> obj
    member public TryVirtToPhys : concreteHeapAddress -> obj option
    member public ChangedStaticFields : unit -> System.Collections.Generic.HashSet<fieldId>
    member internal PhysToVirt : obj -> concreteHeapAddress
    member internal TryPhysToVirt : obj -> concreteHeapAddress option
    member internal Allocate : concreteHeapAddress -> obj -> unit
    member internal AllocateDelegate : concreteHeapAddress -> Delegate -> unit
    member internal AllocateBoxedLocation : concreteHeapAddress -> obj -> Type -> unit
    member internal ReadClassField : concreteHeapAddress -> fieldId -> obj
    member internal ReadArrayIndex : concreteHeapAddress -> int list -> obj
    member internal GetAllArrayData : concreteHeapAddress -> seq<int list * obj>
    member internal ReadArrayLowerBound : concreteHeapAddress -> int -> int
    member internal ReadArrayLength : concreteHeapAddress -> int -> int
    member internal ReadBoxedLocation : concreteHeapAddress -> ValueType
    member internal ReadDelegate : concreteHeapAddress -> Delegate
    member internal WriteClassField : concreteHeapAddress -> fieldId -> obj -> unit
    member internal WriteArrayIndex : concreteHeapAddress -> int list -> obj -> unit
    member internal WriteBoxedLocation : concreteHeapAddress -> obj -> unit
    member internal InitializeArray : concreteHeapAddress -> RuntimeFieldHandle -> unit
    member internal FillArray : concreteHeapAddress -> int -> int -> obj -> unit
    member internal CopyArray : concreteHeapAddress -> concreteHeapAddress -> int64 -> int64 -> int64 -> unit
    member internal CopyCharArrayToString : concreteHeapAddress -> concreteHeapAddress -> int -> unit
    member internal CopyCharArrayToStringLen : concreteHeapAddress -> concreteHeapAddress -> int -> int -> unit
    member internal ReTrackObject : obj -> unit
    member internal StaticFieldChanged : fieldId -> unit
    member internal Remove : concreteHeapAddress -> unit
