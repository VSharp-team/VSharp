namespace VSharp.Core

open System.Reflection.Emit
open VSharp
open VSharp.Core
open VSharp.TypeUtils
open VSharp.CSharpUtils
open VSharp.Core.Common

module ILCalculator =

    // TODO: add all other operations and cache delegates
    type private binaryDelegateType = delegate of obj * obj -> obj
    type private compareDelegateType = delegate of obj * obj -> int
    type private unaryToIntDelegateType = delegate of obj -> int
    type private unaryToObjDelegateType = delegate of obj -> obj

    let private isValidOperand op =
        let isValidType() =
            let typ = op.GetType()
            isNumeric typ || typ = typeof<bool>
        op <> null && isValidType()

    let add(x : obj, y : obj, t : System.Type) =
        assert(isNumeric <| x.GetType() && isNumeric <| y.GetType())
        let args = [| typeof<obj>; typeof<obj> |]
        let add = DynamicMethod("Add", typeof<obj>, args)
        let il = add.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, x.GetType())
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, y.GetType())
        il.Emit(OpCodes.Add)
        il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
        let add = add.CreateDelegate(typeof<binaryDelegateType>) :?> binaryDelegateType
        add.Invoke(x, y)

    let addOvf(x : obj, y : obj, t : System.Type) =
        assert(isNumeric <| x.GetType() && isNumeric <| y.GetType())
        let args = [| typeof<obj>; typeof<obj> |]
        let addOvf = DynamicMethod("AddOvf", typeof<obj>, args)
        let il = addOvf.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, x.GetType())
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, y.GetType())
        il.Emit(OpCodes.Add_Ovf)
        il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
        let addOvf = addOvf.CreateDelegate(typeof<binaryDelegateType>) :?> binaryDelegateType
        addOvf.Invoke(x, y)

    let addOvfUn(x : obj, y : obj, t : System.Type) =
        assert(isNumeric <| x.GetType() && isNumeric <| y.GetType())
        let args = [| typeof<obj>; typeof<obj> |]
        let addOvfUn = DynamicMethod("AddOvf_Un", typeof<obj>, args)
        let il = addOvfUn.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, x.GetType())
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, y.GetType())
        il.Emit(OpCodes.Add_Ovf_Un)
        il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
        let addOvfUn = addOvfUn.CreateDelegate(typeof<binaryDelegateType>) :?> binaryDelegateType
        addOvfUn.Invoke(x, y)

    let sub(x : obj, y : obj, t : System.Type) =
        assert(isNumeric <| x.GetType() && isNumeric <| y.GetType())
        let args = [| typeof<obj>; typeof<obj> |]
        let sub = DynamicMethod("Sub", typeof<obj>, args)
        let il = sub.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, x.GetType())
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, y.GetType())
        il.Emit(OpCodes.Sub)
        il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
        let sub = sub.CreateDelegate(typeof<binaryDelegateType>) :?> binaryDelegateType
        sub.Invoke(x, y)

    let subOvf(x : obj, y : obj, t : System.Type) =
        assert(isNumeric <| x.GetType() && isNumeric <| y.GetType())
        let args = [| typeof<obj>; typeof<obj> |]
        let subOvf = DynamicMethod("SubOvf", typeof<obj>, args)
        let il = subOvf.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, x.GetType())
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, y.GetType())
        il.Emit(OpCodes.Sub_Ovf)
        il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
        let subOvf = subOvf.CreateDelegate(typeof<binaryDelegateType>) :?> binaryDelegateType
        subOvf.Invoke(x, y)

    let subOvfUn(x : obj, y : obj, t : System.Type) =
        assert(isNumeric <| x.GetType() && isNumeric <| y.GetType())
        let args = [| typeof<obj>; typeof<obj> |]
        let subOvfUn = DynamicMethod("SubOvf_Un", typeof<obj>, args)
        let il = subOvfUn.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, x.GetType())
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, y.GetType())
        il.Emit(OpCodes.Sub_Ovf_Un)
        il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
        let subOvfUn = subOvfUn.CreateDelegate(typeof<binaryDelegateType>) :?> binaryDelegateType
        subOvfUn.Invoke(x, y)

    let mul(x : obj, y : obj, t : System.Type) =
        assert(isNumeric <| x.GetType() && isNumeric <| y.GetType())
        let args = [| typeof<obj>; typeof<obj> |]
        let mul = DynamicMethod("Mul", typeof<obj>, args)
        let il = mul.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, x.GetType())
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, y.GetType())
        il.Emit(OpCodes.Mul)
        il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
        let mul = mul.CreateDelegate(typeof<binaryDelegateType>) :?> binaryDelegateType
        mul.Invoke(x, y)

    let mulOvf(x : obj, y : obj, t : System.Type) =
        assert(isNumeric <| x.GetType() && isNumeric <| y.GetType())
        let args = [| typeof<obj>; typeof<obj> |]
        let mulOvf = DynamicMethod("MulOvf", typeof<obj>, args)
        let il = mulOvf.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, x.GetType())
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, y.GetType())
        il.Emit(OpCodes.Mul_Ovf)
        il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
        let mulOvf = mulOvf.CreateDelegate(typeof<binaryDelegateType>) :?> binaryDelegateType
        mulOvf.Invoke(x, y)

    let mulOvfUn(x : obj, y : obj, t : System.Type) =
        assert(isNumeric <| x.GetType() && isNumeric <| y.GetType())
        let args = [| typeof<obj>; typeof<obj> |]
        let mulOvfUn = DynamicMethod("MulOvf_Un", typeof<obj>, args)
        let il = mulOvfUn.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, x.GetType())
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, y.GetType())
        il.Emit(OpCodes.Mul_Ovf_Un)
        il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
        let mulOvfUn = mulOvfUn.CreateDelegate(typeof<binaryDelegateType>) :?> binaryDelegateType
        mulOvfUn.Invoke(x, y)

    let div(x : obj, y : obj, t : System.Type) =
        assert(isNumeric <| x.GetType() && isNumeric <| y.GetType())
        let args = [| typeof<obj>; typeof<obj> |]
        let div = DynamicMethod("Div", typeof<obj>, args)
        let il = div.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, x.GetType())
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, y.GetType())
        il.Emit(OpCodes.Div)
        il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
        let div = div.CreateDelegate(typeof<binaryDelegateType>) :?> binaryDelegateType
        div.Invoke(x, y)

    let divUn(x : obj, y : obj, t : System.Type) =
        assert(isNumeric <| x.GetType() && isNumeric <| y.GetType())
        let args = [| typeof<obj>; typeof<obj> |]
        let divUn = DynamicMethod("Div_Un", typeof<obj>, args)
        let il = divUn.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, x.GetType())
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, y.GetType())
        il.Emit(OpCodes.Div_Un)
        il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
        let divUn = divUn.CreateDelegate(typeof<binaryDelegateType>) :?> binaryDelegateType
        divUn.Invoke(x, y)

    let rem(x : obj, y : obj, t : System.Type) =
        assert(isNumeric <| x.GetType() && isNumeric <| y.GetType())
        let args = [| typeof<obj>; typeof<obj> |]
        let rem = DynamicMethod("Rem", typeof<obj>, args)
        let il = rem.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, x.GetType())
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, y.GetType())
        il.Emit(OpCodes.Rem)
        il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
        let rem = rem.CreateDelegate(typeof<binaryDelegateType>) :?> binaryDelegateType
        rem.Invoke(x, y)

    let remUn(x : obj, y : obj, t : System.Type) =
        assert(isNumeric <| x.GetType() && isNumeric <| y.GetType())
        let args = [| typeof<obj>; typeof<obj> |]
        let remUn = DynamicMethod("Rem_Un", typeof<obj>, args)
        let il = remUn.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, x.GetType())
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, y.GetType())
        il.Emit(OpCodes.Rem_Un)
        il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
        let remUn = remUn.CreateDelegate(typeof<binaryDelegateType>) :?> binaryDelegateType
        remUn.Invoke(x, y)

    let shiftLeft(x : obj, y : obj, t : System.Type) =
        assert(isNumeric <| x.GetType() && isNumeric <| y.GetType())
        let args = [| typeof<obj>; typeof<obj> |]
        let shl = DynamicMethod("ShiftLeft", typeof<obj>, args)
        let il = shl.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, x.GetType())
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, y.GetType())
        il.Emit(OpCodes.Shl)
        il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
        let shl = shl.CreateDelegate(typeof<binaryDelegateType>) :?> binaryDelegateType
        shl.Invoke(x, y)

    let shiftRight(x : obj, y : obj, t : System.Type) =
        assert(isNumeric <| x.GetType() && isNumeric <| y.GetType())
        let args = [| typeof<obj>; typeof<obj> |]
        let shr = DynamicMethod("ShiftRight", typeof<obj>, args)
        let il = shr.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, x.GetType())
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, y.GetType())
        il.Emit(OpCodes.Shr)
        il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
        let shr = shr.CreateDelegate(typeof<binaryDelegateType>) :?> binaryDelegateType
        shr.Invoke(x, y)

    let shiftRightUn(x : obj, y : obj, t : System.Type) =
        assert(isNumeric <| x.GetType() && isNumeric <| y.GetType())
        let args = [| typeof<obj>; typeof<obj> |]
        let shrUn = DynamicMethod("ShiftRight_Un", typeof<obj>, args)
        let il = shrUn.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, x.GetType())
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, y.GetType())
        il.Emit(OpCodes.Shr_Un)
        il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
        let shrUn = shrUn.CreateDelegate(typeof<binaryDelegateType>) :?> binaryDelegateType
        shrUn.Invoke(x, y)

    let compare(x : obj, y : obj) : int =
        assert(isValidOperand x && isValidOperand y)
        let args = [| typeof<obj>; typeof<obj> |]
        let compare = DynamicMethod("Compare", typeof<int>, args)
        let il = compare.GetILGenerator(256)
        let eq = il.DefineLabel()
        let gt = il.DefineLabel()
        let lt = il.DefineLabel()
        let xType = x.GetType()
        let yType = y.GetType()

        // TODO: each opcode should have it's concrete version, so cgt.un calls exactly cgt.un
        let areUnsigned = isUnsigned xType && isUnsigned yType

        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, xType)
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, yType)
        il.Emit(OpCodes.Ceq)
        il.Emit(OpCodes.Brtrue, eq)

        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, xType)
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, yType)
        if areUnsigned then il.Emit(OpCodes.Cgt_Un)
        else il.Emit(OpCodes.Cgt)
        il.Emit(OpCodes.Brtrue, gt)

        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, xType)
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, yType)
        if areUnsigned then il.Emit(OpCodes.Clt_Un)
        else il.Emit(OpCodes.Clt)
        il.Emit(OpCodes.Brtrue, lt)

        il.MarkLabel(eq)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ret)

        il.MarkLabel(gt)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Ret)

        il.MarkLabel(lt)
        il.Emit(OpCodes.Ldc_I4_M1)
        il.Emit(OpCodes.Ret)

        let compare = compare.CreateDelegate(typeof<compareDelegateType>) :?> compareDelegateType
        compare.Invoke(x, y)

    let equality(x : obj, y : obj) : bool =
        assert(isValidOperand x && isValidOperand y)
        let args = [| typeof<obj>; typeof<obj> |]
        let equality = DynamicMethod("Equality", typeof<int>, args)
        let xType = x.GetType()
        let yType = y.GetType()
        let il = equality.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, xType)
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, yType)
        il.Emit(OpCodes.Ceq)
        il.Emit(OpCodes.Ret)
        let equality = equality.CreateDelegate(typeof<compareDelegateType>) :?> compareDelegateType
        equality.Invoke(x, y) = 1

    let less(x : obj, y : obj) : bool =
        assert(isValidOperand x && isValidOperand y)
        let args = [| typeof<obj>; typeof<obj> |]
        let less = DynamicMethod("Less", typeof<int>, args)
        let xType = x.GetType()
        let yType = y.GetType()
        let il = less.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, xType)
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, yType)
        il.Emit(OpCodes.Clt)
        il.Emit(OpCodes.Ret)
        let less = less.CreateDelegate(typeof<compareDelegateType>) :?> compareDelegateType
        less.Invoke(x, y) = 1

    let lessOrEq(x : obj, y : obj) : bool =
        assert(isValidOperand x && isValidOperand y)
        let args = [| typeof<obj>; typeof<obj> |]
        let lessOrEq = DynamicMethod("LessOrEq", typeof<int>, args)
        let xType = x.GetType()
        let yType = y.GetType()
        let il = lessOrEq.GetILGenerator(256)
        let success = il.DefineLabel()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, xType)
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, yType)
        il.Emit(OpCodes.Ble_S, success)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ret)
        il.MarkLabel(success)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Ret)
        let lessOrEq = lessOrEq.CreateDelegate(typeof<compareDelegateType>) :?> compareDelegateType
        lessOrEq.Invoke(x, y) = 1

    let lessUn(x : obj, y : obj) : bool =
        assert(isValidOperand x && isValidOperand y)
        let args = [| typeof<obj>; typeof<obj> |]
        let lessUn = DynamicMethod("LessUn", typeof<int>, args)
        let xType = x.GetType()
        let yType = y.GetType()
        let il = lessUn.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, xType)
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, yType)
        il.Emit(OpCodes.Clt_Un)
        il.Emit(OpCodes.Ret)
        let lessUn = lessUn.CreateDelegate(typeof<compareDelegateType>) :?> compareDelegateType
        lessUn.Invoke(x, y) = 1

    let lessOrEqUn(x : obj, y : obj) : bool =
        assert(isValidOperand x && isValidOperand y)
        let args = [| typeof<obj>; typeof<obj> |]
        let lessOrEqUn = DynamicMethod("LessOrEqUn", typeof<int>, args)
        let xType = x.GetType()
        let yType = y.GetType()
        let il = lessOrEqUn.GetILGenerator(256)
        let success = il.DefineLabel()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, xType)
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, yType)
        il.Emit(OpCodes.Ble_Un_S, success)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ret)
        il.MarkLabel(success)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Ret)
        let lessOrEqUn = lessOrEqUn.CreateDelegate(typeof<compareDelegateType>) :?> compareDelegateType
        lessOrEqUn.Invoke(x, y) = 1

    let greater(x : obj, y : obj) : bool =
        assert(isValidOperand x && isValidOperand y)
        let args = [| typeof<obj>; typeof<obj> |]
        let greater = DynamicMethod("Greater", typeof<int>, args)
        let xType = x.GetType()
        let yType = y.GetType()
        let il = greater.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, xType)
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, yType)
        il.Emit(OpCodes.Cgt)
        il.Emit(OpCodes.Ret)
        let greater = greater.CreateDelegate(typeof<compareDelegateType>) :?> compareDelegateType
        greater.Invoke(x, y) = 1

    let greaterOrEq(x : obj, y : obj) : bool =
        assert(isValidOperand x && isValidOperand y)
        let args = [| typeof<obj>; typeof<obj> |]
        let greaterOrEq = DynamicMethod("GreaterOrEq", typeof<int>, args)
        let xType = x.GetType()
        let yType = y.GetType()
        let il = greaterOrEq.GetILGenerator(256)
        let success = il.DefineLabel()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, xType)
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, yType)
        il.Emit(OpCodes.Bge_S, success)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ret)
        il.MarkLabel(success)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Ret)
        let greaterOrEq = greaterOrEq.CreateDelegate(typeof<compareDelegateType>) :?> compareDelegateType
        greaterOrEq.Invoke(x, y) = 1

    let greaterUn(x : obj, y : obj) : bool =
        assert(isValidOperand x && isValidOperand y)
        let args = [| typeof<obj>; typeof<obj> |]
        let greaterUn = DynamicMethod("GreaterUn", typeof<int>, args)
        let xType = x.GetType()
        let yType = y.GetType()
        let il = greaterUn.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, xType)
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, yType)
        il.Emit(OpCodes.Cgt_Un)
        il.Emit(OpCodes.Ret)
        let greaterUn = greaterUn.CreateDelegate(typeof<compareDelegateType>) :?> compareDelegateType
        greaterUn.Invoke(x, y) = 1

    let greaterOrEqUn(x : obj, y : obj) : bool =
        assert(isValidOperand x && isValidOperand y)
        let args = [| typeof<obj>; typeof<obj> |]
        let greaterOrEqUn = DynamicMethod("GreaterOrEqUn", typeof<int>, args)
        let il = greaterOrEqUn.GetILGenerator(256)
        let xType = x.GetType()
        let yType = y.GetType()
        let success = il.DefineLabel()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, xType)
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, yType)
        il.Emit(OpCodes.Bge_Un_S, success)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ret)
        il.MarkLabel(success)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Ret)
        let greaterOrEqUn = greaterOrEqUn.CreateDelegate(typeof<compareDelegateType>) :?> compareDelegateType
        greaterOrEqUn.Invoke(x, y) = 1

    let private floatIsZero x =
        assert(isValidOperand x)
        let typ = x.GetType()
        assert(isReal typ)
        let args = [| typeof<obj> |]
        let fuzzyIsZero = DynamicMethod("FuzzyIsZero", typeof<int>, args)
        let il = fuzzyIsZero.GetILGenerator(256)
        let greater = il.DefineLabel()
        let less = il.DefineLabel()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, typ)
        il.Emit(OpCodes.Dup)
        il.Emit(OpCodes.Ldc_R8, 1e-8)
        il.Emit(OpCodes.Bgt, greater)
        il.Emit(OpCodes.Ldc_R8, -1e-8)
        il.Emit(OpCodes.Blt, less)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Ret)
        il.MarkLabel(less)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ret)
        il.MarkLabel(greater)
        il.Emit(OpCodes.Pop)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ret)
        let fuzzyIsZero = fuzzyIsZero.CreateDelegate(typeof<unaryToIntDelegateType>) :?> unaryToIntDelegateType
        fuzzyIsZero.Invoke(x) = 1

    let private integralIsZero x =
        assert(isValidOperand x)
        let typ = x.GetType()
        assert(isReal typ |> not)
        let args = [| typeof<obj> |]
        let isZero = DynamicMethod("IsZero", typeof<int>, args)
        let il = isZero.GetILGenerator(256)
        let zeroCase = il.DefineLabel()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, typ)
        il.Emit(OpCodes.Brfalse, zeroCase)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ret)
        il.MarkLabel(zeroCase)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Ret)
        let isZero = isZero.CreateDelegate(typeof<unaryToIntDelegateType>) :?> unaryToIntDelegateType
        isZero.Invoke(x) = 1

    let isZero (x : obj) =
        match x with
        | :? double
        | :? single -> floatIsZero x
        | _ -> integralIsZero x

    let equal (x : obj) (y : obj) =
        sub(x, y, x.GetType()) |> isZero

    let isPowOfTwo (x : obj) =
        assert(isNumeric <| x.GetType())
        let check (x : obj) =
            if not (isZero(x)) then
                let args = [| typeof<obj> |]
                let isPowOfTwo = DynamicMethod("IsPowOfTwo", typeof<int>, args)
                let il = isPowOfTwo.GetILGenerator(256)
                let zeroCase = il.DefineLabel()
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Unbox_Any, x.GetType())
                il.Emit(OpCodes.Dup)
                il.Emit(OpCodes.Ldc_I4_M1)
                il.Emit(OpCodes.Add)
                il.Emit(OpCodes.And)
                il.Emit(OpCodes.Brfalse, zeroCase)
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Ret)
                il.MarkLabel(zeroCase)
                il.Emit(OpCodes.Ldc_I4_1)
                il.Emit(OpCodes.Ret)
                let isPowOfTwo = isPowOfTwo.CreateDelegate(typeof<unaryToIntDelegateType>) :?> unaryToIntDelegateType
                isPowOfTwo.Invoke(x) = 1
            else false
        match x with
        | :? single as s ->
            let floored = floor s
            floored = s && check (int64 floored)
        | :? double as d ->
            let floored = floor d
            floored = d && check (int64 floored)
        | _ -> check x

    let bitwiseNot(x : obj, t : System.Type) =
        assert(isValidOperand x)
        let args = [| typeof<obj> |]
        let bitwiseNot = DynamicMethod("BitwiseNot", typeof<obj>, args)
        let il = bitwiseNot.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, x.GetType())
        il.Emit(OpCodes.Not)
        il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
        let bitwiseNot = bitwiseNot.CreateDelegate(typeof<unaryToObjDelegateType>) :?> unaryToObjDelegateType
        bitwiseNot.Invoke(x)

    let bitwiseAnd(x : obj, y : obj, t : System.Type) =
        assert(isValidOperand x && isValidOperand y)
        let args = [| typeof<obj>; typeof<obj> |]
        let bAnd = DynamicMethod("BitwiseAnd", typeof<obj>, args)
        let il = bAnd.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, x.GetType())
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, y.GetType())
        il.Emit(OpCodes.And)
        il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
        let bAnd = bAnd.CreateDelegate(typeof<binaryDelegateType>) :?> binaryDelegateType
        bAnd.Invoke(x, y)

    let bitwiseOr(x : obj, y : obj, t : System.Type) =
        assert(isValidOperand x && isValidOperand y)
        let args = [| typeof<obj>; typeof<obj> |]
        let bOr = DynamicMethod("BitwiseOr", typeof<obj>, args)
        let il = bOr.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, x.GetType())
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, y.GetType())
        il.Emit(OpCodes.Or)
        il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
        let bOr = bOr.CreateDelegate(typeof<binaryDelegateType>) :?> binaryDelegateType
        bOr.Invoke(x, y)

    let bitwiseXor(x : obj, y : obj, t : System.Type) =
        assert(isValidOperand x && isValidOperand y)
        let args = [| typeof<obj>; typeof<obj> |]
        let bXor = DynamicMethod("BitwiseXor", typeof<obj>, args)
        let il = bXor.GetILGenerator(256)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, x.GetType())
        il.Emit(OpCodes.Ldarg_1)
        il.Emit(OpCodes.Unbox_Any, y.GetType())
        il.Emit(OpCodes.Xor)
        il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
        let bXor = bXor.CreateDelegate(typeof<binaryDelegateType>) :?> binaryDelegateType
        bXor.Invoke(x, y)

[<AutoOpen>]
module internal Arithmetics =

    let private bool = typeof<bool>

    let private makeAddition t x y k =
        makeBinary OperationType.Add x y t |> k

    let private makeProduct t x y k =
        makeBinary OperationType.Multiply x y t |> k

    let private makeShift op t x y k =
        makeBinary op x y t |> k


// ------------------------------- Simplification of "+" -------------------------------

    let private simplifyConcreteAddition t x y =
        castConcrete (ILCalculator.add(x, y, t)) t
//        castConcrete (Calculator.Add(x, y, t)) t

    let rec private simplifyAdditionToSum t a b y matched unmatched =
        // Simplifying (a + b) + y at this step
        match a.term, b.term, y.term with
        // (a + b) + y = (a + y) + b if a and y concrete and unchecked
        | Concrete(aval, _), _, Concrete(yval, _) ->
            let x = simplifyConcreteAddition t aval yval
            simplifyAddition t x b matched
        // ((-y) + b) + y = b if unchecked
        | UnaryMinus(a, _), _, _ when a = y -> matched b
        // (a + (-y)) + y = a if unchecked
        | _, UnaryMinus(b, _), _ when b = y -> matched a
        // (a + b) + (-a) = b if unchecked
        | _, _, UnaryMinus(y, _) when a = y -> matched b
        // (a + b) + (-b) = a if unchecked
        | _, _, UnaryMinus(y, _) when b = y -> matched a
        | _ -> unmatched ()
            // Trying to simplify pairwise combinations of x- and y-summands
//            let summandsOfY =
//                match y with
//                | Add(c, d, _) -> [c; d]
//                | _ -> [y]
//            simplifyPairwiseCombinations
//                [a; b]
//                summandsOfY
//                t
//                id
//                simplifyAdditionExt
//                (simplifyAddition t)
//                matched
//                unmatched

    and private simplifyAdditionToUnaryMinus t x y matched unmatched =
        // Simplifying (-x) + y at this step
        match x with
        // (-y) + y = 0
        | _ when x = y -> castConcrete 0 t |> matched
        // -(y + b) = -b
        | Add(a, b, _) when a = y -> matched b
        // -(a + y) = -a
        | Add(a, b, _) when b = y -> matched a
        | _ -> unmatched ()

    and private simplifyAdditionToProduct t a b y matched unmatched =
        // Simplifying (a * b) + y at this step
        match a.term, y with
        // (a * y) + y = (a + 1) * y if unckecked and a is concrete
        | Concrete(aval, atyp), _ when b = y ->
            let aPlusOne = simplifyConcreteAddition atyp aval 1
            simplifyMultiplication t aPlusOne y matched
        // (a * b) + (c * b) = (a + c) * b if unchecked and a and c are concrete
        | Concrete(aval, _), Mul(ConcreteT(cval, _), d, _) when d = b ->
            let aPlusC = simplifyConcreteAddition t aval cval
            simplifyMultiplication t aPlusC b matched
        | _ -> unmatched ()

    and private simplifyAdditionToShift t a b y matched unmatched =
        // Simplifying (a << b) + y at this step
        match b.term, y with
        // (a << b) + (a << b) = 0            if unchecked, b = (size of a) * 8 - 1
        // (a << b) + (a << b) = a << (b + 1) if unchecked, b < (size of a) * 8 - 1
        | Concrete(bval, bt), ShiftLeft(c, ConcreteT(d, _), _) when a = c && bval = d ->
            let tooBigShift = ILCalculator.compare(bval, ((sizeOf a) * 8) - 1) = 0
            if tooBigShift then
                castConcrete 0 t |> matched
            else
                simplifyShift OperationType.ShiftLeft t a (castConcrete (ILCalculator.add(bval, 1, bt)) bt) matched
        | _ -> unmatched ()

    and private simplifyAdditionToExpression x y t matched unmatched =
        match x with
        | Add(a, b, _) -> simplifyAdditionToSum t a b y matched unmatched
        | UnaryMinusT(x, _) -> simplifyAdditionToUnaryMinus t x y matched unmatched
        | Mul(a, b, _) -> simplifyAdditionToProduct t a b y matched unmatched
        | ShiftLeft(a, b, _) -> simplifyAdditionToShift t a b y matched unmatched
        | _ -> unmatched ()

    and private simplifyAdditionExt t x y matched unmatched =
        match x.term, y.term with
        | Concrete(xval, _), Concrete(yval, _) ->
            simplifyConcreteAddition t xval yval |> matched
        | Concrete(xval, _), _ when ILCalculator.isZero xval -> matched y
        | _, Concrete(yval, _) when ILCalculator.isZero yval -> matched x
        | Expression _, Expression _ ->
            simplifyAdditionToExpression x y t matched (fun () ->
            simplifyAdditionToExpression y x t matched unmatched)
        | Expression _, _ -> simplifyAdditionToExpression x y t matched unmatched
        | _, Expression _ -> simplifyAdditionToExpression y x t matched unmatched
        | _ -> unmatched ()

    and private simplifyAddition (t : System.Type) x y k =
        let defaultCase () =
            let sorted = if isConcrete y then (y, x) else (x, y)
            makeAddition t (fst sorted) (snd sorted) k
        simplifyGenericBinary "addition" x y k
            (simplifyConcreteBinary simplifyConcreteAddition t)
            (fun x y k -> simplifyAdditionExt t x y k defaultCase)
            (fun x y k -> simplifyAddition t x y k)

    and private simplifySubtraction t x y k =
        simplifyUnaryMinus t y (fun minusY ->
        simplifyAddition t x minusY k)

// ------------------------------ Simplification of bitwise not ------------------------------

    and private simplifyBinaryNot t x k =
        match x.term with
        | Concrete(x, _) -> castConcrete (ILCalculator.bitwiseNot(x, t)) t |> k
        | _ -> makeUnary OperationType.BitwiseNot x t |> k

// ------------------------------- Simplification of unary "-" -------------------------------

    and private simplifyConcreteUnaryMinus t x =
        let zero = Reflection.createObject (x.GetType())
        castConcrete (ILCalculator.sub(zero, x, t)) t

    and private simplifyUnaryMinus (t : System.Type) x k =
        let simplifyConcrete _ obj _ =
            simplifyConcreteUnaryMinus t obj
        simplifyGenericUnary "unary minus" x k simplifyConcrete (fun x k ->
        match x with
        // -(-(x)) = x if both unchecked
        | UnaryMinusT(x, _) -> k x
        // -(a + b) = (-a) + (-b) if all unchecked
        | Add(a, b, _) ->
            simplifyUnaryMinus t a (fun minusA ->
            simplifyUnaryMinus t b (fun minusB ->
            simplifyAddition t minusA minusB k))
         // -(a * x) = (-a) * x if both unchecked
        | Mul(ConcreteT(_, at) as a, y, _) ->
            simplifyUnaryMinus at a (fun minusA ->
            simplifyMultiplication t minusA y k)
        | _ -> k (makeUnary OperationType.UnaryMinus x t)
        )

// ------------------------------- Simplification of "*" -------------------------------

    and private simplifyConcreteMultiplication t x y =
        castConcrete (ILCalculator.mul(x, y, t)) t

    and private simplifyMultiplicationOfProduct t a b y matched unmatched =
        // Simplifying (a * b) * y at this step
        match a, b, y with
        // (a * b) * y = (a * y) * b if a and y concrete and unchecked
        | ConcreteT(aval, _), _, ConcreteT(yval, _) ->
            let x = simplifyConcreteMultiplication t aval yval
            simplifyMultiplication t x b matched
        // ((a / y) * b) * y = a * b if unchecked
        | Div(a, c, _, _), b, _ when c = y -> simplifyMultiplication t a b matched
        // (a * (b / y)) * y = a * b if unchecked
        | a, Div(b, c, _, _), _ when c = y -> simplifyMultiplication t a b matched
        // (a * b) * (c / a) = b * c if unchecked
        | _, _, Div(c, d, _, _) when d = a -> simplifyMultiplication t b c matched
        // (a * b) * (c / b) = a * c if unchecked
        | _, _, Div(c, d, _, _) when d = b -> simplifyMultiplication t a c matched
        | _ -> unmatched ()
            // Trying to simplify pairwise combinations of x- and y-factors
//            let factorsOfY =
//                match y with
//                | Mul(c, d, _) -> [c; d]
//                | _ -> [y]
//            simplifyPairwiseCombinations
//                [a; b]
//                factorsOfY
//                t
//                id
//                simplifyMultiplicationExt
//                (simplifyMultiplication t)
//                matched
//                unmatched

    and private simplifyMultiplicationOfDivision isSigned t a b y matched unmatched =
        // Simplifying (a / b) * y at this step
        match a, b, y with
        // (a / b) * y = (a * y) / b if a and y are concrete and unchecked
        | ConcreteT(aval, _), b, ConcreteT(yval, _) ->
            let aMulY = simplifyConcreteMultiplication t aval yval
            simplifyDivision isSigned t aMulY b matched
        // (a / (y * d)) * y = a/d if unchecked
        | _, Mul(c, d, _), _ when c = y -> simplifyDivision isSigned t a d matched
        // (a / (c * y)) * y = a/c if unchecked
        | _, Mul(c, d, _), _ when d = y -> simplifyDivision isSigned t a c matched
        | _ -> unmatched ()

    and private simplifyMultiplicationOfShifts (t : System.Type) a b y matched unmatched =
        // Simplifying (a << b) * y at this step
        match b.term, y with
        // (a << b) * (c << d) = (a * c) << (b + d) if unchecked, b and d are conctere, b + d < (size of a) * 8
        // (a << b) * (c << d) = 0 if unchecked, b and d are conctere, b + d >= (size of a) * 8
        | Concrete(bval, bt), ShiftLeft(c, ConcreteT(dval, _), _) ->
            let smallShift = ILCalculator.compare(ILCalculator.add(bval, dval, t), bitSizeOf a t) = -1
            if smallShift then
                simplifyMultiplication t a c (fun mul ->
                let bPlusD = castConcrete (ILCalculator.add(bval, dval, bt)) bt
                simplifyShift OperationType.ShiftLeft t mul bPlusD matched)
            else
                castConcrete 0 t |> matched
        // (a << b) * 2^n = a << (b + n) if unchecked, b is concrete, b + n < (size of a) * 8
        // (a << b) * 2^n = 0 if unchecked, b is concrete, b + n >= (size of a) * 8
        | Concrete(bval, bt), ConcreteT(powOf2, _) when ILCalculator.isPowOfTwo(powOf2) ->
            let n = Calculator.WhatPowerOf2(powOf2)
            let tooBigShift = ILCalculator.compare(ILCalculator.add(bval, n, t), bitSizeOf a t) >= 0
            if tooBigShift then castConcrete 0 t |> matched
            else
                simplifyShift OperationType.ShiftLeft t a (castConcrete (ILCalculator.add(bval, n, bt)) bt) matched
        | _ -> unmatched ()

    and private simplifyMultiplicationOfExpression t x y matched unmatched =
        match x with
        | Mul(a, b, _) -> simplifyMultiplicationOfProduct t a b y matched unmatched
        | Div(a, b, _, isSigned) -> simplifyMultiplicationOfDivision isSigned t a b y matched unmatched
        | ShiftLeft(a, b, _) -> simplifyMultiplicationOfShifts t a b y matched unmatched
        | _ -> unmatched ()

    and private simplifyMultiplicationExt (t : System.Type) (x : term) y matched unmatched =
        match x.term, y.term with
        | Concrete(xval, _), _ when ILCalculator.isZero(xval) -> castConcrete 0 t |> matched
        | _, Concrete(yval, _) when ILCalculator.isZero(yval) -> castConcrete 0 t |> matched
        | Concrete(x, _), _ when ILCalculator.equal x (convert 1 t) -> matched y
        | _, Concrete(y, _) when ILCalculator.equal y (convert 1 t) -> matched x
        | Concrete(x, _), _ when not <| isUnsigned t && ILCalculator.equal x (convert -1 t) ->
            simplifyUnaryMinus t y matched
        | _, Concrete(y, _) when not <| isUnsigned t && ILCalculator.equal y (convert -1 t) ->
            simplifyUnaryMinus t x matched
        | Expression _, Expression _ ->
            simplifyMultiplicationOfExpression t x y matched (fun () ->
            simplifyMultiplicationOfExpression t y x matched unmatched)
        | Expression _, _ -> simplifyMultiplicationOfExpression t x y matched unmatched
        | _, Expression _ -> simplifyMultiplicationOfExpression t y x matched unmatched
        | _ -> unmatched ()

    and private simplifyMultiplication t (x : term) y k =
        let defaultCase () =
            let sorted = if (isConcrete y) then (y, x) else (x, y)
            makeProduct t (fst sorted) (snd sorted) k
        simplifyGenericBinary "product" x y k
                              (simplifyConcreteBinary simplifyConcreteMultiplication t)
                              (fun x y k -> simplifyMultiplicationExt t x y k defaultCase)
                              (fun x y k -> simplifyMultiplication t x y k)

// ------------------------------- Simplification of "/" -------------------------------

    and simplifyConcreteDivision isSigned t x y =
        let result =
            if isSigned then ILCalculator.div(x, y, t)
            else ILCalculator.divUn(x, y, t)
        castConcrete result t

    and private simplifyDivision isSigned t x y k =
        simplifyGenericBinary "division" x y k
            (simplifyConcreteBinary (simplifyConcreteDivision isSigned) t)
            (fun x y k ->
                match x, y with
                // 0 / y = 0
                | ConcreteT(xval, _), _ when ILCalculator.isZero(xval) -> x |> k
                // x / 1 = x
                | _, ConcreteT(yval, _) when ILCalculator.equal yval (convert 1 (typeOf y)) -> x |> k
                // x / -1 = -x
                | _, ConcreteT(yval, _) when not <| isUnsigned t && ILCalculator.equal yval (convert -1 (typeOf y)) ->
                    simplifyUnaryMinus t x k
                // x / x = 1 if unchecked
                | x, y when x = y -> castConcrete 1 t |> k
                // x / -x = -1 if unchecked
                | x, UnaryMinusT(y, _) when not <| isUnsigned t && x = y -> castConcrete -1 t |> k
                // x / 2^n = x >> n if unchecked and x is unsigned
                | _, ConcreteT(powOf2, _) when ILCalculator.isPowOfTwo(powOf2) && not isSigned ->
                    let n = Calculator.WhatPowerOf2(powOf2) |> int |> makeNumber
                    simplifyShift OperationType.ShiftRight_Un t x n k
                // (a >> b) / 2^n = a >> (b + n) if unchecked, b is concrete, b + n < (size of a) * 8
                // (a >> b) / 2^n = 0 if unchecked, b is concrete, b + n >= (size of a) * 8
                | ShiftRightThroughCast(a, ConcreteT(b, bt), _), ConcreteT(powOf2, _)
                | ShiftRight(a, ConcreteT(b, bt), _, _), ConcreteT(powOf2, _)
                    when ILCalculator.isPowOfTwo(powOf2) && a |> typeOf |> isUnsigned ->
                        let n = Calculator.WhatPowerOf2(powOf2)
                        let tooBigShift = ILCalculator.compare(ILCalculator.add(b, n, t), bitSizeOf a t) >= 0
                        if tooBigShift then castConcrete 0 t |> k
                        else
                            let op = if isSigned then OperationType.ShiftRight else OperationType.ShiftRight_Un
                            simplifyShift op t a (castConcrete (ILCalculator.add(b, n, bt)) bt) k
                | _ ->
                    let op = if isSigned then OperationType.Divide else OperationType.Divide_Un
                    (makeBinary op x y t) |> k)
            (fun x y k -> simplifyDivision isSigned t x y k)

// ------------------------------- Simplification of "%" -------------------------------

    and private simplifyConcreteRemainder isSigned t x y =
        let success = ref true
        let result =
            if isSigned then
                ILCalculator.rem(x, y, t)
            else ILCalculator.remUn(x, y, t)
        assert success.Value
        castConcrete result t

    and private divides isSigned t x y =
        let res =
            if isSigned then ILCalculator.rem(x, y, t)
            else ILCalculator.remUn(x, y, t)
        ILCalculator.isZero res

    and simplifyRemainder isSigned t x y k =
        simplifyGenericBinary "remainder" x y k
            (simplifyConcreteBinary (simplifyConcreteRemainder isSigned) t)
            (fun x y k ->
                match x, y with
                // 0 % y = 0
                | ConcreteT(xval, _), _ when ILCalculator.isZero(xval) -> x |> k
                // x % 1 = 0
                | _, ConcreteT(y, _) when ILCalculator.equal y (convert 1 t) -> castConcrete 0 t |> k
                // x % -1 = 0
                | _, ConcreteT(y, _) when not <| isUnsigned t && ILCalculator.equal y (convert -1 t) ->
                    castConcrete 0 t |> k
                // x % x = 0
                | x, y when x = y -> castConcrete 0 t |> k
                // x % -x = 0 if unchecked
                | x, UnaryMinusT(y, _) when x = y -> castConcrete 0 t |> k
                // (a * b) % y = 0 if unchecked, b and y concrete and a % y = 0
                | Mul(ConcreteT(a, _), _, _), ConcreteT(y, _) when divides isSigned t a y ->
                     castConcrete 0 t |> k
                | _ ->
                    let op = if isSigned then OperationType.Remainder else OperationType.Remainder_Un
                    makeBinary op x y t |> k)
            (fun x y k -> simplifyRemainder isSigned t x y k)

// ---------------------------------------- Simplification of "<<", ">>" ----------------------------------------

    and private simplifyConcreteShift operation t x y =
        match operation with
        | OperationType.ShiftLeft -> castConcrete (ILCalculator.shiftLeft(x, y, t)) t
        | OperationType.ShiftRight -> castConcrete (ILCalculator.shiftRight(x, y, t)) t
        | OperationType.ShiftRight_Un -> castConcrete (ILCalculator.shiftRightUn(x, y, t)) t
        | _ -> __unreachable__()

    and private simplifyShiftLeftMul t a b y matched unmatched =
        // Simplifying (a * b) << y at this step
        match a.term, b.term, y.term with
        // (2^n * b) << y = b << (y + n) if unchecked, y is concrete, y + n < bitSize of a
        // (2^n * b) << y = 0 if unchecked, y is concrete, y + n >= bitSize of a
        | Concrete(powOf2, _), _, Concrete(yval, yt)
            when ILCalculator.isPowOfTwo(powOf2) ->
                let n = Calculator.WhatPowerOf2(powOf2)
                let tooBigShift = ILCalculator.compare(ILCalculator.add(yval, n, t), bitSizeOf a t) >= 0
                if tooBigShift then castConcrete 0 t |> matched
                else
                    simplifyShift OperationType.ShiftLeft t b (castConcrete (ILCalculator.add(yval, n, yt)) yt) matched
        | _ -> unmatched ()

    and private simplifyShiftRightDiv op t a b y matched unmatched =
        // Simplifying (a / b) >> y at this step
        match b.term, y.term with
        // (a / 2^n) >> y = a >> (y + n) if y is concrete, a is unsigned, y + n < bitSize of a
        // (a / 2^n) >> y = 0 if y is concrete, a is unsigned, y + n >= bitSize of a
        | Concrete(powOf2, _), Concrete(yval, yt)
            when ILCalculator.isPowOfTwo(powOf2) && a |> typeOf |> isUnsigned ->
                let n = Calculator.WhatPowerOf2(powOf2)
                let tooBigShift = ILCalculator.compare(ILCalculator.add(yval, n, t), bitSizeOf a t) >= 0
                if tooBigShift then castConcrete 0 t |> matched
                else
                    simplifyShift op t a (castConcrete (ILCalculator.add(yval, n, yt)) yt) matched
        | _ -> unmatched ()

    and private simplifyShiftLeftOfAddition t a y (matched : term -> 'a) unmatched =
        // Simplifying (a + a) << y at this step
        match y.term with
        // (a + a) << y = 0 if unchecked, y is concrete, y = (size of a) * 8 - 1
        // (a + a) << y = a << (y + 1) if unchecked, y is concrete, y < (size of a) * 8 - 1
        | Concrete(yval, yt) ->
            let tooBigShift = ILCalculator.compare(yval, ((sizeOf a) * 8) - 1) = 0
            if tooBigShift then castConcrete 0 t |> matched
            else
                simplifyShift OperationType.ShiftLeft t a (castConcrete (ILCalculator.add(yval, 1, yt)) yt) matched
        | _ -> unmatched ()

    and private simplifyShiftOfShifted op t a b y matched unmatched =
        // Simplifying (a op b) op y at this step
        match b.term, y.term, op with
        // (a op b) op y = a op (b + y) if unchecked, b and y are concrete, b + y < (size of a) * 8
        | Concrete(bval, bt), Concrete(yval, _), _ when ILCalculator.compare(ILCalculator.add(bval, yval, t), bitSizeOf a t) = -1 ->
            simplifyShift op t a (castConcrete (ILCalculator.add(bval, yval, bt)) bt) matched
        // (a op b) op y = 0 if unchecked, b and y are concrete, b + y >= (size of a) * 8
        | Concrete _, Concrete _, OperationType.ShiftLeft ->
            castConcrete 0 t |> matched
        | Concrete _, Concrete _, OperationType.ShiftRight
        | Concrete _, Concrete _, OperationType.ShiftRight_Un when a |> typeOf |> isUnsigned ->
            castConcrete 0 t |> matched
        | _ -> unmatched ()

    and private simplifyShiftOfExpression op t x y matched unmatched =
        match x, op with
        | Mul(a, b, _), OperationType.ShiftLeft -> simplifyShiftLeftMul t a b y matched unmatched
        | Div(a, b, _, true), OperationType.ShiftRight
        | Div(a, b, _, false), OperationType.ShiftRight_Un ->
            simplifyShiftRightDiv op t a b y matched unmatched
        | Add(a, b, _), OperationType.ShiftLeft when a = b -> simplifyShiftLeftOfAddition t a y matched unmatched
        | ShiftLeft(a, b, _), OperationType.ShiftLeft -> simplifyShiftOfShifted op t a b y matched unmatched
        | ShiftRight(a, b, _, true), OperationType.ShiftRight
        | ShiftRight(a, b, _, false), OperationType.ShiftRight_Un ->
            simplifyShiftOfShifted op t a b y matched unmatched
        | _ -> unmatched ()

    and private simplifyShiftExt op t x y matched unmatched =
        match x.term, y.term with
        | Concrete(x, _), _ when ILCalculator.isZero(x) -> castConcrete 0 t |> matched
        | _, Concrete(y, _) when ILCalculator.isZero(y) -> x |> matched
        | Expression _, Expression _
        | Expression _, _ -> simplifyShiftOfExpression op t x y matched unmatched
        | _ -> unmatched ()

    and private simplifyShift operation (t : System.Type) (x : term) y (k : term -> 'a) =
        assert(let t = typeOf y in t = typeof<int> || t = typeof<System.IntPtr>)
        let defaultCase () =
            makeShift operation t x y k
        simplifyGenericBinary "shift" x y k
            (simplifyConcreteBinary (simplifyConcreteShift operation) t)
            (fun x y k -> simplifyShiftExt operation t x y k defaultCase)
            (simplifyShift operation t)

    and private simplifyBitwise (op : OperationType) x y t k =
        match x.term, y.term with
        | Concrete(x, _), Concrete(y, _) ->
            match op with
            | OperationType.BitwiseAnd -> k <| Concrete (ILCalculator.bitwiseAnd(x, y, t)) t
            | OperationType.BitwiseOr -> k <| Concrete (ILCalculator.bitwiseOr(x, y, t)) t
            | OperationType.BitwiseXor -> k <| Concrete (ILCalculator.bitwiseXor(x, y, t)) t
            | _ -> __notImplemented__()
        | _ -> k (Expression (Operator op) [x; y] t)

// ------------------------------- Simplification of "=", "!=", "<", ">", ">=", "<=" -------------------------------

    and fastNumericCompare n m =
        if n = m then True()
        elif isConcrete n && isConcrete m then False()
        else makeBinary OperationType.Equal n m bool

    and private concreteAddressComparison op (x : concreteHeapAddress) (y : concreteHeapAddress) =
        let result =
            match op with
            | OperationType.Equal -> VectorTime.equals x y
            | OperationType.Less -> VectorTime.less x y
            | OperationType.Greater -> VectorTime.greater x y
            | OperationType.GreaterOrEqual -> VectorTime.greaterOrEqual x y
            | OperationType.LessOrEqual -> VectorTime.lessOrEqual x y
            | _ -> internalfail $"concreteAddressComparison: unexpected operation {op}"
        Concrete result typeof<bool>

    and private concreteNumericComparison op x y =
        let result =
            match op with
            | OperationType.Equal -> ILCalculator.equality(x, y)
            | OperationType.Less -> ILCalculator.less(x, y)
            | OperationType.Less_Un -> ILCalculator.lessUn(x, y)
            | OperationType.LessOrEqual -> ILCalculator.lessOrEq(x, y)
            | OperationType.LessOrEqual_Un -> ILCalculator.lessOrEqUn(x, y)
            | OperationType.Greater -> ILCalculator.greater(x, y)
            | OperationType.Greater_Un -> ILCalculator.greaterUn(x, y)
            | OperationType.GreaterOrEqual -> ILCalculator.greaterOrEq(x, y)
            | OperationType.GreaterOrEqual_Un -> ILCalculator.greaterOrEqUn(x, y)
            | _ -> internalfail $"concreteNumericComparison: unexpected operation {op}"
        Concrete result typeof<bool>

    and private simplifyConcreteComparison op _ x y =
        match box x, box y with
        | :? concreteHeapAddress as a1, (:? concreteHeapAddress as a2) ->
            concreteAddressComparison op a1 a2
        | _ -> concreteNumericComparison op x y

    and private simplifyComparisonExt op x y resultIfSame k =
        match x, y with
        | _ when x = y -> Concrete resultIfSame bool |> k
        | Add(ConcreteT(_, t) as c, x, _), y when x = y && (op = OperationType.Equal || op = OperationType.NotEqual) ->
            simplifyComparison op c (castConcrete 0 t) resultIfSame k
        | x, Add(ConcreteT(_, t) as c, y, _) when x = y && (op = OperationType.Equal || op = OperationType.NotEqual) ->
            simplifyComparison op (castConcrete 0 t) c resultIfSame k
        | _ -> makeBinary op x y bool |> k

    and private simplifyComparison op x y resultIfSame k =
        simplifyGenericBinary "comparison" x y k
            (simplifyConcreteBinary (simplifyConcreteComparison op) bool)
            (fun x y k -> simplifyComparisonExt op x y resultIfSame k)
            (fun x y -> simplifyComparison op x y resultIfSame)

    and simplifyEqual x y k = simplifyComparison OperationType.Equal x y true k
    and simplifyNotEqual x y k = simplifyEqual x y ((!!) >> k)
    and simplifyLess x y k = simplifyComparison OperationType.Less x y false k
    and simplifyLessUn x y k = simplifyComparison OperationType.Less_Un x y false k
    and simplifyLessOrEqual x y k = simplifyComparison OperationType.LessOrEqual x y true k
    and simplifyLessOrEqualUn x y k = simplifyComparison OperationType.LessOrEqual_Un x y true k
    and simplifyGreater x y k = simplifyComparison OperationType.Greater x y false k
    and simplifyGreaterUn x y k = simplifyComparison OperationType.Greater_Un x y false k
    and simplifyGreaterOrEqual x y k = simplifyComparison OperationType.GreaterOrEqual x y true k
    and simplifyGreaterOrEqualUn x y k = simplifyComparison OperationType.GreaterOrEqual_Un x y true k

// ------------------------------- Simplification of no overflow check -------------------------------

    let simplifyAddNoOvf x y =
        match x.term, y.term with
        | Concrete(x, t1), Concrete(y, t2) ->
            let mutable noOverflow = true
            assert(isNumeric t1 && isNumeric t2)
            let retType = deduceSimpleArithmeticOperationTargetType t1 t2
            try
                ILCalculator.addOvf(x, y, retType) |> ignore
            with :? System.OverflowException ->
                noOverflow <- false
            makeBool noOverflow
        | _ -> makeBinary OperationType.AddNoOvf x y bool

    let simplifyAddNoOvfUn x y =
        match x.term, y.term with
        | Concrete(x, t1), Concrete(y, t2) ->
            let mutable noOverflow = true
            assert(isNumeric t1 && isNumeric t2)
            let retType = deduceSimpleArithmeticOperationTargetType t1 t2
            try
                ILCalculator.addOvfUn(x, y, retType) |> ignore
            with :? System.OverflowException ->
                noOverflow <- false
            makeBool noOverflow
        | _ -> makeBinary OperationType.AddNoOvf_Un x y bool

    let simplifyMultiplyNoOvf x y =
        match x.term, y.term with
        | Concrete(x, t1), Concrete(y, t2) ->
            let mutable noOverflow = true
            assert(isNumeric t1 && isNumeric t2)
            let retType = deduceSimpleArithmeticOperationTargetType t1 t2
            try
                ILCalculator.mulOvf(x, y, retType) |> ignore
            with :? System.OverflowException ->
                noOverflow <- false
            makeBool noOverflow
        | _ -> makeBinary OperationType.MultiplyNoOvf x y bool

    let simplifyMultiplyNoOvfUn x y =
        match x.term, y.term with
        | Concrete(x, t1), Concrete(y, t2) ->
            let mutable noOverflow = true
            assert(isNumeric t1 && isNumeric t2)
            let retType = deduceSimpleArithmeticOperationTargetType t1 t2
            try
                ILCalculator.mulOvfUn(x, y, retType) |> ignore
            with :? System.OverflowException ->
                noOverflow <- false
            makeBool noOverflow
        | _ -> makeBinary OperationType.MultiplyNoOvf_Un x y bool

    let simplifySubNoOvf x y =
        match x.term, y.term with
        | Concrete(x, t1), Concrete(y, t2) ->
            let mutable noOverflow = true
            assert(isNumeric t1 && isNumeric t2)
            let retType = deduceSimpleArithmeticOperationTargetType t1 t2
            try
                ILCalculator.subOvf(x, y, retType) |> ignore
            with :? System.OverflowException ->
                noOverflow <- false
            makeBool noOverflow
        | _ -> makeBinary OperationType.SubNoOvf x y bool

    let simplifySubNoOvfUn x y =
        match x.term, y.term with
        | Concrete(x, t1), Concrete(y, t2) ->
            let mutable noOverflow = true
            assert(isNumeric t1 && isNumeric t2)
            let retType = deduceSimpleArithmeticOperationTargetType t1 t2
            try
                ILCalculator.subOvfUn(x, y, retType) |> ignore
            with :? System.OverflowException ->
                noOverflow <- false
            makeBool noOverflow
        | _ -> makeBinary OperationType.SubNoOvf_Un x y bool

// ------------------------------- General functions -------------------------------

    let inline private deduceArithmeticTargetType x y =
        deduceSimpleArithmeticOperationTargetType (typeOf x) (typeOf y)

    let add x y =
        simplifyAddition (deduceArithmeticTargetType x y) x y id

    let sub x y =
        simplifySubtraction (deduceArithmeticTargetType x y) x y id

    let neg x =
        simplifyUnaryMinus (typeOf x) x id

    let mul x y =
        simplifyMultiplication (deduceArithmeticTargetType x y) x y id

    let div x y =
        simplifyDivision true (deduceArithmeticTargetType x y) x y id

    let rem x y =
        simplifyRemainder true (deduceArithmeticTargetType x y) x y id

    let remUn x y =
        simplifyRemainder false (deduceArithmeticTargetType x y) x y id

    let eq x y =
        simplifyEqual x y id

    let simplifyBinaryOperation op x y k =
        let t = Operations.deduceArithmeticBinaryExpressionTargetType op (typeOf x) (typeOf y)
        match op with
        | OperationType.Add -> simplifyAddition t x y k
        | OperationType.Subtract -> simplifySubtraction t x y k
        | OperationType.Multiply -> simplifyMultiplication t x y k
        | OperationType.Divide -> simplifyDivision true t x y k
        | OperationType.Divide_Un -> simplifyDivision false t x y k
        | OperationType.Remainder -> simplifyRemainder true t x y k
        | OperationType.Remainder_Un -> simplifyRemainder false t x y k
        | OperationType.ShiftLeft -> simplifyShift op t x y k
        | OperationType.ShiftRight
        | OperationType.ShiftRight_Un -> simplifyShift op t x y k
        | OperationType.Equal -> simplifyEqual x y k
        | OperationType.NotEqual -> simplifyNotEqual x y k
        | OperationType.Greater -> simplifyGreater x y k
        | OperationType.Greater_Un -> simplifyGreaterUn x y k
        | OperationType.GreaterOrEqual -> simplifyGreaterOrEqual x y k
        | OperationType.GreaterOrEqual_Un -> simplifyGreaterOrEqualUn x y k
        | OperationType.Less -> simplifyLess x y k
        | OperationType.Less_Un -> simplifyLessUn x y k
        | OperationType.LessOrEqual -> simplifyLessOrEqual x y k
        | OperationType.LessOrEqual_Un -> simplifyLessOrEqualUn x y k
        | OperationType.BitwiseAnd
        | OperationType.BitwiseOr
        | OperationType.BitwiseXor -> simplifyBitwise op x y t k
        | OperationType.AddNoOvf -> simplifyAddNoOvf x y |> k
        | OperationType.AddNoOvf_Un -> simplifyAddNoOvfUn x y |> k
        | OperationType.MultiplyNoOvf -> simplifyMultiplyNoOvf x y |> k
        | OperationType.MultiplyNoOvf_Un -> simplifyMultiplyNoOvfUn x y |> k
        | OperationType.SubNoOvf -> simplifySubNoOvf x y |> k
        | OperationType.SubNoOvf_Un -> simplifySubNoOvfUn x y |> k
        | _ -> internalfailf $"{op} is not a binary arithmetical operator"

    let simplifyUnaryOperation op x t k =
        match op with
        | OperationType.BitwiseNot -> simplifyBinaryNot t x k
        | OperationType.UnaryMinus -> simplifyUnaryMinus t x k
        | _ -> internalfailf $"{op} is not an unary arithmetical operator"

    let isArithmeticalOperation op t1 t2 =
        (isNumeric t1 || t1 = addressType) && (isNumeric t2 || t2 = addressType) &&
        match op with
        | OperationType.Add
        | OperationType.AddNoOvf
        | OperationType.AddNoOvf_Un
        | OperationType.Subtract
        | OperationType.SubNoOvf
        | OperationType.SubNoOvf_Un
        | OperationType.Multiply
        | OperationType.MultiplyNoOvf
        | OperationType.MultiplyNoOvf_Un
        | OperationType.Divide
        | OperationType.Divide_Un
        | OperationType.Remainder
        | OperationType.Remainder_Un
        | OperationType.ShiftLeft
        | OperationType.ShiftRight
        | OperationType.ShiftRight_Un
        | OperationType.Equal
        | OperationType.NotEqual
        | OperationType.Greater
        | OperationType.Greater_Un
        | OperationType.GreaterOrEqual
        | OperationType.GreaterOrEqual_Un
        | OperationType.Less
        | OperationType.Less_Un
        | OperationType.LessOrEqual
        | OperationType.LessOrEqual_Un
        | OperationType.BitwiseAnd
        | OperationType.BitwiseOr
        | OperationType.BitwiseXor
        | OperationType.BitwiseNot
        | OperationType.UnaryMinus -> true
        | _ -> false

    let checkEqualZero y k =
        simplifyEqual y (castConcrete 0 (typeOf y)) k

    let makeExpressionNoOvf expr =
        let collectConditions acc expr next into =
            match expr with
            | Add(x, y, t) -> acc &&& (if isSigned t then simplifyAddNoOvf x y else simplifyAddNoOvfUn x y) |> next
            | Sub(x, y, t) -> acc &&& (if isSigned t then simplifySubNoOvf x y else simplifySubNoOvfUn x y) |> next
            | Mul(x, y, t) -> acc &&& (if isSigned t then simplifyMultiplyNoOvf x y else simplifyMultiplyNoOvfUn x y) |> next
            | {term = Expression _ } -> into acc
            | _ -> next acc
        Seq.singleton expr |> fold collectConditions (True())

// ------------------------------- Standard functions -------------------------------

    let impl<'a when 'a : comparison> (concrete : 'a -> 'a) standFunc arg =
        arg |> Merging.guardedApply (fun term ->
            match term.term with
            | Concrete(obj, _) ->
                let a = obj :?> 'a
                makeNumber (concrete a)
            | Constant(_, _, t)
            | Expression(_, _, t) ->
                Expression (Application standFunc) [term] t
            | term -> internalfailf $"expected number, but {term} got!")

    let powImpl<'a when 'a : comparison> convert isNaN isPosInf isNegInf concrete b p =
        let mkPowExpr args typ = Expression (Application StandardFunction.Power) args typ
        let zero = convert 0.0
        let one = convert 1.0
        let minusInf = convert -infinity
        let zeroTerm = makeNumber zero
        let oneTerm = makeNumber one
        let infTerm = convert infinity |> makeNumber
        let minusOneTerm = convert -1.0 |> makeNumber
        let (<<) x y = simplifyLess x y id
        let (===) x y = simplifyEqual x y id
        let (%%%) x y = simplifyRemainder true (typeOf x) x y id
        b |> Merging.guardedApply (fun term ->
            match term.term with
            | Concrete(bObj, _) ->
                let bConc = term
                let b = bObj :?> 'a
                p |> Merging.guardedApply (fun term ->
                    match term.term with
                    | Concrete(pObj, _) ->
                        let p = pObj :?> 'a
                        makeNumber (concrete(b, p))
                    | Constant(_, _, t)
                    | Expression(_, _, t) ->
                        let p = term
                        // base is concrete, exponent is symbolic
                        match b with
                        | _ when b = zero ->
                            let pIsLessZero = p << zeroTerm
                            let pIsZero = p === zeroTerm
                            Union([(pIsZero, oneTerm); (pIsLessZero, infTerm);
                                   (!!pIsLessZero, zeroTerm)])
                        | _ when b = one -> oneTerm
                        | _ when isNaN b ->
                            // NOTE hack for .NET 5, msdn says: if any operand is NaN, result is NaN
                            let pIsZero = p === zeroTerm
                            Union([(pIsZero, oneTerm); (!!pIsZero, bConc)])
                        | _ when isPosInf b ->
                            let pIsZero = p === zeroTerm
                            let pIsLessZero = p << zeroTerm
                            Union([(pIsZero, oneTerm); (pIsLessZero, zeroTerm);
                                  (!!pIsZero &&& !!pIsLessZero, infTerm)])
                        | _ when isNegInf b ->
                            let pIsZero = p === zeroTerm
                            let pIsLessZero = p << zeroTerm
                            if isIntegral t then
                                let pIsGreaterZeroAndEven = (p %%% (Concrete 2 t)) === makeNumber 0
                                Union([(pIsZero, oneTerm); (pIsLessZero, zeroTerm); (pIsGreaterZeroAndEven, infTerm);
                                       (!!pIsZero &&& !!pIsLessZero &&& !!pIsGreaterZeroAndEven, makeNumber minusInf)])
                            else Union([(pIsZero, oneTerm); (pIsLessZero, zeroTerm);
                                        (!!pIsZero &&& !!pIsLessZero, infTerm)])
                        | _ -> mkPowExpr [bConc; p] t
                    | term -> internalfailf $"expected number for power, but {term} got!")
            | Constant(_, _, t) | Expression(_, _, t) ->
                let b = term
                p |> Merging.guardedApply (fun term ->
                    match term.term with
                    | Concrete(pObj, _) ->
                        let pConc = term
                        // base is symbolic, exponent is concrete
                        match pObj :?> 'a with
                        | p when p = zero -> oneTerm
                        | p when p = one -> b
                        | p when isNaN p -> pConc
                        | p when isPosInf p ->
                            let bIsOne = b === oneTerm
                            let bIsMinusOne = b === minusOneTerm
                            let bIsBetweenMinOneOne = (minusOneTerm << b) &&& (b << oneTerm)
                            Union([(bIsOne, oneTerm); (bIsMinusOne, makeNumber nan);
                                   (bIsBetweenMinOneOne, zeroTerm);
                                   (!!bIsOne &&& !!bIsMinusOne &&& !!bIsBetweenMinOneOne, infTerm)])
                        | p when isNegInf p ->
                            let bIsOne = b === oneTerm
                            let bIsMinusOne = b === minusOneTerm
                            let bIsBetweenMinOneOne = (minusOneTerm << b) &&& (b << oneTerm)
                            Union([(bIsOne, oneTerm); (bIsMinusOne, makeNumber nan);
                                   (bIsBetweenMinOneOne, infTerm);
                                   (!!bIsOne &&& !!bIsMinusOne &&& !!bIsBetweenMinOneOne, zeroTerm)])
                        | _ -> mkPowExpr [b; pConc] t
                    | Constant(_, _, t) | Expression(_, _, t) ->
                        mkPowExpr [b; term] t
                    | term -> internalfailf $"expected number for power, but {term} got!")
            | term -> internalfailf $"expected number for base, but {term} got!")

    let atan2Impl<'a when 'a : comparison> convert isNan isInf concrete y x =
        let atanOp = Application StandardFunction.Arctangent2
        let inf, Nan = convert infinity, convert nan
        let (===) x y = simplifyEqual x y id
        y |> Merging.guardedApply (fun term ->
            match term.term with
            | Concrete(yObj, _) ->
                let yConc = term
                let y = yObj :?> 'a
                x |> Merging.guardedApply (fun term ->
                    match term.term with
                    | Concrete(xObj, _) -> makeNumber(concrete (y, xObj :?> 'a))
                    | Constant(_, _, t)
                    | Expression(_, _, t) ->
                        let x = term
                        // x is symbolic, y is concrete
                        let exp = Expression atanOp [yConc; x] t
                        match y with
                        | y when isNan y -> yConc
                        | y when isInf y ->
                              let xIsInf = x === makeNumber inf
                              Union([(xIsInf, makeNumber Nan); (!!xIsInf, exp)])
                        | _ -> exp
                    | term -> internalfailf $"expected number for x, but {term} got!")
            | Constant(_, _, t)
            | Expression(_, _, t) ->
                let y = term
                x |> Merging.guardedApply (fun term ->
                    match term.term with
                    | Concrete(xObj, _) ->
                        let xConc = term
                        // x is concrete, y is symbolic
                        let exp = Expression atanOp [y; xConc] t
                        match xObj :?> 'a with
                        | x when isNan x -> xConc
                        | x when isInf x ->
                            let yIsInf = y === makeNumber inf
                            Union([(yIsInf, makeNumber Nan); (!!yIsInf, exp)])
                        | _ -> exp
                    | Constant(_, _, t)
                    | Expression(_, _, t) ->
                        Expression atanOp [y; term] t
                    | term -> internalfailf $"expected number for x, but {term} got!")
            | term -> internalfailf $"expected number for y, but {term} got!")

    let acos x = impl<double> System.Math.Acos StandardFunction.Arccosine x
    let asin x = impl<double> System.Math.Asin StandardFunction.Arcsine x
    let atan x = impl<double> System.Math.Atan StandardFunction.Arctangent x
    let atan2 y x = atan2Impl<double> double System.Double.IsNaN System.Double.IsInfinity System.Math.Atan2 y x
    let ceiling x = impl<double> System.Math.Ceiling StandardFunction.Ceiling x
    let cos x = impl<double> System.Math.Cos StandardFunction.Cosine x
    let cosh x = impl<double> System.Math.Cosh StandardFunction.HyperbolicCosine x
    let floor x = impl<double> System.Math.Floor StandardFunction.Floor x
    let sin x = impl<double> System.Math.Sin StandardFunction.Sine x
    let tan x = impl<double> System.Math.Tan StandardFunction.Tangent x
    let sinh x = impl<double> System.Math.Sinh StandardFunction.HyperbolicSine x
    let tanh x = impl<double> System.Math.Tanh StandardFunction.HyperbolicTangent x
    let round x = impl<double> System.Math.Round StandardFunction.Round x
    let sqrt x = impl<double> System.Math.Sqrt StandardFunction.SquareRoot x
    let log x = impl<double> System.Math.Log StandardFunction.Logarithm x
    let log10 x = impl<double> System.Math.Log10 StandardFunction.Logarithm10 x
    let exp x = impl<double> System.Math.Exp StandardFunction.Exponent x
    let pow b p = powImpl<double> double System.Double.IsNaN System.Double.IsPositiveInfinity System.Double.IsNegativeInfinity System.Math.Pow b p
    let abs x = impl<double> System.Math.Abs StandardFunction.Absolute x
    let absS x = impl<single> System.Math.Abs StandardFunction.AbsoluteS x

    let standardFunction args = function
        | Arccosine -> acos (List.head args)
        | Arcsine -> asin (List.head args)
        | Arctangent -> atan (List.head args)
        | Arctangent2 -> atan2 (List.item 0 args) (List.item 1 args)
        | Ceiling -> ceiling (List.head args)
        | Cosine -> cos (List.head args)
        | HyperbolicCosine -> cosh (List.head args)
        | Floor -> floor (List.head args)
        | Sine -> sin (List.head args)
        | Tangent -> tan (List.head args)
        | HyperbolicSine -> sinh (List.head args)
        | HyperbolicTangent -> tanh (List.head args)
        | Round -> round (List.head args)
        | SquareRoot -> sqrt (List.head args)
        | Logarithm -> log (List.head args)
        | Logarithm10 -> log10 (List.head args)
        | Exponent -> exp (List.head args)
        | Power -> pow (List.item 0 args) (List.item 1 args)
        | Absolute -> abs (List.head args)
        | AbsoluteS -> absS (List.head args)
