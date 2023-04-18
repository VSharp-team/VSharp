namespace VSharp.Core

open System.Reflection.Emit
open VSharp
open VSharp.Core
open VSharp.TypeUtils
open VSharp.CSharpUtils
open VSharp.Core.Common

module Calculator1 =

    // TODO: add all other operations and cache delegates
    type binaryDelegateType = delegate of obj * obj -> obj

    let Add(x : obj, y : obj, t : System.Type) =
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

    let AddOvf(x : obj, y : obj, t : System.Type) =
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

    let Sub(x : obj, y : obj, t : System.Type) =
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

    let Mul(x : obj, y : obj, t : System.Type) =
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

    let MulOvf(x : obj, y : obj, t : System.Type) =
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

    let Div(x : obj, y : obj, t : System.Type) =
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

    let ShiftRight(x : obj, y : obj, t : System.Type) =
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

    type compareDelegateType = delegate of obj * obj -> int

    let Compare(x : obj, y : obj) : int =
        assert(isNumeric <| x.GetType() && isNumeric <| y.GetType())
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

    type unaryToIntDelegateType = delegate of obj -> int

    let IsZero x =
        assert(isNumeric <| x.GetType())
        let args = [| typeof<obj> |]
        let isZero = DynamicMethod("IsZero", typeof<int>, args)
        let il = isZero.GetILGenerator(256)
        let zeroCase = il.DefineLabel()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Unbox_Any, x.GetType())
        il.Emit(OpCodes.Brfalse, zeroCase)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ret)
        il.MarkLabel(zeroCase)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Ret)
        let isZero = isZero.CreateDelegate(typeof<unaryToIntDelegateType>) :?> unaryToIntDelegateType
        isZero.Invoke(x) = 1

    type unaryToObjDelegateType = delegate of obj -> obj

    let BitwiseNot(x : obj, t : System.Type) =
        assert(x <> null && isNumeric <| x.GetType())
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
        castConcrete (Calculator1.Add(x, y, t)) t
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
            let tooBigShift = Calculator1.Compare(bval, ((sizeOf a) * 8) - 1) = 0
            if tooBigShift then
                castConcrete 0 t |> matched
            else
                simplifyShift OperationType.ShiftLeft t a (castConcrete (Calculator1.Add(bval, 1, bt)) bt) matched
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
        | Concrete(xval, _), _ when Calculator1.IsZero xval -> matched y
        | _, Concrete(yval, _) when Calculator1.IsZero yval -> matched x
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
        | Concrete(x, _) -> castConcrete (Calculator1.BitwiseNot(x, t)) t |> k
        | _ -> makeUnary OperationType.BitwiseNot x t |> k

// ------------------------------- Simplification of unary "-" -------------------------------

    and private simplifyConcreteUnaryMinus t x =
        let zero = Reflection.createObject (x.GetType())
        castConcrete (Calculator1.Sub(zero, x, t)) t

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
        castConcrete (Calculator1.Mul(x, y, t)) t

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
            let smallShift = Calculator1.Compare(Calculator1.Add(bval, dval, t), bitSizeOf a t) = -1
            if smallShift then
                simplifyMultiplication t a c (fun mul ->
                let bPlusD = castConcrete (Calculator1.Add(bval, dval, bt)) bt
                simplifyShift OperationType.ShiftLeft t mul bPlusD matched)
            else
                castConcrete 0 t |> matched
        // (a << b) * 2^n = a << (b + n) if unchecked, b is concrete, b + n < (size of a) * 8
        // (a << b) * 2^n = 0 if unchecked, b is concrete, b + n >= (size of a) * 8
        | Concrete(bval, bt), ConcreteT(powOf2, _) when Calculator.IsPowOfTwo(powOf2) ->
            let n = Calculator.WhatPowerOf2(powOf2)
            let tooBigShift = Calculator1.Compare(Calculator1.Add(bval, n, t), bitSizeOf a t) >= 0
            if tooBigShift then castConcrete 0 t |> matched
            else
                simplifyShift OperationType.ShiftLeft t a (castConcrete (Calculator1.Add(bval, n, bt)) bt) matched
        | _ -> unmatched ()

    and private simplifyMultiplicationOfExpression t x y matched unmatched =
        match x with
        | Mul(a, b, _) -> simplifyMultiplicationOfProduct t a b y matched unmatched
        | Div(a, b, _, isSigned) -> simplifyMultiplicationOfDivision isSigned t a b y matched unmatched
        | ShiftLeft(a, b, _) -> simplifyMultiplicationOfShifts t a b y matched unmatched
        | _ -> unmatched ()

    and private simplifyMultiplicationExt (t : System.Type) (x : term) y matched unmatched =
        match x.term, y.term with
        | Concrete(xval, _), _ when Calculator1.IsZero(xval) -> castConcrete 0 t |> matched
        | _, Concrete(yval, _) when Calculator1.IsZero(yval) -> castConcrete 0 t |> matched
        | Concrete(x, _), _ when Calculator.FuzzyEqual(x, System.Convert.ChangeType(1, t)) -> matched y
        | _, Concrete(y, _) when Calculator.FuzzyEqual(y, System.Convert.ChangeType(1, t)) -> matched x
        | Concrete(x, _), _ when not <| isUnsigned t && Calculator.FuzzyEqual(x, System.Convert.ChangeType(-1, t)) ->
            simplifyUnaryMinus t y matched
        | _, Concrete(y, _) when not <| isUnsigned t && Calculator.FuzzyEqual(y, System.Convert.ChangeType(-1, t)) ->
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

    and simplifyConcreteDivision t x y =
        let result = Calculator1.Div(x, y, t)
        castConcrete result t

    and private simplifyDivision isSigned t x y k =
        simplifyGenericBinary "division" x y k
            (simplifyConcreteBinary simplifyConcreteDivision t)
            (fun x y k ->
                match x, y with
                // 0 / y = 0
                | ConcreteT(xval, _), _ when Calculator1.IsZero(xval) -> x |> k
                // x / 1 = x
                | _, ConcreteT(yval, _) when Calculator.FuzzyEqual(yval, System.Convert.ChangeType(1, typeOf x)) -> x |> k
                // x / -1 = -x
                | _, ConcreteT(yval, _) when not <| isUnsigned t && Calculator.FuzzyEqual(yval, convert -1 (typeOf y)) ->
                    simplifyUnaryMinus t x k
                // x / x = 1 if unchecked
                | x, y when x = y -> castConcrete 1 t |> k
                // x / -x = -1 if unchecked
                | x, UnaryMinusT(y, _) when not <| isUnsigned t && x = y -> castConcrete -1 t |> k
                // x / 2^n = x >> n if unchecked and x is unsigned
                | _, ConcreteT(powOf2, _) when Calculator.IsPowOfTwo(powOf2) && not isSigned ->
                    let n = Calculator.WhatPowerOf2(powOf2) |> int |> makeNumber
                    simplifyShift OperationType.ShiftRight_Un t x n k
                // (a >> b) / 2^n = a >> (b + n) if unchecked, b is concrete, b + n < (size of a) * 8
                // (a >> b) / 2^n = 0 if unchecked, b is concrete, b + n >= (size of a) * 8
//                | CastExpr(ShiftRight(a, b, Numeric(Id t2)), (Numeric(Id t1) as t)) when not <| typeIsLessType t1 t2 -> Some(ShiftRight(primitiveCast x t, y, t)) ->
                | ShiftRightThroughCast(a, ConcreteT(b, bt), _), ConcreteT(powOf2, _)
                | ShiftRight(a, ConcreteT(b, bt), _, _), ConcreteT(powOf2, _)
                    when Calculator.IsPowOfTwo(powOf2) && a |> typeOf |> isUnsigned ->
                        let n = Calculator.WhatPowerOf2(powOf2)
                        let tooBigShift = Calculator1.Compare(Calculator1.Add(b, n, t), bitSizeOf a t) >= 0
                        if tooBigShift then castConcrete 0 t |> k
                        else
                            let op = if isSigned then OperationType.ShiftRight else OperationType.ShiftRight_Un
                            simplifyShift op t a (castConcrete (Calculator1.Add(b, n, bt)) bt) k
                | _ ->
                    let op = if isSigned then OperationType.Divide else OperationType.Divide_Un
                    (makeBinary op x y t) |> k)
            (fun x y k -> simplifyDivision isSigned t x y k)

// ------------------------------- Simplification of "%" -------------------------------

    and private simplifyConcreteRemainder t x y =
        let success = ref true
        let result = Calculator.Rem(x, y, t, success)
        assert success.Value
        castConcrete result t

    and private divides t x y =
        let success = ref true
        Calculator1.IsZero(Calculator.Rem(x, y, t, success)) && success.Value

    and simplifyRemainder isSigned t x y k =
        simplifyGenericBinary "remainder" x y k
            (simplifyConcreteBinary simplifyConcreteRemainder t)
            (fun x y k ->
                match x, y with
                // 0 % y = 0
                | ConcreteT(xval, _), _ when Calculator1.IsZero(xval) -> x |> k
                // x % 1 = 0
                | _, ConcreteT(y, _) when Calculator.FuzzyEqual(y, System.Convert.ChangeType(1, t)) -> castConcrete 0 t |> k
                // x % -1 = 0
                | _, ConcreteT(y, _) when not <| isUnsigned t && Calculator.FuzzyEqual(y, System.Convert.ChangeType(-1, t)) ->
                    castConcrete 0 t |> k
                // x % x = 0
                | x, y when x = y -> castConcrete 0 t |> k
                // x % -x = 0 if unchecked
                | x, UnaryMinusT(y, _) when x = y -> castConcrete 0 t |> k
                // (a * b) % y = 0 if unchecked, b and y concrete and a % y = 0
                | Mul(ConcreteT(a, _), _, _), ConcreteT(y, _) when divides t a y ->
                     castConcrete 0 t |> k
                | _ ->
                    let op = if isSigned then OperationType.Remainder else OperationType.Remainder_Un
                    makeBinary op x y t |> k)
            (fun x y k -> simplifyRemainder isSigned t x y k)

// ---------------------------------------- Simplification of "<<", ">>" ----------------------------------------

    and private simplifyConcreteShift operation t x y =
        match operation with
        | OperationType.ShiftLeft -> castConcrete (Calculator.ShiftLeft(x, y, t)) t
        | OperationType.ShiftRight
        | OperationType.ShiftRight_Un -> castConcrete (Calculator1.ShiftRight(x, y, t)) t
        | _ -> __unreachable__()

    and private simplifyShiftLeftMul t a b y matched unmatched =
        // Simplifying (a * b) << y at this step
        match a.term, b.term, y.term with
        // (2^n * b) << y = b << (y + n) if unchecked, y is concrete, y + n < bitSize of a
        // (2^n * b) << y = 0 if unchecked, y is concrete, y + n >= bitSize of a
        | Concrete(powOf2, _), _, Concrete(yval, yt)
            when Calculator.IsPowOfTwo(powOf2) ->
                let n = Calculator.WhatPowerOf2(powOf2)
                let tooBigShift = Calculator1.Compare(Calculator1.Add(yval, n, t), bitSizeOf a t) >= 0
                if tooBigShift then castConcrete 0 t |> matched
                else
                    simplifyShift OperationType.ShiftLeft t b (castConcrete (Calculator1.Add(yval, n, yt)) yt) matched
        | _ -> unmatched ()

    and private simplifyShiftRightDiv op t a b y matched unmatched =
        // Simplifying (a / b) >> y at this step
        match b.term, y.term with
        // (a / 2^n) >> y = a >> (y + n) if y is concrete, a is unsigned, y + n < bitSize of a
        // (a / 2^n) >> y = 0 if y is concrete, a is unsigned, y + n >= bitSize of a
        | Concrete(powOf2, _), Concrete(yval, yt)
            when Calculator.IsPowOfTwo(powOf2) && a |> typeOf |> isUnsigned ->
                let n = Calculator.WhatPowerOf2(powOf2)
                let tooBigShift = Calculator1.Compare(Calculator1.Add(yval, n, t), bitSizeOf a t) >= 0
                if tooBigShift then castConcrete 0 t |> matched
                else
                    simplifyShift op t a (castConcrete (Calculator1.Add(yval, n, yt)) yt) matched
        | _ -> unmatched ()

    and private simplifyShiftLeftOfAddition t a y (matched : term -> 'a) unmatched =
        // Simplifying (a + a) << y at this step
        match y.term with
        // (a + a) << y = 0 if unchecked, y is concrete, y = (size of a) * 8 - 1
        // (a + a) << y = a << (y + 1) if unchecked, y is concrete, y < (size of a) * 8 - 1
        | Concrete(yval, yt) ->
            let tooBigShift = Calculator1.Compare(yval, ((sizeOf a) * 8) - 1) = 0
            if tooBigShift then castConcrete 0 t |> matched
            else
                simplifyShift OperationType.ShiftLeft t a (castConcrete (Calculator1.Add(yval, 1, yt)) yt) matched
        | _ -> unmatched ()

    and private simplifyShiftOfShifted op t a b y matched unmatched =
        // Simplifying (a op b) op y at this step
        match b.term, y.term, op with
        // (a op b) op y = a op (b + y) if unchecked, b and y are concrete, b + y < (size of a) * 8
        | Concrete(bval, bt), Concrete(yval, _), _ when Calculator1.Compare(Calculator1.Add(bval, yval, t), bitSizeOf a t) = -1 ->
            simplifyShift op t a (castConcrete (Calculator1.Add(bval, yval, bt)) bt) matched
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
        | Concrete(x, _), _ when Calculator1.IsZero(x) -> castConcrete 0 t |> matched
        | _, Concrete(y, _) when Calculator1.IsZero(y) -> x |> matched
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
            (fun x y k -> simplifyShift operation t x y k)

// TODO: IMPLEMENT BITWISE OPERATIONS!
    and private simplifyBitwise (op : OperationType) x y t resType k =
        match x.term, y.term with
        | Concrete(x, _), Concrete(y, _) ->
            match op with
            | OperationType.BitwiseAnd -> k <| Concrete (Calculator.BitwiseAnd(x, y, t)) resType
            | OperationType.BitwiseOr -> k <| Concrete (Calculator.BitwiseOr(x, y, t)) resType
            | OperationType.BitwiseXor -> k <| Concrete (Calculator.BitwiseXor(x, y, t)) resType
            | _ -> __notImplemented__()
        | _ -> k (Expression (Operator op) [x; y] resType)

// ------------------------------- Simplification of "=", "!=", "<", ">", ">=", "<=" -------------------------------

    and fastNumericCompare n m =
        if n = m then True
        elif isConcrete n && isConcrete m then False
        else makeBinary OperationType.Equal n m bool

    and private simplifyConcreteComparison operator _ x y =
        let bx = box x
        let by = box y
        if (bx :? int32 list) && (by :? int32 list) then
            Concrete (List.compareWith compare (bx :?> int32 list) (by :?> int32 list) |> operator) bool
        else
            Concrete (Calculator1.Compare(bx, by) |> operator) bool

    and private simplifyComparison op x y comparator sameIsTrue k =
        simplifyGenericBinary "comparison" x y k
            (simplifyConcreteBinary (simplifyConcreteComparison comparator) bool)
            (fun x y k ->
                match x, y with
                | _ when x = y -> Concrete sameIsTrue bool |> k
                | Add(ConcreteT(_, t) as c, x, _), y when x = y && (op = OperationType.Equal || op = OperationType.NotEqual) ->
                    simplifyComparison op c (castConcrete 0 t) comparator sameIsTrue k
                | x, Add(ConcreteT(_, t) as c, y, _) when x = y && (op = OperationType.Equal || op = OperationType.NotEqual) ->
                    simplifyComparison op (castConcrete 0 t) c comparator sameIsTrue k
                | _ -> makeBinary op x y bool |> k)
            (fun x y k -> simplifyComparison op x y comparator sameIsTrue k)

    and simplifyEqual x y k = simplifyComparison OperationType.Equal x y ((=) 0) true k
    and simplifyNotEqual x y k = simplifyEqual x y ((!!) >> k)
    and simplifyLess x y k = simplifyComparison OperationType.Less x y ((>) 0) false k
    and simplifyLessUn x y k = simplifyComparison OperationType.Less_Un x y ((>) 0) false k
    and simplifyLessOrEqual x y k = simplifyComparison OperationType.LessOrEqual x y ((>=) 0) true k
    and simplifyLessOrEqualUn x y k = simplifyComparison OperationType.LessOrEqual_Un x y ((>=) 0) true k
    and simplifyGreater x y k = simplifyLessOrEqual x y ((!!) >> k)
    and simplifyGreaterUn x y k = simplifyLessOrEqualUn x y ((!!) >> k)
    and simplifyGreaterOrEqual x y k = simplifyLess x y ((!!) >> k)
    and simplifyGreaterOrEqualUn x y k = simplifyLessUn x y ((!!) >> k)

// ------------------------------- Simplification of no overflow check -------------------------------

    let simplifyAddNoOvf x y =
        match x.term, y.term with
        | Concrete(x, t1), Concrete(y, t2) ->
            let mutable noOverflow = true
            assert(t1 = t2)
            try
                Calculator1.AddOvf(x, y, t1) |> ignore
            with :? System.OverflowException ->
                noOverflow <- false
            makeBool noOverflow
        | _ -> makeBinary OperationType.AddNoOvf x y bool

    let simplifyMultiplyNoOvf x y =
        match x.term, y.term with
        | Concrete(x, t1), Concrete(y, t2) ->
            let mutable noOverflow = true
            assert(t1 = t2)
            try
                Calculator1.MulOvf(x, y, t1) |> ignore
            with :? System.OverflowException ->
                noOverflow <- false
            makeBool noOverflow
        | _ -> makeBinary OperationType.MultiplyNoOvf x y bool

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
        | OperationType.BitwiseXor -> simplifyBitwise op x y t (typeOf x) k
        | OperationType.AddNoOvf -> simplifyAddNoOvf x y |> k
        | OperationType.MultiplyNoOvf -> simplifyMultiplyNoOvf x y |> k
        | _ -> internalfailf "%O is not a binary arithmetical operator" op

    let simplifyUnaryOperation op x t k =
        match op with
        | OperationType.BitwiseNot -> simplifyBinaryNot t x k
        | OperationType.UnaryMinus -> simplifyUnaryMinus t x k
        | _ -> internalfailf "%O is not an unary arithmetical operator" op

    let isArithmeticalOperation op t1 t2 =
        (isNumeric t1 || t1 = addressType) && (isNumeric t2 || t2 = addressType) &&
        match op with
        | OperationType.Add
        | OperationType.AddNoOvf
        | OperationType.Subtract
        | OperationType.Multiply
        | OperationType.MultiplyNoOvf
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

    // TODO: implement without using of expression AddNoOvf or MultiplyNoOvf:
    // TODO: - if signed, then it should keep the sign
    let rec makeExpressionNoOvf expr k =
        match expr with
        | Add(x, y, _) -> simplifyAddNoOvf x y |> k
        | Mul(x, y, _) -> simplifyMultiplyNoOvf x y |> k
        | {term = Expression(_, args, _) } ->
            Cps.List.foldlk (fun acc x k -> makeExpressionNoOvf x (fun x' -> k (acc &&& x'))) True args k
        | _ -> k True

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
            | term -> internalfailf "expected number, but %O got!" term)

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
                    | term -> internalfailf "expected number for power, but %O got!" term)
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
                    | term -> internalfailf "expected number for power, but %O got!" term)
            | term -> internalfailf "expected number for base, but %O got!" term)

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
                    | term -> internalfailf "expected number for x, but %O got!" term)
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
                    | term -> internalfailf "expected number for x, but %O got!" term)
            | term -> internalfailf "expected number for y, but %O got!" term)

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
