namespace VSharp.Core

open VSharp
open VSharp.TypeUtils
open VSharp.CSharpUtils
open VSharp.Core.Common
open VSharp.Core.Types
open VSharp.Core.Types.Constructor

[<AutoOpen>]
module internal Arithmetics =

    let private makeAddition t x y k =
        makeBinary OperationType.Add x y (fromDotNetType t) |> k

    let private makeProduct t x y k =
        makeBinary OperationType.Multiply x y (fromDotNetType t) |> k

    let private makeShift op t x y k =
        makeBinary op x y (fromDotNetType t) |> k


// ------------------------------- Simplification of "+" -------------------------------

    let private simplifyConcreteAddition t x y =
        castConcrete (Calculator.Add(x, y, t)) t

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
        | _ ->
            // Trying to simplify pairwise combinations of x- and y-summands
            let summandsOfY =
                match y with
                | Add(c, d, _) -> [c; d]
                | _ -> [y]
            simplifyPairwiseCombinations
                [a; b]
                summandsOfY
                t
                id
                simplifyAdditionExt
                (simplifyAddition t)
                matched
                unmatched

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
            let aPlusOne = simplifyConcreteAddition (Types.toDotNetType atyp) aval 1
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
        | Concrete(x, xt), ShiftLeft(c, ConcreteT(d, _), _) when a = c && x = d ->
            let tooBigShift = Calculator.Compare(x, ((Terms.sizeOf a) * 8) - 1) = 0
            if tooBigShift then
                castConcrete 0 t |> matched
            else
                let xt' = toDotNetType xt
                simplifyShift OperationType.ShiftLeft t a (castConcrete (Calculator.Add(x, 1, xt')) xt') matched
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
        | Concrete(xval, _), _ when Calculator.IsZero(xval) -> matched y
        | _, Concrete(yval, _) when Calculator.IsZero(yval) -> matched x
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
        k <| makeUnary OperationType.BitwiseNot x (fromDotNetType t)

// ------------------------------- Simplification of unary "-" -------------------------------

    and private simplifyConcreteUnaryMinus t x =
        castConcrete (Calculator.UnaryMinus(x, t)) t

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
            simplifyUnaryMinus (Types.toDotNetType at) a (fun minusA ->
            simplifyMultiplication t minusA y k)
        | _ -> k (makeUnary OperationType.UnaryMinus x (fromDotNetType t))
        )

// ------------------------------- Simplification of "*" -------------------------------

    and private simplifyConcreteMultiplication t x y =
        castConcrete (Calculator.Mul(x, y, t)) t

    and private simplifyMultiplicationOfProduct t a b y matched unmatched =
        // Simplifying (a * b) * y at this step
        match a, b, y with
        // (a * b) * y = (a * y) * b if a and y concrete and unchecked
        | ConcreteT(aval, _), _, ConcreteT(yval, _) ->
            let x = simplifyConcreteMultiplication t aval yval
            simplifyMultiplication t x b matched
        // ((a / y) * b) * y = a * b if unchecked
        | Div(a, c, _), b, _ when c = y -> simplifyMultiplication t a b matched
        // (a * (b / y)) * y = a * b if unchecked
        | a, Div(b, c, _), _ when c = y -> simplifyMultiplication t a b matched
        // (a * b) * (c / a) = b * c if unchecked
        | _, _, Div(c, d, _) when d = a -> simplifyMultiplication t b c matched
        // (a * b) * (c / b) = a * c if unchecked
        | _, _, Div(c, d, _) when d = b -> simplifyMultiplication t a c matched
        | _ ->
            // Trying to simplify pairwise combinations of x- and y-factors
            let factorsOfY =
                match y with
                | Mul(c, d, _) -> [c; d]
                | _ -> [y]
            simplifyPairwiseCombinations
                [a; b]
                factorsOfY
                t
                id
                simplifyMultiplicationExt
                (simplifyMultiplication t)
                matched
                unmatched

    and private simplifyMultiplicationOfDivision t a b y matched unmatched =
        // Simplifying (a / b) * y at this step
        match a, b, y with
        // (a / b) * y = (a * y) / b if a and y are concrete and unchecked
        | ConcreteT(aval, _), b, ConcreteT(yval, _) ->
            let aMulY = simplifyConcreteMultiplication t aval yval
            simplifyDivision t aMulY b matched
        // (a / (y * d)) * y = a/d if unchecked
        | _, Mul(c, d, _), _ when c = y -> simplifyDivision t a d matched
        // (a / (c * y)) * y = a/c if unchecked
        | _, Mul(c, d, _), _ when d = y -> simplifyDivision t a c matched
        | _ -> unmatched ()

    and private simplifyMultiplicationOfShifts (t : System.Type) a b y matched unmatched =
        // Simplifying (a << b) * y at this step
        match b.term, y with
        // (a << b) * (c << d) = (a * c) << (b + d) if unchecked, b and d are conctere, b + d < (size of a) * 8
        // (a << b) * (c << d) = 0 if unchecked, b and d are conctere, b + d >= (size of a) * 8
        | Concrete(bval, bt), ShiftLeft(c, (ConcreteT(dval, _)), _) ->
            let smallShift = Calculator.Compare(Calculator.Add(bval, dval, t), bitSizeOf a t) = -1
            if smallShift then
                simplifyMultiplication t a c (fun mul ->
                let bt' = toDotNetType bt
                let bPlusD = castConcrete (Calculator.Add(bval, dval, bt')) bt'
                simplifyShift OperationType.ShiftLeft t mul bPlusD matched)
            else
                castConcrete 0 t |> matched
        // (a << b) * 2^n = a << (b + n) if unchecked, b is concrete, b + n < (size of a) * 8
        // (a << b) * 2^n = 0 if unchecked, b is concrete, b + n >= (size of a) * 8
        | Concrete(bval, bt), ConcreteT(powOf2, _) when Calculator.IsPowOfTwo(powOf2) ->
            let n = Calculator.WhatPowerOf2(powOf2)
            let tooBigShift = Calculator.Compare(Calculator.Add(bval, n, t), bitSizeOf a t) >= 0
            if tooBigShift then castConcrete 0 t |> matched
            else
                let bt' = toDotNetType bt
                simplifyShift OperationType.ShiftLeft t a (castConcrete (Calculator.Add(bval, n, bt')) bt') matched
        | _ -> unmatched ()

    and private simplifyMultiplicationOfExpression t x y matched unmatched =
        match x with
        | Mul(a, b, _) -> simplifyMultiplicationOfProduct t a b y matched unmatched
        | Div(a, b, _) -> simplifyMultiplicationOfDivision t a b y matched unmatched
        | ShiftLeft(a, b, _) -> simplifyMultiplicationOfShifts t a b y matched unmatched
        | _ -> unmatched ()

    and private simplifyMultiplicationExt (t : System.Type) (x : term) y matched unmatched =
        match x.term, y.term with
        | Concrete(xval, _), _ when Calculator.IsZero(xval) -> castConcrete 0 t |> matched
        | _, Concrete(yval, _) when Calculator.IsZero(yval) -> castConcrete 0 t |> matched
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
        let success = ref true
        let result = Calculator.Div(x, y, t, success)
        castConcrete result t


    and private simplifyDivision t x y k =
        simplifyGenericBinary "division" x y k
            (simplifyConcreteBinary simplifyConcreteDivision t)
            (fun x y k ->
                match x, y with
                // 0 / y = 0
                | ConcreteT(xval, _), _ when Calculator.IsZero(xval) -> x |> k
                // x / 1 = x
                | _, ConcreteT(yval, _) when Calculator.FuzzyEqual(yval, System.Convert.ChangeType(1, typeOf x |> toDotNetType)) -> x |> k
                // x / -1 = -x
                | _, ConcreteT(yval, _) when not <| isUnsigned t && Calculator.FuzzyEqual(yval, System.Convert.ChangeType(-1, typeOf x |> toDotNetType)) ->
                    simplifyUnaryMinus t x k
                // x / x = 1 if unchecked
                | x, y when x = y -> castConcrete 1 t |> k
                // x / -x = -1 if unchecked
                | x, UnaryMinusT(y, _) when not <| isUnsigned t && x = y -> castConcrete -1 t |> k
                // (a >> b) / 2^n = a >> (b + n) if unchecked, b is concrete, b + n < (size of a) * 8
                // (a >> b) / 2^n = 0 if unchecked, b is concrete, b + n >= (size of a) * 8
//                | CastExpr(ShiftRight(a, b, Numeric(Id t2)), (Numeric(Id t1) as t)) when not <| typeIsLessType t1 t2 -> Some(ShiftRight(primitiveCast x t, y, t)) ->
                | ShiftRightThroughCast(a, ConcreteT(b, bt), _), ConcreteT(powOf2, _)
                | ShiftRight(a, ConcreteT(b, bt), _), ConcreteT(powOf2, _)
                    when Calculator.IsPowOfTwo(powOf2) && a |> typeOf |> toDotNetType |> isUnsigned ->
                        let n = Calculator.WhatPowerOf2(powOf2)
                        let tooBigShift = Calculator.Compare(Calculator.Add(b, n, t), bitSizeOf a t) >= 0
                        if tooBigShift then castConcrete 0 t |> k
                        else
                            let bt' = toDotNetType bt
                            simplifyShift OperationType.ShiftRight t a (castConcrete (Calculator.Add(b, n, bt')) bt') k
                // (a / b) / y = a / (b * y) if unchecked and b and y concrete
                | Div(a, (ConcreteT(bval, _)), _), ConcreteT(yval, _) ->
                    let bMulY = simplifyConcreteMultiplication t bval yval
                    simplifyDivision t a bMulY k
                | _ -> (makeBinary OperationType.Divide x y (fromDotNetType t)) |> k)
            (fun x y k -> simplifyDivision t x y k)


// ------------------------------- Simplification of "%" -------------------------------

    and private simplifyConcreteRemainder t x y =
        let success = ref true
        let result =
            Calculator.Rem(x, y, t, success)
        castConcrete result t

    and private divides t x y =
        let success = ref true
        Calculator.IsZero(Calculator.Rem(x, y, t, success)) && !success

    and simplifyRemainder t x y k =
        simplifyGenericBinary "remainder" x y k
            (simplifyConcreteBinary simplifyConcreteRemainder t)
            (fun x y k ->
                match x, y with
                // 0 % y = 0
                | ConcreteT(xval, _), _ when Calculator.IsZero(xval) -> x |> k
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
                | _ -> makeBinary OperationType.Remainder x y (fromDotNetType t) |> k)
            (fun x y k -> simplifyRemainder t x y k)

// ---------------------------------------- Simplification of "<<", ">>" ----------------------------------------

    and private simplifyConcreteShift operation t x y =
        match operation with
        | OperationType.ShiftLeft -> castConcrete (Calculator.ShiftLeft(x, y, t)) t
        | OperationType.ShiftRight -> castConcrete (Calculator.ShiftRight(x, y, t)) t
        | _ -> __unreachable__()

    and private simplifyShiftLeftMul t a b y matched unmatched =
        // Simplifying (a * b) << y at this step
        match a.term, b.term, y.term with
        // (2^n * b) << y = b << (y + n) if unchecked, y is concrete, y + n < bitSize of a
        // (2^n * b) << y = 0 if unchecked, y is concrete, y + n >= bitSize of a
        |  Concrete(powOf2, _), _, Concrete(yval, yt)
            when Calculator.IsPowOfTwo(powOf2) ->
                let n = Calculator.WhatPowerOf2(powOf2)
                let tooBigShift = Calculator.Compare(Calculator.Add(yval, n, t), bitSizeOf a t) >= 0
                if tooBigShift then castConcrete 0 t |> matched
                else
                    let yt' = toDotNetType yt
                    simplifyShift OperationType.ShiftLeft t b (castConcrete (Calculator.Add(yval, n, yt')) yt') matched
        | _ -> unmatched ()

    and private simplifyShiftRightDiv t a b y matched unmatched =
        // Simplifying (a / b) >> y at this step
        match b.term, y.term with
        // (a / 2^n) >> y = a >> (y + n) if y is concrete, a is unsigned, y + n < bitSize of a
        // (a / 2^n) >> y = 0 if y is concrete, a is unsigned, y + n >= bitSize of a
        |   Concrete(powOf2, _), Concrete(yval, yt)
            when Calculator.IsPowOfTwo(powOf2) && a |> typeOf |> toDotNetType |> isUnsigned ->
                let n = Calculator.WhatPowerOf2(powOf2)
                let tooBigShift = Calculator.Compare(Calculator.Add(yval, n, t), bitSizeOf a t) >= 0
                if tooBigShift then castConcrete 0 t |> matched
                else
                    let yt' = toDotNetType yt
                    simplifyShift OperationType.ShiftRight t a (castConcrete (Calculator.Add(yval, n, yt')) yt') matched
        | _ -> unmatched ()

    and private simplifyShiftLeftOfAddition t a y (matched : term -> 'a) unmatched =
        // Simplifying (a + a) << y at this step
        match y.term with
        // (a + a) << y = 0 if unchecked, y is concrete, y = (size of a) * 8 - 1
        // (a + a) << y = a << (y + 1) if unchecked, y is concrete, y < (size of a) * 8 - 1
        | Concrete(yval, yt) ->
            let tooBigShift = Calculator.Compare(yval, ((Terms.sizeOf a) * 8) - 1) = 0
            if tooBigShift then castConcrete 0 t |> matched
            else
                let yt' = toDotNetType yt
                simplifyShift OperationType.ShiftLeft t a (castConcrete (Calculator.Add(yval, 1, yt')) yt') matched
        | _ -> unmatched ()

    and private simplifyShiftOfShifted op t a b y matched unmatched =
        // Simplifying (a op b) op y at this step
        match b.term, y.term, op with
        // (a op b) op y = a op (b + y) if unchecked, b and y are concrete, b + y < (size of a) * 8
        | Concrete(x, xt), Concrete(c, _), _ when Calculator.Compare(Calculator.Add(x, c, t), bitSizeOf a t) = -1 ->
            let xt' = toDotNetType xt
            simplifyShift op t a (castConcrete (Calculator.Add(x, c, xt')) xt') matched
        // (a op b) op y = 0 if unchecked, b and y are concrete, b + y >= (size of a) * 8
        | Concrete _, Concrete _, OperationType.ShiftLeft ->
            castConcrete 0 t |> matched
        | Concrete _, Concrete _, OperationType.ShiftRight when a |> typeOf |> toDotNetType |> isUnsigned ->
            castConcrete 0 t |> matched
        | _ -> unmatched ()

    and private simplifyShiftOfExpression op t x y matched unmatched =
        match x, op with
        | Mul(a, b, _), OperationType.ShiftLeft -> simplifyShiftLeftMul t a b y matched unmatched
        | Div(a, b, _), OperationType.ShiftRight -> simplifyShiftRightDiv t a b y matched unmatched
        | Add(a, b, _), OperationType.ShiftLeft when a = b -> simplifyShiftLeftOfAddition t a y matched unmatched
        | ShiftLeft(a, b, _), OperationType.ShiftLeft -> simplifyShiftOfShifted op t a b y matched unmatched
        | ShiftRight(a, b, _), OperationType.ShiftRight -> simplifyShiftOfShifted op t a b y matched unmatched
        | _ -> unmatched ()

    and private simplifyShiftExt op t x y matched unmatched =
        match x.term, y.term with
        | Concrete(x, _), _ when Calculator.IsZero(x) -> castConcrete 0 t |> matched
        | _, Concrete(y, _) when Calculator.IsZero(y) -> x |> matched
        | Expression _, Expression _
        | Expression _, _ -> simplifyShiftOfExpression op t x y matched unmatched
        | _ -> unmatched ()

    and private simplifyShift operation (t : System.Type) (x : term) y (k : term -> 'a) =
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
            | BitwiseAnd -> k <| Concrete (Calculator.BitwiseAnd(x, y, t)) resType
            | BitwiseOr -> k <| Concrete (Calculator.BitwiseOr(x, y, t)) resType
            | BitwiseXor -> k <| Concrete (Calculator.BitwiseXor(x, y, t)) resType
            | _ -> __notImplemented__()
        | _ -> k (Expression (Operator op) [x; y] resType)

// ------------------------------- Simplification of "=", "!=", "<", ">", ">=", "<=" -------------------------------

    and fastNumericCompare n m =
        if n = m then True
        elif isConcrete n && isConcrete m then False
        else makeBinary OperationType.Equal n m Bool

    and private simplifyConcreteComparison operator _ x y =
        let bx = box x
        let by = box y
        if (bx :? uint32 list) && (by :? uint32 list) then
            Concrete (List.compareWith compare (bx :?> uint32 list) (by :?> uint32 list) |> operator) Bool
        else
            Concrete (Calculator.Compare(bx, by) |> operator) Bool

    and private simplifyComparison op x y comparator sameIsTrue k =
        simplifyGenericBinary "comparison" x y k
            (simplifyConcreteBinary (simplifyConcreteComparison comparator) Bool)
            (fun x y k ->
                match x, y with
                | _ when x = y -> Concrete sameIsTrue Bool |> k
                | Add((ConcreteT(_, t) as c), x, _), y when x = y ->
                    simplifyComparison op c (castConcrete 0 (toDotNetType t)) comparator sameIsTrue k
                | x, Add((ConcreteT(_, t) as c), y, _) when x = y ->
                    simplifyComparison op (castConcrete 0 (toDotNetType t)) c comparator sameIsTrue k
                | _ -> makeBinary op x y Bool |> k)
            (fun x y k -> simplifyComparison op x y comparator sameIsTrue k)

    and simplifyEqual x y k = simplifyComparison OperationType.Equal x y ((=) 0) true k
    and simplifyNotEqual x y k = simplifyComparison OperationType.Equal x y ((=) 0) true ((!!) >> k)
    and simplifyLess x y k = simplifyComparison OperationType.Less x y ((>) 0) false k
    and simplifyLessOrEqual x y k = simplifyComparison OperationType.LessOrEqual x y ((>=) 0) true k
    and simplifyGreater x y k = simplifyComparison OperationType.LessOrEqual x y ((>=) 0) true ((!!) >> k)
    and simplifyGreaterOrEqual x y k = simplifyComparison OperationType.Less x y ((>) 0) false ((!!) >> k)

// ------------------------------- General functions -------------------------------

    let private getDotNetType = typeOf >> toDotNetType
    let inline private deduceArithmeticTargetType x y =
        TypeUtils.deduceSimpleArithmeticOperationTargetType (getDotNetType x) (getDotNetType y)

    let add x y =
        simplifyAddition (deduceArithmeticTargetType x y) x y id

    let sub x y =
        simplifySubtraction (deduceArithmeticTargetType x y) x y id

    let neg x =
        simplifyUnaryMinus (getDotNetType x) x id

    let mul x y =
        simplifyMultiplication (deduceArithmeticTargetType x y) x y id

    let div x y =
        simplifyDivision (deduceArithmeticTargetType x y) x y id

    let rem x y =
        simplifyRemainder (deduceArithmeticTargetType x y) x y id

    let eq x y =
        simplifyEqual x y id

    let simplifyBinaryOperation op x y k =
        let getDotNetType = typeOf >> toDotNetType
        let t = Operations.deduceArithmeticBinaryExpressionTargetType op (getDotNetType x) (getDotNetType y)
        match op with
        | OperationType.Add -> simplifyAddition t x y k
        | OperationType.Subtract -> simplifySubtraction t x y k
        | OperationType.Multiply -> simplifyMultiplication t x y k
        | OperationType.Divide -> simplifyDivision t x y k
        | OperationType.Remainder -> simplifyRemainder t x y k
        | OperationType.ShiftLeft-> simplifyShift op t x y k
        | OperationType.ShiftRight -> simplifyShift op t x y k
        | OperationType.Equal -> simplifyEqual x y k
        | OperationType.NotEqual -> simplifyNotEqual x y k
        | OperationType.Greater -> simplifyGreater x y k
        | OperationType.GreaterOrEqual -> simplifyGreaterOrEqual x y k
        | OperationType.Less -> simplifyLess x y k
        | OperationType.LessOrEqual -> simplifyLessOrEqual x y k
        | OperationType.BitwiseAnd
        | OperationType.BitwiseOr
        | OperationType.BitwiseXor -> simplifyBitwise op x y t (typeOf x) k
        | _ -> internalfailf "%O is not a binary arithmetical operator" op

    let simplifyUnaryOperation op x t k =
        match op with
        | OperationType.BitwiseNot -> simplifyBinaryNot t x k
        | OperationType.UnaryMinus -> simplifyUnaryMinus t x k
        | _ -> internalfailf "%O is not an unary arithmetical operator" op

    let isArithmeticalOperation op t1 t2 =
        (Types.isNumeric t1 || t1 = AddressType) && (Types.isNumeric t2 || t2 = AddressType) &&
        match op with
        | OperationType.Add
        | OperationType.Subtract
        | OperationType.Multiply
        | OperationType.Divide
        | OperationType.Remainder
        | OperationType.ShiftLeft
        | OperationType.ShiftRight
        | OperationType.Equal
        | OperationType.NotEqual
        | OperationType.Greater
        | OperationType.GreaterOrEqual
        | OperationType.Less
        | OperationType.LessOrEqual
        | OperationType.BitwiseAnd
        | OperationType.BitwiseOr
        | OperationType.BitwiseXor
        | OperationType.BitwiseNot
        | OperationType.UnaryMinus -> true
        | _ -> false

    let checkEqualZero y k =
        simplifyEqual y (castConcrete 0 (toDotNetType(typeOf y))) k
