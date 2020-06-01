namespace VSharp.Core

open VSharp
open VSharp.TypeUtils
open VSharp.CSharpUtils
open VSharp.Core.Common
open VSharp.Core.Types
open VSharp.Core.Types.Constructor

[<AutoOpen>]
module internal Arithmetics =

    let private makeAddition mtd t x y k =
        makeBinary OperationType.Add x y (fromDotNetType t) mtd |> k

    let private makeProduct mtd t x y k =
        makeBinary OperationType.Multiply x y (fromDotNetType t) mtd |> k

    let private makeShift mtd op t x y k =
        makeBinary op x y (fromDotNetType t) mtd |> k


// ------------------------------- Simplification of "+" -------------------------------

    let private simplifyConcreteAddition mtd t x y =
//        if isChecked then
//            let success = ref true
//            let result = Calculator.AddChecked(x, y, t, success)
//            if !success then CastConcrete isChecked result t mtd
//            else overflow mtd
//        else
        CastConcrete (Calculator.Add(x, y, t)) t mtd

    let rec private simplifyAdditionToSum mtd t xmtd a b y matched unmatched =
        // Simplifying (a + b) + y at this step
        match a.term, b.term, y.term with
        // (a + b) + y = (a + y) + b if a and y concrete and unchecked
        | Concrete(aval, _), _, Concrete(yval, _) ->
            let x = simplifyConcreteAddition (Metadata.combine3 mtd a.metadata y.metadata) t aval yval
            simplifyAddition mtd t x b matched
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
            let summandsOfY, mtd' =
                match y with
                | Add(c, d, _) -> [c; d], Metadata.combine3 mtd xmtd y.metadata
                | _ -> [y], Metadata.combine mtd xmtd
            simplifyPairwiseCombinations
                [a; b]
                summandsOfY
                t
                id
                (simplifyAdditionExt mtd')
                (simplifyAddition mtd' t)
                matched
                unmatched

    and private simplifyAdditionToUnaryMinus mtd t xmtd x y matched unmatched =
        // Simplifying (-x) + y at this step
        match x with
        // (-y) + y = 0
        | _ when x = y -> (CastConcrete 0 t (Metadata.combine3 mtd xmtd y.metadata)) |> matched
        // -(y + b) = -b
        | Add(a, b, _) when a = y -> matched b
        // -(a + y) = -a
        | Add(a, b, _) when b = y -> matched a
        | _ -> unmatched ()

    and private simplifyAdditionToProduct mtd t xmtd a b y matched unmatched =
        // Simplifying (a * b) + y at this step
        match a.term, y with
        // (a * y) + y = (a + 1) * y if unckecked and a is concrete
        | Concrete(aval, atyp), _ when b = y ->
            let mtd' = Metadata.combine3 mtd a.metadata y.metadata
            let aPlusOne = simplifyConcreteAddition mtd' (Types.toDotNetType atyp) aval 1
            simplifyMultiplication xmtd t aPlusOne y matched
        // (a * b) + (c * b) = (a + c) * b if unchecked and a and c are concrete
        | Concrete(aval, _), Mul(ConcreteT(cval, _) as c, d, _) when d = b ->
            let mtd' = Metadata.combine3 mtd a.metadata c.metadata
            let aPlusC = simplifyConcreteAddition mtd' t aval cval
            simplifyMultiplication xmtd t aPlusC b matched
        | _ -> unmatched ()

    and private simplifyAdditionToShift mtd t xmtd a b y matched unmatched =
        // Simplifying (a << b) + y at this step
        match b.term, y with
        // (a << b) + (a << b) = 0            if unchecked, b = (size of a) * 8 - 1
        // (a << b) + (a << b) = a << (b + 1) if unchecked, b < (size of a) * 8 - 1
        | Concrete(x, _), ShiftLeft(c, ConcreteT(d, _), _) when a = c && x = d ->
            let tooBigShift = Calculator.Compare(x, ((Terms.sizeOf a) * 8) - 1) = 0
            if tooBigShift then
                let mtd' = Metadata.combine3 mtd xmtd y.metadata
                CastConcrete 0 t mtd' |> matched
            else
                let mtd' = Metadata.combine3 mtd b.metadata y.metadata
                simplifyShift xmtd OperationType.ShiftLeft t a (CastConcrete (Calculator.Add(x, 1, t)) t mtd') matched
        | _ -> unmatched ()

    and private simplifyAdditionToExpression mtd x y t matched unmatched =
        let xmtd = x.metadata
        match x with
        | Add(a, b, _) -> simplifyAdditionToSum mtd t xmtd a b y matched unmatched
        | UnaryMinusT(x, _) -> simplifyAdditionToUnaryMinus mtd t xmtd x y matched unmatched
        | Mul(a, b, _) -> simplifyAdditionToProduct mtd t xmtd a b y matched unmatched
        | ShiftLeft(a, b, _) -> simplifyAdditionToShift mtd t xmtd a b y matched unmatched
        | _ -> unmatched ()

    and private simplifyAdditionExt mtd t x y matched unmatched =
        match x.term, y.term with
        | Concrete(xval, _), Concrete(yval, _) ->
            let mtd' = Metadata.combine3 mtd x.metadata y.metadata
            simplifyConcreteAddition mtd' t xval yval |> matched
        | Concrete(xval, _), _ when Calculator.IsZero(xval) -> matched y
        | _, Concrete(yval, _) when Calculator.IsZero(yval) -> matched x
        | Expression _, Expression _ ->
            simplifyAdditionToExpression mtd x y t matched (fun () ->
            simplifyAdditionToExpression mtd y x t matched unmatched)
        | Expression _, _ -> simplifyAdditionToExpression mtd x y t matched unmatched
        | _, Expression _ -> simplifyAdditionToExpression mtd y x t matched unmatched
        | _ -> unmatched ()

    and private simplifyAddition mtd (t : System.Type) x y k =
        let defaultCase () =
            let sorted = if isConcrete y then (y, x) else (x, y)
            makeAddition mtd t (fst sorted) (snd sorted) k
        simplifyGenericBinary "addition" x y k
                              (simplifyConcreteBinary simplifyConcreteAddition mtd t)
                              (fun x y k -> simplifyAdditionExt mtd t x y k defaultCase)
                              (fun x y k -> simplifyAddition mtd t x y k)

    and private simplifySubtraction mtd t x y k =
        simplifyUnaryMinus mtd t y (fun minusY ->
        simplifyAddition mtd t x minusY k)

// ------------------------------- Simplification of unary "-" -------------------------------

    and private simplifyConcreteUnaryMinus mtd t x =
//        if isChecked then
//            let success = ref true
//            let result = Calculator.UnaryMinusChecked(x, t, success)
//            if !success then CastConcrete isChecked result t mtd
//            else overflow mtd
//        else
        CastConcrete (Calculator.UnaryMinus(x, t)) t mtd

    and private simplifyUnaryMinus mtd (t : System.Type) x k =
        let simplifyConcrete x obj _ =
            simplifyConcreteUnaryMinus (Metadata.combine mtd x.metadata) t obj
        simplifyGenericUnary "unary minus" x k simplifyConcrete (fun x k ->
        match x with
        // -(-(x)) = x if both unchecked
        | UnaryMinusT(x, _) -> k x
        // -(a + b) = (-a) + (-b) if all unchecked
        | Add(a, b, _) ->
            simplifyUnaryMinus mtd t a (fun minusA ->
            simplifyUnaryMinus mtd t b (fun minusB ->
            simplifyAddition x.metadata t minusA minusB k))
         // -(a * x) = (-a) * x if both unchecked
        | Mul(ConcreteT(_, at) as a, y, _) ->
            simplifyUnaryMinus mtd (Types.toDotNetType at) a (fun minusA ->
            simplifyMultiplication x.metadata t minusA y k)
        | _ -> k (makeUnary OperationType.UnaryMinus x (fromDotNetType t) mtd)
        )

// ------------------------------- Simplification of "*" -------------------------------

    and private simplifyConcreteMultiplication mtd t x y =
//        if isChecked then
//            let success = ref true
//            let result = Calculator.MulChecked(x, y, t, success)
//            if !success then CastConcrete isChecked result t mtd
//            else overflow mtd
//        else
        CastConcrete (Calculator.Mul(x, y, t)) t mtd

    and private simplifyMultiplicationOfProduct mtd t xmtd a b y matched unmatched =
        // Simplifying (a * b) * y at this step
        match a, b, y with
        // (a * b) * y = (a * y) * b if a and y concrete and unchecked
        | ConcreteT(aval, _), _, ConcreteT(yval, _) ->
            let mtd' = Metadata.combine3 mtd a.metadata y.metadata
            let x = simplifyConcreteMultiplication mtd' t aval yval
            simplifyMultiplication xmtd t x b matched
        // ((a / y) * b) * y = a * b if unchecked
        | Div(a, c, _), b, _ when c = y -> simplifyMultiplication xmtd t a b matched
        // (a * (b / y)) * y = a * b if unchecked
        | a, Div(b, c, _), _ when c = y -> simplifyMultiplication xmtd t a b matched
        // (a * b) * (c / a) = b * c if unchecked
        | _, _, Div(c, d, _) when d = a -> simplifyMultiplication mtd t b c matched
        // (a * b) * (c / b) = a * c if unchecked
        | _, _, Div(c, d, _) when d = b -> simplifyMultiplication mtd t a c matched
        | _ ->
            // Trying to simplify pairwise combinations of x- and y-factors
            let factorsOfY, mtd' =
                match y with
                | Mul(c, d, _) -> [c; d], Metadata.combine3 mtd xmtd y.metadata
                | _ -> [y], Metadata.combine mtd xmtd
            simplifyPairwiseCombinations
                [a; b]
                factorsOfY
                t
                id
                (simplifyMultiplicationExt mtd' )
                (simplifyMultiplication mtd' t)
                matched
                unmatched

    and private simplifyMultiplicationOfDivision mtd t xmtd a b y matched unmatched =
        // Simplifying (a / b) * y at this step
        match a, b, y with
        // (a / b) * y = (a * y) / b if a and y are concrete and unchecked
        | ConcreteT(aval, _), b, ConcreteT(yval, _) ->
            let mtd' = Metadata.combine3 mtd a.metadata y.metadata
            let aMulY = simplifyConcreteMultiplication mtd' t aval yval
            simplifyDivision xmtd t aMulY b matched
        // (a / (y * d)) * y = a/d if unchecked
        | _, Mul(c, d, _), _ when c = y -> simplifyDivision xmtd t a d matched
        // (a / (c * y)) * y = a/c if unchecked
        | _, Mul(c, d, _), _ when d = y -> simplifyDivision xmtd t a c matched
        | _ -> unmatched ()

    and private simplifyMultiplicationOfShifts mtd (t : System.Type) xmtd a b y matched unmatched =
        // Simplifying (a << b) * y at this step
        match b.term, y with
        // (a << b) * (c << d) = (a * c) << (b + d) if unchecked, b and d are conctere, b + d < (size of a) * 8
        // (a << b) * (c << d) = 0 if unchecked, b and d are conctere, b + d >= (size of a) * 8
        | Concrete(bval, _), ShiftLeft(c, (ConcreteT(dval, _) as d), _) ->
            let smallShift = Calculator.Compare(Calculator.Add(bval, dval, t), bitSizeOf a t) = -1
            let mtd' = Metadata.combine3 mtd b.metadata d.metadata
            if smallShift then
                simplifyMultiplication mtd t a c (fun mul ->
                let bPlusD = CastConcrete (Calculator.Add(bval, dval, t)) t mtd'
                simplifyShift xmtd OperationType.ShiftLeft t mul bPlusD matched)
            else
                CastConcrete 0 t mtd |> matched
        // (a << b) * 2^n = a << (b + n) if unchecked, b is concrete, b + n < (size of a) * 8
        // (a << b) * 2^n = 0 if unchecked, b is concrete, b + n >= (size of a) * 8
        | Concrete(x, _), ConcreteT(powOf2, _) when Calculator.IsPowOfTwo(powOf2) ->
            let n = Calculator.WhatPowerOf2(powOf2)
            let tooBigShift = Calculator.Compare(Calculator.Add(x, n, t), bitSizeOf a t) >= 0
            if tooBigShift then CastConcrete 0 t (Metadata.combine3 mtd xmtd y.metadata) |> matched
            else
                let mtd' = Metadata.combine3 mtd b.metadata y.metadata
                simplifyShift xmtd OperationType.ShiftLeft t a (CastConcrete (Calculator.Add(x, n, t)) t mtd') matched
        | _ -> unmatched ()

    and private simplifyMultiplicationOfExpression mtd t x y matched unmatched =
        match x with
        | Mul(a, b, _) -> simplifyMultiplicationOfProduct mtd t x.metadata a b y matched unmatched
        | Div(a, b, _) -> simplifyMultiplicationOfDivision mtd t x.metadata a b y matched unmatched
        | ShiftLeft(a, b, _) -> simplifyMultiplicationOfShifts mtd t x.metadata a b y matched unmatched
        | _ -> unmatched ()

    and private simplifyMultiplicationExt mtd (t : System.Type) (x : term) y matched unmatched =
        match x.term, y.term with
        | Concrete(xval, _), _ when Calculator.IsZero(xval) -> CastConcrete 0 t x.metadata |> matched
        | _, Concrete(yval, _) when Calculator.IsZero(yval) -> CastConcrete 0 t y.metadata |> matched
        | Concrete(x, _), _ when Calculator.FuzzyEqual(x, System.Convert.ChangeType(1, t)) -> matched y
        | _, Concrete(y, _) when Calculator.FuzzyEqual(y, System.Convert.ChangeType(1, t)) -> matched x
        | Concrete(x, _), _ when not <| isUnsigned t && Calculator.FuzzyEqual(x, System.Convert.ChangeType(-1, t)) ->
            simplifyUnaryMinus mtd t y matched
        | _, Concrete(y, _) when not <| isUnsigned t && Calculator.FuzzyEqual(y, System.Convert.ChangeType(-1, t)) ->
            simplifyUnaryMinus mtd t x matched
        | Expression _, Expression _ ->
            simplifyMultiplicationOfExpression mtd t x y matched (fun () ->
            simplifyMultiplicationOfExpression mtd t y x matched unmatched)
        | Expression _, _ -> simplifyMultiplicationOfExpression mtd t x y matched unmatched
        | _, Expression _ -> simplifyMultiplicationOfExpression mtd t y x matched unmatched
        | _ -> unmatched ()

    and private simplifyMultiplication mtd t (x : term) y k =
        let defaultCase () =
            let sorted = if (isConcrete y) then (y, x) else (x, y)
            makeProduct mtd t (fst sorted) (snd sorted) k
        simplifyGenericBinary "product" x y k
                              (simplifyConcreteBinary simplifyConcreteMultiplication mtd t)
                              (fun x y k -> simplifyMultiplicationExt mtd t x y k defaultCase)
                              (fun x y k -> simplifyMultiplication mtd t x y k)

// ------------------------------- Simplification of "/" -------------------------------

    and simplifyConcreteDivision mtd t x y =
        let success = ref true
        let result = Calculator.Div(x, y, t, success)
        CastConcrete result t mtd


    and private simplifyDivision (mtd : termMetadata) t x y k =
        simplifyGenericBinary "division" x y k
            (simplifyConcreteBinary simplifyConcreteDivision mtd t)
            (fun x y k ->
                match x, y with
                // 0 / y = 0
                | ConcreteT(xval, _), _ when Calculator.IsZero(xval) -> x |> k
                // x / 1 = x
                | _, ConcreteT(yval, _) when Calculator.FuzzyEqual(yval, System.Convert.ChangeType(1, typeOf x |> toDotNetType)) -> x |> k
                // x / -1 = -x
                | _, ConcreteT(yval, _) when not <| isUnsigned t && Calculator.FuzzyEqual(yval, System.Convert.ChangeType(-1, typeOf x |> toDotNetType)) ->
                    simplifyUnaryMinus mtd t x k
                // x / x = 1 if unchecked
                | x, y when x = y -> CastConcrete 1 t mtd |> k
                // x / -x = -1 if unchecked
                | x, UnaryMinusT(y, _) when not <| isUnsigned t && x = y -> CastConcrete -1 t mtd |> k
                // (a >> b) / 2^n = a >> (b + n) if unchecked, b is concrete, b + n < (size of a) * 8
                // (a >> b) / 2^n = 0 if unchecked, b is concrete, b + n >= (size of a) * 8
                | ShiftRight(a, ConcreteT(b, _), _), ConcreteT(powOf2, _)
                    when Calculator.IsPowOfTwo(powOf2) && a |> typeOf |> toDotNetType |> isUnsigned ->
                        let n = Calculator.WhatPowerOf2(powOf2)
                        let tooBigShift = Calculator.Compare(Calculator.Add(b, n, t), bitSizeOf a t) >= 0
                        if tooBigShift then CastConcrete 0 t mtd |> k
                        else
                            simplifyShift x.metadata OperationType.ShiftRight t a (CastConcrete (Calculator.Add(b, n, t)) t mtd) k
                // (a / b) / y = a / (b * y) if unchecked and b and y concrete
                | Div(a, (ConcreteT(bval, _) as b), _), ConcreteT(yval, _) ->
                    let bMulY = simplifyConcreteMultiplication (Metadata.combine3 mtd b.metadata y.metadata) t bval yval
                    simplifyDivision x.metadata t a bMulY k
                | _ -> (makeBinary OperationType.Divide x y (fromDotNetType t) mtd) |> k)
            (fun x y k -> simplifyDivision mtd t x y k)



// ------------------------------- Simplification of "%" -------------------------------

    and private simplifyConcreteRemainder mtd t x y =
        let success = ref true
        let result =
//            if isChecked then
//                Calculator.RemChecked(x, y, t, success)
//            else
            Calculator.Rem(x, y, t, success)
        CastConcrete result t mtd
//        if !success then CastConcrete isChecked result t mtd
//        else
//            match result with
////            | :? System.DivideByZeroException -> divideByZero mtd
////            | :? System.OverflowException -> overflow mtd
//            | _ -> __notImplemented__()


    and private divides t x y =
        let success = ref true
        Calculator.IsZero(Calculator.Rem(x, y, t, success)) && !success

    and simplifyRemainder mtd t x y k =
        simplifyGenericBinary "remainder" x y k
            (simplifyConcreteBinary simplifyConcreteRemainder mtd t)
            (fun x y k ->
                match x, y with
                // 0 % y = 0
                | ConcreteT(xval, _), _ when Calculator.IsZero(xval) -> x |> k
                // x % 1 = 0
                | _, ConcreteT(y, _) when Calculator.FuzzyEqual(y, System.Convert.ChangeType(1, t)) -> CastConcrete 0 t mtd |> k
                // x % -1 = 0
                | _, ConcreteT(y, _) when not <| isUnsigned t && Calculator.FuzzyEqual(y, System.Convert.ChangeType(-1, t)) ->
                    CastConcrete 0 t mtd |> k
                // x % x = 0
                | x, y when x = y -> CastConcrete 0 t mtd |> k
                // x % -x = 0 if unchecked
                | x, UnaryMinusT(y, _) when x = y -> CastConcrete 0 t mtd |> k
                // (a * b) % y = 0 if unchecked, b and y concrete and a % y = 0
                | Mul(ConcreteT(a, _), _, _), ConcreteT(y, _) when divides t a y ->
                     CastConcrete 0 t mtd |> k
                | _ -> makeBinary OperationType.Remainder x y (fromDotNetType t) mtd |> k)
            (fun x y k -> simplifyRemainder mtd t x y k)

// ---------------------------------------- Simplification of "<<", ">>" ----------------------------------------

    and private simplifyConcreteShift operation mtd t x y =
        match operation with
        | OperationType.ShiftLeft -> CastConcrete (Calculator.ShiftLeft(x, y, t)) t mtd
        | OperationType.ShiftRight -> CastConcrete (Calculator.ShiftRight(x, y, t)) t mtd
        | _ -> __unreachable__()

    and private simplifyShiftLeftMul mtd t xmtd a b y matched unmatched =
        // Simplifying (a * b) << y at this step
        match a.term, b.term, y.term with
        // (2^n * b) << y = b << (y + n) if unchecked, y is concrete, y + n < bitSize of a
        // (2^n * b) << y = 0 if unchecked, y is concrete, y + n >= bitSize of a
        |  Concrete(powOf2, _), _, Concrete(yval, _)
            when Calculator.IsPowOfTwo(powOf2) ->
                let n = Calculator.WhatPowerOf2(powOf2)
                let tooBigShift = Calculator.Compare(Calculator.Add(yval, n, t), bitSizeOf a t) >= 0
                if tooBigShift then CastConcrete 0 t mtd |> matched
                else
                    let mtd' = Metadata.combine3 xmtd a.metadata y.metadata
                    simplifyShift mtd OperationType.ShiftLeft t b (CastConcrete (Calculator.Add(yval, n, t)) t mtd') matched
        | _ -> unmatched ()

    and private simplifyShiftRightDiv mtd t xmtd a b y matched unmatched =
        // Simplifying (a / b) >> y at this step
        match b.term, y.term with
        // (a / 2^n) >> y = a >> (y + n) if y is concrete, a is unsigned, y + n < bitSize of a
        // (a / 2^n) >> y = 0 if y is concrete, a is unsigned, y + n >= bitSize of a
        |   Concrete(powOf2, _), Concrete(yval, _)
            when Calculator.IsPowOfTwo(powOf2) && a |> typeOf |> toDotNetType |> isUnsigned ->
                let n = Calculator.WhatPowerOf2(powOf2)
                let tooBigShift = Calculator.Compare(Calculator.Add(yval, n, t), bitSizeOf a t) >= 0
                if tooBigShift then CastConcrete 0 t mtd |> matched
                else
                    let mtd' = Metadata.combine3 xmtd b.metadata y.metadata
                    simplifyShift mtd OperationType.ShiftRight t a (CastConcrete (Calculator.Add(yval, n, t)) t mtd') matched
        | _ -> unmatched ()

    and private simplifyShiftLeftOfAddition mtd t xmtd a b y (matched : term -> 'a) unmatched =
        // Simplifying (a + a) << y at this step
        match y.term with
        // (a + a) << y = 0 if unchecked, y is concrete, y = (size of a) * 8 - 1
        // (a + a) << y = a << (y + 1) if unchecked, y is concrete, y < (size of a) * 8 - 1
        | Concrete(c, _) ->
            let tooBigShift = Calculator.Compare(c, ((Terms.sizeOf a) * 8) - 1) = 0
            if tooBigShift then CastConcrete 0 t mtd |> matched
            else
                let mtd' = Metadata.combine xmtd b.metadata
                simplifyShift mtd OperationType.ShiftLeft t a (CastConcrete (Calculator.Add(c, 1, t)) t mtd') matched
        | _ -> unmatched ()

    and private simplifyShiftOfShifted mtd op t xmtd a b y matched unmatched =
        // Simplifying (a op b) op y at this step
        match b.term, y.term, op with
        // (a op b) op y = a op (b + y) if unchecked, b and y are concrete, b + y < (size of a) * 8
        | Concrete(x, _), Concrete(c, _), _ when Calculator.Compare(Calculator.Add(x, c, t), bitSizeOf a t) = -1 ->
            let mtd' = Metadata.combine3 mtd b.metadata y.metadata
            simplifyShift xmtd op t a (CastConcrete (Calculator.Add(x, c, t)) t mtd') matched
        // (a op b) op y = 0 if unchecked, b and y are concrete, b + y >= (size of a) * 8
        | Concrete(_, _), Concrete(_, _), OperationType.ShiftLeft ->
            CastConcrete 0 t mtd |> matched
        | Concrete(_, _), Concrete(_, _), OperationType.ShiftRight when a |> typeOf |> toDotNetType |> isUnsigned ->
            CastConcrete 0 t mtd |> matched
        | _ -> unmatched ()

    and private simplifyShiftOfExpression mtd op t x y matched unmatched =
        let xmtd = x.metadata
        match x, op with
        | Mul(a, b, _), OperationType.ShiftLeft -> simplifyShiftLeftMul mtd t xmtd a b y matched unmatched
        | Div(a, b, _), OperationType.ShiftRight -> simplifyShiftRightDiv mtd t xmtd a b y matched unmatched
        | Add(a, b, _), OperationType.ShiftLeft when a = b -> simplifyShiftLeftOfAddition mtd t xmtd a b y matched unmatched
        | ShiftLeft(a, b, _), OperationType.ShiftLeft -> simplifyShiftOfShifted mtd op t xmtd a b y matched unmatched
        | ShiftRight(a, b, _), OperationType.ShiftRight -> simplifyShiftOfShifted mtd op t xmtd a b y matched unmatched
        | _ -> unmatched ()

    and private simplifyShiftExt mtd op t x y matched unmatched =
        match x.term, y.term with
        | Concrete(x, _), _ when Calculator.IsZero(x) -> CastConcrete 0 t mtd |> matched
        | _, Concrete(y, _) when Calculator.IsZero(y) -> x |> matched
        | Expression _, Expression _
        | Expression _, _ -> simplifyShiftOfExpression mtd op t x y matched unmatched
        | _ -> unmatched ()

    and private simplifyShift mtd operation (t : System.Type) (x : term) y (k : term -> 'a) =
        let defaultCase () =
            makeShift mtd operation t x y k
        simplifyGenericBinary "shift" x y k
                                (simplifyConcreteBinary (simplifyConcreteShift operation) mtd t)
                                (fun x y k -> simplifyShiftExt mtd operation t x y k defaultCase)
                                (fun x y k -> simplifyShift mtd operation t x y k)

// TODO: IMPLEMENT BITWISE OPERATIONS!
    and private simplifyBitwise mtd (op : OperationType) x y t resType k =
        match x.term, y.term with
        | Concrete(x, _), Concrete(y, _) ->
            match op with
            | OperationType.LogicalAnd -> k <| Concrete mtd (Calculator.BitwiseAnd(x, y, t)) resType
            | OperationType.LogicalOr -> k <| Concrete mtd (Calculator.BitwiseOr(x, y, t)) resType
            | OperationType.LogicalXor -> k <| Concrete mtd (Calculator.BitwiseXor(x, y, t)) resType
            | _ -> __notImplemented__()
        | _ -> k (Expression mtd (Operator op) [x; y] resType)

// ------------------------------- Simplification of "=", "!=", "<", ">", ">=", "<=" -------------------------------

    and fastNumericCompare mtd n m =
        if n = m then makeTrue mtd
        elif isConcrete n && isConcrete m then makeFalse mtd
        else makeBinary OperationType.Equal n m Bool mtd

    and private simplifyConcreteComparison operator mtd _ x y =
        let bx = box x
        let by = box y
        if (bx :? int list) && (by :? int list) then
            Concrete mtd (List.compareWith compare (bx :?> int list) (by :?> int list) |> operator) Bool
        else
            Concrete mtd (Calculator.Compare(bx, by) |> operator) Bool

    and private simplifyComparison mtd op x y comparator sameIsTrue k =
        simplifyGenericBinary "comparison" x y k
            (simplifyConcreteBinary (simplifyConcreteComparison comparator) mtd Bool)
            (fun x y k ->
                match x, y with
                | _ when x = y -> Concrete mtd sameIsTrue Bool |> k
                | Add((ConcreteT(_, t) as c), x, _), y when x = y ->
                    simplifyComparison mtd op c (CastConcrete 0 (toDotNetType t) mtd) comparator sameIsTrue k
                | x, Add((ConcreteT(_, t) as c), y, _) when x = y ->
                    simplifyComparison mtd op (CastConcrete 0 (toDotNetType t) mtd) c comparator sameIsTrue k
                | _ -> makeBinary op x y Bool mtd |> k)
            (fun x y k -> simplifyComparison mtd op x y comparator sameIsTrue k)

    and simplifyEqual mtd x y k = simplifyComparison mtd OperationType.Equal x y ((=) 0) true k
    and simplifyNotEqual mtd x y k = simplifyComparison mtd OperationType.Equal x y ((=) 0) true ((!!) >> k)
    and simplifyLess mtd x y k = simplifyComparison mtd OperationType.Less x y ((>) 0) false k
    and simplifyLessOrEqual mtd x y k = simplifyComparison mtd OperationType.LessOrEqual x y ((>=) 0) true k
    and simplifyGreater mtd x y k = simplifyComparison mtd OperationType.LessOrEqual x y ((>=) 0) true ((!!) >> k)
    and simplifyGreaterOrEqual mtd x y k = simplifyComparison mtd OperationType.Less x y ((>) 0) false ((!!) >> k)

// ------------------------------- General functions -------------------------------

    let private getDotNetType = typeOf >> toDotNetType
    let inline private deduceArithmeticTargetType x y =
        TypeUtils.deduceSimpleArithmeticOperationTargetType (getDotNetType x) (getDotNetType y)

    let add mtd x y =
        simplifyAddition mtd (deduceArithmeticTargetType x y) x y id

    let sub mtd x y =
        simplifySubtraction mtd (deduceArithmeticTargetType x y) x y id

    let neg mtd x =
        simplifyUnaryMinus mtd (getDotNetType x) x id

    let mul mtd x y =
        simplifyMultiplication mtd (deduceArithmeticTargetType x y) x y id

    let div mtd x y =
        simplifyDivision mtd (deduceArithmeticTargetType x y) x y id

    let rem mtd x y =
        simplifyRemainder mtd (deduceArithmeticTargetType x y) x y id

    let eq mtd x y =
        simplifyEqual mtd x y id

    let simplifyBinaryOperation metadata op x y k =
        let getDotNetType = typeOf >> toDotNetType
        let t = Operations.deduceArithmeticBinaryExpressionTargetType op (getDotNetType x) (getDotNetType y)
        match op with
        | OperationType.Add -> simplifyAddition metadata t x y k
        | OperationType.Subtract -> simplifySubtraction metadata t x y k
        | OperationType.Multiply -> simplifyMultiplication metadata t x y k
        | OperationType.Divide -> simplifyDivision metadata t x y k
        | OperationType.Remainder -> simplifyRemainder metadata t x y k
        | OperationType.ShiftLeft-> simplifyShift metadata op t x y k
        | OperationType.ShiftRight -> simplifyShift metadata op t x y k
        | OperationType.Equal -> simplifyEqual metadata x y k
        | OperationType.NotEqual -> simplifyNotEqual metadata x y k
        | OperationType.Greater -> simplifyGreater metadata x y k
        | OperationType.GreaterOrEqual -> simplifyGreaterOrEqual metadata x y k
        | OperationType.Less -> simplifyLess metadata x y k
        | OperationType.LessOrEqual -> simplifyLessOrEqual metadata x y k
        | OperationType.LogicalAnd
        | OperationType.LogicalOr
        | OperationType.LogicalXor -> simplifyBitwise metadata op x y t (typeOf x) k
        | _ -> internalfailf "%O is not a binary arithmetical operator" op

    let simplifyUnaryOperation metadata op x t k =
        match op with
        | OperationType.LogicalNeg -> __notImplemented__()
        | OperationType.UnaryMinus -> simplifyUnaryMinus metadata t x k
        | _ -> internalfailf "%O is not an unary arithmetical operator" op

    let isArithmeticalOperation op t1 t2 =
        Types.isNumeric t1 && Types.isNumeric t2 &&
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
        | OperationType.LogicalAnd
        | OperationType.LogicalOr
        | OperationType.LogicalXor
        | OperationType.LogicalNeg
        | OperationType.UnaryMinus -> true
        | _ -> false

    let checkEqualZero mtd y k =
        simplifyEqual mtd y (CastConcrete 0 (toDotNetType(typeOf y)) mtd) k
