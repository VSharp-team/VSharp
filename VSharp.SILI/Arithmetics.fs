namespace VSharp

open JetBrains.Decompiler.Ast
open VSharp.CSharpUtils
open VSharp.Common
open VSharp.Terms
open VSharp.Types
open Types.Constructor

[<AutoOpen>]
module internal Arithmetics =

    let private makeAddition mtd isChecked state t x y k =
        (MakeBinary OperationType.Add x y isChecked (FromConcreteDotNetType t) mtd, state) |> k

    let private makeProduct mtd isChecked state t x y k =
        (MakeBinary OperationType.Multiply x y isChecked (FromConcreteDotNetType t) mtd, state) |> k

    let private makeShift mtd op state isChecked t x y k =
        (MakeBinary op x y isChecked (FromConcreteDotNetType t) mtd, state) |> k

// ------------------------------- Simplification of "+" -------------------------------

    let private simplifyConcreteAddition mtd isChecked state t x y =
        if isChecked then
            let success = ref true
            let result = Calculator.AddChecked(x, y, t, success)
            if !success then CastConcrete result t mtd, state
            else
                let t, s = State.activator.CreateInstance mtd typeof<System.OverflowException> [] state in
                Error t mtd, s
        else
            CastConcrete (Calculator.Add(x, y, t)) t mtd, state

    let rec private simplifyAdditionToSum mtd state t xmtd a b y matched unmatched =
        // Simplifying (a + b) + y at this step
        match a.term, b.term, y.term with
        // (a + b) + y = (a + y) + b if a and y concrete and unchecked
        | Concrete(aval, at), _, Concrete(yval, yt) ->
            let x, state = simplifyConcreteAddition (Metadata.combine3 mtd a.metadata y.metadata) false state t aval yval in
            simplifyAddition mtd false state t x b matched
        // ((-y) + b) + y = b if unchecked
        | UnaryMinus(a, false, _), _, _ when a = y -> matched (b, state)
        // (a + (-y)) + y = a if unchecked
        | _, UnaryMinus(b, false, _), _ when b = y -> matched (a, state)
        // (a + b) + (-a) = b if unchecked
        | _, _, UnaryMinus(y, false, _) when a = y -> matched (b, state)
        // (a + b) + (-b) = a if unchecked
        | _, _, UnaryMinus(y, false, _) when b = y -> matched (a, state)
        | _ ->
            // Trying to simplify pairwise combinations of x- and y-summands
            let summandsOfY, mtd' =
                match y with
                | Add(c, d, false, yt) -> [(c, state); (d, state)], Metadata.combine3 mtd xmtd y.metadata
                | _ -> [(y, state)], Metadata.combine mtd xmtd
            in
            simplifyPairwiseCombinations
                [(a, state); (b, state)]
                summandsOfY
                t
                fst
                (simplifyAdditionExt mtd' false state)
                (simplifyAddition mtd' false state t)
                (withSnd state >> matched)
                unmatched

    and private simplifyAdditionToUnaryMinus mtd state t xmtd x y matched unmatched =
        // Simplifying (-x) + y at this step
        match x with
        // (-y) + y = 0
        | _ when x = y -> (CastConcrete 0 t (Metadata.combine3 mtd xmtd y.metadata), state) |> matched
        // -(y + b) = -b
        | Add(a, b, false, _) when a = y -> matched (b, state)
        // -(a + y) = -a
        | Add(a, b, false, _) when b = y -> matched (a, state)
        | _ -> unmatched ()

    and private simplifyAdditionToProduct mtd state t xmtd a b y matched unmatched =
        // Simplifying (a * b) + y at this step
        match a.term, b.term, y with
        // (a * y) + y = (a + 1) * y if unckecked and a is concrete
        | Concrete(aval, atyp), _, _ when b = y ->
            let mtd' = Metadata.combine3 mtd a.metadata y.metadata in
            let aPlusOne, state = simplifyConcreteAddition mtd' false state (Types.ToDotNetType atyp) aval 1 in
            simplifyMultiplication xmtd false state t aPlusOne y matched
        // (a * b) + (c * b) = (a + c) * b if unchecked and a and c are concrete
        | Concrete(aval, _), _, Mul(ConcreteT(cval, _) as c, d, false, _) when d = b ->
            let mtd' = Metadata.combine3 mtd a.metadata c.metadata in
            let aPlusC, state = simplifyConcreteAddition mtd' false state t aval cval in
            simplifyMultiplication xmtd false state t aPlusC b matched
        | _ -> unmatched ()

    and private simplifyAdditionToShift mtd state t xmtd a b y matched unmatched =
        // Simplifying (a << b) + y at this step
        match b.term, y with
        // (a << b) + (a << b) = 0            if unchecked, b = (size of a) * 8 - 1
        // (a << b) + (a << b) = a << (b + 1) if unchecked, b < (size of a) * 8 - 1
        | Concrete(x, _), ShiftLeft(c, ConcreteT(d, _), false, _) when a = c && x = d ->
            let tooBigShift = Calculator.Compare(x, ((SizeOfNumeric (TypeOf a)) * 8) - 1) = 0 in
            if tooBigShift then
                let mtd' = Metadata.combine3 mtd xmtd y.metadata in
                (CastConcrete 0 t mtd', state) |> matched
            else
                let mtd' = Metadata.combine3 mtd b.metadata y.metadata in
                simplifyShift xmtd OperationType.ShiftLeft false state t a (CastConcrete (Calculator.Add(x, 1, t)) t mtd') matched
        | _ -> unmatched ()

    and private simplifyAdditionToExpression mtd state x y t matched unmatched =
        let xmtd = x.metadata in
        match x with
        | Add(a, b, false, _) -> simplifyAdditionToSum mtd state t xmtd a b y matched unmatched
        | UnaryMinusT(x, false, _) -> simplifyAdditionToUnaryMinus mtd state t xmtd x y matched unmatched
        | Mul(a, b, false, _) -> simplifyAdditionToProduct mtd state t xmtd a b y matched unmatched
        | ShiftLeft(a, b, false, _) -> simplifyAdditionToShift mtd state t xmtd a b y matched unmatched
        | _ -> unmatched ()

    and private simplifyAdditionExt mtd isChecked state t x y matched unmatched =
        match x.term, y.term with
        | Concrete(xval, _), Concrete(yval, _) ->
            let mtd' = Metadata.combine3 mtd x.metadata y.metadata in
            simplifyConcreteAddition mtd' isChecked state t xval yval |> matched
        | Concrete(xval, _), _ when Calculator.IsZero(xval) -> matched (y, state)
        | _, Concrete(yval, _) when Calculator.IsZero(yval) -> matched (x, state)
        | Expression _, Expression _ when not isChecked ->
            simplifyAdditionToExpression mtd state x y t matched (fun () ->
            simplifyAdditionToExpression mtd state y x t matched unmatched)
        | Expression _, _ when not isChecked -> simplifyAdditionToExpression mtd state x y t matched unmatched
        | _, Expression _ when not isChecked -> simplifyAdditionToExpression mtd state y x t matched unmatched
        | _ -> unmatched ()

    and private simplifyAddition mtd isChecked state t x y k =
        let defaultCase () =
            let sorted = if not isChecked && (IsConcrete y) then (y, x) else (x, y) in
                makeAddition mtd isChecked state t (fst sorted) (snd sorted) k
        simplifyGenericBinary "addition" state x y k
                              (simplifyConcreteBinary simplifyConcreteAddition mtd isChecked t)
                              (fun x y state k -> simplifyAdditionExt mtd isChecked state t x y k defaultCase)
                              (fun x y state k -> simplifyAddition mtd isChecked state t x y k)

    and private simplifySubtraction mtd isChecked state t x y k =
        simplifyUnaryMinus mtd isChecked state t y (fun (minusY, state) ->
        simplifyAddition mtd isChecked state t x minusY k)

// ------------------------------- Simplification of unary "-" -------------------------------

    and private simplifyConcreteUnaryMinus mtd isChecked state t x =
        if isChecked then
            let success = ref true
            let result = Calculator.UnaryMinusChecked(x, t, success)
            if !success then CastConcrete result t mtd, state
            else
                let t, s = State.activator.CreateInstance mtd typeof<System.OverflowException> [] state in
                Error t mtd, s
        else
            CastConcrete (Calculator.UnaryMinus(x, t)) t mtd, state

    and private simplifyUnaryMinus mtd isChecked state t x k =
        let simplifyConcrete x xc _ state =
            simplifyConcreteUnaryMinus (Metadata.combine mtd x.metadata) isChecked state t xc
        in
        simplifyGenericUnary "unary minus" state x k simplifyConcrete (fun x state k ->
        match x with
        // -(-(x)) = x if both unchecked
        | UnaryMinusT(x, false, _) when not isChecked -> k (x, state)
        // -(a + b) = (-a) + (-b) if all unchecked
        | Add(a, b, false, _) when not isChecked ->
            simplifyUnaryMinus mtd false state t a (fun (minusA, state) ->
            simplifyUnaryMinus mtd false state t b (fun (minusB, state) ->
            simplifyAddition x.metadata false state t minusA minusB k))
         // -(a * x) = (-a) * x if both unchecked
        | Mul(ConcreteT(_, at) as a, y, false, _) when not isChecked ->
            simplifyUnaryMinus mtd isChecked state (Types.ToDotNetType at) a (fun (minusA, state) ->
            simplifyMultiplication x.metadata false state t minusA y k)
        | _ -> k (MakeUnary OperationType.UnaryMinus x isChecked (FromConcreteDotNetType t) mtd, state))

// ------------------------------- Simplification of "*" -------------------------------

    and private simplifyConcreteMultiplication mtd isChecked state t x y =
        if isChecked then
            let success = ref true
            let result = Calculator.MulChecked(x, y, t, success)
            if !success then CastConcrete result t mtd, state
            else
                let t, s = State.activator.CreateInstance mtd typeof<System.OverflowException> [] state in
                Error t mtd, s
        else
            CastConcrete (Calculator.Mul(x, y, t)) t mtd, state

    and private simplifyMultiplicationOfProduct mtd state t xmtd a b y matched unmatched =
        // Simplifying (a * b) * y at this step
        match a, b, y with
        // (a * b) * y = (a * y) * b if a and y concrete and unchecked
        | ConcreteT(aval, _), _, ConcreteT(yval, _) ->
            let mtd' = Metadata.combine3 mtd a.metadata y.metadata in
            let x, state = simplifyConcreteMultiplication mtd' false state t a y in
            simplifyMultiplication xmtd false state t x b matched
        // ((a / y) * b) * y = a * b if unchecked
        | Div(a, c, false, _), b, _ when c = y -> simplifyMultiplication xmtd false state t a b matched
        // (a * (b / y)) * y = a * b if unchecked
        | a, Div(b, c, false, _), _ when c = y -> simplifyMultiplication xmtd false state t a b matched
        // (a * b) * (c / a) = b * c if unchecked
        | _, _, Div(c, d, false, _) when d = a -> simplifyMultiplication mtd false state t b c matched
        // (a * b) * (c / b) = a * c if unchecked
        | _, _, Div(c, d, false, _) when d = b -> simplifyMultiplication mtd false state t a c matched
        | _ ->
            // Trying to simplify pairwise combinations of x- and y-factors
            let factorsOfY, mtd' =
                match y with
                | Mul(c, d, false, yt) -> [(c, state); (d, state)], Metadata.combine3 mtd xmtd y.metadata
                | _ -> [(y, state)], Metadata.combine mtd xmtd
            in
            simplifyPairwiseCombinations
                [(a, state); (b, state)]
                factorsOfY
                t
                fst
                (simplifyMultiplicationExt mtd' false state)
                (simplifyMultiplication mtd' false state t)
                (withSnd state >> matched)
                unmatched

    and private simplifyMultiplicationOfDivision mtd state t xmtd a b y matched unmatched =
        // Simplifying (a / b) * y at this step
        match a, b, y with
        // (a / y) * y = a if unchecked
        | _ when b = y -> matched (a, state)
        // (a / b) * y = (a * y) / b if a and y are concrete and unckecked
        | ConcreteT(aval, _), b, ConcreteT(yval, _) ->
            let mtd' = Metadata.combine3 mtd a.metadata y.metadata in
            let aMulY, state = simplifyConcreteMultiplication mtd' false state t a y
            simplifyDivision xmtd false state t aMulY b matched
        // (a / (y * d)) * y = a/d if unchecked
        | _, Mul(c, d, false, _), _ when c = y -> simplifyDivision xmtd false state t a d matched
        // (a / (c * y)) * y = a/c if unchecked
        | _, Mul(c, d, false, _), _ when d = y -> simplifyDivision xmtd false state t a c matched
        | _ -> unmatched ()

    and private simplifyMultiplicationOfShifts mtd state t xmtd a b y matched unmatched =
        // Simplifying (a << b) * y at this step
        match b.term, y with
        // (a << b) * (c << d) = (a * c) << (b + d) if unchecked, b and d are conctere, b + d < (size of a) * 8
        // (a << b) * (c << d) = 0 if unchecked, b and d are conctere, b + d >= (size of a) * 8
        | Concrete(bval, _), ShiftLeft(c, (ConcreteT(dval, _) as d), false, _) ->
            let smallShift = Calculator.Compare(Calculator.Add(bval, dval, t), BitSizeOf a (TypeOf a) t) = -1 in
            let mtd' = Metadata.combine3 mtd b.metadata d.metadata in
            if smallShift then
                simplifyMultiplication mtd false state t a c (fun mul ->
                let bPlusD = CastConcrete (Calculator.Add(bval, dval, t)) t mtd'
                simplifyShift xmtd OperationType.ShiftLeft false (snd mul) t (fst mul) bPlusD matched)
            else
                (CastConcrete 0 t mtd, state) |> matched
        // (a << b) * 2^n = a << (b + n) if unchecked, b is concrete, b + n < (size of a) * 8
        // (a << b) * 2^n = 0 if unchecked, b is concrete, b + n >= (size of a) * 8
        | Concrete(x, _), ConcreteT(powOf2, _) when Calculator.IsPowOfTwo(powOf2) ->
            let n = Calculator.WhatPowerOf2(powOf2) in
            let tooBigShift = Calculator.Compare(Calculator.Add(x, n, t), BitSizeOf a (TypeOf a) t) >= 0 in
            if tooBigShift then (CastConcrete 0 t (Metadata.combine3 mtd xmtd y.metadata), state) |> matched
            else
                let mtd' = Metadata.combine3 mtd b.metadata y.metadata in
                simplifyShift xmtd OperationType.ShiftLeft false state t a (CastConcrete (Calculator.Add(x, n, t)) t mtd') matched
        | _ -> unmatched ()

    and private simplifyMultiplicationOfExpression mtd state t x y matched unmatched =
        match x with
        | Mul(a, b, false, _) -> simplifyMultiplicationOfProduct mtd state t x.metadata a b y matched unmatched
        | Div(a, b, false, _) -> simplifyMultiplicationOfDivision mtd state t x.metadata a b y matched unmatched
        | ShiftLeft(a, b, false, _) -> simplifyMultiplicationOfShifts mtd state t x.metadata a b y matched unmatched
        | _ -> unmatched ()

    and private simplifyMultiplicationExt mtd isChecked state t x y matched unmatched =
        match x.term, y.term with
        | Concrete(xval, typeOfX), _ when Calculator.IsZero(xval) -> (CastConcrete 0 t x.metadata, state) |> matched
        | _, Concrete(yval, typeOfY) when Calculator.IsZero(yval) -> (CastConcrete 0 t y.metadata, state) |> matched
        | Concrete(x, typeOfX), _ when Calculator.FuzzyEqual(x, System.Convert.ChangeType(1, t)) -> matched (y, state)
        | _, Concrete(y, typeOfY) when Calculator.FuzzyEqual(y, System.Convert.ChangeType(1, t)) -> matched (x, state)
        | Concrete(x, typeOfX), _ when not <| IsUnsigned t && Calculator.FuzzyEqual(x, System.Convert.ChangeType(-1, t)) ->
            simplifyUnaryMinus mtd isChecked state t y matched
        | _, Concrete(y, typeOfY) when not <| IsUnsigned t && Calculator.FuzzyEqual(y, System.Convert.ChangeType(-1, t)) ->
            simplifyUnaryMinus mtd isChecked state t x matched
        | Expression _, Expression _ when not isChecked ->
            simplifyMultiplicationOfExpression mtd state t x y matched (fun () ->
            simplifyMultiplicationOfExpression mtd state t y x matched unmatched)
        | Expression _, _ when not isChecked -> simplifyMultiplicationOfExpression mtd state t x y matched unmatched
        | _, Expression _ when not isChecked -> simplifyMultiplicationOfExpression mtd state t y x matched unmatched
        | _ -> unmatched ()

    and private simplifyMultiplication mtd isChecked state t x y k =
        let defaultCase () =
            let sorted = if (IsConcrete y) then (y, x) else (x, y)
            makeProduct mtd isChecked state t (fst sorted) (snd sorted) k
        simplifyGenericBinary "product" state x y k
                              (simplifyConcreteBinary simplifyConcreteMultiplication mtd isChecked t)
                              (fun x y state k -> simplifyMultiplicationExt mtd isChecked state t x y k defaultCase)
                              (fun x y state k -> simplifyMultiplication mtd isChecked state t x y k)

// ------------------------------- Simplification of "/" -------------------------------

    and simplifyConcreteDivision mtd isChecked state t x y =
        let success = ref true
        let result =
            if isChecked then
                Calculator.DivChecked(x, y, t, success)
            else
                Calculator.Div(x, y, t, success)
        if !success then CastConcrete result t mtd, state
        else
        let t, s =
            match result with
            | :? System.DivideByZeroException -> State.activator.CreateInstance mtd typeof<System.DivideByZeroException> [] state
            | :? System.OverflowException -> State.activator.CreateInstance mtd typeof<System.OverflowException> [] state
            | _ -> __notImplemented__()
        in Error t mtd, s

    and private simplifyDivision mtd isChecked state t x y k =
        let defaultCase () =
            let sorted = if not isChecked && (IsConcrete y) then (y, x) else (x, y) in
            makeProduct mtd isChecked state t (fst sorted) (snd sorted) k
        in
        simplifyGenericBinary "division" state x y k
            (simplifyConcreteBinary simplifyConcreteDivision mtd isChecked t)
            (fun x y state k ->
                match x, y with
                // 0 / y = 0
                | ConcreteT(xval, _), _ when Calculator.IsZero(xval) -> (x, state) |> k
                // x / 1 = x
                | _, ConcreteT(yval, _) when Calculator.FuzzyEqual(yval, System.Convert.ChangeType(1, TypeOf x |> ToDotNetType)) -> (x, state) |> k
                // x / -1 = -x
                | _, ConcreteT(yval, _) when not <| IsUnsigned t && Calculator.FuzzyEqual(yval, System.Convert.ChangeType(-1, TypeOf x |> ToDotNetType)) ->
                    simplifyUnaryMinus mtd isChecked state t x k
                // x / x = 1 if unchecked
                | x, y when not isChecked && x = y -> (CastConcrete 1 t mtd, state) |> k
                // x / -x = -1 if unchecked
                | x, UnaryMinusT(y, false, _) when not <| IsUnsigned t && not isChecked && x = y -> (CastConcrete -1 t mtd, state) |> k
                // (a >> b) / 2^n = a >> (b + n) if unchecked, b is concrete, b + n < (size of a) * 8
                // (a >> b) / 2^n = 0 if unchecked, b is concrete, b + n >= (size of a) * 8
                | ShiftRight(a, ConcreteT(b, _), false, _), ConcreteT(powOf2, _)
                    when Calculator.IsPowOfTwo(powOf2) && a |> TypeOf |> ToDotNetType |> IsUnsigned ->
                        let n = Calculator.WhatPowerOf2(powOf2) in
                        let tooBigShift = Calculator.Compare(Calculator.Add(b, n, t), BitSizeOf a (TypeOf a) t) >= 0 in
                        if tooBigShift then (CastConcrete 0 t mtd, state) |> k
                        else
                            simplifyShift x.metadata OperationType.ShiftRight false state t a (CastConcrete (Calculator.Add(b, n, t)) t mtd) k
                // (a / b) / y = a / (b * y) if unchecked and b and y concrete
                | Div(a, (ConcreteT(bval, _) as b), false, _), ConcreteT(yval, _) when not isChecked ->
                    let bMulY, state = simplifyConcreteMultiplication (Metadata.combine3 mtd b.metadata y.metadata) false state t bval yval in
                    simplifyDivision x.metadata false state t a bMulY k
                | _ -> (MakeBinary OperationType.Divide x y isChecked (FromConcreteDotNetType t) mtd, state) |> k)
            (fun x y state k -> simplifyDivision mtd isChecked state t x y k)

    and private checkNotZero doDivide mtd isChecked state t x y k =
        simplifyEqual mtd y (CastConcrete 0 (ToDotNetType(TypeOf y)) mtd) (fun yIsZero ->
            if Terms.IsFalse yIsZero then doDivide mtd isChecked state t x y k
            elif Terms.IsTrue yIsZero
            then State.activator.CreateInstance mtd typeof<System.DivideByZeroException> [] state |> (fun (t, s) -> k (Error t mtd, s))
            else
                let errorTerm, errorState = State.activator.CreateInstance mtd typeof<System.DivideByZeroException> [] state in
                let y = Merging.merge2Terms !!yIsZero yIsZero y (Error errorTerm mtd) in
                let state = Merging.merge2States !!yIsZero yIsZero state errorState in
                doDivide mtd isChecked state t x y k)

// ------------------------------- Simplification of "%" -------------------------------

    and private simplifyConcreteRemainder mtd isChecked state t x y =
        let success = ref true
        let result =
            if isChecked then
                Calculator.RemChecked(x, y, t, success)
            else
                Calculator.Rem(x, y, t, success)
        if !success then CastConcrete result t mtd, state
        else
        let t, s =
            match result with
            | :? System.DivideByZeroException -> State.activator.CreateInstance mtd typeof<System.DivideByZeroException> [] state
            | :? System.OverflowException -> State.activator.CreateInstance mtd typeof<System.OverflowException> [] state
            | _ -> __notImplemented__()
        in Error t mtd, s


    and private isRemainderZero state t x y =
        let success = ref true
        Calculator.IsZero(Calculator.Rem(x, y, t, success)) && !success

    and internal simplifyRemainder mtd isChecked state t x y k =
        let defaultCase () =
            let sorted = if not isChecked && (IsConcrete y) then (y, x) else (x, y)
            makeProduct mtd isChecked state t (fst sorted) (snd sorted) k
        simplifyGenericBinary "remainder" state x y k
            (simplifyConcreteBinary simplifyConcreteRemainder mtd isChecked t)
            (fun x y state k ->
                match x, y with
                // 0 % y = 0
                | ConcreteT(xval, _), _ when Calculator.IsZero(xval) -> (x, state) |> k
                // x % 1 = 0
                | _, ConcreteT(y, _) when Calculator.FuzzyEqual(y, System.Convert.ChangeType(1, t)) -> (CastConcrete 0 t mtd, state) |> k
                // x % -1 = 0
                | _, ConcreteT(y, _) when not <| IsUnsigned t && Calculator.FuzzyEqual(y, System.Convert.ChangeType(-1, t)) ->
                    (CastConcrete 0 t mtd, state) |> k
                // x % x = 0
                | x, y when not isChecked && x = y -> (CastConcrete 0 t mtd, state) |> k
                // x % -x = 0 if unchecked
                | x, UnaryMinusT(y, false, _) when not isChecked && x = y -> (CastConcrete 0 t mtd, state) |> k
                // (a * b) % y = 0 if unchecked, b and y concrete and a % y = 0
                | Mul(ConcreteT(a, _), b, false, _), ConcreteT(y, _) when not isChecked && isRemainderZero state t a y ->
                     (CastConcrete 0 t mtd, state) |> k
                | _ -> (MakeBinary OperationType.Remainder x y isChecked (FromConcreteDotNetType t) mtd, state) |> k)
            (fun x y state k -> simplifyRemainder mtd isChecked state t x y k)

// ---------------------------------------- Simplification of "<<", ">>" ----------------------------------------

    and private simplifyConcreteShift operation mtd _ state t x y =
        match operation with
        | OperationType.ShiftLeft -> (CastConcrete (Calculator.ShiftLeft(x, y, t)) t mtd, state)
        | OperationType.ShiftRight -> (CastConcrete (Calculator.ShiftRight(x, y, t)) t mtd, state)
        | _ -> __unreachable__()

    and private simplifyShiftLeftMul mtd state t xmtd a b y matched unmatched =
        // Simplifying (a * b) << y at this step
        match a.term, b.term, y.term with
        // (2^n * b) << y = b << (y + n) if unchecked, y is concrete, y + n < bitSize of a
        // (2^n * b) << y = 0 if unchecked, y is concrete, y + n >= bitSize of a
        |  Concrete(powOf2, _), _, Concrete(yval, _)
            when Calculator.IsPowOfTwo(powOf2) ->
                let n = Calculator.WhatPowerOf2(powOf2) in
                let tooBigShift = Calculator.Compare(Calculator.Add(yval, n, t), BitSizeOf a (TypeOf a) t) >= 0 in
                if tooBigShift then (CastConcrete 0 t mtd, state) |> matched
                else
                    let mtd' = Metadata.combine3 xmtd a.metadata y.metadata in
                    simplifyShift mtd OperationType.ShiftLeft false state t b (CastConcrete (Calculator.Add(yval, n, t)) t mtd') matched
        | _ -> unmatched ()

    and private simplifyShiftRightDiv mtd state t xmtd a b y matched unmatched =
        // Simplifying (a / b) >> y at this step
        match b.term, y.term with
        // (a / 2^n) >> y = a >> (y + n) if y is concrete, a is unsigned, y + n < bitSize of a
        // (a / 2^n) >> y = 0 if y is concrete, a is unsigned, y + n >= bitSize of a
        |   Concrete(powOf2, _), Concrete(yval, _)
            when Calculator.IsPowOfTwo(powOf2) && a |> TypeOf |> ToDotNetType |> IsUnsigned ->
                let n = Calculator.WhatPowerOf2(powOf2) in
                let tooBigShift = Calculator.Compare(Calculator.Add(yval, n, t), BitSizeOf a (TypeOf a) t) >= 0 in
                if tooBigShift then (CastConcrete 0 t mtd, state) |> matched
                else
                    let mtd' = Metadata.combine3 xmtd b.metadata y.metadata in
                    simplifyShift mtd OperationType.ShiftRight false state t a (CastConcrete (Calculator.Add(yval, n, t)) t mtd') matched
        | _ -> unmatched ()

    and private simplifyShiftLeftOfAddition mtd state t xmtd a b y matched unmatched =
        // Simplifying (a + a) << y at this step
        match y.term with
        // (a + a) << y = 0 if unchecked, y is concrete, y = (size of a) * 8 - 1
        // (a + a) << y = a << (y + 1) if unchecked, y is concrete, y < (size of a) * 8 - 1
        | Concrete(c, _) ->
            let tooBigShift = Calculator.Compare(c, ((SizeOfNumeric (TypeOf a)) * 8) - 1) = 0 in
            if tooBigShift then (CastConcrete 0 t mtd, state) |> matched
            else
                let mtd' = Metadata.combine xmtd b.metadata in
                simplifyShift mtd OperationType.ShiftLeft false state t a (CastConcrete (Calculator.Add(c, 1, t)) t mtd') matched
        | _ -> unmatched ()

    and private simplifyShiftOfShifted mtd op state t xmtd a b y matched unmatched =
        // Simplifying (a op b) op y at this step
        match b.term, y.term, op with
        // (a op b) op y = a op (b + y) if unchecked, b and y are concrete, b + y < (size of a) * 8
        | Concrete(x, _), Concrete(c, _), _ when Calculator.Compare(Calculator.Add(x, c, t), BitSizeOf a (TypeOf a) t) = -1 ->
            let mtd' = Metadata.combine3 mtd b.metadata y.metadata in
            simplifyShift xmtd op false state t a (CastConcrete (Calculator.Add(x, c, t)) t mtd') matched
        // (a op b) op y = 0 if unchecked, b and y are concrete, b + y >= (size of a) * 8
        | Concrete(x, _), Concrete(c, _), OperationType.ShiftLeft ->
            (CastConcrete 0 t mtd, state) |> matched
        | Concrete(x, _), Concrete(c, _), OperationType.ShiftRight when a |> TypeOf |> ToDotNetType |> IsUnsigned ->
            (CastConcrete 0 t mtd, state) |> matched
        | _ -> unmatched ()

    and private simplifyShiftOfExpression mtd op isChecked state t x y matched unmatched =
        let xmtd = x.metadata in
        match x, op with
        | Mul(a, b, false, _), OperationType.ShiftLeft when not isChecked -> simplifyShiftLeftMul mtd state t xmtd a b y matched unmatched
        | Div(a, b, false, _), OperationType.ShiftRight -> simplifyShiftRightDiv mtd state t xmtd a b y matched unmatched
        | Add(a, b, false, _), OperationType.ShiftLeft when a = b && not isChecked -> simplifyShiftLeftOfAddition mtd state t xmtd a b y matched unmatched
        | ShiftLeft(a, b, false, _), OperationType.ShiftLeft -> simplifyShiftOfShifted mtd op state t xmtd a b y matched unmatched
        | ShiftRight(a, b, false, _), OperationType.ShiftRight -> simplifyShiftOfShifted mtd op state t xmtd a b y matched unmatched
        | _ -> unmatched ()

    and private simplifyShiftExt mtd op isChecked state t x y matched unmatched =
        match x.term, y.term with
        | Concrete(x, typeOfX), _ when Calculator.IsZero(x) -> (CastConcrete 0 t mtd, state) |> matched
        | _, Concrete(y, typeOfY) when Calculator.IsZero(y) -> (x, state) |> matched
        | Expression _, Expression _
        | Expression _, _ -> simplifyShiftOfExpression mtd op isChecked state t x y matched unmatched
        | _ -> unmatched ()

    and private simplifyShift mtd operation isChecked state t x y k =
        let defaultCase () =
            makeShift mtd operation state isChecked t x y k
        simplifyGenericBinary "shift" state x y k
                                (simplifyConcreteBinary (simplifyConcreteShift operation) mtd isChecked t)
                                (fun x y state k -> simplifyShiftExt mtd operation isChecked state t x y k defaultCase)
                                (fun x y state k -> simplifyShift mtd operation isChecked state t x y k)

// TODO: IMPLEMENT BITWISE OPERATIONS!

// ------------------------------- Simplification of "=", "!=", "<", ">", ">=", "<=" -------------------------------

    and private simplifyConcreteComparison operator mtd _ state _ x y =
        Concrete (Calculator.Compare(x, y) |> operator) Bool mtd, state

    and private simplifyComparison mtd op x y comparator sameIsTrue k =
        simplifyGenericBinary "comparison" State.empty x y (fst >> k)
            (simplifyConcreteBinary (simplifyConcreteComparison comparator) mtd false Bool)
            (fun x y s k ->
                match x, y with
                | _ when x = y -> (Concrete sameIsTrue Bool mtd, s) |> k
                | Add((ConcreteT(_, t) as c), x, false, _), y when x = y ->
                    simplifyComparison mtd op c (CastConcrete 0 (ToDotNetType t) mtd) comparator sameIsTrue (withSnd s >> k)
                | x, Add((ConcreteT(_, t) as c), y, false, _) when x = y ->
                    simplifyComparison mtd op (CastConcrete 0 (ToDotNetType t) mtd) c comparator sameIsTrue (withSnd s >> k)
                | _ -> (MakeBinary op x y false Bool mtd, s) |> k)
            (fun x y state k -> simplifyComparison mtd op x y comparator sameIsTrue (withSnd state >> k))

    and internal simplifyEqual mtd x y k =
        simplifyComparison mtd OperationType.Equal x y ((=) 0) true k
    and internal simplifyNotEqual mtd x y k =
        simplifyComparison mtd OperationType.Equal x y ((=) 0) true ((!!) >> k)
    and internal simplifyLess mtd x y k =
        simplifyComparison mtd OperationType.Less x y ((>) 0) false k
    and internal simplifyLessOrEqual mtd x y k =
        simplifyComparison mtd OperationType.LessOrEqual x y ((>=) 0) true k
    and internal simplifyGreater mtd x y k =
        simplifyComparison mtd OperationType.LessOrEqual x y ((>=) 0) true ((!!) >> k)
    and internal simplifyGreaterOrEqual mtd x y k =
        simplifyComparison mtd OperationType.Less x y ((>) 0) false ((!!) >> k)

// ------------------------------- General functions -------------------------------

    // WARNING: These operators are safe versions of concrete simplifyBinaryOperation.
    // Use them, only if you can guarantee that operation will complete without exceptions,
    // otherwise you should use simplifyBinaryOperation with state

    let add mtd x y =
        simplifyAddition mtd false State.empty (Types.ToDotNetType (Terms.TypeOf x)) x y fst

    let sub mtd x y =
        simplifySubtraction mtd false State.empty (Types.ToDotNetType (Terms.TypeOf x)) x y fst

    let mul mtd x y =
        simplifyMultiplication mtd false State.empty (Types.ToDotNetType (Terms.TypeOf x)) x y fst

    let rem mtd x y =
        simplifyRemainder mtd false State.empty (Types.ToDotNetType (Terms.TypeOf x)) x y fst

    let eq mtd x y =
        simplifyEqual mtd x y id

    let internal simplifyBinaryOperation metadata op state x y isChecked t k =
        match op with
        | OperationType.Add -> simplifyAddition metadata isChecked state t x y k
        | OperationType.Subtract -> simplifySubtraction metadata isChecked state t x y k
        | OperationType.Multiply -> simplifyMultiplication metadata isChecked state t x y k
        | OperationType.Divide -> checkNotZero simplifyDivision metadata isChecked state t x y k
        | OperationType.Remainder -> checkNotZero simplifyRemainder metadata isChecked state t x y k
        | OperationType.ShiftLeft-> simplifyShift metadata op isChecked state t x y k
        | OperationType.ShiftRight -> simplifyShift metadata op isChecked state t x y k
        | OperationType.Equal -> simplifyEqual metadata x y (withSnd state >> k)
        | OperationType.NotEqual -> simplifyNotEqual metadata x y (withSnd state >> k)
        | OperationType.Greater -> simplifyGreater metadata x y (withSnd state >> k)
        | OperationType.GreaterOrEqual -> simplifyGreaterOrEqual metadata x y (withSnd state >> k)
        | OperationType.Less -> simplifyLess metadata x y (withSnd state >> k)
        | OperationType.LessOrEqual -> simplifyLessOrEqual metadata x y (withSnd state >> k)
        | OperationType.LogicalAnd
        | OperationType.LogicalOr
        | OperationType.LogicalXor -> __notImplemented__()
        | _ -> internalfailf "%O is not a binary arithmetical operator" op

    let internal simplifyUnaryOperation metadata op state x isChecked t k =
        match op with
        | OperationType.LogicalNeg -> __notImplemented__()
        | OperationType.UnaryMinus -> simplifyUnaryMinus metadata isChecked state t x k
        | OperationType.UnaryPlus -> k (x, state)
        | _ -> internalfailf "%O is not an unary arithmetical operator" op

    let internal isArithmeticalOperation op t1 t2 =
        (Types.IsNumeric t1 || Types.IsBottom t1) && (Types.IsNumeric t2 || Types.IsBottom t2) &&
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
        | OperationType.UnaryMinus
        | OperationType.UnaryPlus -> true
        | _ -> false
