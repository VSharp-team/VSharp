namespace VSharp.Core.Symbolic

open System
open System.Collections.Generic

module Types =
    let private numericTypes =
        new HashSet<Type>([typedefof<byte>; typedefof<sbyte>;
                           typedefof<int16>; typedefof<uint16>;
                           typedefof<int32>; typedefof<uint32>;
                           typedefof<int64>; typedefof<uint64>;
                           typedefof<single>; typedefof<double>;
                           typedefof<decimal>])

    let private primitiveSolvableTypes =
        new HashSet<Type>(Seq.append numericTypes
            [typedefof<bool>; typedefof<char>;])

    let IsNumeric = numericTypes.Contains

    let IsPrimitiveSolvable = primitiveSolvableTypes.Contains
