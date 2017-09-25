namespace VSharp

[<AutoOpen>]
module ExternSDK =
    let private m() =
        Interpreter.currentInternalCallMetadata

    let public Error term = Error (m()) term
    let public Concrete obj typ = Concrete (m()) obj typ
    let public Constant name source typ = Constant (m()) name source typ
    let public Array lower constant contents lengths typ = Array (m()) lower constant contents lengths typ
    let public Expression op args typ = Expression (m()) op args typ
    let public Struct fields typ = Struct (m()) fields typ
    let public StackRef key path = StackRef (m()) key path
    let public HeapRef path time = HeapRef (m()) path time
    let public StaticRef key path = StaticRef (m()) key path
    let public Union gvs = Union (m()) gvs

    let NoResult () = NoResult (m())
    let Break () = Break (m())
    let Continue () = Continue (m())
    let Return term = Return (m()) term
    let Throw term = Throw (m()) term
    let Guarded metadata grs = Guarded (m()) grs

    let MakeNumber n = MakeNumber n (m())
    let MakeConcreteString (s : string) = MakeConcreteString s (m())

    let CreateInstance t args state = State.activator.CreateInstance (m()) t args state

    let simplifyEqual x y k = Arithmetics.simplifyEqual (m()) x y k
    let simplifyNotEqual x y k = Arithmetics.simplifyNotEqual (m()) x y k
    let simplifyLess x y k = Arithmetics.simplifyLess (m()) x y k
    let simplifyLessOrEqual x y k = Arithmetics.simplifyLessOrEqual (m()) x y k
    let simplifyGreater x y k = Arithmetics.simplifyGreater (m()) x y k
    let simplifyGreaterOrEqual x y k = Arithmetics.simplifyGreaterOrEqual (m()) x y k
    let simplifyRemainder isChecked state t x y k = Arithmetics.simplifyRemainder (m()) isChecked state t x y k

    let simplifyAnd x y k = Propositional.simplifyAnd (m()) x y k
    let simplifyOr x y k = Propositional.simplifyOr (m()) x y k
    let simplifyNegation x k = Propositional.simplifyNegation (m()) x k

    module Memory =
        let deref state reference = Memory.deref (m()) state reference

    module VSharp =
        module Array =
            let length term = VSharp.Array.length (m()) term
