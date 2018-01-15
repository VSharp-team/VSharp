namespace VSharp

type Timestamp = uint32

module Timestamp =
    let zero : Timestamp = System.UInt32.MinValue
    let empty = zero // TODO: should be empty list
    let infinity : Timestamp = System.UInt32.MaxValue
    let inc (t : Timestamp) : Timestamp = t + 1u

    let compose (t1 : Timestamp) (t2 : Timestamp) : Timestamp =
        // TODO
        t1

    let decompose (t1 : Timestamp) (t2 : Timestamp) : Timestamp =
        // TODO
        t1
