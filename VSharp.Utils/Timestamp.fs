namespace VSharp

type timestamp = uint32

module Timestamp =
    let zero : timestamp = System.UInt32.MinValue
    let empty = zero // TODO: should be empty list
    let infinity : timestamp = System.UInt32.MaxValue
    let inc (t : timestamp) : timestamp = t + 1u

    let compose (t1 : timestamp) (t2 : timestamp) : timestamp =
        // TODO
        t1

    let decompose (t1 : timestamp) (t2 : timestamp) : timestamp =
        // TODO
        t1
