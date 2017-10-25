namespace VSharp

type Timestamp = uint32

module Timestamp =
    let zero : Timestamp = System.UInt32.MinValue
    let infinity : Timestamp = System.UInt32.MaxValue
    let inc (t : Timestamp) : Timestamp = t + 1u
