namespace VSharp.Core.Utils

module internal Wrappers =
    let internal toString x = x.ToString()
    let internal format f objs = System.String.Format(f, objs)
    let internal format1 f (obj : obj) = System.String.Format(f, obj)
    let internal format2 f (obj1 : obj) (obj2 : obj) = System.String.Format(f, obj1, obj2)
    let internal format3 f (obj1 : obj) (obj2 : obj) (obj3 : obj) = System.String.Format(f, obj1, obj2, obj3)
    let internal join s (ss : seq<string>) = System.String.Join(s, ss)
    let internal cons x xs = x :: xs