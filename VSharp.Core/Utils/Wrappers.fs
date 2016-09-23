namespace VSharp.Core.Utils

module internal Wrappers =
    let internal toString x = x.ToString()
    let internal format f objs = System.String.Format(f, objs)
    let internal format1 f (obj : obj) = System.String.Format(f, obj)
    let internal join s (ss : seq<string>) = System.String.Join(s, ss)
