namespace VSharp

[<AutoOpen>]
module public Wrappers =
    let public __notImplemented__() = raise (new System.NotImplementedException())
    let public toString x = x.ToString()
    let public format f objs = System.String.Format(f, objs)
    let public format1 f (obj : obj) = System.String.Format(f, obj)
    let public format2 f (obj1 : obj) (obj2 : obj) = System.String.Format(f, obj1, obj2)
    let public format3 f (obj1 : obj) (obj2 : obj) (obj3 : obj) = System.String.Format(f, obj1, obj2, obj3)
    let public join s (ss : seq<string>) = System.String.Join(s, ss)
    let public cons x xs = x :: xs
    let public withFst x = fun y -> (x, y)
    let public withSnd y = fun x -> (x, y)
    let public mapAdd (map : Map<'a, 'b>) key value = map.Add(key, value)
    let public getDictValueOrUpdate (dict : System.Collections.Generic.IDictionary<'a, 'b>) key fallback =
        if dict.ContainsKey(key) then dict.[key]
        else
            let newVal = fallback() in
            dict.Add(key, newVal)
            newVal
