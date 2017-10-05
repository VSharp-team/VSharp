namespace VSharp

module public Cps =
    let ret f = fun x k -> k (f x)
    let ret2 f = fun x y k -> k (f x y)

    module public List =
        let rec map f xs k =
            match xs with
            | [] -> k []
            | x::xs' -> map f xs' (k << cons (f x))

        let rec mapk f xs k = 
            match xs with
            | [] -> k []
            | x::xs' -> f x (fun y -> mapk f xs' (k << cons y))

        let rec foldl f a xs k =
            match xs with
            | [] -> k a
            | x::xs' -> foldl f (f a x) xs' k

        let rec foldlk f a xs k =
            match xs with
            | [] -> k a
            | x::xs' -> f a x (fun a' -> foldlk f a' xs' k)

        let rec foldr f a xs k =
            match xs with
            | [] -> k a
            | x::xs' -> foldr f a xs' (f x >> k)

        let rec foldrk f a xs k =
            match xs with
            | [] -> k a
            | x::xs' -> foldrk f a xs' (fun a' -> f x a' k)

        let rec mapFold f a xs k =
            match xs with
            | [] -> k ([], a)
            | x::xs' -> 
                let (x', a') = f a x in
                    mapFold f a' xs' (fun (ys, b) -> k (x' :: ys, b))

        let rec mapFoldk f a xs k =
            match xs with
            | [] -> k ([], a)
            | x::xs' -> 
                f a x (fun (x', a') -> 
                    mapFoldk f a' xs' (fun (ys, b) -> k (x' :: ys, b)))

        let rec reduce f xs k =
            match xs with
            | [] -> raise(System.ArgumentException("List should be not empty"))
            | [x] -> k x
            | x::xs-> foldl f x xs k

        let rec reducek f xs k =
            match xs with
            | [] -> raise(System.ArgumentException("List should be not empty"))
            | [x] -> k x
            | x::xs-> foldlk f x xs k

    module public Seq =
        let rec map f xs k =
            match xs with
            | Seq.Empty -> k []
            | Seq.Cons(x, xs') -> map f xs' (k << cons (f x))

        let rec mapk f xs k = 
            match xs with
            | Seq.Empty -> k []
            | Seq.Cons(x, xs') -> f x (fun y -> mapk f xs' (k << cons y))

        let rec foldl f a xs k =
            match xs with
            | Seq.Empty -> k a
            | Seq.Cons(x, xs') -> foldl f (f a x) xs' k

        let rec foldlk f a xs k =
            match xs with
            | Seq.Empty -> k a
            | Seq.Cons(x, xs') -> f a x (fun a' -> foldlk f a' xs' k)

        let rec foldr f a xs k =
            match xs with
            | Seq.Empty -> k a
            | Seq.Cons(x, xs') -> foldr f a xs' (f x >> k)

        let rec foldrk f a xs k =
            match xs with
            | Seq.Empty -> k a
            | Seq.Cons(x, xs') -> foldrk f a xs' (fun a' -> f x a' k)

        let rec mapFold f a xs k =
            match xs with
            | Seq.Empty -> k ([], a)
            | Seq.Cons(x, xs') -> 
                let (x', a') = f a x in
                    mapFold f a' xs' (fun (ys, b) -> k (x'::ys, b))

        let rec mapFoldk f a xs k =
            match xs with
            | Seq.Empty -> k ([], a)
            | Seq.Cons(x, xs') ->
                f a x (fun (x', a') ->
                    mapFoldk f a' xs' (fun (ys, b) -> k (x'::ys, b)))
