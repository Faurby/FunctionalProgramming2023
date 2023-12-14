module Dictionary
    open System.Collections.Generic

    type Gaddag = 
        | Gaddag of Dictionary<char, bool * Gaddag>

    let emptyDict () = new Dictionary<char, bool * Gaddag>()
    let empty     () = Gaddag (emptyDict ())

    let def = (false, empty ())

    let tryFind k (d : Dictionary<char, bool * Gaddag>) =
        let mutable result = def
        if d.TryGetValue(k, &result) then Some result else None

    let addChar c terminate (dict : Dictionary<char, bool * Gaddag>) =
        match tryFind c dict with
        | Some (b, Gaddag dict') ->
            dict.[c] <- (terminate || b, Gaddag dict')
            dict'
        | None -> 
            let dict' = emptyDict ()
            dict.Add (c, (terminate, Gaddag dict'))
            dict'

    let addChars (str : string) terminate dict offset limit start =
        let mutable dict' = dict
        for i in start..offset..limit do
            dict' <- addChar str.[i] (i = limit && terminate) dict'

        dict'

    let add (str : string) dict =
        let l = str.Length
        for i in 0..l - 2 do
            let dict'   = addChars str false dict (-1) 0 i
            let dict''  = addChar (char 0) false dict'
            addChars str true dict'' 1 (l - 1) (i + 1) |> ignore

        let dict' = addChars str false dict (-1) 0 (l - 1)
        addChar (char 0) true dict' |> ignore

    let insert (str : string) (Gaddag dict) =
        add str dict
        Gaddag dict

    let step c (Gaddag dict) = tryFind c dict
    let reverse (Gaddag dict) = tryFind (char 0) dict
    
    let lookup (str : string) dict =
        let rec aux dict  =
            function
            | [] -> reverse dict |> Option.map fst |> Option.defaultValue false
            | c :: cs ->
                match step c dict with
                | Some (_, dict') -> aux dict' cs
                | None            -> false
        
        aux dict (List.rev [for c in str -> c])