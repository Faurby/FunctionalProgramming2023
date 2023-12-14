module MultiSet

    type MultiSet<'T when 'T : comparison> = 
        | MS of uint32 * Map<'T, uint32>
        override q.ToString() =
            match q with
            | MS (_, m) ->
                let s = Map.fold (fun acc k x -> sprintf "%s(%A, #%d), " acc k x) "" m
                sprintf "{%s}" (s.Substring(0, s.Length - 2))


    let empty : MultiSet<'T> = MS (0u, Map.empty)

    let isEmpty : MultiSet<'T> -> bool =
        function
        | MS (0u, m) when Map.isEmpty m -> true
        | _                              -> false

    let size (MS (s, _)) = s

    let numItems x (MS (_, ms)) = 
        match Map.tryFind x ms with
        | Some i -> i
        | None   -> 0u

    let add x v (MS (i, s) as ms) = 
        if v = 0u then ms else MS (i + v, Map.add x (numItems x ms + v) s)

    let addMax x v (MS (i, s) as ms) =
        let n = numItems x ms
        if n >= v then ms else add x (v - n) ms

    let addSingle x ms = add x 1u ms

    let fold folder acc (MS (_, m)) = Map.fold folder acc m
    let foldBack folder (MS (_, m)) acc = Map.foldBack folder m acc

    let map (mapper : 'T -> 'U) m =
        fold (fun acc k x -> add  (mapper k) x acc) empty m

    let ofList lst = List.fold (fun acc x -> addSingle x acc) empty lst
    let toList ms = foldBack (fun k v acc -> [for _ in 1u .. v do yield k] @ acc) ms []

    let union s1 s2 = foldBack addMax s1 s2
    let sum s1 s2 = foldBack add s1 s2

    let keys (MS (_, s)) = Map.toList s |> List.map fst

    let intersection s1 s2 = 
        let addMin acc x =
            match min (numItems x s1) (numItems x s2) with
            | 0u -> acc
            | v  -> add x v acc

        List.fold addMin empty (keys s1)
        
    let remove x v (MS (j, m) as ms) =
        let s = numItems x ms
        match s with
        | 0u            -> ms
        | s when s <= v -> MS (j - s, Map.remove x m)
        | s             -> MS (j - v, Map.add x (numItems x ms - v) m)

    let removeSingle x ms = remove x 1u ms


    let subtract s1 s2 = foldBack remove s2 s1

    let contains x (MS (_ ,s)) = Map.containsKey x s
    