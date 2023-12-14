module MultiSet

    type MultiSet<'a when 'a : comparison> = MS of Map<'a, uint32>

// ---- Green exercises ----
    let empty = MS Map.empty<'a, uint32>

    let isEmpty (MS(s)) = Map.isEmpty s

    let size (MS(s)) = Map.fold (fun acc _ value -> acc + value) 0u s

    let contains x (MS(s)) = Map.containsKey x s

    let numItems x (MS(s)) = Map.tryFind x s |> Option.defaultValue 0u

    let add x n (MS(s) as ms) = 
        let currentOccurences = numItems x ms
        MS (Map.add x (n + currentOccurences) s)
        // or alt:
        // MS (Map.add x (n + numItems x ms) s)

    let addSingle x (MS(s)) = add x 1u (MS(s))

    let remove x n (MS(s) as ms) =
        match numItems x ms with
        | 0u -> ms
        | occurences when n > occurences -> MS (Map.remove x s)
        | _ -> add x n ms

    let removeSingle x ms = remove x 1u ms

    let fold (folder: ('a -> 'b -> uint32 -> 'a)) acc (MS(s)) = Map.fold folder acc s
    let foldBack folder (MS(s)) acc = Map.foldBack folder s acc

// ---- Yellow Exercises ----

    let ofList lst = List.fold (fun acc elem -> addSingle elem acc) empty lst

    let toList ms = foldBack (fun k v acc -> [for _ in 1u .. v -> k] @ acc) ms []

    let map mapper ms = fold (fun acc k v -> add (mapper k) v acc) empty ms

    let union s1 s2 =
        fold (fun acc k s1occurences ->
            let s2occurences = numItems k s2
            if s2occurences > s1occurences
            then add k s2occurences s1
            else acc
            ) s1 s2

    let sum s1 s2 = foldBack add s1 s2

    let subtract s1 s2 = foldBack remove s1 s2

    let intersection s1 s2 = 
        fold (fun acc k s1occurences ->
            let s2occurences = numItems k s2
            if s1occurences = s2occurences
            then add k s1occurences acc
            else acc
            ) empty s1