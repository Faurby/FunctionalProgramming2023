// ---- Green Exercises ----
// Exercise 5.1

let sum m n =
    let rec aux n acc =
        match n with
        | 0 -> m + acc
        | n -> aux (n-1) (m + n + acc)
    aux n 0

// Exercise 5.2

let length lst =
    let rec aux lst acc =
        match lst with
        | [] -> acc
        | x :: xs -> aux xs (1+acc)
    aux lst 0

// Exercise 5.3

let foldBack f lst acc =
    let rec aux lst c =
        match lst with
        | [] -> c acc
        | x :: xs -> aux xs (fun acc' -> c (f x acc'))
    aux lst id

(*
    f = (+)
    aux [1;2;3] id 
    (fun acc' -> id (f 1 acc'))
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^ becomes c
    (fun acc'' -> (fun acc' -> id (f 1 acc')) (f 2 acc''))
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ becomes c
    (fun acc''' -> (fun acc'' -> (fun acc' -> id (f 1 acc')) (f 2 acc'')) (f 3 acc'''))

    (fun acc''' -> (fun acc'' -> (fun acc' -> id (f 1 acc')) (f 2 acc'')) (f 3 acc'''))
    
    (fun acc''' -> (fun acc'' -> (fun acc' -> id (f 1 acc')) (f 2 acc'')) (f 3 acc''')) 0

    (fun acc'' -> (fun acc' -> id (f 1 acc')) (f 2 acc'')) ((+) 3 0 ))

    (fun acc'' -> (fun acc' -> id (f 1 acc')) (f 2 acc'')) 3)
    
    (fun acc' -> id (f 1 acc')) (f 2 3)))

    (fun acc' -> id (f 1 acc')) (+ 2 3))

    (fun acc' -> id (f 1 acc')) 5)

    id (f 1 5)

    id 6

    6
*)

// Exercise 5.4
let factA x =
    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (x * acc) (x - 1)
    aux 1 x

let factC x =
    let rec aux n c =
        match n with
        | 0 -> c 1
        | x -> aux (n-1) (fun r -> c (x*r))
    aux x id

// ---- Yellow Exercises ----
// Exercuse 5.5
let fibW x =
    let mutable res1 = 0
    let mutable res2 = 1
    let mutable i = 1
    while (i <= x) do
        let temp = res1
        res1 <- res2
        res2 <- temp + res2
        i <- i + 1
    res1

let fibA = 
    let rec aux acc1 acc2 =
        function
        | 0 -> acc1
        | 1 -> acc2
        | n -> aux acc2 (acc1 + acc2) (n - 1)

    aux 0 1

let fibC =
    let rec aux c =
        function
        | 0 -> c 0
        | 1 -> c 1
        | n -> aux (fun x -> aux (fun y -> c (x + y)) (n - 1)) (n - 2)

    aux id