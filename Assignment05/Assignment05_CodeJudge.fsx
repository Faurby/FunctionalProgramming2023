module Solution

(* Exercise 5.1 *)

let sum m n =
    let rec aux acc =
        function
        | 0 -> m + acc
        | n -> aux (m + n + acc) (n - 1)

    aux 0 n

(* Exercise 5.2 *)

let length lst =
    let rec aux acc =
        function
        | []    -> acc
        | _::xs -> aux (acc + 1) xs
    
    aux 0 lst

(* Exercise 5.3 *)

let foldBack f lst acc =
    let rec aux c acc =
        function
        | []      -> c acc
        | x :: xs -> aux (fun acc' -> c (f x acc')) acc xs

    aux id acc lst


(* Exercise 5.4 *)

let factA x =
    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (x * acc) (x - 1)

    aux 1 x

let factC =
    let rec aux c =
        function
        | 0 -> c 1
        | n -> aux (fun v -> c (n * v)) (n - 1)

    aux id


(* TODO: *)
(* Compare the running time between factA and factC. Which solution is faster and why? 
   <Your answer goes here>
*)

(* Exercise 5.5 *)

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
        | n -> aux 
            (fun x -> aux 
                (
                    fun y -> c (x + y)
                ) (n - 1)
            ) (n - 2)

    aux id


(* TODO: *)
(* Compare the running time of fibW, fibA and fibC
   <Your answer goes here>

*)

(* Exercise 5.6 *)

let rec bigListK c =
    function
    | 0 -> c []
    | n -> bigListK (fun res -> 1 :: c res) (n - 1)

(* TODO *)
(* The call bigListK id 130000 causes a stack overflow. 
   Analyse the problem and describe exactly why this happens. 
   Why is this not an iterative function?

   <Your answer goes here>
*)

type aExp =
    | N of int
    | V of string
    | WL
    | PV of aExp
    | Add of aExp * aExp
    | Sub of aExp * aExp
    | Mul of aExp * aExp
    | CharToInt of cExp


and cExp =
   | C  of char  (* Character value *)
   | CV of aExp  (* Character lookup at word index *)
   | ToUpper of cExp
   | ToLower of cExp
   | IntToChar of aExp


let rec arithEvalSimple a w s =
    let fa a = arithEvalSimple a w s
    let fc c = charEvalSimple c w s

    match a with
    | N n         -> n
    | V x         -> s |> Map.tryFind x |> Option.defaultValue 0
    | WL          -> List.length w
    | PV a        -> List.item (fa a) w |> snd
    | Add (a, b)  -> fa a + fa b
    | Mul (a, b)  -> fa a * fa b
    | Sub (a, b)  -> fa a - fa b
    | CharToInt c -> int (fc c)

and charEvalSimple c w s =
    let fa a = arithEvalSimple a w s
    let fc c = charEvalSimple c w s

    match c with
    | C c         -> c
    | CV a        -> List.item (fa a) w |> fst
    | ToUpper c   -> System.Char.ToUpper (fc c)
    | ToLower c   -> System.Char.ToLower (fc c)
    | IntToChar a -> char (fa a)

let rec arithEvalAux w s cont =
    let fa = arithEvalAux w s
    let fc = charEvalAux w s

    function
    | N n         -> cont n
    | V x         -> s |> Map.tryFind x |> Option.defaultValue 0 |> cont
    | WL          -> cont (List.length w)
    | PV a        -> fa (fun x -> List.item x w |> snd |> cont) a
    | Add (a, b)  -> fa (fun x -> fa (fun y -> cont (x + y)) b) a
    | Mul (a, b)  -> fa (fun x -> fa (fun y -> cont (x * y)) b) a
    | Sub (a, b)  -> fa (fun x -> fa (fun y -> cont (x - y)) b) a
    | CharToInt c -> fc (fun x -> cont (int x)) c

and charEvalAux w s cont =
    let fa = arithEvalAux w s
    let fc = charEvalAux w s

    function
    | C c         -> cont c
    | CV a        -> fa (fun x -> List.item x w |> fst |> cont) a
    | ToUpper c   -> fc (fun x -> cont (System.Char.ToUpper x)) c
    | ToLower c   -> fc (fun x -> cont (System.Char.ToLower x)) c
    | IntToChar a -> fa (fun x -> cont (char x)) a

let arithEval a w s = arithEvalAux w s id a
let charEval  c w s = charEvalAux w s id c


    