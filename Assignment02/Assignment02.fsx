// ---- GREEN EXERCISES ----
// Exercise 2.1

let rec downto1 n =
    if n > 0
    then n :: downto1 (n-1)
    else []


let rec downto2 n =
    match n with
    | n when n > 0 -> n :: (downto1 (n-1))
    | _ -> []

// Exercise 2.2
let rec removeOddIdx xs =
    match xs with
    | x :: _ :: xs -> x :: removeOddIdx xs
    | x :: [] -> [x]
    | _ -> []

// Exercise 2.3
let rec combinePair xs : ('a * 'a) list=
    match xs with
    | x :: y :: xs -> (x, y) :: combinePair xs
    | _ :: [] -> []
    | [] -> []

// Exercise 2.4
type complex = float * float

let mkComplex a b : complex = (a, b)

let complexToPair (c: complex) = (fst c, snd c)

let (|+|) ((a, b):complex) ((c, d):complex) = (a+c, b+d)
let (|*|) ((a, b):complex) ((c, d):complex) = (a*c - b*d, b*c + a*d)

let (~-) ((a, b):complex) = (-a, -b)
let (~&) ((a, b):complex) = (a / (a**2. + b**2.), 0.0 - b / (a**2. + b**2.))

let (|-|) (a: complex) (b: complex) = a |+| (-b)
let (|/|) (a: complex) (b: complex) = a |*| (&b)

// Exercise 2.5
let explode1 (s: string) = Seq.toList s

let rec explode2 s = 
    match s with
    | "" -> []
    | s  -> s.[0] :: explode2 (s.[1..]) 
//  | s  -> s.[0] :: explode2 (s.Remove(0,1)) or this

// Exercise 2.6
let implode (lst: char list) = List.foldBack (fun elem acc -> string(elem) + acc) lst ""
let implodeRev (lst: char list) = List.fold (fun acc elem -> string(elem) + acc) "" lst

// Exercise 2.7
// https://fsharpforfunandprofit.com/posts/function-composition/
let toUpper s = s |> explode1 |> List.map (fun x -> System.Char.ToUpper x) |> implode
let toUpper2 = explode1 >> List.map System.Char.ToUpper >> implode

// Exercise 2.8
let rec ack =
    function
    | (m, n) when m = 0 -> n + 1
    | (m, n) when m > 0 && n = 0 -> ack (m-1, 1)
    | (m, n) when m > 0 && n > 0 -> ack (m-1, ack(m, n-1))
    | (_, _) -> 0

ack(10, 0)

// ---- YELLOW EXERCISES ----
// Exercise 2.9
let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start)

time (fun () -> ack (3, 11))

let timeArg1 f a = time (fun () -> f a)

// Exercise 2.10
let rec downto3 f (n: int) e =
    if n <= 0 then e
    else downto3 f (n-1) (f n e)

let rec downto32 f (n: int) e =
    match n with
    | n when n <= 0 -> e
    | n -> downto3 f (n-1) (f n e)

let fac a = downto3 (*) a 1
let range g n = downto3 (fun elem acc -> (g elem) :: acc) n []


// Exercise 2.11
type word = (char * int) list                                                                                                                                                                               
let hello = ('H', 4)::('E', 1)::('L', 1)::('L', 1)::('O', 1)::[]

type squareFun = word -> int -> int -> int
//                w      pos    acc    result

// Exercise 2.12
let singleLetterScore : squareFun = fun w pos acc -> (snd (w.[pos])) + acc

let singleLetterScore2 : squareFun = 
    fun w pos acc -> 
        let (_, point) = w.[pos]
        2 * point + acc

let singleLetterScore3 (w: word) (pos: int) (acc: int) : int = 
        let (_, point) = w.[pos]
        2 * point + acc

let doubleLetterScore : squareFun = fun w pos acc -> 2 * (snd (w.[pos])) + acc
let tripleLetterScore : squareFun = fun w pos acc -> 3 * (snd (w.[pos])) + acc

// Exercise 2.13
let doubleWordScore : squareFun = fun _ _ acc -> acc * 2
let tripleWordScore : squareFun = fun _ _ acc -> acc * 3

// Exercise 2.14
let isConsonant c =
    match System.Char.ToUpper c with
    | 'A' | 'E' | 'Y' | 'U' | 'I' | 'O' | 'Æ' | 'Ø' | 'Å' -> false
    | _ -> true 

let countConsonants (w: word) = List.fold (fun acc (elem, _) -> if isConsonant elem then 1 + acc else acc) 0 w

let oddConsonants : squareFun = 
    fun w _ acc -> 
        if (countConsonants w) % 2 <> 0 then acc * -1
        else acc

// ---- RED EXERCISES ----
// Exercise 2.15
type square = (int * squareFun) list
//              ^         ^
//          priority    function to calculate points

let SLS : square = [(0, singleLetterScore)]
let DLS : square = [(0, doubleLetterScore)]
let TLS : square = [(0, tripleLetterScore)]
let DWS : square = SLS @ [(1, doubleWordScore)]
let TWS : square = SLS @ [(1, tripleWordScore)]

let calculatePoints (lst: square list) (w: word) = 
    let a =
        List.mapi (fun i square -> List.map (fun (priority, f) -> (priority, f w i)) square) lst
        |> List.fold (@) []
        |> List.sortBy (fun x -> fst x)
        |> List.map (fun x -> snd x)
        |> List.fold (>>) id 
    a 0

let calculatePoints2 (lst: square list) (w: word) = 
        List.mapi (fun i square -> List.map (fun (priority, f) -> (priority, f w i)) square) lst
        |> List.fold (@) []
        |> List.sortBy (fun x -> fst x)
        |> List.map (fun x -> snd x)
        |> List.fold (>>) id 
        <| 0

let calculatePoints3 : square list -> word -> int =
    fun lst w ->
        List.mapi (fun i square -> List.map (fun (priority, f) -> (priority, f w i)) square) lst
        |> List.fold (@) []
        |> List.sortBy (fun x -> fst x)
        |> List.map (fun x -> snd x)
        |> List.fold (>>) id 
        <| 0

let calculatePoints4 (lst: square list) (w: word) =
        List.mapi (fun i square -> List.map (fun (priority, f) -> (priority, f w i)) square) lst
        |> List.fold (@) []
        |> List.sortBy (fun x -> fst x)
        |> List.map (fun x -> snd x)
        |> List.fold (fun acc elem -> elem acc) 0

(*We go from   
   [
     [(0, DLS)];
     [(0, SLS)];
     [(0, TLS)];
     [(0, SLS)];
     [(0, SLS); (1, DWS)]
   ]

    to

   [
     [(0, DLS hello 0)];
     [(0, SLS hello 1)];
     [(0, TLS hello 2)];
     [(0, SLS hello 3)];
     [(0, SLS hello 4); (1, DWS hello 4)]
   ]

    to

   [
     (0, DLS hello 0);
     (0, SLS hello 1);
     (0, TLS hello 2);
     (0, SLS hello 3);
     (0, SLS hello 4);
     (1, DWS hello 4);
   ]

   to

     [
     (0, DLS hello 0);
     (0, SLS hello 1);
     (0, TLS hello 2);
     (0, SLS hello 3);
     (0, SLS hello 4);
     (1, DWS hello 4);
   ]

   to

   [
     (DLS hello 0);
     (SLS hello 1);
     (TLS hello 2);
     (SLS hello 3);
     (SLS hello 4);
     (DWS hello 4);
   ]

   to

    (DLS hello 0) 
     >> (SLS hello 1) 
     >> (TLS hello 2) 
     >> (SLS hello 3) 
     >> (SLS hello 4)
     >> (DWS hello 4)
    
    which must take an acc as initial value (e.g. 0)

    which gives the total = 28.
*)