// ---- Green Exercises ----
// Exercise 3.1

type aExp =
| N of int           // Integer value
| V of string        // Variable
| WL                 // Length of the word
| PV of aExp         // Point value of character at specific word index
| Add of aExp * aExp // Addition
| Sub of aExp * aExp // Subtraction
| Mul of aExp * aExp // Multiplication

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

let a1 = N 42;;
let a2 = N 4 .+. (N 5 .-. N 6)
let a3 = N 4 .*. N 2 .+. N 34
let a4 = (N 4 .+. N 2) .*. N 34
let a5 = N 4 .+. (N 2 .*. N 34)

let rec arithEvalSimple =
    function
    | N x -> x
    | Add (x, y) -> arithEvalSimple x + arithEvalSimple y
    | Sub (x, y) -> arithEvalSimple x - arithEvalSimple y
    | Mul (x, y) -> arithEvalSimple x * arithEvalSimple y

arithEvalSimple a1;; // 42
arithEvalSimple a2;; // 3
arithEvalSimple a3;; // 42
arithEvalSimple a4;; // 204
arithEvalSimple a5;; // 72

// Exercise 3.2
let a6 = V "x"
let a7 = N 4 .+. (V "y" .-. V "z")

let rec arithEvalState a s =
    match a with
    | N x -> x
    | V x -> Map.tryFind x s |> Option.defaultValue 0
    // Alt solutions:
    // | V x -> 
    //     match Map.tryFind x s with
    //     | Some s -> s
    //     | None -> 0
    // | V x -> 
    //     try 
    //         Map.find x s
    //     with
    //       | :? System.Collections.Generic.KeyNotFoundException -> 0
    | Add (x, y) -> arithEvalState x s + arithEvalState y s
    | Sub (x, y) -> arithEvalState x s - arithEvalState y s
    | Mul (x, y) -> arithEvalState x s * arithEvalState y s

arithEvalState a6 (Map.ofList [("x", 5)])           // 5
arithEvalState a6 (Map.ofList [("y", 5)])           // 0
arithEvalState a7 (Map.ofList [("x", 4); ("y", 5)]) // 9
arithEvalState a7 (Map.ofList [("y", 4); ("z", 5)]) // 3

type word = (char * int) list

// Exercise 3.3

let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_");;
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithDoubleWordScore = N 2 .*. V "_acc_";;
let arithTripleWordScore = N 3 .*. V "_acc_";;

let hello = ('H', 4)::('E', 1)::('L', 1)::('L', 1)::('O', 1)::[]

let rec arithEval a (w: word) s =
    match a with
    | N x -> x
    | V x -> Map.tryFind x s |> Option.defaultValue 0
    | WL -> List.length w
    | PV x -> snd w.[arithEval x w s]
    | Add (x, y) -> arithEval x w s + arithEval y w s
    | Sub (x, y) -> arithEval x w s - arithEval y w s
    | Mul (x, y) -> arithEval x w s * arithEval y w s

arithEval WL [] Map.empty;; // 0
arithEval WL hello Map.empty;; // 5
arithEval (PV (N 0)) hello Map.empty;; // 4
arithEval arithSingleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 0)]);;  // 1
arithEval arithSingleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 42)]);; // 43
arithEval arithDoubleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 0)]);;  // 2
arithEval arithDoubleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 42)]);; // 44
arithEval arithTripleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 0)]);;  // 3
arithEval arithTripleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 42)]);; // 45

// Exercise 3.4
type cExp =
    | C of char      (* Character value *)
    | ToUpper of cExp (* Converts lower case to upper case character, non-letters are unchanged *)
    | ToLower of cExp (* Converts upper case to lower case character, non-letters are unchanged *)
    | CV of aExp      (* Character lookup at word index *)

let rec charEval c (w: word) s =
    match c with
    | C c       -> c
    | ToUpper c -> System.Char.ToUpper (charEval c w s)
    | ToLower c -> System.Char.ToLower (charEval c w s)
    | CV a      -> fst w.[arithEval a w s]

charEval (C 'H') [] Map.empty;; // 'H'
charEval (ToLower (CV (N 0))) hello Map.empty;; // 'h'
charEval (ToUpper (C 'h')) [] Map.empty;; // 'H'
charEval (ToLower (C '*')) [] Map.empty;; // '*'
charEval (CV (V "x" .-. N 1)) hello (Map.ofList [("x", 5)]);; // 'O'

// Exercise 3.5
type bExp =
    | TT (* true *)
    | FF (* false *)
    | AEq of aExp * aExp (* numeric equality = *)
    | ALt of aExp * aExp (* numeric less than < *)
    | Not of bExp (* boolean not ! *)
    | Conj of bExp * bExp (* boolean conjunction *)
    | IsDigit of cExp (* check for digit *)
    | IsLetter of cExp (* check for letter *)
    | IsVowel of cExp (* check for vowel *)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)
let (.=.) a b = AEq (a, b)
let (.<.) a b = ALt (a, b)
let (.<>.) a b = ~~(a .=. b) (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b) (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b) (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

let isVowel c =
    match System.Char.ToUpper c with
    | 'A' | 'E' | 'Y' | 'U' | 'I' | 'O' -> true
    | _ -> false

let rec boolEval b w s =
    match b with
    | TT -> true
    | FF -> false
    | AEq (a1, a2) -> arithEval a1 w s = arithEval a2 w s
    | ALt (a1, a2) -> arithEval a1 w s < arithEval a2 w s
    | Not b -> not (boolEval b w s)
    | Conj (b1, b2) -> boolEval b1 w s && boolEval b2 w s
    | IsDigit c -> System.Char.IsDigit (charEval c w s)
    | IsLetter c -> System.Char.IsLetter (charEval c w s)
    | IsVowel c -> isVowel (charEval c w s)

boolEval TT [] Map.empty;; // true
boolEval FF [] Map.empty;; // false
boolEval ((V "x" .+. V "y") .=. (V "y" .+. V "x")) [] (Map.ofList [("x", 5); ("y", 7)]);; // true
boolEval ((V "x" .+. V "y") .=. (V "y" .-. V "x")) [] (Map.ofList [("x", 5); ("y", 7)]);; // false
boolEval (IsLetter (CV (V "x"))) hello (Map.ofList [("x", 4)]);; // true
boolEval (IsLetter (CV (V "x"))) (('1', 0)::hello) (Map.ofList [("x", 0)]);; // false
boolEval (IsDigit (CV (V "x"))) hello (Map.ofList [("x", 4)]);; // false
boolEval (IsDigit (CV (V "x"))) (('1', 0)::hello) (Map.ofList [("x", 0)]);; // true

// ---- YELLOW EXERCISES ----
// Exercise 3.6
let isConsonant c = Not (IsVowel c)

// Examples
boolEval (isConsonant (C 'H')) [] Map.empty // true
boolEval (isConsonant (C 'h')) [] Map.empty // true
boolEval (isConsonant (C 'A')) [] Map.empty // false
boolEval (isConsonant (CV (V "x"))) hello (Map.ofList [("x", 0)]);; // true
boolEval (isConsonant (CV (V "x"))) hello (Map.ofList [("x", 1)]);; // false


// Exercise 3.7

type stmnt =
    | Skip (* does nothing *)
    | Ass of string * aExp (* variable assignment *)
    | Seq of stmnt * stmnt (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt (* while statement *)

let rec evalStmnt stm w s =
    match stm with
    | Skip -> s
    | Ass (x, a) -> 
        let v = arithEval a w s
        Map.add x v s
    | Seq (stm1, stm2) ->
        let s' = evalStmnt stm1 w s
        evalStmnt stm2 w s'
    | ITE (guard, stm1, stm2) ->
        let b = boolEval guard w s
        if b then evalStmnt stm1 w s
        else evalStmnt stm2 w s
    | While (guard, stm) ->
        let b = boolEval guard w s
        if b then 
            let s' = evalStmnt stm w s
            evalStmnt (While (guard, stm)) w s'
        else
            s

// Examples
evalStmnt Skip [] Map.empty;; // map []
evalStmnt (Ass ("x", N 5)) [] Map.empty;; // map [("x", 5)]
evalStmnt (Seq (Ass ("x", WL), Ass ("y", N 7))) hello Map.empty;; // map[("x", 5); ("y, 7")]
evalStmnt (ITE (WL .>=. N 5, Ass ("x", N 1), Ass ("x", N 2))) hello Map.empty;; // map [("x", 1)]
evalStmnt (ITE (WL .<. N 5, Ass ("x", N 1), Ass ("x", N 2))) hello Map.empty;; //map [("x", 2)]
evalStmnt (While (V "x" .<=. WL, Seq (Ass ("y", V "y" .+. V "x"), Ass ("x", V "x" .+. N 1)))) hello Map.empty;; // map [("x", 6); ("y", 15)]
evalStmnt (While (V "x" .<=. WL, Seq (Ass ("y", V "y" .+. V "x"), Ass ("x", V "x" .+. N 1)))) hello (Map.ofList [("x", 3); ("y", 100)]);; // map [("x", 6); ("y", 112)]

// Exercise 3.8
type squareFun = word -> int -> int -> int

let stmntToSquareFun stm w pos acc =
    evalStmnt stm w (Map.ofList [("_pos_", pos); ("_acc_", acc)])
    |> Map.find "_result_"

let singleLetterScore = stmntToSquareFun (Ass ("_result_", arithSingleLetterScore))
let doubleLetterScore = stmntToSquareFun (Ass ("_result_", arithDoubleLetterScore))
let tripleLetterScore = stmntToSquareFun (Ass ("_result_", arithTripleLetterScore))
let doubleWordScore = stmntToSquareFun (Ass ("_result_", arithDoubleWordScore))
let tripleWordScore = stmntToSquareFun (Ass ("_result_", arithTripleWordScore))

let containsNumbers =
    stmntToSquareFun
        (Seq (Ass ("_result_", V "_acc_"),
            While (V "i" .<. WL,
                ITE (IsDigit (CV (V "i")),
                    Seq (
                        Ass ("_result_", V "_result_" .*. N -1),
                        Ass ("i", WL)),
                    Ass ("i", V "i" .+. N 1)))))

singleLetterScore hello 0 0;; // 4
doubleLetterScore hello 0 0;; // 8
tripleLetterScore hello 0 0;; // 12
singleLetterScore hello 0 42;; // 46
doubleLetterScore hello 0 42;; // 50
tripleLetterScore hello 0 42;; // 54
containsNumbers hello 5 50;; // 50
containsNumbers (('0', 100)::hello) 5 50;; // -50
containsNumbers (hello @ [('0', 100)]) 5 50;; // -50

// ---- RED EXERCISES ----
// Exercise 3.9

let oddConsonants = 
    Seq (Ass ("_result_", V "_acc_"),
                While (V "i" .<. WL,
                        Seq(
                            ITE (isConsonant (CV (V "i")),
                                Ass ("_result_", V "_result_" .*. N -1),
                                Skip),
                            Ass ("i", V "i" .+. N 1))))

// Exercise 3.10
type square2 = (int * stmnt) list

let SLS = [(0, Ass ("_result_", arithSingleLetterScore))]
let DLS = [(0, Ass ("_result_", arithDoubleLetterScore))]
let TLS = [(0, Ass ("_result_", arithTripleLetterScore))]
let DWS = [(1, Ass ("_result_", arithDoubleWordScore))] @ SLS
let TWS = [(1, Ass ("_result_", arithTripleWordScore))] @ SLS

let calculatePoints squares w =
        squares |>
        List.mapi (fun i -> List.map (fun (p, f) -> (p, f w i))) |>
        List.fold (@) [] |>
        List.sortBy fst |>
        List.map snd |>
        List.fold (>>) id <|
        0

let calculatePoints2 squares w = 
        squares |>
        List.map (List.map (fun (p, stm) -> p, stmntToSquareFun stm)) |>
        fun res -> calculatePoints res w

calculatePoints2 [DLS; SLS; TLS; SLS; DWS] hello;;
calculatePoints2 [DLS; DWS; TLS; TWS; DWS] hello;;