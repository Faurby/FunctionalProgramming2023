module Assignment3
    
    let isVowel c =
        ['a'; 'e'; 'i'; 'o'; 'u'] |>
        List.exists (fun d -> c = d || c = System.Char.ToUpper d)

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)

    let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_")
    let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_")
    let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_")

    let arithDoubleWordScore = N 2 .*. V "_acc_"
    let arithTripleWordScore = N 3 .*. V "_acc_"

    let rec arithEvalSimple =
        function
        | N n        -> n
        | Add (a, b) -> arithEvalSimple a + arithEvalSimple b
        | Sub (a, b) -> arithEvalSimple a - arithEvalSimple b
        | Mul (a, b) -> arithEvalSimple a * arithEvalSimple b
        | _          -> failwith "Unsupported arithmetic expression"

    let rec arithEvalState =
        function
        | N n        -> fun _ -> n
        | V x        -> Map.tryFind x >> Option.defaultValue 0
        | Add (a, b) -> fun s -> arithEvalState a s + arithEvalState b s
        | Sub (a, b) -> fun s -> arithEvalState a s - arithEvalState b s
        | Mul (a, b) -> fun s -> arithEvalState a s * arithEvalState b s
        | _          -> failwith "Unsupported arithmetic expression"
    
    let rec arithEval =
        function
        | N n        -> fun _ _ -> n
        | V x        -> fun _   -> Map.tryFind x >> Option.defaultValue 0
        | WL         -> fun w _ -> List.length w
        | PV a       -> fun w s -> List.item (arithEval a w s) w |> snd
        | Add (a, b) -> fun w s -> arithEval a w s + arithEval b w s
        | Sub (a, b) -> fun w s -> arithEval a w s - arithEval b w s
        | Mul (a, b) -> fun w s -> arithEval a w s * arithEval b w s

    type cExp =
       | C  of char      (* Character value *)
       | ToUpper of cExp (* Converts lower case to upper case character, non-characters unchanged *)
       | ToLower of cExp (* Converts upper case to lower case character, non characters unchanged *)
       | CV of aExp      (* Character lookup at word index *)

    let rec charEval =
        function
        | C c       -> fun _ _ -> c
        | ToUpper c -> fun w s -> System.Char.ToUpper (charEval c w s)
        | ToLower c -> fun w s -> System.Char.ToLower (charEval c w s)
        | CV a      -> fun w s -> List.item (arithEval a w s) w |> fst

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp           (* boolean not *)
       | Conj of bExp * bExp   (* boolean conjunction *)

       | IsDigit of cExp       (* check for digit *)
       | IsLetter of cExp      (* check for letter *)
       | IsVowel of cExp       (* check for vowel *)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)


    let rec boolEval =
        function 
        | TT            -> fun _ _ -> true
        | FF            -> fun _ _ -> false

        | AEq (a1, a2)  -> fun w s -> arithEval a1 w s = arithEval a2 w s
        | ALt (a1, a2)  -> fun w s -> arithEval a1 w s < arithEval a2 w s

        | Not b         -> fun w s -> not (boolEval b w s)
        | Conj (b1, b2) -> fun w s -> boolEval b1 w s && boolEval b2 w s

        | IsVowel c     -> fun w s -> isVowel (charEval c w s)
        | IsLetter c    -> fun w s -> System.Char.IsLetter (charEval c w s)
        | IsDigit c     -> fun w s -> System.Char.IsDigit (charEval c w s)
        
    let isConsonant c =  Conj (Not (IsVowel c), IsLetter c)

    type stmnt =
       | Skip                        (* does nothing *)
       | Ass of string * aExp        (* variable assignment *)
       | Seq of stmnt * stmnt        (* sequential composition *)
       | ITE of bExp * stmnt * stmnt (* if-then-else statement *)    
       | While of bExp * stmnt       (* while statement *)

    let rec evalStmnt stm w s =
        match stm with
        | Skip                -> s
        | Ass (x, a)          -> Map.add x (arithEval a w s) s
        | Seq (stm1, stm2)    -> evalStmnt stm2 w (evalStmnt stm1 w s)
        | ITE (b, stm1, stm2) -> if boolEval b w s then evalStmnt stm1 w s else evalStmnt stm2 w s
        | While (b, stm)      -> evalStmnt (ITE (b, Seq (stm, While (b, stm)), Skip)) w s

    let stmntToSquareFun stm w pos acc = 
        evalStmnt stm w (Map.ofList [("_pos_", pos); ("_acc_", acc)]) |>
        Map.find "_result_"

    let singleLetterScore = stmntToSquareFun (Ass ("_result_", arithSingleLetterScore))
    let doubleLetterScore = stmntToSquareFun (Ass ("_result_", arithDoubleLetterScore))
    let tripleLetterScore = stmntToSquareFun (Ass ("_result_", arithTripleLetterScore))

    let doubleWordScore = stmntToSquareFun (Ass ("_result_", arithDoubleWordScore))
    let tripleWordScore = stmntToSquareFun (Ass ("_result_", arithTripleWordScore))

    let oddConsonants = 
        Seq (Ass ("_result_", V "_acc_"),
                  While (V "i" .<. WL,
                         Seq(
                             ITE (isConsonant (CV (V "i")),
                                  Ass ("_result_", V "_result_" .*. N -1),
                                  Skip),
                             Ass ("i", V "i" .+. N 1))))

    
    
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


    let hello = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1)]
   