module Eval

    open StateMonad

    let add a b = a >>= (fun x -> b >>= fun y -> ret (x + y)) 
    let div a b = a >>= (fun x -> b >>= fun y -> if y = 0 then fail DivisionByZero else ret (x / y))

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)
    

    let rec arithEval a : SM<int> =
        match a with
        | N n  -> ret n
        | V x  -> lookup x
        | WL   -> wordLength
        | PV a -> arithEval a >>= pointValue
        | Add (a1, a2) -> 
            arithEval a1 >>=
                fun x -> 
                    arithEval a2 >>=
                        fun y -> ret (x + y)
        | Sub (a1, a2) -> 
            arithEval a1 >>=
                fun x -> 
                    arithEval a2 >>=
                        fun y -> ret (x - y)
        | Mul (a1, a2) -> 
            arithEval a1 >>=
                fun x -> 
                    arithEval a2 >>=
                        fun y -> ret (x * y)
        | Div (a1, a2) -> div (arithEval a1) (arithEval a2)
        | Mod (a1, a2) ->
            arithEval a1 >>= 
                fun x -> 
                    arithEval a2 >>=
                        fun y -> if y = 0 then fail DivisionByZero else ret (x % y)

        | CharToInt c -> charEval c >>= (int >> ret)   



    and charEval c : SM<'char> =
        match c with
        | C  c -> ret c 
        | CV a -> arithEval a >>= characterValue
        | ToUpper c   -> charEval c >>= (System.Char.ToUpper >> ret)
        | ToLower c   -> charEval c >>= (System.Char.ToLower >> ret)
        | IntToChar a -> arithEval a >>= (char >> ret)

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsDigit of cExp      (* Check for digit *)
       | IsLetter of cExp     (* Check for digit *)
       | IsVowel of cExp      (* check for vowel *)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)


    let isVowel c =
        ['a'; 'e'; 'i'; 'o'; 'u'] |>
        List.exists (fun d -> c = d || c = System.Char.ToUpper d)

    let isConsonant c =
        System.Char.IsLetter c && not (isVowel c)


    let rec boolEval =
        function
        | TT            -> ret true
        | FF            -> ret false
        | AEq (a1, a2)  -> 
            arithEval a1 >>=
                fun x -> 
                    arithEval a2 >>=
                        (fun y -> ret (x = y))
        | ALt (a1, a2)  -> 
            arithEval a1 >>=
                fun x -> 
                    arithEval a2 >>=
                        (fun y -> ret (x < y))
        | Not b         -> boolEval b >>= (not >> ret)
        | Conj (b1, b2) ->
            boolEval b1 >>=
                fun x -> 
                    boolEval b2 >>=
                        (fun y -> ret (x && y))
        | IsLetter c     -> charEval c >>= (System.Char.IsLetter >> ret)
        | IsDigit c     -> charEval c >>= (System.Char.IsDigit >> ret)
        | IsVowel c     -> charEval c >>= (isVowel >> ret)

    type stmnt =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stmnt * stmnt        (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt     (* while statement *)

    let rec stmntEval : stmnt -> SM<unit> =
        function
        | Declare x       -> declare x
        | Skip            -> ret ()
        | Ass (x, a)      -> arithEval a >>= update x
        | Seq (s1, s2)    -> stmntEval s1 >>>= stmntEval s2
        | ITE (b, s1, s2) -> boolEval b >>= (fun x -> push >>>= (if x then stmntEval s1 else stmntEval s2) >>>= pop)
        | While (b, s)    -> stmntEval (ITE (b, Seq(s, While (b, s)), Skip))

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let prog = new StateBuilder()

    let binop f a1 a2 = 
        prog {
            let! x = a1
            let! y = a2
            return f x y
        }

    let rec arithEval2 a = 
        prog {
            match a with
            | N n -> return n 
            | V x -> return! (lookup x)
            | WL  -> return! wordLength
            | Add(a1, a2) -> return! binop ( + ) (arithEval2 a1) (arithEval2 a2)
            | Sub(a1, a2) -> return! binop ( - ) (arithEval2 a1) (arithEval2 a2)
            | Mul(a1, a2) -> return! binop ( * ) (arithEval2 a1) (arithEval2 a2)
            | Div(a1, a2) ->
                let! x = arithEval2 a1
                let! y = arithEval2 a2
                if y = 0 then return! fail DivisionByZero else return x / y
            | Mod(a1, a2) ->
                let! x = arithEval2 a1
                let! y = arithEval2 a2
                if y = 0 then return! fail DivisionByZero else return x % y
            | PV a ->
                let! x = arithEval2 a
                return! pointValue x
            | CharToInt c ->
                let! x = charEval2 c
                return (int x)
        }

    and charEval2 c =
        prog {
            match c with
            | C c -> return c
            | CV a -> 
                let! x = arithEval2 a
                return! characterValue x
            | ToUpper c ->
                let! x = charEval2 c
                return (System.Char.ToUpper x)
            | ToLower c ->
                let! x = charEval2 c
                return (System.Char.ToLower x)
            | IntToChar a ->
                let! x = arithEval2 a
                return (char x)
        }

    let rec boolEval2 b =
        prog {
            match b with
            | TT -> return true
            | FF -> return false
            | ALt(a1, a2) ->
                let! x = arithEval2 a1
                let! y = arithEval2 a2
                return (x < y)
            | AEq(a1, a2) ->
                let! x = arithEval2 a1
                let! y = arithEval2 a2
                return (x = y)
            | Conj(b1, b2) ->
                let! x = boolEval2 b1
                let! y = boolEval2 b2
                return (x && y)
            | Not b ->
                let! x = boolEval2 b
                return (not x)
            | IsVowel c ->
                let! x = charEval2 c
                return (isVowel x)
            | IsDigit c ->
                let! x = charEval2 c
                return (System.Char.IsDigit x)
            | IsLetter c ->
                let! x = charEval2 c
                return (System.Char.IsLetter x)

        }

    let rec stmntEval2 stm =
        prog {
            match stm with
            | Declare x   -> return! declare x
            | Ass (x, a) ->
                let! v = arithEval2 a
                return! update x v
            | Skip        -> return ()
            | Seq(s1, s2) ->
                do! stmntEval2 s1
                do! stmntEval2 s2
            | ITE(b, s1, s2) ->
                let! x = boolEval2 b
                do! push
                if x then do! stmntEval2 s1 else do! stmntEval2 s2
                do! pop
            | While(b, s) -> return! stmntEval2 (ITE(b, Seq (s, While (b, s)), Skip))
        }

    open Types

    let stmntToSquareFun stm =
        fun w pos acc ->
            stmntEval stm >>>= lookup "_result_" |>
            evalSM (mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] w (["_pos_"; "_acc_"; "_result_"]))
          
    
    type squareStmnt = Map<int, stmnt>  
    let stmntsToSquare : squareStmnt -> square = Map.map (fun _ -> stmntToSquareFun)
    
    let getSuccess = function Success x -> x
    
    let calculatePointsAux (squares : (int * squareFun) list list) (w : word) =
        squares |>
        List.mapi (fun i -> List.map (fun (p, f) -> (p, fun x -> getSuccess (f w i x)))) |>
        List.fold (@) [] |>
        List.sortBy fst |>
        List.map snd |>
        List.fold (>>) id <|
        0
        
    let calculatePoints (squares : square list) w = 
        squares |>
        List.map Map.toList |>
        calculatePointsAux <| w

    let stmntToBoardFun stm m = 
        fun (x, y) ->
            stmntEval stm >>>= lookup "_result_" >>= (fun r -> ret (Map.tryFind r m)) |>
            evalSM (mkState [("_x_", x); ("_y_", y); ("_result_", 0)] [] (["_x_"; "_y_"; "_result_"]))

    let mkBoard c defaultSq boardStmnt sqs =
        let squares = Map.map (fun _ -> stmntsToSquare) sqs
        {
            center = c;
            defaultSquare = Map.find defaultSq squares
            squares = stmntToBoardFun boardStmnt squares
        }

    let hello = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1)]
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
