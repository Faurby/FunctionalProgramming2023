module StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> =
        S (fun s ->
              match s.vars with
              | []       -> failwith "empty state"
              | _ :: vs -> Success ((), {s with vars = vs}))            

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    let declare (x : string) : SM<unit> =
        S (fun s -> 
              if Set.contains x s.reserved then 
                  Failure (ReservedName x)
              else
                  match s.vars with 
                  | [] -> failwith "empty state"
                  | m :: vs -> 
                      if Map.containsKey x m then
                          Failure (VarExists x)
                      else
                          Success ((), {s with vars = Map.add x 0 m :: vs}))

    let update x v : SM<unit> =
        let rec aux = 
            function
            | [] -> None
            | m :: ms ->
                if Map.containsKey x m then
                    Some (Map.add x v m::ms)
                else
                    match aux ms with
                    | Some ms' -> Some (m :: ms')
                    | None     -> None

        S (fun s ->
              match aux s.vars with
              | Some vs -> Success ((), {s with vars = vs})
              | None     -> Failure (VarNotFound x))

    let wordLength : SM<int> =
        S (fun s -> Success (s.word |> List.length, s))

    let characterValue pos : SM<char> =
        S (fun s -> 
              if pos < 0 || pos >= List.length s.word then
                  Failure (IndexOutOfBounds pos)
              else
                  Success (List.item pos s.word |> fst, s))

    let pointValue pos : SM<int> =
        S (fun s -> 
              if pos < 0 || pos >= List.length s.word then
                  Failure (IndexOutOfBounds pos)
              else
                  Success (List.item pos s.word |> snd, s))

    