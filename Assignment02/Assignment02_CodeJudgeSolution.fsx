module Assignment2
// Exercise 2.1
    let rec downto1 x =
        if x <= 0 then 
            []
        else
            x :: downto1 (x - 1)

    let rec downto2 =
        function
        | x when x <= 0 -> []
        | x             -> x :: downto2 (x - 1)

// Exercise 2.2
    let rec removeOddIdx =
        function
        | []       -> []
        | [x]      -> [x]
        | x::_::xs -> x :: removeOddIdx xs

// Exercise 2.3
    let rec combinePair =
        function
        | []       -> []
        | [_]      -> []
        | x::y::xs -> (x, y)::(combinePair xs)

// Exercise 2.4
    type complex = float*float
    let mkComplex a b : complex = (a,b)
    let complexToPair = id
    let (|+|) (a,b) (c,d) : complex = (a+c,b+d)
    let (|*|) (a,b) (c,d) : complex = (a*c-b*d,b*c+a*d)
    let (|-|) (a : float,b : float) (c,d) : complex = (a,b) |+| (-c,-d)
    let (|/|) (a, b) (c,d) : complex =
      let di = c*c + d*d
      if di=0.0
        then failwith "Complex div"
        else (a,b) |*| (c/di,-d/di)

// Exercise 2.5
    let explode1 (s : string) = [for c in s -> c]

    let rec explode2 (s:string) = 
      if s.Length > 0 then s.Chars 0 :: explode2 (s.Remove (0,1)) else []

// Exercise 2.6
    let implode lst = List.foldBack ((+) << string) lst ""
    let implodeRev = List.fold (fun acc c -> string c + acc) ""

// Exercise 2.7
    let toUpper s = 
        s |> 
        explode1 |>
        List.map (System.Char.ToUpper) |>
        implode

// Exercise 2.8
    let rec ack (m, n) =
      match (m, n) with
        (0, n) when n>=0         -> n + 1
      | (m, 0) when m>=0         -> ack (m - 1, 1)
      | (m, n) when m>=0 && n>=0 -> ack (m - 1, ack (m, n - 1))
      | _ -> failwith"Ack with negative numbers"


// YELLOW EXERCISES
// Exercise 2.10
    let rec downto3 f =
        function
        | n when n <= 0 -> id
        | n             -> f n >> downto3 f (n - 1)

    let rec downto4 f n = List.foldBack (fun n acc -> f n >> acc) [0..n] id
    let rec downto5 f n = List.foldBack (fun n -> (>>) (f n)) [0..n] id

    let fac n = downto3 (fun n e -> n * e) n 1

    let range g n = downto3 (fun n xs -> g n :: xs) n []

// Exercise 2.11
    type word = (char * int) list


    let hello = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1)]

    type squareFun = (char * int) list -> int -> int -> int

// Exercise 2.12
    let singleLetterScore w pos acc = snd (List.item pos w) + acc 
    let doubleLetterScore w pos acc = 2 * snd (List.item pos w) + acc
    let tripleLetterScore w pos acc = 3 * snd (List.item pos w) + acc 

// Exercise 2.13
    let doubleWordScore w pos acc = acc * 2
    let tripleWordScore w pos acc = acc * 3

// Exercise 2.14
    type square = (int * squareFun) list

    let isVowel c = List.exists ((=) (System.Char.ToUpper c)) ['A'; 'E'; 'O'; 'I'; 'U']
    let isConsonant c = System.Char.IsLetter c && not (isVowel c)

    let oddConsonants cs _ acc =
        let count = 
            cs |>
            List.map fst |>
            List.filter isConsonant |>
            List.length

        if count % 2 = 0 then acc else -acc

// RED EXERCISE
// Exercise 2.15

    let calculatePoints (squares : square list) (w : word) =
        squares |>
        List.mapi (fun i -> List.map (fun (p, f) -> (p, f w i))) |>
        List.fold (@) [] |>
        List.sortBy fst |>
        List.map snd |>
        List.fold (>>) id <|
        0

    let SLS : square = [(0, singleLetterScore)]
    let DLS : square = [(0, doubleLetterScore)]
    let TLS : square = [(0, tripleLetterScore)]

    let DWS : square = SLS @ [(1, doubleWordScore)]
    let TWS : square = SLS @ [(1, tripleWordScore)]