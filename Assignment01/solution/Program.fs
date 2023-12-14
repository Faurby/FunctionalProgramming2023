module solution

//Green Exercises
    open System

    //Exercise 1.1
    let sqr x = x * x

    //Exercise 1.2
    let pow x n = Math.Pow(x, n)

    //Exercise 1.3 (based on 1.4 from HR)
    let rec sum =
        function
        | 0 -> 0
        | n -> n + sum (n - 1)

    //Exercise 1.4 (based on 1.5 from HR)
    let rec fib = 
        function
        | 0 -> 0
        | 1 -> 1
        | x -> fib (x - 1) + fib (x - 2)

    //Exercise 1.5
    let dup (s : string) = s + s

    //Exercise 1.6
    let rec dupn s =
        function
        | 0 -> ""
        | x -> s + dupn s (x - 1)

    //Exercise 1.7 (based on 2.8 from HR)
    let rec bin =
        function
        | (0, 0) -> 1
        | (n, k) when k = 0 || k = n -> 1
        | (n, k) -> bin (n - 1, k - 1) + bin (n - 1, k)

//Yellow Exercises

    //Exercise 1.8
    let timediff (h1, m1) (h2, m2) = (h2 - h1) * 60 + (m2 - m1)

    //Exercise 1.9
    let minutes t = timediff (0, 0) t

    //Exercise 1.10 (based on 2.13 from HR)
    let curry f a b = f (a, b)
    let uncurry f (a, b) = f a b

    //Assignment 1.11
    let empty def : int -> char * int = fun _ -> def

//Red Exercises

    //Assignment 1.12
    let add newPos cv word =
        function
        | pos when newPos = pos -> cv
        | pos            -> word pos

    //Assignment 1.13
    let hello = 
        empty (char 0, 0) |>
        add 0 ('H', 4) |>
        add 1 ('E', 1) |>
        add 2 ('L', 1) |>
        add 3 ('L', 1) |>
        add 4 ('O', 1)

    //Assignment 1.14
    let singleLetterScore f i = snd (f i)
    let doubleLetterScore f i = 2 * snd (f i)
    let trippleLetterScore f i = 3 * snd (f i)

    //The following is just examples from 1.14, and therefore not necessary for the solution
    let result =
        trippleLetterScore hello 0 +
        singleLetterScore hello 1 +
        singleLetterScore hello 2 +
        singleLetterScore hello 3 +
        doubleLetterScore hello 4