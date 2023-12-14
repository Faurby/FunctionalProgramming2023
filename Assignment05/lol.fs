


f : 'a -> 'b



append [0,1,2] [3,4,5]

0 ::  append [1,2] [3,4,5]
0 :: 1 :: append [2] [3,4,5]
0 :: 1 :: 2 :: [3,4,5]
0 :: 1 :: [2,3,4,5]
0 :: [1,2,3,4,5]
[0,1,2,3,4,5]


appendC [0,1,2] [3,4,5] id
appendC [1,2] [3,4,5] (fun r -> id (0 :: r))
appendC [2] [3,4,5] (fun r -> (fun r -> c (0 :: r)) (1 :: r))
appendC [] [3,4,5] (fun r -> (fun r -> (fun r -> c (0 :: r)) (1 :: r)) (2 :: r))
(fun r -> (fun r -> (fun r -> c (0 :: r)) (1 :: r)) (2 :: r)) [3,4,5]
(fun r -> (fun r -> c (0 :: r)) (1 :: r)) [2,3,4,5]
(fun r -> id (0 :: r)) [1,2,3,4,5]
id [0,1,2,3,4,5]
[0,1,2,3,4,5]


foldBack (+) [0,1,2] 10
helper [0,1,2] id
helper [1,2] (fun acc2 -> id ((+) 0 acc2))

helper [2]  (fun acc2 -> (fun acc2 -> id ((+) 0 acc2))  ((+) 1 acc2))

helper [] (fun acc2 -> (fun acc2 -> (fun acc2 -> id ((+) 0 acc2)) ((+) 1 acc2)) ((+) 2 acc2))
(fun acc2 -> (fun acc2 -> (fun acc2 -> id ((+) 0 acc2)) ((+) 1 acc2)) ((+) 2 acc2)) 10
(fun acc2 -> (fun acc2 -> id ((+) 0 acc2)) ((+) 1 acc2)) ((+) 2 10)
(fun acc2 -> (fun acc2 -> id ((+) 0 acc2)) ((+) 1 acc2)) (12)
(fun acc2 -> id ((+) 0 acc2)) ((+) 1 12)
(fun acc2 -> id ((+) 0 acc2)) (13)
id ((+) 0 13)
id 13
13



(fun str -> str.allCaps) (sayHello "Bob")