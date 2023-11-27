-- Q1
-- Base case: n = 0
revk [] id = id [] = []

-- n = k
revk [a_k,a_{k+1}...,a_n] (\yk -> yk ++ [a_{k-1},...,a_1]) 
= revk [a_{k+1}...,a_n] (\yk1 -> (\yk -> yk + [a_{k-1},...,a_1]) (yk1 ++ a_k))
= revk [a_{k+1}...,a_n] (\yk1 -> (yk1 ++ [a_k]) ++ [a_{k-1},...,a_1])
= revk [a_{k+1}...,a_n] (\yk1 -> yk1 ++ [a_k, a_{k-1},...,a_1])

-- Q2 
fibk 4 Show
= fibk 3 (FibAdd 4 Show)
= fibk 2 (FibAdd 3 (FibAdd 4 Show))
= fibk 1 (FibAdd 2 (FibAdd 3 (FibAdd 4 Show)))
= appcont (FibAdd 2 (FibAdd 3 (FibAdd 4 Show))) 1
= fibk 0 (Add 1 (FibAdd 3 (FibAdd 4 Show)))
= appcont (Add 1 (FibAdd 3 (FibAdd 4 Show))) 0
= appcont (FibAdd 3 (FibAdd 4 Show)) 1
= fibk 1 (Add 1 (FibAdd 4 Show))
= appcont (Add 1 (FibAdd 4 Show)) 1
= appcont (FibAdd 4 Show) 2
= fibk 2 (Add 2 Show)
= fibk 1 (FibAdd 2 (Add 2 Show))
= appcont (FibAdd 2 (Add 2 Show)) 1
= fibk 0 (Add 1 (Add 2 Show))
= appcont (Add 1 (Add 2 Show)) 0
= appcont (Add 2 Show) 1
= appcont Show 3
= 3

fib 4
= fib 3 + fib 2
= (fib 2 + fib 1) + fib 2
= ((fib 1 + fib 0) + fib 1) + fib 2
= ((1 + 0) + fib 1) + fib 2
= (1 + fib 1) + fib 2
= (1 + 1) + fib 2
= 2 + fib 2
= 2 + (fib 1 + fib 0)
= 2 + (1 + 0)
= 3