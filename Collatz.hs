module Collatz where
next :: Int -> Int
next n = if mod n 2 == 0 then n `div` 2 else 3*n+1
eval :: Int -> String
eval 1 = "->1"
eval n = "->" ++ (show n) ++ (eval . next) n
collatz :: Int -> String
collatz n = (show n) ++ eval (next n)
--ddk