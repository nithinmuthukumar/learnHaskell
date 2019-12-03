module Main where
import MazeRunner
import Collatz

spinWords :: String -> String
spinWords str = unwords [if length x<5 then x else reverse x| x<- words str ]

main :: IO ()
main = do
    putStrLn ( collatz 3)
    
