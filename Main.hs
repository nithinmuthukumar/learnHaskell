module Main where
import MazeRunner
import Collatz
import UnlimitedGameOfLife
import LongestPalindrome
import MatrixMul

spinWords :: String -> String
spinWords str = unwords [if length x<5 then x else reverse x| x<- words str ]



reverseEveryOther :: String -> String
reverseEveryOther str= unwords [if (mod i 2)/= 0 then w else reverse w | (i,w)<- zip [1..] (words str)]

grid :: [[Int]]
grid= [
    [1,0,0,1,1,1,0,1],
    [1,0,1,1,0,0,1,1],
    [1,1,0,0,1,1,0,1],
    [1,0,1,0,1,0,1,0],
    [1,1,1,1,1,0,1,0]]
main :: IO ()
main = do
    --putStrLn $ show $ getGeneration grid 1
    
    putStrLn $ show $ matMul [[1, 2],[3,4]] [[2,2],[1,1]]
    
    
